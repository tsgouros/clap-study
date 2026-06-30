# LEAD CERTIFICATE ANALYSIS ####################################################

# Research question: What percentage of rental properties in Rhode Island and 
# Providence are compliant with lead safety laws, and are absentee landlords
# less likely to hold certificates?

# Data sources:
#   Lead_Certificates.csv: Rhode Island lead certificate registry (2002 to 2026)
#   geolocatedDf.csv: Property tax records with geocoded coordinates (2002 to 2024)
#   providence_dewey_data.csv: Dewey rental listings (2013 to 2026, not used in join)
#   NHoods: Providence neighborhood shapefiles

# Join strategy overview:
#   This script uses a three-layer classification approach.
#   Layer 1 is computed from tax data alone before any join occurs and assigns
#   rental_status to every property using the distance between the owner mailing
#   address and the property address.

#   Layer 2 expands each lead certificate into one row per active year so that
#   a temporal filter can restrict the spatial join to only years where a
#   certificate was actually in force. A 2013 certificate cannot match a 2002
#   tax record under this design.

#   Layer 3 scores each spatial match using both coordinate proximity and
#   address string agreement, deduplicates to one best match per property per
#   year, and classifies match quality.

#   The final combined status crosses Layer 1 and Layer 3 into the
#   property_lead_status field used for all downstream analysis.

# Coordinate reference system: EPSG 4269 (NAD83) is used throughout because it
# is the standard for US municipal and state data and matches the projection of
# the Providence shapefiles. All sf objects are transformed to this CRS before
# any spatial operation.

################################################################################
# SECTION 1: LIBRARIES AND DATA LOADING #####

library(tidyverse)
library(lubridate)
library(sf)
library(tidygeocoder)
library(purrr)

lead_raw <- read.csv("Lead_Certificates.csv", fileEncoding = "latin1")
tax_raw <- read.csv("geolocatedDf.csv")
pvd_dewey <- read.csv("providence_dewey_data.csv")

# Dictionaries
ordinal_dict <- c(
  "\\b1st\\b"  = "first",
  "\\b2nd\\b"  = "second",
  "\\b3rd\\b"  = "third",
  "\\b4th\\b"  = "fourth",
  "\\b5th\\b"  = "fifth",
  "\\b6th\\b"  = "sixth",
  "\\b7th\\b"  = "seventh",
  "\\b8th\\b"  = "eighth",
  "\\b9th\\b"  = "ninth",
  "\\b10th\\b" = "tenth",
  "\\b11th\\b" = "eleventh",
  "\\b12th\\b" = "twelfth",
  "\\b13th\\b" = "thirteenth",
  "\\b14th\\b" = "fourteenth",
  "\\b15th\\b" = "fifteenth"
)

type_dict<- c(
  "avenue" = "ave", "avenu" = "ave", "ave" = "ave",
  "street" = "st", "sty" = "st", "steet" = "st", "str" = "st", "stre" = "st", 
  "ste" = "st", "st" = "st",
  "boulevard" = "blvd", "blv" = "blvd", "blvd" = "blvd",
  "court" = "ct", "ct" = "ct",
  "drive" = "dr", "dr" = "dr",
  "road" = "rd", "rd" = "rd",
  "place" = "pl", "pl" = "pl",
  "lane" = "ln", "ln" = "ln",
  "terrace" = "ter", "ter" = "ter",
  "square" = "sq", "sq" = "sq",
  "parkway" = "pkwy", "pkwy" = "pkwy",
  "circle" = "cir", "cir" = "cir",
  "way" = "way"
)

################################################################################
# SECTION 2: LEAD CERTIFICATE CLEANING + EDA ####

## 2A: LEAD CLEANING ####
# String formatting:
#   - all fields are lowercased.
#   - the abbreviation "mt" becomes "mount"
#   - the street type "tr" becomes "ter"
#   - ordinal street names are standardized to words
#   - special punctuation is removed
#   - if Street.Name contains the street type, delete the street type and 
#     fill in Street.Type accordingly.
#
# Date formatting:
#   The Expiration.Date column contains a mix of real dates and plain English
#   phrases describing expiration rules. These phrases must be converted to 
#   actual dates before any analysis or temporal join can be done.
#
#   - "june 30th" or "in june" expiration is June 30 of the issue year
#   - "2 year", "interior", "dust wipe" expiration is issue date plus two years
#   - "no expiration" certificate never expires, assigned sentinel date 9999-12-31
#   - "specific project" expiration is unknown and tied to project data we do not 
#      have. These 2463 records (~1% of data) are dropped later
#      rather than imputed because the missingness is not random.
#      Everything else is parsed as a real date using mdy()

types_regex <- paste0("(?i)\\b(", paste(names(type_dict), collapse = "|"), ")$")

lead_complete <- lead_raw[complete.cases(lead_raw), ]

lead_clean <- lead_complete %>%
  mutate(
    Owner           = str_to_lower(Owner),
    Street.Name     = str_to_lower(Street.Name),
    Street.Type     = str_to_lower(Street.Type),
    Street.Name = str_replace_all(Street.Name, "\\bmt\\b", "mount"),
    Street.Type = str_replace_all(Street.Type, "\\tr\\b", "ter"),
    Street.Name = stringr::str_replace_all(Street.Name, ordinal_dict),
    Street.Name = stringr::str_remove_all(Street.Name, "[[:punct:]]")
  ) %>%
  mutate(
    existing_type = unname(type_dict[tolower(str_trim(Street.Type))]),
    is_invalid = is.na(existing_type),
    hidden_type = if_else(is_invalid, str_extract(Street.Name, types_regex), 
                          NA_character_),
    Street.Type = case_when(
      !is.na(hidden_type) ~ unname(type_dict[(hidden_type)]),
      !is_invalid ~ existing_type,
      TRUE ~ Street.Type
    ),
    Street.Name = if_else(
      !is.na(hidden_type),
      str_remove(Street.Name, paste0("(?i)\\s*\\b", hidden_type, "$")),
      Street.Name
    )
  ) %>%
  select(-existing_type, -is_invalid, -hidden_type)
 
# Date formatting
 lead_clean <- lead_clean %>% 
   mutate(
    Expiration.Date = str_squish(iconv(Expiration.Date, "latin1", "ASCII", sub = " ")),
    issue_dt        = mdy(Issue.Date, quiet = TRUE),
    ) %>%
  mutate(
    final_exp_date = case_when(
      str_detect(Expiration.Date, "(?i)june 30th|in june") ~
        make_date(year(issue_dt), 6, 30),
      str_detect(Expiration.Date, "(?i)2 year|interior|dust wipe") ~
        issue_dt + years(2),
      str_detect(Expiration.Date, "(?i)no expiration") ~
        as.Date("9999-12-31"),
      str_detect(Expiration.Date, "(?i)specific project") ~
        as.Date(NA),
      TRUE ~ mdy(Expiration.Date, quiet = TRUE)
    )
  )

## 2B: LEAD EXPLORATORY DATA ANALYSIS ####
#
# NUMBER OF UNIQUE PROPERTIES
# A property is defined as a distinct combination of street number, street name,
# street type, and city because the same street name exists across multiple
# cities and unit differences at the same address represent the same building.
# This should return approximately 16,274 per the known count.

n_unique_addresses <- lead_clean %>%
  distinct(Street.No, Street.Name, Street.Type, City.Town) %>%
  nrow()

cat("Unique properties/addresses:", n_unique_addresses, "\n")

# NUMBER OF CITIES AND TOWNS
# City.Town has already been lowercased in cleaning so counts are
# case insensitive. This is the number of distinct municipalities
# that appear in the registry across Rhode Island.

n_cities <- lead_clean %>%
  distinct(City.Town) %>%
  nrow()

cat("Distinct cities and towns:", n_cities, "\n")

# NUMBER OF UNIQUE CERTIFICATES ISSUED
# Certificate.Number is the unique identifier per issuance event.
# This counts distinct certificate numbers so renewals of the same
# certificate (same number, different issue date) are counted once.
# If you want to count total issuance events including renewals
# use nrow(lead_clean) instead.

n_unique_certs <- lead_clean %>%
  distinct(Certificate.Number) %>%
  nrow()

cat("Unique certificates issued:", n_unique_certs, "\n")

total_certificates <- nrow(lead_clean)
print(total_certificates)

# NUMBER OF DISTINCT CERTIFICATE TYPES
# The Certificate column contains the type label such as Lead Safe,
# Lead Free, Lead Safe Interior, and so on. This counts how many
# distinct types appear in the registry.

n_cert_types <- lead_clean %>%
  distinct(Certificate) %>%
  nrow()
cat("Distinct certificate types:", n_cert_types, "\n")

lead_clean %>%
  count(Certificate, sort = TRUE) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

summary_stats <- tibble(
  Metric = c(
    "Unique properties and addresses",
    "Distinct cities and towns",
    "Unique certificates issued",
    "Distinct certificate types", 
    "Total Certificates"
  ),
  Count = c(
    lead_clean %>%
      distinct(Street.No, Street.Name, Street.Type, City.Town) %>%
      nrow(),
    lead_clean %>%
      distinct(City.Town) %>%
      nrow(),
    lead_clean %>%
      distinct(Certificate.Number) %>%
      nrow(),
    lead_clean %>%
      distinct(Certificate) %>%
      nrow(),
    nrow(lead_clean)
  )
)
print(summary_stats)

################################################################################
# SECTION 3: GEOCODING LEAD CERTIFICATES (PROVIDENCE ONLY) ####

# Analysis is scoped to Providence because property tax data is available only
# for Providence addresses. Geocoding is run on distinct addresses rather than
# all rows to avoid sending duplicate addresses to the API.

# The full_address field concatenates all address components with a single space
# separator. This is the string passed to ArcGIS geocoder.

# NOTE: lead_geocoded is the output of the geocoder and should be saved to disk
# after running so that the API call does not need to be repeated on reruns.
################################################################################

lead_cert_prov <- lead_clean %>%
  filter(City.Town == "Providence") %>%
  mutate(
    full_address = paste(Street.No, Street.Name, Street.Type,
                         City.Town, State, Zip.Code, sep = " ")
  )

cat("Distinct addresses to geocode:", n_distinct(lead_cert_prov$full_address), "\n")

# lead_geocoded <- lead_cert_prov %>%
#  geocode(address = full_address, method = "arcgis",
#         lat = latitude, long = longitude)

# The above code might time out. As an alternative, the following code breaks 
# lead_cert_prov into 'chunks' of 1000 rows each and geocodes one chunk at a time.

chunk_size <- 1000
output_file <- "lead_geocoded_progress.csv"

if (file.exists(output_file)) file.remove(output_file)

n_rows <- nrow(lead_cert_prov)
chunk_indices <- split(1:n_rows, ceiling(1:n_rows / chunk_size))
num_chunks <- length(chunk_indices)

cat("Starting geocoding in", num_chunks, "chunks...\n")

for (i in seq_along(chunk_indices)) {
  cat(paste0("\n--- Processing Chunk ", i, " of ", num_chunks, 
             " (Rows ", min(chunk_indices[[i]]), "-", max(chunk_indices[[i]]), 
             ") ---\n"))
  current_chunk <- lead_cert_prov[chunk_indices[[i]], ]
  geocoded_chunk <- current_chunk %>%
    geocode(address = full_address, method = "arcgis",
            lat = latitude, long = longitude)
  
  write_csv(geocoded_chunk, output_file, append = TRUE, col_names = (i == 1))
  
  cat(paste0("Chunk ", i, " saved to disk.\n"))
  
  # Pause for 5 seconds between chunks to give the ArcGIS server a little break.
  Sys.sleep(5) 
}

# lead_geocoded <- read_csv(output_file, fileEncoding = "CP1252")

cat("Geocoding complete. Rows with missing coordinates:",
    sum(is.na(lead_geocoded$latitude) & is.na(lead_geocoded$longitude)), "\n")

# Uncomment to re-load csv file.
# write.csv(lead_geocoded, "lead_geocoded.csv", row.names = FALSE)

summary(lead_geocoded)

################################################################################
# SECTION 4: VISUALIZATION - ACTIVE CERTIFICATES OVER TIME ####

# To plot how many certificates were active in each year the data must be
# expanded from one row per certificate into one row per certificate per active
# year. A certificate issued in 2005 expiring in 2007 becomes three rows:
# 2005, 2006, and 2007.

# Certificates with sentinel expiration 9999-12-31 are capped at 2027 to
# prevent inflating counts beyond the data coverage period.

# Project based certificates with NA expiration are excluded here as well
# because we cannot determine their active window.

# The line chart shows all RI cities in gray and highlights the top five by
# peak active certificate count. Vertical dashed lines mark the 2023 rental
# registry law and 2025 for reference.
################################################################################

active_certs <- lead_clean %>%
  select(Certificate.Number, City.Town, issue_dt, final_exp_date) %>%
  filter(!is.na(final_exp_date)) %>%
  mutate(
    issue_yr = year(issue_dt),
    exp_yr   = year(final_exp_date),
    exp_yr   = pmin(exp_yr, 2027)
  ) %>%
  rowwise() %>%
  mutate(year = list(seq(issue_yr, exp_yr))) %>%
  unnest(year) %>%
  group_by(year, City.Town) %>%
  summarise(active_certificates = n(), .groups = "drop")

top5 <- active_certs %>%
  group_by(City.Town) %>%
  summarise(peak = max(active_certificates)) %>%
  slice_max(peak, n = 5) %>%
  pull(City.Town)
print(top5)

total_certs_per_town <- active_certs %>%
  group_by(City.Town) %>%
  summarise(total = sum(active_certificates)) %>%
  arrange(desc(total))
print(total_certs_per_town)

total_by_year <- active_certs %>%
  group_by(year) %>%
  summarise(total = sum(active_certificates)) %>%
  arrange(desc(year))
print(total_by_year)

histogram <- ggplot(total_by_year, aes(x = year, y = total)) +
  geom_col(fill = "#0868ac") +
  scale_x_continuous(
    breaks = seq(min(total_by_year$year),
                 max(total_by_year$year),
                 by = 1)
  ) +
  labs(
    title = "Total Active Certificates by Year",
    x = "Year",
    y = "Total Active Certificates"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank())
histogram

total_by_year <- active_certs %>%
  filter(City.Town != "Providence") %>%
  group_by(year) %>%
  summarise(total = sum(active_certificates)) %>%
  arrange(desc(year))
print(total_by_year)

histogram_nopvd <- ggplot(total_by_year, aes(x = year, y = total)) +
  geom_col(fill = "#0868ac") +
  scale_x_continuous(
    breaks = seq(min(total_by_year$year),
                 max(total_by_year$year),
                 by = 1)
  ) +
  labs(
    title = "Total Active Certificates by Year",
    x = "Year",
    y = "Total Active Certificates"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank())
histogram_nopvd

for_graph <- active_certs %>%
  mutate(highlight = ifelse(City.Town %in% top5, City.Town, "Other"))

graph_certs_over_time <- ggplot(
  for_graph,
  aes(x = year, y = active_certificates,
      color = reorder(City.Town, -active_certificates))) +
  geom_line(
    data = filter(for_graph, highlight == "Other"),
    aes(group = City.Town),
    linewidth = 0.4, alpha = 0.4, color = "#e0e0e0"
  ) +
  geom_line(
    data = filter(for_graph, highlight != "Other"),
    aes(group = highlight),
    color = "white", linewidth = 1.5
  ) +
  geom_line(
    data = filter(for_graph, highlight != "Other"),
    linewidth = 1, alpha = 1
  ) +
  scale_color_manual(values = c(
    "Providence" = "#0868ac",
    "Pawtucket" = "#43a2ca",
    "Woonsocket" = "#7bccc4",
    "Central Falls" = "#a8ddb5",
    "Cranston" = "#ccebe1"
  )) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(2002, 2026, by = 2)) +
  scale_y_continuous(expand = c(0, 1)) +
  geom_vline(xintercept = 2023, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Providence consistenly leads in total registered and active lead certificates since 2002",
    subtitle = "The yearly aggregated totals of active lead certficates across Rhode Island cities/towns from 2002-2027",
    x = "",
    y = "Active certificates",
    caption = "Note: The dashed line represents the year the rental registry law went into effect",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed",
                                      color = "lightgrey", linewidth = 0.2),
    axis.text = element_text(size = 12),
    axis.ticks.x = element_line(color = "lightgrey", linewidth = 0.4),
    axis.ticks.length.x = unit(5, "pt"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )
graph_certs_over_time

################################################################################
# SECTION 5: TAX DATA CLEANING AND RENTAL CLASSIFICATION (LAYER 1) ####

# Layer 1 is the rental status classification derived entirely from the tax data
# before any join with the lead registry. It must be computed here and never
# touched again so that it cannot be affected by join artifacts.

# Classification logic:
#   owner_occupied: owner mailing address is within 30 meters of property.
#                   These properties may include 2 to 3 family homes where the
#                   owner occupies one unit and rents others. They are tracked
#                   but excluded from the compliance denominator because the
#                   lead law exempts owner occupied single families.
#   suspected_rental: owner address is between 30 and 80000 meters away (roughly
#                     50 miles). Owner is local but does not live on site.
#   absentee_landlord: owner address is more than 80000 meters away indicating an
#               out of state or corporate landlord. These are the absentee
#               landlord cases DeeAnn asked about.

# lead_law_relevant flags the properties that belong in the compliance
# denominator: suspected rentals and absentee owners. Owner occupied are excluded
# unless unit level data later confirms multi family rental status.

# Missingness note: rows missing either property or owner coordinates cannot be
# classified and are dropped. The counts below document what was lost.
################################################################################

cat("Missing ownerLat:", sum(is.na(tax_raw$ownerLat)), "\n")
cat("Missing propertyLat:", sum(is.na(tax_raw$propertyLat)), "\n")
cat("Missing both:", sum(is.na(tax_raw$ownerLat) & is.na(tax_raw$propertyLat)), "\n")

tax_clean <- tax_raw %>%
  filter(!is.na(ownerLat) & !is.na(ownerLong) & !is.na(propertyLat) & !is.na(propertyLong))

cat("Tax data rows after coordinate filter:", nrow(tax_clean), "\n")

props_sf  <- st_as_sf(tax_clean,
                      coords = c("propertyLong", "propertyLat"),
                      crs    = st_crs("EPSG:4269"))

owners_sf <- st_as_sf(tax_clean,
                      coords = c("ownerLong", "ownerLat"),
                      crs    = st_crs("EPSG:4269"))

tax_clean <- tax_clean %>%
  mutate(
    propertyAddress = str_replace_all(propertyAddress, "\\btr\\b", "ter"),
    propertyCompositeAddress = str_replace_all(propertyCompositeAddress, 
                                               "\\btr\\b", "ter"),
    propertyAddress = str_replace_all(propertyAddress, "\\bmt\\b", "mount"),
    propertyCompositeAddress = str_replace_all(propertyCompositeAddress, 
                                               "\\bmt\\b", "mount"),
    propertyCompositeAddress = str_replace_all(propertyCompositeAddress, 
                                               " 2(\\d{3})", " 02\\1"),
    dist_meters = as.numeric(st_distance(props_sf, owners_sf, by_element = TRUE)),
    dist_miles  = dist_meters / 1609.34
  )

tax_classified <- tax_clean %>%
  mutate(
    rental_status = case_when(
      dist_meters <= 30 ~ "owner_occupied",
      dist_meters > 30 & dist_meters <= 80000  ~ "suspected_rental",
      dist_meters > 80000 ~ "absentee_landlord",
      TRUE ~ "unknown"
    ),
    lead_law_relevant = rental_status %in% c("suspected_rental", "absentee_landlord")
  )

# FULL PANEL DISTRIBUTION (includes all year by year observations)
# Each property appears once per year it exists in the tax records
# so these counts are inflated by the number of years observed.
# This is the correct denominator for the year by year compliance join
# but not for reporting how many unique properties exist in each category.

cat("Layer 1 rental status distribution (all years, inflated):\n")
print(
  tax_classified %>%
    count(rental_status) %>%
    mutate(pct = round(n / sum(n) * 100, 1))
)

# UNIQUE PROPERTY DISTRIBUTION
# Collapse to one row per unique address to get the true count of
# distinct properties in each rental status category.
# When a property appears in multiple years its rental_status can
# technically vary if the owner address changed year to year.
# We take the most recent year observation as the authoritative
# classification since it reflects the current ownership structure.

tax_classified_unique <- tax_classified %>%
  arrange(desc(year)) %>%
  distinct(propertyCompositeAddress, .keep_all = TRUE)

cat("\nLayer 1 rental status distribution (unique properties only):\n")
print(
  tax_classified_unique %>%
    count(rental_status) %>%
    mutate(pct = round(n / sum(n) * 100, 1))
)

cat("\nTotal unique properties:", nrow(tax_classified_unique), "\n")

# SIDE BY SIDE COMPARISON TABLE
# Shows the inflation factor for each category so you can report
# the correct number in the memo while understanding why the
# panel counts are larger

comparison_table <- tax_classified %>%
  count(rental_status, name = "panel_count") %>%
  left_join(
    tax_classified_unique %>%
      count(rental_status, name = "unique_count"),
    by = "rental_status"
  ) %>%
  mutate(
    panel_pct = round(panel_count / sum(panel_count) * 100, 1),
    unique_pct = round(unique_count / sum(unique_count) * 100, 1),
    avg_years_observed = round(panel_count / unique_count, 1)
  ) %>%
  arrange(desc(unique_count))

cat("\nPanel vs unique property counts by rental status:\n")
print(comparison_table)

################################################################################
# SECTION 6: LEAD CERTIFICATE DEDUPLICATION AND TEMPORAL EXPANSION (LAYER 2) ####
#
# Three issues were found in the raw lead registry data that must be resolved
# before the temporal expansion and spatial join can produce accurate results.
#
# Issue 1: True duplicate rows
# Some rows are complete duplicates where every single field is identical.
# These are data entry errors and are safe to drop. The deduplication uses
# distinct() with no arguments which requires ALL fields to match before
# treating two rows as the same record. This is deliberately stricter than
# deduplicating on Certificate.Number alone or on Certificate.Number plus
# issue_dt, either of which would silently destroy real data in the cases
# described below.
#
# Issue 2: Renewals misidentified as duplicates
# The registry reuses the same Certificate.Number across renewal cycles.
# A certificate first issued in 2008 and renewed in 2010 appears as two rows
# with the same Certificate.Number but different issue dates and expiration
# dates. These are not duplicates. Each renewal row represents a separate
# active window and must be kept so that the temporal expansion produces the
# correct coverage years for each cycle independently.
# Deduplicating on Certificate.Number alone would collapse all renewals into
# one row and erase years of compliance history for that property.
#
# Issue 3: Same cert number and issue date but differing fields
# A third pattern exists where Certificate.Number and issue_dt match but
# Unit, Certificate type, or another field differs. These are likely separate
# unit level certificates for a multi unit building where the registry used
# the same number for each unit on the same date. They are also not duplicates
# and must be preserved. Deduplicating on Certificate.Number plus issue_dt
# would drop them incorrectly.
#
# The diagnostic below measures how many rows each approach would remove so
# the correct strategy can be confirmed before any data is dropped.
#
# Temporal expansion:
# After deduplication each row is expanded into one row per active year.
# A certificate issued in 2010 expiring in 2012 becomes three rows with
# active_year values 2010, 2011, and 2012. The spatial join in Section 7
# matches each expanded cert row only to tax records from the same year so
# a certificate not yet issued or already expired cannot influence the join.
# Certificates with NA expiration (project based) are excluded because their
# active window cannot be determined. Sentinel dates of 9999-12-31 are capped
# at 2027 to match the lead registry coverage period.
################################################################################

# STEP 1: Diagnostic to confirm which deduplication definition is correct
# Run these counts and compare before committing to a drop strategy

n_two_field <- nrow(lead_geocoded) - nrow(distinct(lead_geocoded, Certificate.Number, issue_dt))
n_all_field <- nrow(lead_geocoded) - nrow(distinct(lead_geocoded))

cat("Rows flagged as duplicate by cert number + issue date only:", n_two_field, "\n")
cat("Rows flagged as duplicate when ALL fields must match:      ", n_all_field, "\n")
cat("Difference (same cert+date but other fields differ, NOT duplicates):",
    n_two_field - n_all_field, "\n")

# STEP 2: Inspect cases where cert number and issue date match
# but at least one other field differs so we know what we would be losing
# under the less safe two field approach
partial_match_check <- lead_geocoded %>%
  group_by(Certificate.Number, issue_dt) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  group_by(Certificate.Number, issue_dt) %>%
  summarise(
    n_rows         = n(),
    unit_distinct  = n_distinct(Unit),
    cert_distinct  = n_distinct(Certificate),
    exp_distinct   = n_distinct(final_exp_date),
    owner_distinct = n_distinct(Owner),
    .groups        = "drop"
  ) %>%
  filter(
    unit_distinct > 1 | cert_distinct > 1 |
      exp_distinct  > 1 | owner_distinct > 1
  )
partial_match_check
cat("Groups where cert+date match but other fields differ:", nrow(partial_match_check), "\n")
cat("These rows would be incorrectly dropped by the two field approach\n")

# STEP 3: Remove only complete duplicates where every field is identical
# If n_two_field == n_all_field from Step 1 the two approaches produce the
# same result for this dataset. If they differ the all field approach is the
# only safe one and the difference count tells you how many real records the
# two field approach would have silently destroyed.

lead_no_true_dups <- lead_geocoded %>%
  distinct(.keep_all = TRUE)

cat("Rows removed as complete duplicates (all fields identical):",
    nrow(lead_geocoded) - nrow(lead_no_true_dups), "\n")
cat("Rows retained after deduplication:", nrow(lead_no_true_dups), "\n")

# STEP 4: Confirm renewals survived the deduplication
# Renewals share a Certificate.Number but differ in at least issue_dt and
# final_exp_date so distinct() on all fields correctly keeps them.
# This check verifies that certificates with multiple rows still have multiple
# rows after deduplication meaning their renewal history is intact.

renewal_survivors <- lead_no_true_dups %>%
  group_by(Certificate.Number) %>%
  filter(n() > 1) %>%
  select(Certificate.Number, Unit, Certificate, issue_dt, final_exp_date) %>%
  arrange(Certificate.Number, issue_dt)

cat("Certificate numbers with more than one row after dedup (renewals intact):",
    n_distinct(renewal_survivors$Certificate.Number), "\n")

# STEP 5: Temporal expansion into one row per active year
# Each row in lead_no_true_dups becomes N rows where N is the number of years
# the certificate was active. Renewal rows each expand independently so a
# certificate renewed three times produces three separate sets of active year
# rows covering each renewal window. Overlapping windows between a renewal
# issued before the prior one expired are resolved in Section 8 by the
# per address per year deduplication which keeps only the most recent issue.

#beginning of the lead certificate should match with the year of the property tax record (fiscal year)
#second check, address with also owner 
lead_cert_expanded <- lead_no_true_dups %>%
  filter(!is.na(final_exp_date), !is.na(latitude), !is.na(longitude)) %>%
  mutate(
    issue_yr = year(issue_dt),
    exp_yr   = pmin(year(final_exp_date), 2026)
  ) %>%
  rowwise() %>%
  mutate(active_year = list(seq(issue_yr, exp_yr))) %>%
  unnest(active_year) %>%
  ungroup()

cat("Expanded lead cert rows (one per certificate per active year):",
    nrow(lead_cert_expanded), "\n")

lead_sf_expanded <- lead_cert_expanded %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("EPSG:4269"))

tax_sf_classified <- tax_classified %>%
  mutate(property_year = as.integer(year)) %>%
  st_as_sf(coords = c("propertyLong", "propertyLat"), crs = st_crs("EPSG:4269"))

################################################################################
# 6B: RENEWAL RATE ANALYSIS ####

# This section is separate from the join pipeline and produces no object used
# downstream. It answers DeeAnn's question about whether properties are staying
# in the lead registry over time or dropping out after their first certificate. 
# What is the renewal rate among the properties? when it reaches its expiration date,
# is there a new issuance in the following year?

################################################################################
# For each certificate, the renewal question is:
# when this cert expired, did a new active window start within a reasonable
# buffer period? That is the only moment where a real renewal decision happened.

# Get one row per certificate per expiration event
# Each row in lead_no_true_dups already represents one issuance window
# so exp_yr is the year the renewal decision was due

cert_windows <- lead_no_true_dups %>%
  filter(!is.na(final_exp_date), !is.na(issue_dt)) %>%
  mutate(
    issue_yr = year(issue_dt),
    exp_yr   = pmin(year(final_exp_date), 2027)
  ) %>%
  select(Certificate.Number, Owner, Street.No, Street.Name,
         Street.Type, City.Town, issue_dt, issue_yr,
         final_exp_date, exp_yr) %>%
  distinct() %>%
  arrange(Certificate.Number, issue_dt)

# For each certificate window, look ahead to see if the same
# certificate number has another window starting within 2 years of expiration
# That is the renewal: the cert expired and a new window opened promptly
# days_gap may be too lenient can change
cert_windows_lead <- cert_windows %>%
  group_by(Certificate.Number) %>%
  mutate(
    next_issue_dt = lead(issue_dt),
    days_gap = as.numeric(difftime(next_issue_dt, final_exp_date, units = "days")),
    was_renewed = !is.na(next_issue_dt) &
      days_gap <= 365 &
      days_gap >= -730,
    not_renewed_within_buffer = !was_renewed
  )%>%
  ungroup()

# Aggregate at the expiration year level
# Only look at windows that have already expired so we are not penalizing
# certificates that are still currently active

renewal_by_exp_yr <- cert_windows_lead %>%
  filter(exp_yr <= 2027) %>%
  group_by(exp_yr) %>%
  summarise(
    total_expired = n(),
    renewed = sum(was_renewed, na.rm = TRUE),
    not_renewed = sum(not_renewed_within_buffer, na.rm = TRUE),
    renewal_rate = round(renewed / total_expired * 100, 1),
    not_renewed_rate = round(not_renewed / total_expired * 100, 1)
  ) %>%
  arrange(exp_yr)

print(renewal_by_exp_yr)

# renewal rate at the 2 year expiration point
graph_renewal_2yr <- ggplot(
  renewal_by_exp_yr,
  aes(x = exp_yr, y = renewal_rate)
) +
  geom_line(linewidth = 1, color = "#0868ac") +
  geom_point(size = 2,    color = "#0868ac") +
  geom_vline(xintercept = 2023, linetype = "dashed", alpha = 0.5) +
  annotate("text", x = 2023.2, y = 5,
           label = "Rental registry law",
           hjust = 0, size = 3, color = "gray40") +
  scale_x_continuous(breaks = seq(2004, 2026, by = 2)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title    = "Certificate renewal rate at expiration (2 year cycle)",
    subtitle = "Of certificates expiring in year X, what percentage opened a new window within 1 year",
    x        = "Year certificate expired",
    y        = "Renewal rate (%)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed",
                                      color = "lightgrey", linewidth = 0.2),
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )
graph_renewal_2yr

# Diagnosing the early years
# Confirm early year certificate issuance: if 2002 to 2005 have far fewer certificates
# than 2006 onwards the high early renewal rate is a small denominator artifact
cert_windows %>%
  count(issue_yr) %>%
  arrange(issue_yr) %>%
  print()

###
# Is the early high renewal rate driven by a small group of repeat renewers
# rather than broad compliance across all early certificates?

cert_windows_lead %>%
  filter(exp_yr <= 2007) %>%
  group_by(Certificate.Number) %>%
  summarise(
    n_windows    = n(),
    was_renewed  = any(was_renewed, na.rm = TRUE),
    owner        = first(Owner),
    address      = first(paste(Street.No, Street.Name, Street.Type)),
    .groups      = "drop"
  ) %>%
  arrange(desc(n_windows)) %>%
  print(n = 20)

# Also check: are the early renewers the same properties renewing repeatedly
# versus many different properties each renewing once?

cert_windows_lead %>%
  filter(exp_yr <= 2005, was_renewed == TRUE) %>%
  count(Street.No, Street.Name, Street.Type, City.Town, sort = TRUE) %>%
  print(n = 20)


################################################################################
## 6C: ADDRESS CHECK ####

# Each address in the lead registry should match to an address in the property 
# tax records. To check this, lead and tax are joined, with lead as the left 
# table.

max_tax_yr <- max(tax_sf_classified$property_year, na.rm = TRUE)
lead_check <- lead_sf_expanded %>%
   filter(active_year <= max_tax_yr)

lead_years <- sort(unique(lead_sf_expanded$active_year))

lead_tax_join <- map_dfr(lead_years, function(yr) {
  lead_yr <- lead_check  %>% filter(active_year == yr)
  tax_yr  <- tax_sf_classified %>% filter(property_year == yr)
  
  if (nrow(lead_yr) == 0 || nrow(tax_yr) == 0) {
    return(lead_yr)
  }
  
  st_join(
    lead_yr,
    tax_yr %>% select(propertyCompositeAddress, propertyDesc),
    join = st_is_within_distance,
    dist = 45
  )
})

# What percentage of lead addresses have no match within 45 meters?
cbind(
  count = table(is.na(lead_tax_join$propertyCompositeAddress)),
  pct = round(prop.table(table(is.na(lead_tax_join$propertyCompositeAddress)))*100, 1)
)

# With the lead-tax spatial join, there are still lead addresses that are missing 
# matches.
# For each lead address, how far is the nearest match?
#
# force_match: 
#   Matches each row of lead to the nearest entry in tax, regardless of how far.
#   Produces a table where every address has one match.
#   Creates a new column, matched_distance, for the distance between the matched 
#   lead and tax addresses.

force_match <- map_dfr(lead_years, function(yr) {
  lead_yr <- lead_check  %>% filter(active_year == yr)
  tax_yr  <- tax_sf_classified %>%
    filter(property_year == yr) %>%
    select(propertyCompositeAddress, property_year)

  if (nrow(lead_yr) == 0 || nrow(tax_yr) == 0) {
    return(lead_yr)
  }

  nearest_idx <- st_nearest_feature(lead_yr, tax_yr)
  distances <- st_distance(lead_yr, tax_yr[nearest_idx, ], by_element = TRUE)
  matched_data <- bind_cols(
    lead_yr,
    st_drop_geometry(tax_yr)[nearest_idx, ]
  ) %>%
    mutate(matched_distance = as.numeric(distances))

  return(matched_data)
})

# force_match safety check: how many addresses in lead had no match in tax?
# Output should be 100% false
cbind(
 count = table(is.na(force_match$propertyCompositeAddress)),
  pct = round(prop.table(table(is.na(force_match$propertyCompositeAddress)))*100, 1)
)

# print summary statistics for matched_distance
quantile(force_match$matched_distance,
         probs = c(0.25, 0.5, 0.75, 0.90, 0.95, 0.99),
         na.rm = TRUE)

################################################################################
# SECTION 7: YEAR BY YEAR SPATIAL JOIN (LAYER 3) ####
#
# The join loops over each year present in the tax data. In each iteration both
# datasets are filtered to that year before the spatial join is run. This means
# a certificate that was not active in year Y cannot match a tax record from Y
# regardless of how close the coordinates are.

# A spatial join is conducted on tax (tax_sf_classified) and lead 
# (lead_sf_expanded) so that all lead properties that fall within a certain 
# radius of the given tax property are returned.
# Unmatched rows are separated from the matched ones in order to preserve them.
# For each matched row, the two tax and lead addresses are compared. The match is
# kept only if 1) the address_dist is 0 (exact match) or 2) the address_dist is
# <= 1.02 (house numbers differ by 2 plus allowance of 1 string character diff)
# AND the property is a 2-5 family property.
#
# NULL is returned for years where either dataset slice is empty so that map_dfr
# can skip them cleanly.
################################################################################

source("address.r")

clean_address <- function(addr) {
  if (is.null(addr)) return(addr)
  addr %>%
    stringr::str_remove_all("\\bnull\\b") %>%
    stringr::str_remove("\\b(n|s|e|w|ne|nw|se|sw|north|south|east|west)$") %>%
    stringr::str_replace("^(\\d+)[-/]\\d+", "\\1") %>%
    stringr::str_squish()
}

years_to_join <- sort(unique(tax_sf_classified$property_year))
cat("Years to join:", paste(range(years_to_join), collapse = " to "), "\n")

# The threshold is 47m because 90% of lead addresses have their nearest match 
# within this distance, as found in Address Check.
DISTANCE_THRESHOLD <- 47

joined_by_year <- map_dfr(years_to_join, function(yr) {
  
  tax_yr  <- tax_sf_classified %>% filter(property_year == yr)
  lead_yr <- lead_sf_expanded  %>% filter(active_year  == yr)

  if (nrow(tax_yr) == 0 || nrow(lead_yr) == 0) {
    message("Skipping year ", yr, ": one or both slices are empty")
    return(NULL)
  }
  
  message("Joining year ", yr,
          " | tax rows: ", nrow(tax_yr),
          " | lead rows: ", nrow(lead_yr))
  
  spatial_join <- st_join(
    tax_yr,
    lead_yr %>% select(
      Certificate.Number, Certificate, active_year, issue_yr, exp_yr, 
      issue_dt, final_exp_date,
      full_address, Street.No, Street.Name, Street.Type, Unit
    ),
    join = st_is_within_distance,
    dist = DISTANCE_THRESHOLD
  ) %>%
    mutate(join_year = yr)
  
  unmatched <- spatial_join %>% filter(is.na(Certificate.Number))
  matched   <- spatial_join %>% filter(!is.na(Certificate.Number))
  
  message("  -> Spatial Join Results | Matched: ", nrow(matched), 
          " rows | Unmatched: ", nrow(unmatched), " rows")
  
  if (nrow(matched) > 0) {
    best_match <- matched %>%
      tidyr::unite(
        col = "constructed_lead_address", 
        Street.No, Street.Name, Street.Type, 
        sep = " ", 
        na.rm = TRUE,    
        remove = FALSE    
      ) %>%
      mutate(
        clean_tax_addr = clean_address(propertyAddress),
        clean_lead_addr = clean_address(constructed_lead_address)
      ) %>%
      rowwise() %>%
      mutate(
        address_dist = compareNumName(clean_tax_addr, clean_lead_addr)
      ) %>%
      ungroup() %>%
      
      # Core logic block 
      mutate(
        valid_match = (address_dist == 0) | 
          (address_dist <= 1.02 & tolower(propertyDesc) == "2-5 family"),
        mismatch = !valid_match,
        
        across(
          c(Certificate.Number, Certificate, active_year, issue_yr, exp_yr, 
            full_address, Street.No, Street.Name, Street.Type, Unit),
          ~ ifelse(mismatch, NA, .)
        )
      ) %>%
      group_by(platLotUnit) %>%
      filter(!mismatch | all(mismatch)) %>%
      ungroup() %>%
      
      distinct(platLotUnit, Certificate.Number, .keep_all = TRUE)
    
  } else {
    best_match <- matched
  }
  
  yr_slice <- bind_rows(best_match, unmatched)
  
  message("  -> Summary for ", yr, ":\n",
          "     - Final table size: ", nrow(yr_slice), " rows\n",
          "     - Target baseline was: >= ", nrow(tax_yr), " rows\n")
  
  return(yr_slice)
})

cat("Total rows after year-by-year execution loop:", nrow(joined_by_year), "\n")

# Missingness check
cbind(
  count = table(is.na(joined_by_year$Certificate.Number)),
  pct = round(prop.table(table(is.na(joined_by_year$Certificate.Number))) * 100, 1)
)

## Single year tax-lead join, for debugging ####

# test_yr <- 2020
# 
# tax_yr  <- tax_sf_classified %>% filter(property_year == test_yr)
# lead_yr <- lead_sf_expanded  %>% filter(active_year  == test_yr)
# 
# test_lead_join <- function(distance_threshold) {
#   
#   cat("\n========================================\n")
#   cat("RUNNING TEST: Dist =", distance_threshold, "m\n")
#   cat("========================================\n")
#   
#   cat("Running spatial join...\n")
#   tspatial_join <- st_join(
#     tax_yr,
#     lead_yr %>% select(
#       Certificate.Number, Certificate, active_year, issue_yr, exp_yr, 
#       full_address, Street.No, Street.Name, Street.Type, Unit
#     ),
#     join = st_is_within_distance,
#     dist = distance_threshold
#   ) %>%
#     mutate(join_year = test_yr)
#   
#   # Separate matched and unmatched rows.
#   unmatched <- tspatial_join %>% filter(is.na(Certificate.Number))
#   matched   <- tspatial_join %>% filter(!is.na(Certificate.Number))
#   
#   cat("Spatial Join Results:\n")
#   cat(" - Matched:", nrow(matched), "rows\n")
#   cat(" - Unmatched:", nrow(unmatched), "rows\n")
#   
#   # For matched rows, compare addresses.
#   if (nrow(matched) > 0) {
#     cat("Running address comparisons...\n")
#     best_match <- matched %>%
#       tidyr::unite(
#         col = "constructed_lead_address", 
#         Street.No, Street.Name, Street.Type, 
#         sep = " ", 
#         na.rm = TRUE,    
#         remove = FALSE    
#       ) %>%
#       mutate(
#         clean_tax_addr = clean_address(propertyAddress),
#         clean_lead_addr = clean_address(constructed_lead_address)
#       ) %>%
#       rowwise() %>%
#       mutate(
#         address_dist = compareNumName(clean_tax_addr, clean_lead_addr)
#         ) %>%
#       ungroup() %>%
#       
#       # Logic block for address scores
#       mutate(
#         valid_match = (address_dist == 0) | 
#           (address_dist <= 1.02 & tolower(propertyDesc) == "2-5 family"),
#         mismatch = !valid_match,
#         
#         across(
#           c(Certificate.Number, Certificate, active_year, issue_yr, exp_yr, 
#             full_address, Street.No, Street.Name, Street.Type, Unit),
#           ~ ifelse(mismatch, NA, .)
#         )
#       ) %>%
#       group_by(platLotUnit) %>%
#       filter(!mismatch| all(mismatch)) %>%
#       ungroup() %>%
#       
#       # Shrink duplicate NA rows for a single property down to one row
#       distinct(platLotUnit, Certificate.Number, .keep_all = TRUE)
#     
#   } else {
#     best_match <- matched
#   }
#   
#   # Recombine matched and unmatched into one final dataset
#   final_dataset <- bind_rows(best_match, unmatched) %>%
#     select(
#       propertyAddress, 
#       constructed_lead_address,
#       propertyDesc,
#       address_dist,
#       Certificate.Number
#     )
#   
#   cat("Final Results:\n")
#   cat(" - Original Tax Baseline:", nrow(tax_yr), "rows\n")
#   cat(" - Final Output Rows:", nrow(final_dataset), "rows\n")
#   
#   return(final_dataset)
# 
# }
# 
# # TESTS
# # Test 1: 45m 
# result_45 <- test_lead_join(distance_threshold = 45)
# 
# # Test 2: 20m 
# # result_20 <- test_lead_join(distance_threshold = 20)
# 
# # Test 3: 100m 
# result_100 <- test_lead_join(distance_threshold = 100)


################################################################################
# SECTION 8: RENTAL COMPLIANCE ANALYSIS ####

# Now after the two datasets have been temporally joined, 
# can attempt to move forward through mapping and investigating the initial 
# mismatch issue based on the graphs 
################################################################################

rentals <- joined_by_year %>% filter(lead_law_relevant)
n_distinct(rentals$platLotUnit)

# Q: How many properties are in the lead registry at all, over the entire time span?

property_baseline <- rentals %>%
  group_by(platLotUnit) %>%
  summarise(
    total_years_observed = n(),
    total_certs = n_distinct(Certificate.Number, na.rm = TRUE),
    ever_registered = total_certs > 0 
  ) %>%
  ungroup()

cbind(
  count = table(property_baseline$ever_registered),
  pct = round(prop.table(table(property_baseline$ever_registered))*100, 1)
)

# A: 33.2% of rental properties have at least one certificate. 66.8% have never 
# registered.

# Q: How many properties have an unbroken chain of renewals since first filing?

# Get one row per unique certificate window per property
property_cert_windows <- rentals %>%
  filter(!is.na(Certificate.Number)) %>%
  distinct(platLotUnit, Certificate.Number, issue_dt, final_exp_date) %>%
  arrange(platLotUnit, issue_dt)

# Calculate the gaps and evaluate consistent renewal
property_renewals <- property_cert_windows %>%
  group_by(platLotUnit) %>%
  mutate(
    next_issue_dt = lead(issue_dt),
    
    days_gap = as.numeric(difftime(next_issue_dt, final_exp_date, units = "days")),
    
    # Valid if renewed within 1 year after expiration, or up to 2 years early
    was_renewed = !is.na(next_issue_dt) & days_gap <= 365 & days_gap >= -730
  ) %>%

    summarise(
    # How many times did a certificate expire and require a renewal?
    renewal_opportunities = sum(!is.na(next_issue_dt)),
    
    # How many times did they successfully renew within the buffer?
    successful_renewals = sum(was_renewed, na.rm = TRUE),
    
    # TRUE if they renewed on time, every single time. 
    # FALSE if they missed a deadline. 
    # NA if they only ever had 1 certificate (no renewal needed yet).
    consistently_renewed = case_when(
      renewal_opportunities == 0 ~ NA, 
      TRUE ~ (renewal_opportunities == successful_renewals)
    )
  ) %>%
  ungroup()

cbind(
  count = table(property_renewals$consistently_renewed),
  pct = round(prop.table(table(property_renewals$consistently_renewed))*100, 1)
)

# A: 30.2% of properties have consistently renewed since first registering. 

## 8B: MATCH QUALITY and MAPS ####
# This was the inital code used to map the joined datset to get a picture as to 
# where the certificates are concentrated to the properties in the property 
# tax data

# Including it as an example code

# '''
# neighborhoods <- st_read("NHoods") %>%
#   st_transform(crs = st_crs("EPSG:4269"))
# 
# pvd_bbox <- st_bbox(neighborhoods)
# 
# geo_unique_points <- tax_classified %>%
#   distinct(propertyLong, propertyLat, .keep_all = TRUE) %>%
#   st_as_sf(coords = c("propertyLong", "propertyLat"),
#            crs    = st_crs("EPSG:4269"))
# 
# lead_unique_points <- lead_geocoded %>%
#   distinct(longitude, latitude, .keep_all = TRUE) %>%
#   filter(!is.na(longitude), !is.na(latitude)) %>%
#   st_as_sf(coords = c("longitude", "latitude"),
#            crs    = st_crs("EPSG:4269"))
# 
# map_pvd <- ggplot() +
#   geom_sf(data = neighborhoods, fill = "gray95", color = "white") +
#   geom_sf(data = geo_unique_points, alpha = 0.3, color = "steelblue",  size = 0.1) +
#   geom_sf(data = lead_unique_points, alpha = 0.3, color = "darkorange", size = 0.1) +
#   coord_sf(
#     xlim   = c(pvd_bbox["xmin"], pvd_bbox["xmax"]),
#     ylim   = c(pvd_bbox["ymin"], pvd_bbox["ymax"]),
#     expand = FALSE
#   ) +
#   theme_void() +
#   labs(title = "Visual inspection: tax properties vs lead certificates (Providence)")
# 
# map_pvd
# 
# map_neighborhood <- function(hood_names, index = 1, output_dir = "neighborhood_maps") {
#   
#   if (index > length(hood_names)) {
#     message("All ", length(hood_names), " maps saved to '", output_dir, "/'")
#     return(invisible(NULL))
#   }
#   
#   hood_name   <- hood_names[[index]]
#   target_hood <- neighborhoods %>% filter(LNAME == hood_name)
#   
#   if (nrow(target_hood) == 0) {
#     message("Skipping '", hood_name, "': not found in neighborhood layer")
#     return(map_neighborhood(hood_names, index + 1, output_dir))
#   }
#   
#   geo_hood <- tryCatch(
#     st_filter(geo_unique_points,  target_hood),
#     error = function(e) { message("geo filter failed: ", e$message); NULL }
#   )
#   lead_hood <- tryCatch(
#     st_filter(lead_unique_points, target_hood),
#     error = function(e) { message("lead filter failed: ", e$message); NULL }
#   )
#   
#   p <- ggplot() +
#     geom_sf(data = target_hood, fill = "gray95", color = "black", linewidth = 1)
#   
#   if (!is.null(geo_hood)  && nrow(geo_hood)  > 0)
#     p <- p + geom_sf(data = geo_hood,
#                      aes(color = "Property Tax"), alpha = 0.6, size = 2)
#   
#   if (!is.null(lead_hood) && nrow(lead_hood) > 0)
#     p <- p + geom_sf(data = lead_hood,
#                      aes(color = "Lead Cert"), alpha = 0.6, size = 1.5)
#   
#   p <- p +
#     scale_color_manual(
#       name   = "Layer",
#       values = c("Property Tax" = "steelblue", "Lead Cert" = "darkorange")
#     ) +
#     theme_void() +
#     theme(
#       plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
#       plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
#       legend.position = "bottom"
#     ) +
#     labs(
#       title    = hood_name,
#       subtitle = paste0(
#         "Tax properties: ", if (!is.null(geo_hood))  nrow(geo_hood)  else 0, "  |  ",
#         "Lead certs: ",     if (!is.null(lead_hood)) nrow(lead_hood) else 0
#       )
#     )
#   
#   safe_name <- gsub("[^A-Za-z0-9_]", "_", hood_name)
#   out_path  <- file.path(output_dir, paste0(safe_name, ".png"))
#   ggsave(out_path, plot = p, width = 8, height = 6, dpi = 150, bg = "white")
#   message("Saved: ", out_path)
#   
#   map_neighborhood(hood_names, index + 1, output_dir)
# }
# 
# output_dir <- "neighborhood_maps"
# dir.create(output_dir, showWarnings = FALSE)
# 
# all_hoods <- neighborhoods %>%
#   st_drop_geometry() %>%
#   pull(LNAME) %>%
#   unique() %>%
#   sort()
# 
# map_neighborhood(all_hoods, output_dir = output_dir)
# '''