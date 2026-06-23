## Some impromptu fuzzy matching for comparing street addresses.
library(stringdist)

## Returns the number (as a character string), without any letters
## on it. If the number is of the form "264-266", you get the whole
## thing.
getStreetNumber <- function(wholeAddress) {
    numberPos <- regexpr("^[-0-9]+", wholeAddress)
    len <- attr(numberPos,"match.length");
    return(list(payload=substring(wholeAddress,
                                  numberPos[1],
                                  numberPos[1] + len - 1),
                leftover=gsub("^([a-z] +| +)", "",
                              substring(wholeAddress,
                                        numberPos[1] + len))))
}

## Return street type (Street, Avenue, Lane, whatever). This is meant
## to pick it off the end, so get the unit first.
getStreetType <- function(wholeAddress) {
    abbrevs <- c("ave"="avenue",
                 "st"="street",
                 "blvd"="boulevard",
                 "ln"="lane",
                 "dr"="drive",
                 "hwy"="highway",
                 "pkwy"="parkway",
                 "way"="way",
                 "pl"="place",
                 "rd"="road",
                 "sq"="square")

    ## Grab the last word in the address.
    typePos <- regexpr("[A-z]+$", wholeAddress)
    type <- substr(wholeAddress,
                   typePos[1],
                   typePos[1] + attr(typePos,"match.length"))

    ## Is it in the abbrevs names?
    if (type %in% names(abbrevs)) {
        return(list(payload=as.character(abbrevs[type]),
                    leftover=substr(wholeAddress, 1, typePos[1] - 2)));
    } else if (sum(grepl(type, abbrevs))==1) {
        ## Or maybe it's in the abbrevs values?
        return(list(payload=as.character(abbrevs[grepl(type, abbrevs)]),
                    leftover=substr(wholeAddress, 1, typePos[1] - 2)));
    }

    ## Or maybe it's close to the abbrevs names?
    ## stringdist is a hamming distance.
    m <- min(stringdist(type, names(abbrevs)));
    if (m < nchar(type)/2) {
        return(list(payload=as.character(abbrevs[m==stringdist(type, names(abbrevs))]),
                    leftover=substr(wholeAddress, 1, typePos[1] - 2)));
    }

    ## Or close to the abbrevs values?
    m <- min(stringdist(type, abbrevs));
    if (m < nchar(type)/2) {
        return(list(payload=as.character(abbrevs[m==stringdist(type, abbrevs)]),
                    leftover=substr(wholeAddress, 1, typePos[1] - 2)));
    }

    ## Perhaps there isn't a sensible street type at all.
    return(list(payload="",leftover=wholeAddress));
}

## Only send lower case data to this function.
getUnit <- function(wholeAddress) {
    ## Currently only anticipating two different ways to specify a
    ## unit, either with the word 'unit' or with a '#'.
    unitPos <- regexpr("(unit|#) *.+$", wholeAddress)
    
    if (unitPos[[1]][1]==-1) {
        ## No unit number, but could be attached to the street number.
        unitPos <- regexec("^[0-9]+([A-z]*)", wholeAddress)
##        cat(">>>", wholeAddress,"\n"); print(attr(unitPos[[1]], "match.length"));
        if ((unitPos[[1]][1]!= -1) &
            (attr(unitPos[[1]], "match.length")[2] > 0)) {
            pstart <- unitPos[[1]][2];
            pend <- unitPos[[1]][2] + attr(unitPos[[1]], "match.length")[2]-1;
            unit <- substr(wholeAddress, pstart, pend)
                    
            return(list(payload=unit,
                        leftover=paste0(substr(wholeAddress, 1, pstart-1),
                                        substring(wholeAddress, pend + 1))))
        } else {
        
            return(list(payload="",leftover=wholeAddress));
        }
    } else {
        unit <- substring(wholeAddress, unitPos[1])

        ## Remove the unit word.
        unit <- gsub("unit *", "", unit);
        unit <- gsub("# *", "", unit)
        return(list(payload=unit,
                    leftover=substr(wholeAddress, 1, unitPos[1] - 2)));
    }
}

## Separate addresses into four component parts: number, street name,
## street type (street, lane, avenue, boulevard, etc), unit.
separateAddress <- function(wholeAddress) {
    out <- list()
    
    cleanAddress <- gsub("^(\\d+)-.*", "\\1", wholeAddress)

    ## Find (and remove) the unit number.
    tmp <- getUnit(tolower(cleanAddress))
    out[["unit"]] <- tmp$payload

    ## Find the street type in what's left over from finding the unit
    ## number.
    tmp <- getStreetType(tmp$leftover)
    out[["streetType"]] <- tmp$payload

    ## Find the street number in what's left over from the street type.
    tmp <- getStreetNumber(tmp$leftover)
    out[["streetNumber"]] <- as.numeric(tmp$payload);

    ## What's leftover is the street name. But get rid of any leftover
    ## from the street number (e.g. the 'B' left over from '34B')
    ## which we don't think is important in doing geolocation.
    out[["streetName"]] <- gsub("^([A-Z] *| *)", "", tmp$leftover);

    out[["street"]] <-
        paste(out[["streetName"]], out[["streetType"]], out[["unit"]])

    return(as_tibble(out) %>%
           select(streetNumber, street, streetName, streetType, unit));
}

## Measure a "distance" between addresses. This is basically a hamming
## distance, but using the numeric value of the address number.
compareAddresses <- function(addressOne, addressTwo, verbose=FALSE) { 

    ## convert...
    first <- separateAddress(addressOne);
    second <- separateAddress(addressTwo);

    # fail safe: if parser throws NA for any field, return 999.
    if (nrow(first) == 0 || nrow(second) == 0 || 
        is.na(first$streetNumber) || is.na(second$streetNumber) ||
        is.na(first$street)       || is.na(second$street)) {
      return(999) 
    }
    
    ## ... then compare.
    streetDist <- stringdist(first$street, second$street);
    numDist <- abs(first$streetNumber - second$streetNumber);

    if (verbose) cat("Comparing", first$street, "and", second$street,
                     "streetDist=", streetDist, "numDist=", numDist, "\n");
    
    if (streetDist <= 4) {
        if (numDist == 0) {
            return(0);
        } else {
            return(streetDist + numDist/100);
        }
    } else {
        return(streetDist + numDist/10);
    }
}


