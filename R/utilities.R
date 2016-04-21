#' Return a list of EBM signal files.
#'
#' @param datadir The directory containing the EBM files.
#' @param pattern A pattern used to find a signal file.
#'
#' @return The data files as a list.
#'
#' @keywords internal
get_data_files <- function(datadir, pattern = NULL) {
    if (is.null(pattern))
        datafiles <- list.files(datadir, pattern = "*\\.ebm", full.names = TRUE, ignore.case = TRUE)
    else
        datafiles <- list.files(datadir, pattern = paste(pattern, "*\\.ebm", sep = ""), full.names = TRUE, ignore.case = TRUE)

    datafiles
}


#' Convert a hexadecimal string to decimal.
#'
#' @param x A hexadecimal string.
#'
#' @return A decimal number.
#'
#' @keywords internal
hex2dec <- function(x) {
    strtoi(paste("0x", x, sep = ""))
}


#' Convert a decimal number to a hexadecimal string.
#'
#' @param x A decimal number.
#'
#' @return A hexadecimal string.
#'
#' @keywords internal
dec2hex <- function(x) {
    sprintf("%x", x)
}


#' A list of known record fields in the EBM file.
#'
#' @param x A hexadecimal string (without "0x").x
#'
#' @return A string describing the header record.
#'
#' @keywords internal
get_ebm_record_type <- function(x) {

    records <- list(
        "30" = "EBM_R_DATA_GUID",

        "80" = "EBM_R_VERSION",
        "81" = "EBM_R_HEADER",
        "84" = "EBM_R_TIME",
        "85" = "EBM_R_CHANNEL",
        "86" = "EBM_R_SAMPLING_RATE",
        "87" = "EBM_R_UNIT_GAIN",
        "88" = "EBM_R_SESSION_COUNT",

        "90" = "EBM_R_CHANNEL_NAME",
        "95" = "EBM_R_DATA_MASK16",
        "96" = "EBM_R_SIGNED_DATA",
        "98" = "EBM_R_CALIBRATE_FUNC",
        "99" = "EBM_R_CALIBRATE_UNIT",

        "a0" = "EBM_R_EVENT",

        "c0" = "EBM_R_SERIALNUMBER",
        "c1" = "EBM_R_DEVICETYPE",

        "d0" = "EBM_R_SUBJECT_NAME",
        "d1" = "EBM_R_SUBJECT_ID",
        "d2" = "EBM_R_SUBJECT_GROUP",
        "d3" = "EBM_R_SUBJECT_ATTENDANT",

        "ff" = "EBM_R_INVALID",
        "20" = "EBM_R_DATA"
        )



    if (x %in% names(records))
        rec_type <- records[[x]]
    else rec_type <- "unknown"

    rec_type
}


#' Parse an EBM record, given the record id as a string and the raw record data.
#'
#' @param record A string describing the record.
#' @param data The raw bytes contained in the record.
#' @param endian The endianness. Defaults to big.
#'
#' @return The parsed record data (number of string).
#'
#' @keywords internal
parse_ebm_record <- function(record, data, endian = "big") {

    if (record == "EBM_R_DATA_GUID") {
        out <- readBin(data, character())
    }
    if (record == "EBM_R_VERSION") {
        minor <- readBin(data[1], integer(), size = 1, n = 1, endian = endian)
        major <- readBin(data[2], integer(), size = 1, n = 1, endian = endian)
        out <- paste(major, ".", minor, sep = "")
    }
    if (record == "EBM_R_HEADER") {
        out <- readBin(data, character(), endian = endian)
    }
    if (record == "EBM_R_TIME") {
        year    <- readBin(data[1:2], integer(), size = 2, n = 1, endian = endian)
        month   <- readBin(data[3], integer(), size = 1, n = 1, endian = endian)
        day     <- readBin(data[4], integer(), size = 1, n = 1, endian = endian)
        hour    <- readBin(data[5], integer(), size = 1, n = 1, endian = endian)
        minute  <- readBin(data[6], integer(), size = 1, n = 1, endian = endian)
        second  <- readBin(data[7], integer(), size = 1, n = 1, endian = endian)
        sec100  <- readBin(data[8], integer(), size = 1, n = 1, endian = endian)

        out <- sprintf("%d%02d%02dT%02d%02d%02d.%02d", year, month, day, hour, minute, second, sec100)
        
    }
    if (record == "EBM_R_CHANNEL") {
        out <- readBin(data, integer(), endian = endian)
    }
    if (record == "EBM_R_SAMPLING_RATE") {
        out <- (readBin(data, integer(), endian = endian) / 1000)
    }
    if (record == "EBM_R_UNIT_GAIN") {
        out <- (readBin(data, integer(), endian = endian) / 1000)
    }
    if (record == "EBM_R_SESSION_COUNT") {
        out <- readBin(data, integer(), endian = endian)
    }
    if (record == "EBM_R_CHANNEL_NAME") {
        out <- readBin(data, character(), endian = endian)
    }
    if (record == "EBM_R_DATA_MASK16") {
        out <- dec2hex(readBin(data, integer(), endian = endian))
    }
    if (record == "EBM_R_SIGNED_DATA") {
        out <- readBin(data, character(), endian = endian)
    }
    if (record == "EBM_R_CALIBRATE_FUNC") {
        out <- readBin(data, character(), endian = endian)
    }
    if (record == "EBM_R_CALIBRATE_UNIT") {
        out <- readBin(data, character(), endian = endian)
    }
    if (record == "EBM_R_EVENT") {
    }
    if (record == "EBM_R_SERIALNUMBER") {
        out <- readBin(data, character(), endian = endian)
    }
    if (record == "EBM_R_DEVICETYPE") {
        out <- readBin(data, character(), endian = endian)
    }
    if (record == "EBM_R_SUBJECT_NAME") {
        out <- readBin(data, character(), endian = endian)
    }
    if (record == "EBM_R_SUBJECT_ID") {
        out <- readBin(data, character(), endian = endian)
    }
    if (record == "EBM_R_SUBJECT_GROUP")  {
        out <- readBin(data, character(), endian = endian)
    }
    if (record == "EBM_R_SUBJECT_ATTENDANT") {
        out <- readBin(data, character(), endian = endian)
    }
    if (record == "EBM_R_INVALID") {
        ## do nothing
    }
    if (record == "EBM_R_DATA") {
        ## do nothing
    }

    ## If no data is found, return an empty string
    if (length(out) == 0)
        out <- ""

    if (is.character(out))
        out <-  gsub("^\\s+|\\s+$", "", out) 

    out
}


#' Create variable name from string.
#'
#' This function creates a sensible variable name by replacing
#' all non-alphanumeric characters with underscores. This is useful
#' when the string is to be used as a the name of a list element.
#'
#' @param s The string to be used as a variable name
#' @return The string with non-alphanumeric characters replaced by underscores
#' @keywords internal
create_variable_name <- function(s) {
    gsub("[^[:alnum:]]", "_", s)
}


#' #' Return and initialise an empty recording structure.
#' The recording structure is a list.
#'
#' @return An empty recording structure.
#'
#' @family recording
#' 
#' @export
new_recording <- function() {
    ## Create containers
    recording            <- list()
    recording$properties <- list()
    recording$signal     <- list()

    recording$properties$time.start.raw <- NA
    recording$properties$time.start     <- NA
    recording$properties$time.stop.raw  <- NA
    recording$properties$time.stop      <- NA

    ## Set subject and casename information
    recording$properties$subject        <- NA

    ## Information on the data format
    recording$properties$format         <- NA
    recording$properties$format.long    <- NA
    recording$properties$device.type    <- NA
    recording$properties$device.version <- NA

    ## The length of the recording in seconds
    recording$properties$length         <- NA

    recording
}

