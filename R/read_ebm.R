#' Read data in the Embla Data Format.
#'
#' This function reads the data stored in an EBM file. This data
#' consists of, e.g., physiologic signals.
#'
#' @param datapath Either the full path to a directory containing
#' EBM-files or the full path to a particular EBM file. If a directory
#' is given, it is assumed that all these files are from the same
#' recording session
#' @param channels A string or a list of strings with channel names
#' (typically the same name as the EBM file names). If the
#' \code{datapath} argument is a directory, these strings are used to
#' filter files from the directory.
#' @param start The offset in seconds from the beginning of the file to start reading data.
#' @param data.length The amount of data in seconds to be read.
#' @param header.only Boolean denoting whether to only read the headers in the EBM file. Default is FALSE.
#'
#' @return The data in the EBM file(s) as a list.
#'
#' @examples
#' \dontrun{
#' # Read all EBM files in a directory
#' ebm <- read.ebm("/tmp/datapath")
#'
#' # Read one particular EBM file
#' ebm <- read.ebm("/tmp/datapath/ECG.ebm")
#'
#' # Read some channels
#' ebm <- read.ebm("/tmp/datapath", channels = c("ECG", "Fz", "Cz"))
#'
#' # Read 20 seconds of data, starting 10 seconds after the beginning of the file:
#' ebm <- read.ebm("/tmp/datapath/ECG.ebm", start = 10, data.length = 20)
#'
#' # Only read the header
#' ebm <- read.ebm("/tmp/datapath/ECG.ebm", header.only = TRUE)
#' }
#'
#' @export
read.ebm <- function(datapath, channels = NULL, start = 0, data.length = NULL, header.only = FALSE) {
    ## Input is directory: read (i) all or (ii) selected channels
    if (file.info(datapath)$isdir) {
        if (is.null(channels))
            file_list <- get_data_files(datapath)
        else
            file_list <- unlist(lapply(channels, function(i) get_data_files(datapath, i)))
    } else {
        ## Input is a single file, so read only this
        if (file.exists(datapath))
            file_list <- datapath
        else
            stop("File not found. Cannot continue.")
    }

    if (length(file_list) < 1)
        stop("No files found. Cannot continue.")

    ## Read the header of one of the files
    tmp    <- read.ebm.single(file_list[1], header.only = TRUE)
    header <- tmp$header

    ## Create a recording template
    recording <- new_recording()

    ## --- start and stop times and data length
    recording$properties$time.start.raw <- header$EBM_R_TIME
    recording$properties$time.stop.raw  <- NA

    recording$properties$time.start     <- as.POSIXct(strptime(header$EBM_R_TIME, format = "%Y%m%dT%H%M%OS")) + start

    if (is.null(data.length))
        recording$properties$time.stop <- recording$properties$time.start + unlist(header$data_length)
    else
        recording$properties$time.stop <- recording$properties$time.start + unlist(data.length)

    if ((start == 0) & (is.null(data.length)))
        recording$properties$length <- header$data_length
    if ((start > 0) & (is.null(data.length)))
        recording$properties$length <- header$data_length - start
    if ((start >= 0) & (! is.null(data.length)))
        recording$properties$length <- data.length

    ## --- general properties
    recording$properties$subject        <- header$EBM_R_SUBJECT_NAME
    recording$properties$format         <- "EBM"
    recording$properties$format.long    <- "Embla Data Format"
    recording$properties$device.type    <- "Generic"
    recording$properties$device.version <- NA

    recording$header.signal             <- list()
    recording$header.raw                <- header

    ## Read selected channels and store the data
    if (! header.only) {
        for (f in file_list) {
            tmp     <- read.ebm.single(f, start = start, data.length = data.length, header.only = FALSE)
            channel <- create_variable_name(tmp$header$EBM_R_CHANNEL_NAME)

            if (length(channel) == 0) {
            	warning(sprintf("Channel has zero length for file: %s", f))
            	next # no data, no channel?
            }
            
            recording$header.signal[[channel]]       <- tmp$header

            recording$signal[[channel]]$data         <- tmp$signal$data
            recording$signal[[channel]]$t            <- tmp$signal$t #seq.int(0, (length(tmp$data) - 1)) / (tmp$header$EBM_R_SAMPLING_RATE)

            recording$signal[[channel]]$samplingrate <- tmp$header$EBM_R_SAMPLING_RATE
            recording$signal[[channel]]$unit         <- tmp$header$EBM_R_CALIBRATE_UNIT

        }
    }

    recording
}


#' Read data in Embla Data Format from a single file.
#'
#' This function reads the data stored in an EBM file. This data
#' consists of, e.g., physiologic signals.
#'
#' @param datafile The full path to a particular EBM file.
#' @param start The offset in seconds from the beginning of the file to start reading data.
#' @param data.length The amount of data in seconds to be read.
#' @param header.only Boolean denoting whether to only read the headers in the EBM file. Default is FALSE.
#'
#' @return The data in the EBM file as a list.
#'
#' @keywords internal
read.ebm.single <- function(datafile, start = 0, data.length = NULL, header.only = FALSE) {
    ## Define constants
    EBM_RAWDATA_HEADER      <- 'Embla data file'
    EBM_RESULTS_HEADER      <- 'Embla results file'

    EBM_UNKNOWN_DATASIZE    <- 'ffffffff'

    EBM_END_OF_SIGNATURE    <- '1a'
    EBM_MAX_SIGNATURE_SIZE  <- 80
    EBM_MAC_ENDIAN          <- 'ff'
    EBM_INTEL_ENDIAN        <- '00'

    EBM_R_DATA              <- '20'
    EBM_R_TIME              <- '84'

    endian                  <- "big"

    ## Container for header
    header <- list()

    ## Open the file for reading
    f <- file(datafile, "rb")

    ## Read the signature to determine file type and endianness
    signature <- vector(mode = "raw", length = EBM_MAX_SIGNATURE_SIZE)

    i <- 1
    keep.reading <- TRUE
    while (keep.reading == TRUE) {
        temp.sigdata <- readBin(f, "raw", size = 1, n = 1, signed = FALSE, endian = endian)
        if (length(temp.sigdata) == 0) {
        	warning(sprintf("Zero length read trying to get signature. Early EOF? (%s)", datafile))
        	break
        }
        signature[i] <- temp.sigdata
        if (temp.sigdata == EBM_END_OF_SIGNATURE) break

        i <- i + 1
    }
    
    header$data_header <- readBin(signature[-i], character())
    header$endian      <- readBin(f, "raw", size = 1, n = 32, signed = FALSE, endian = endian)
    ebmvertmp          <- paste(header$endian[2:6], collapse = "")

    ## -- file type
    if (header$data_header== EBM_RAWDATA_HEADER)
        header$filetype <-"data"
    else if (header$data_header== EBM_RESULTS_HEADER)
        header$filetype <- "results"
    else
        header$filetype <- "unknown"

    ## -- endianness
    if (header$endian[1] == EBM_MAC_ENDIAN)
        header$endian <- "big"
    else if (header$endian[1] == EBM_INTEL_ENDIAN)
        header$endian <- "little"

    ## -- EBM version
    if (ebmvertmp == "ffffffffff") {
        header$ebm_version <- ">= 4.0"
        header$id_rec_size <- 4
    } else {
        header$ebm_version <- "< 4.0"
        header$id_rec_size <- 1
    }

    ## Read the EBM file
    header$time_start  <- list()
    header$time_stop   <- list()
    header$data_length <- list()

    signal             <- list()

    data_id <- 1

    while (length(rec_id  <- readBin(f, "integer", size = header$id_rec_size, n = 1, endian = header$endian)) > 0) {
        rec_size      <- readBin(f, "integer", size = 4, n = 1, endian = header$endian)

        rec_id_string <- get_ebm_record_type(dec2hex(rec_id))

        if (! rec_id_string %in% c( "unknown", "EBM_R_DATA")) {
            rec_data_raw            <- readBin(f, "raw", size = 1, n = rec_size, endian = header$endian)
            header[[rec_id_string]] <- parse_ebm_record(rec_id_string, rec_data_raw, endian = header$endian)
        }

        if (rec_id_string == "unknown")
            seek(f, where = seek(f) + rec_size, origin = "start")


        if (rec_id_string == "EBM_R_TIME")
            current_time <- header[[rec_id_string]]


        if (rec_id_string == "EBM_R_DATA") {
            header$time_start[[data_id]]  <- as.POSIXct(strptime(current_time, format = "%Y%m%dT%H%M%OS"))
            header$data_length[[data_id]] <- rec_size / 2 / header$EBM_R_SAMPLING_RATE
            header$time_stop[[data_id]]   <- header$time_start[[data_id]] + header$data_length[[data_id]]

            if (! header.only)
                signal[[data_id]] <- readBin(f, "integer", size = 2, n = rec_size/2, endian = header$endian, signed = TRUE)
            else
                seek(f, where = seek(f) + rec_size, origin = "start")

            data_id <- data_id + 1
        }

    }

    ## close the file
    close(f)
    

    ## --------------------------------------------------
    ## Do not continue if only the header is read
    ## --------------------------------------------------    
    if (header.only)
        return(list("header" = header, "data" = NULL))
    ## --------------------------------------------------    

    if (length(signal) == 0) {
    	warning(sprintf("Zero length signal in file: %s", datafile))
    	return(list("header" = header, "data" = NULL))
    }

    ## scale the data
    if (header$EBM_R_CALIBRATE_UNIT == "V")
        if ("EBM_R_UNIT_GAIN" %in% names(header))
            for (i in seq.int(length(signal)))
                signal[[i]] <- signal[[i]] * header$EBM_R_UNIT_GAIN


    ## create a time vector for the signal
    time_vector <- list() #vector(mode = "list", length = length(signal))
    data_vector <- list()
    t_start     <- vector(mode = "list", length = length(signal))
    t_stop      <- vector(mode = "list", length = length(signal))

    for (i in seq.int(length(signal))) {
        t_start[[i]] <- as.numeric(difftime(header$time_start[[i]], header$time_start[[1]], units = "secs"))
        t_stop[[i]]  <- as.numeric(difftime(header$time_stop[[i]],  header$time_start[[i]]), units = "secs") + t_start[[i]]

        if (i == 1) {
            time_vector[[i]] <- t_start[[i]] + seq.int(0, (length(signal[[i]]) - 1)) / header$EBM_R_SAMPLING_RATE
            data_vector[[i]] <- signal[[i]]
        } else {
            ## create filler time and data vector
            time_vector[[2 * (i - 1)]] <- seq.int(from = t_stop[[i - 1]], to = t_start[[i]], by = (1 / header$EBM_R_SAMPLING_RATE))
            data_vector[[2 * (i - 1)]] <- rep(NA, length(time_vector[[2 * (i - 1)]]))

            time_vector[[2 * (i - 1) + 1]] <- t_start[[i]] + seq.int(0, (length(signal[[i]]) - 1)) / header$EBM_R_SAMPLING_RATE
            data_vector[[2 * (i - 1) + 1]] <- signal[[i]]
        }
    }

    ## Combine the time and data vectors
    signal      <- list()
    signal$t    <- do.call(c, time_vector)
    signal$data <- do.call(c, data_vector)

    ## Extract a part of the signal
    if (start > 0)
        ind_start <- which.min(signal$t < start)
    else
        ind_start <- 1
    if (! is.null(data.length))
        ind_stop <- which.min(signal$t < (start + data.length))
    else
        ind_stop <- length(signal$t)

    if ((ind_start != 1) | (! is.null(data.length))) {
        signal$t    <- signal$t[ind_start : ind_stop]
        signal$data <- signal$data[ind_start : ind_stop]
    }

    ## Return EBM
    list("header" = header, "signal" = signal)
}
