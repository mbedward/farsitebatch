#' Convert RAWS weather data to FARSITE 4 format
#'
#' The command line version of FARSITE can accept a single set of hourly weather
#' data in RAWS (Remote Automated Weather Station) format or, alternatively,
#' separate sets of summary weather data (daily values for temperature, humidity
#' and rainfall) and wind data (hourly values of speed and direction plus cloud
#' cover). The Windows version of FARSITE only accepts weather data in the
#' separate sets format. This function reads a file of RAWS format hourly data
#' and writes two separate files of summary and wind data.
#'
#' Note that, for some reason, FARSITE expects integer values for all variables
#' when the data are provided as separate daily summary and hourly wind files,
#' but, from inspection of the C++ source code, it accepts floating point values
#' for temperature when the data are in RAWS format (for some reason, rainfall
#' and humidity are read as integers).
#'
#' @param rawsfile Path to a RAWS format data file. This is allowed to have
#'   comment lines (first character \code{'#'}), e.g. for variable names. It may
#'   also have a non-comment header line for units, specified by a character
#'   string: either 'metric' or 'english' (case is ignored). Data records should
#'   have space-delimited values as follows: year, month, day, time (24hr
#'   format, e.g. '1500'), temperature, relative humidity, rainfall, wind speed,
#'   wind direction, cloud cover.
#'
#' @param wtrfile Path for the output daily summary data file. FARSITE expects a
#'   '.wtr' extension.
#'
#' @param wndfile Path for the output hourly wind data file. FARSITE expects a
#'   '.wnd' extension.
#'
#' @param elev A single integer value for elevation that will be written to each
#'   record of the daily summary (.wtr) file.
#'
#' @param units Measurement units: either 'metric' (default) or 'english'. Case
#'   is ignored. Ignored if there is a header line in the RAWS input file
#'   specifying units.
#'
#' @param varnames.as.comment If TRUE, write variable names as a comment line in
#'   each output file.
#'
#' @importFrom dplyr %>% do group_by ungroup
#'
#' @export
#'
raws_to_farsite <- function(rawsfile, wtrfile, wndfile, elev,
                            units = c("metric", "english"),
                            varnames.as.comment = FALSE) {

  buf <- readLines(rawsfile)

  units <- match.arg(tolower(units), c("metric", "english"))

  # Remove any comment lines and blank lines
  buf <- buf %>%
    stringr::str_subset("^\\s*[^\\#]") %>%
    stringr::str_subset("[^\\s]")

  # Check for header line with units
  if (stringr::str_detect(buf[1], "[A-Za-z]")) {
    b <- tolower(buf[1])
    if (str_detect(b, "metric")) units <- "metric"
    else if (str_detect(b, "english")) units <- "english"
    else stop("Invalid specifier for units: ", buf[1], ". Should be metric or english")

    buf <- buf[-1]
  }

  units <- toupper(units)

  # Read data records
  raws.names <- c("year", "month", "day", "hour", "temp", "relhum", "rain", "windspeed", "winddir", "cloud")
  dat.raws <- read.table(textConnection(buf), header = FALSE)
  if (ncol(dat.raws) != length(raws.names)) {
    stop("Expected ", length(raws.names), " columns in RAWS file")
  }
  colnames(dat.raws) <- raws.names

  # Daily summary data for temperature, humidity and rainfall:
  # Month, Day, Rain, MinTempHour, MaxTempHour, MinTemp, MaxTemp, MaxHumid, MinHumid, Elev, RainStartTime, RainEndTime
  dat.wtr <- dat.raws %>%
    group_by(year, month, day) %>%

    do({
      t <- .$temp
      tmin <- min(t)
      tmax <- max(t)
      hour.tmin <- .$hour[which(t == tmin)]
      hour.tmax <- .$hour[which(t == tmax)]

      rain.total <- round(sum(.$rain))

      if (rain.total > 0) {
        h <- .$hour
        h[.$rain <= 0] <- NA
        hour.rain.start <- min(h, na.rm = TRUE)
        hour.rain.end <- max(h, na.rm = TRUE)
      } else {
        hour.rain.start <- 0
        hour.rain.end <- 0
      }

      data.frame(rain.total,
                 hour.tmin = .format_time24(hour.tmin),
                 hour.tmax = .format_time24(hour.tmax),
                 tmin = round(tmin),
                 tmax = round(tmax),
                 relhum.max = round(max(.$relhum)),
                 relhum.min = round(min(.$relhum)),
                 elev,
                 hour.rain.start = .format_time24(hour.rain.start),
                 hour.rain.end = .format_time24(hour.rain.end),

                 stringsAsFactors = FALSE)
    }) %>%

    ungroup() %>%
    dplyr::select(-year)

  .write_with_header(header = units, dat = dat.wtr,
                     filename = wtrfile, sep = " ",
                     varnames.as.comment = varnames.as.comment)


  # Detailed data for wind and cloud cover
  dat.wnd <- dat.raws %>%
    dplyr::select(month, day, hour, windspeed, winddir, cloud) %>%

    dplyr::mutate(
      hour = .format_time24(hour),
      windspeed = round(windspeed),
      winddir = round(winddir) %% 360,
      cloud = round(cloud)
    )

  .write_with_header(header = units, dat = dat.wnd,
                     filename = wndfile, sep = " ",
                     varnames.as.comment = varnames.as.comment)

}


.write_with_header <- function(header, dat, filename,
                               sep = " ",
                               varnames.as.comment = FALSE) {
  buf <- character(0)
  con <- textConnection("buf", open = "w", local = TRUE)

  write.table(dat, con,
              quote = FALSE, sep = sep,
              row.names = FALSE, col.names = FALSE)

  close(con)

  if (varnames.as.comment) {
    comment <- paste("#", paste(colnames(dat), collapse = " "))
    buf <- c(comment, header, buf)
  } else {
    buf <- c(header, buf)
  }

  writeLines(buf, con = filename)
}


.format_time24 <- function(time.int) {
  sprintf("%04d", time.int)
}

