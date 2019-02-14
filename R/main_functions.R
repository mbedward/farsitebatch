#' Create a FARSITE input file
#'
#' This function creates the contents for a FARSITE input file from a template
#' (package file \code{extdata/input_template.txt}) and a set of values for
#' variable components. Note that you should specify either the path to a file
#' of RAWS-format weather data (via \code{path.raws}) or the paths to separate
#' weather and wind files (\code{path.wtr} and \code{path.wnd}).
#'
#' @param start.time Character string for start date and time, e.g. '08 28 0000'.
#'
#' @param end.time Character string for end date and time, e.g. '08 29 2359'.
#'
#' @param path.fms Path to a file of tabular data for fuel moisture in the
#'   six-column format expected by FARSITE. Note, only the data records should
#'   be included in the file without any header information.
#'
#' @param weather.units system of measurement for weather variables: either 'METRIC' or 'ENGLISH'.
#'
#' @param path.raws Path to a file of tabular RAWS weather data in the
#'   ten-column format expected by FARSITE. Note, only the data records should
#'   be included in the file without any header information.
#'
#' @param raws.elev Elevation of weather station that provided the RAWS data.
#'   This is ignored if path.raws is missing or \code{NULL} and weather data is provided as
#'   separate files for daily summary data (\code{path.wtr}) and wind data (\code{path.wnd}).
#'
#' @param path.wtr Path to a file of daily summary data for temperature,
#'   rainfall and humidity in the format expected by FARSITE. Ignored if
#'   \code{path.raws} is specified. Any comment lines or header lines are
#'   ignored. Units ("METRIC" or "ENGLISH") should be specified via the
#'   \code{weather.units} parameter.
#'
#' @param path.wtr Path to a file of hourly data for wind speed, wind direction
#'   and cloud cover in the format expected by FARSITE. Ignored if
#'   \code{path.raws} is specified. Any comment lines or header lines are
#'   ignored. Units ("METRIC" or "ENGLISH") should be specified via the
#'   \code{weather.units} parameter.
#'
#' @param path.fmd Optional path to a custom fuels file.
#'
#' @param path.ros.adj Optional path to the ROS adjustment file.
#'
#' @return Contents of the input file as a single character string.
#'
#' @export
#'
create_input_file <- function(start.time,
                              end.time,
                              path.fms,
                              weather.units,
                              path.raws,
                              raws.elev,
                              path.wtr = NULL,
                              path.wnd = NULL,
                              path.fmd = NULL,
                              path.ros.adj = NULL) {

  # Read a file of tabular dat, drop any blank lines, comment lines or head lines
  # (specifying 'METRIC' or 'ENGLISH') and return count of data records and the
  # data as a character string.
  #
  .read_file <- function(path) {
    x <- readLines(path)

    x <- stringr::str_trim(x)
    x <- x[x != ""]

    comment <- stringr::str_detect(x, "^\\s*\\#")
    x <- x[!comment]

    header <- stringr::str_detect(tolower(x), "metric|english")
    x <- x[!header]

    n <- length(x)

    x <- c(x, "")
    list(n = as.character(n), text = paste(x, collapse = "\n"))
  }


  # This is used when there is no file to read
  EmptyFile <- list(n = "", text = "")


  f <- system.file("extdata", "input_template.txt", package = "farsitebatch")
  txt <- readLines(con = f)
  txt <- paste(txt, collapse = "\n")

  fms <- .read_file(path.fms)

  if (is.null(path.ros.adj)) path.ros.adj <- ""

  if (is.null(path.fmd)) {
    fmd.switch <- "# "
    fmd.file <- ""
  } else {
    fmd.switch <- ""
    fmd.file <- path.fmd
  }

  # check if we have RAWS weather data or separate weather (wtr) and wind (wnd) files
  if (missing(path.raws) || .is_empty_parameter(path.raws)) {
    # No path.raws so we need path.wtr and path.wnd
    if (missing(path.wtr) || .is_empty_parameter(path.wtr) ||
        missing(path.wnd) || .is_empty_parameter(path.wnd)) {

      stop("You must provide either path.raws or both of path.wnd and path.wtr")
    }

    wtr.switch <- ""
    wtr <- .read_file(path.wtr)
    wnd <- .read_file(path.wnd)
    wtr.units <- weather.units
    wnd.units <- weather.units

    raws.switch <- "#"
    raws <- EmptyFile
    raws.units <- ""
    raws.elev <- ""

  } else {
    # Using RAWS data

    raws.switch <- ""
    raws <- .read_file(path.raws)
    raws.units <- weather.units

    wtr.switch <- "#"
    wtr <- EmptyFile
    wnd <- EmptyFile
    wtr.units <- ""
    wnd.units <- ""
  }



  # Note: glue takes the following from the function environment:
  # start.time, end.time,
  # raws.elev, raws.units, wtr.units, wnd.units
  # fmd.switch, fmd.
  input <- glue::glue(txt,
                      num.fms = fms$n,
                      data.fms = fms$text,
                      num.raws = raws$n,
                      data.raws = raws$text,
                      num.wtr = wtr$n,
                      data.wtr = wtr$text,
                      num.wnd = wnd$n,
                      data.wnd = wnd$text,
                      ros.adj.file = path.ros.adj)

  input
}


#' Create an ignition point shapefile
#'
#' FARSITE requires a polygon shapefile for ignition location. This function
#' takes the coordinates for a point, creates a square polygon around it
#' of specified width, and saves this as a shapefile.
#'
#' @param path Path for the output shapefile.
#'
#' @param xy Two element vector with X and Y ordinates of ignition location.
#'
#' @param width Width of the polygon to create.
#'
#' @param crs Optional coordinate reference sytem (e.g. EPSG integer code).
#'
#' @return A spatial data frame (\code{sf} object) with the ignition polygon.
#'
#' @export
#'
create_ignition_polygon <- function(path, xy, width, crs = sf::NA_crs_) {
  stopifnot(length(xy) == 2)

  dw <- width/2

  pts <- c(xy + c(-dw, -dw),
           xy + c(-dw,  dw),
           xy + c( dw,  dw),
           xy + c( dw, -dw),
           xy + c(-dw, -dw) )

  pts <- matrix(pts, ncol=2, byrow = TRUE, dimnames = list(NULL, c("x", "y")))

  mp <- sf::st_multipolygon(list(list(pts)))
  geometry <- sf::st_sfc(mp, crs = crs)
  df <- sf::st_sf(id = 1, geometry)

  df
}


#' Run a FARSITE simulation
#'
#' @param lcp Full path to the landscape file.
#' @param input Full path to the input file.
#' @param ignition Full path to the ignition shapefile.
#' @param outdir Full path to the directory for output files.
#' @param prefix Prefix to use for output file names.
#' @param cmd System command to run FARSITE.
#'
#' @return An integer indicating success (zero) or failure (non-zero). 127
#'   indicates that FARSITE could not be run for some reason. 124 indicates
#'   that the simulation timed out.
#'
#' @export
#'
run_farsite <- function(lcp, input, ignition,
                        outdir, prefix = "sim",
                        cmd = "farsite") {

  args <- format_run_args(lcp = lcp, input = input, ignition = ignition, outdir, prefix)
  runfile <- tempfile(pattern = "farsite_", fileext = ".run")
  cat(args, file = runfile)

  system(paste(cmd, runfile))
}


#' Format FARSITE run command arguments
#'
#' @param lcp Full path to the landscape file.
#' @param input Full path to the input file.
#' @param ignition Full path to the ignition shapefile.
#' @param outdir Full path to the directory for output files.
#' @param prefix Prefix to use for output file names.
#'
#' @return FARSITE run command arguments as a single character string.
#'
#' @export
#'
format_run_args <- function(lcp, input, ignition, outdir, prefix) {
  out <- file.path(outdir, prefix)
  paste(lcp, input, ignition, "0", out, "0")
}


#' Get the final fire area
#'
#' This function determines the final fire area from the raster of fire
#' intensity generated by FARSITE. The precision of the area estimate will
#' depend on the cell resolution specified in the FARSITE input file.
#'
#' @param outdir Full path to the directory containing output files.
#' @param prefix Prefix used for the output file names.
#'
#' @return Fire area in user-defined map units (e.g. square metres).
#'
#' @export
#'
get_fire_area <- function(outdir, prefix) {
  fname <- paste0(prefix, "_Intensity.asc")
  r <- raster(file.path(outdir, fname))

  ncells <- cellStats(r>0, "sum")
  cellarea <- prod(res(r))

  ncells * cellarea
}


#' Get the area burnt by surface, passive-crown or active-crown fire
#'
#' This function determines the area burnt by each of three categories of fire:
#' surface fire; passive crown fire; and active crown fire. The areas are
#' calculated from crown fire raster generated by FARSITE. The precision of the
#' area estimate will depend on the cell resolution specified in the FARSITE
#' input file.
#'
#' @param outdir Full path to the directory containing output files.
#' @param prefix Prefix used for the output file names.
#'
#' @return A named vector of areas ('surface', 'passive', 'active') in
#'   user-defined map units (e.g. square metres).
#'
#' @export
#'
get_firetype_area <- function(outdir, prefix) {
  fname <- paste0(prefix, "_CrownFire.asc")
  r <- raster(file.path(outdir, fname))

  # 1: surface fire
  n1 <- cellStats(r == 1, "sum")

  # 2: passive crown fire
  n2 <- cellStats(r == 2, "sum")

  # 3: active crown fire
  n3 <- cellStats(r == 3, "sum")

  cellarea <- prod(res(r))

  c(surface = n1*cellarea, passive = n2*cellarea, active = n3*cellarea)
}


#' Summarize fire intensity
#'
#' This function summarizes fire intensity by calculating the area of each of a
#' set of intensity classes, based on the raster of fire intensity generated by
#' FARSITE. The precision of the area estimate will depend on the cell
#' resolution specified in the FARSITE input file.
#'
#' @param outdir Full path to the directory containing output files.
#' @param prefix Prefix used for the output file names.
#'
#' @param breaks A vector of breaks to define intensity classes. Classes are
#'   treated as left-open / right-closed.
#'
#' @return A named list with elements: 'breaks' and 'area'. Area values are
#'   expressed in user-defined map units (e.g. square metres).
#'
#' @export
#'
get_intensity_hist <- function(outdir, prefix,
                               breaks = c(0, 350, 1700, 3500, Inf)) {

  fname <- paste0(prefix, "_Intensity.asc")
  r <- raster(file.path(outdir, fname))

  h <- hist(r, maxpixels = ncell(r), breaks = breaks, right = TRUE, plot = FALSE)

  cellarea <- prod(res(r))

  list(breaks = breaks, area = h$counts * cellarea)
}


#' Detect whether a fire reached given locations
#'
#' This is a convenience function to test if a fire reached one or more target locations
#' represented by a set of point coordinates. Each point is tested by checking the
#' FARSITE fire arrival time raster at that location.
#'
#' @param outdir Full path to the directory containing output files.
#' @param prefix Prefix used for the output file names.
#'
#' @param pts A matrix or data frame of point coordinates (X-Y order). The units
#'   and coordinate reference sytem are assumed to match the FARSITE output rasters.
#'
#' @return The number of locations reached by the fire.
#'
#' @export
#'
count_fire_locations <- function(outdir, prefix, pts) {
  if (!inherits(pts, c("matrix", "data.frame")) || ncol(pts) != 2)
    stop("Argument pts should be a two-column matrix or data frame of point coordinates")

  fname <- paste0(prefix, "_ArrivalTime.asc")
  r <- raster(file.path(outdir, fname))

  x <- raster::extract(r, pts)
  sum(!is.na(x))
}


#' Get last fire time step
#'
#' Retrieves the month, day and time (24 hour format) of the last fire
#' perimeter update. This can be used, for example, to check if the fire was still
#' active at the end of the burn period specified of the simulation. If the fire
#' did not burn, a vector of \code{NA} values is returned.
#'
#' @param outdir Full path to the directory containing output files.
#' @param prefix Prefix used for the output file names.
#'
#' @return A named numeric vector with elements 'month', 'day' and 'hour'.
#'
#' @export
#'
get_final_time <- function(outdir, prefix) {
  fname <- paste0(prefix, "_Perimeters.shp")
  perim <- sf::st_read(file.path(outdir, fname), quiet = TRUE)

  nr <- nrow(perim)

  if (nr > 0) {
    out <- unlist( as.data.frame(perim)[nr, c("Month", "Day", "Hour")] )
    names(out) <- tolower(names(out))
  } else {
    out <- c(month = NA, day = NA, hour = NA)
  }

  out
}


.check_file_exists <- function(...) {
  for (f in list(...)) if (!file.exists(f)) stop("Cannot find file: ", f)
}


.is_empty_parameter <- function(x) {
  is.null(x) || (stringr::str_length( stringr::str_trim(x) ) == 0)
}

