#' Create a FARSITE input file
#'
#' This function creates the contents for a FARSITE input file from a template
#' (package file \code{extdata/input_template.txt}) and a set of values for
#' variable components.
#'
#' @param start.time Character string for start date and time, e.g. '08 28 0000'.
#'
#' @param end.time Character string for end date and time, e.g. '08 29 2359'.
#'
#' @param path.fms Path to a file of tabular data for fuel moisture in the
#'   six-column format expected by FARSITE. Note, only the data records should
#'   be included in the file without any header information.
#'
#' @param raws.elev Elevation value.
#'
#' @param raws.units Units of measurement: one of 'Metric' or 'English'.
#'
#' @param path.raws Path to a file of tabular RAWS weather data in the
#'   ten-column format expected by FARSITE. Note, only the data records should
#'   be included in the file without any header information.
#'
#' @param path.fmd Optional path to a custom fuels file.
#'
#' @param path.ros.adj Optional path to the ROS adjustment file.
#'
#' @return Contents of the input file as a single character string.
#'
#' @export
#'
create_input_file <- function(out.path,
                              start.time,
                              end.time,
                              path.fms,
                              raws.elev,
                              raws.units,
                              path.raws,
                              path.fmd = NULL,
                              path.ros.adj = NULL) {

  # Read a file of tabular dat, drop any blank lines,
  # and return count of data records and the data as a character string.
  .read_file <- function(path) {
    x <- readLines(path)
    x <- stringr::str_trim(x)
    x <- x[x != ""]

    n <- length(x)

    x <- c(x, "")
    list(n = n, text = paste(x, collapse = "\n"))
  }


  f <- system.file("extdata", "input_template.txt", package = "farsitebatch")
  txt <- readLines(con = f)
  txt <- paste(txt, collapse = "\n")

  fms <- .read_file(path.fms)
  raws <- .read_file(path.raws)

  if (is.null(path.ros.adj)) path.ros.adj <- ""

  if (is.null(path.fmd)) {
    fmd.switch <- "# "
    fmd.file <- ""
  } else {
    fmd.switch <- ""
    fmd.file <- path.fmd
  }

  raws.units <- stringr::str_to_title(raws.units)

  # Note: glue takes the following from the function environment:
  # start.time, end.time, raws.elev, raws.units, fmd.switch, fmd.
  input <- glue::glue(txt,
                      num.fms = fms$n,
                      data.fms = fms$text,
                      num.raws = raws$n,
                      data.raws = raws$text,
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


#' Format FARSITE run command arguments
#'
#' @param path.lcp Path to the landscape (.lcp) file.
#'
#' @param path.input Path to the FARSITE input file.
#'
#' @param path.ignition Path to the shapefile of ignition locations.
#'
#' @param out.dir Directory for FARSITE output files.
#'
#' @param out.prefix File name prefix for FARSITE output files.
#'
#' @return FARSITE run command arguments as a single character string.
#'
#' @export
#'
format_run_args <- function(path.lcp, path.input, path.ignition, path.outdir, prefix) {
  out <- file.path(path.outdir, prefix)
  paste(path.lcp, path.input, path.ignition, "0", out, "0")
}



.check_file_exists <- function(...) {
  for (f in list(...)) if (!file.exists(f)) stop("Cannot find file: ", f)
}

