#' MABM annual report creation.
#'
#' This function generates an annual report for one or more MABM stations for, by default,
#'  the current calendar year.  The report includes, for each route associated with a
#'  given station, (1) an overview of the year's efforts, (2) a night-by-night tabular
#'  summary with notes, if present, (3) a night-by-night tabular summary of species
#'  detections, (4) a tabular and graphical comparison of species detections from the
#'  current season with previous season(s), and (5) a map of the route indicating the
#'  locations of all bat detections for the season.
#'
#' @section Additional details on parameter inputs:
#' \describe{
#'  \item{MABM_dir}{This is the base directory containing all relevant data for the whole
#'    complement of MABM stations.  In short, this base directory should contain a separate
#'    directory for each MABM station (e.g., Big Oaks; name is not restricted). Each station
#'    directory should in turn contain a directory for *each* route named according to its
#'    shortened route name in the MABM database (e.g., BokNWR1 for Big Oaks NWR East route);
#'    the station directory may also contain an 'Annual Report' directory. An example
#'    hierarchy is shown below.}
#'  \item{distribute}{When TRUE, the function attempts to place the output in an 'Annual
#'   Report' directory located in the base directory for a given MABM station, alongside
#'   the separate directories for each route of that station.  For this to work properly,
#'   the hierarchy outlined above must exist.  If `distribute` = TRUE but a directory
#'   matching the shortened route name (e.g., BokNWR1) is not located, or if `distribute`
#'   = FALSE, the output is placed in an 'Annual Reports' directory just beneath the directory
#'   identified by `MABM_dir`, which is created if does not exist.}
#' }
#'
#' @section Example directory hierarchy:
#' \itemize{
#'   \item MABM
#'   \itemize{
#'    \item{Big Lake}
#'      \itemize{
#'        \item{BglNWR}
#'          \itemize{
#'            \item{BglNWR_canonical_route.shp}
#'          }
#'        \item{Annual Report}
#'        }
#'    \item{Big Oaks}
#'      \itemize{
#'        \item{BokNWR1}
#'          \itemize{
#'            \item{BokNWR1_canonical_route.shp}
#'          }
#'        \item{BokNWR2}
#'          \itemize{
#'            \item{BokNWR2_canonical_route.shp}
#'          }
#'        \item{Annual Report}
#'      }
#'    }
#'  }
#'
#' @section Map generation in the annual reports:
#' Map generation of detected bats occurs only if three requirements are met:
#'  \enumerate{
#'    \item at least one bat call has been detected
#'    \item the calls have been successfully georeferenced (i.e., they possess associated
#'       location information).
#'    \item a shapefile (point or line) named with the form 'shortname_canonical_route.shp'
#'       (e.g., BokNWR1_canonical_route.shp') is present in its respective route directory
#'  }
#'
#' @param station character string indicating the MABM station (e.g., refuge, ES office) for
#'  which to produce a report.  Default (NULL) prompts the user to select from a list of all
#'  available MABM stations.  The selection of multiple stations is allowed.
#' @param year integer indicating the year for which to generate the annual report; defaults
#'  to the current year
#' @param MABM_dir character string indicating the base directory containing MABM station
#'  related data (i.e., MABM route directories with call files, shapefiles, and annual reports).
#'  See Details.
#' @param update logical (default = FALSE) indicating whether to call a macro inside the MABM
#'  database that exports the data necessary for annual report generation.  Generally used
#'  (i.e., update = TRUE) only if (1) this is the first time running \code{MABM_report} or (2)
#'  after the MABM database has been updated.
#' @param distribute logical (default = TRUE) indicating whether to attempt to place the
#'  output report in an 'Annual Report' directory within the base directory for each station,
#'  if located. To work properly, a specific directory hierarchy is expected (see Details).
#'  If `distribute` = FALSE or distribute attempt fails (see Details), the output is placed
#'  in an 'Annual Report' directory beneath \code{MABM_dir}.
#' @param interactive logical (default = TRUE) indicating whether to output a standalone
#'  interactive leaflet map (*.html file) of bat detections along a route in a given year.
#'  If so, it is named similarly to, and exported with, the report *.pdf
#' @export

MABM_report <- function(station = NULL, year = as.integer(format(Sys.Date(), "%Y")),
                        MABM_dir = NULL, update = FALSE, distribute = TRUE,
                        interactive = TRUE) {

  if (is.null(MABM_dir)) {
    ans <- yesno()
    if (ans == "c") stop("Function cancelled.")
    if (ans == "y") MABM_dir <- getwd()
    if (ans == "n") MABM_dir <- choose.dir("C:/", caption = "Select folder with MABM database exports.")
    if (is.na(MABM_dir)) stop("No directory selected.")
  } else MABM_dir <- file.path(MABM_dir)

  if (!update) {
    if (!all(file.exists(file.path(MABM_dir, "MABM_calls.xlsx")),
             file.exists(file.path(MABM_dir, "MABM_survey_details.xlsx")),
             file.exists(file.path(MABM_dir, "MABM_spp_details.xlsx")),
             file.exists(file.path(MABM_dir, "MABM_routes.xlsx"))))
      stop("At least one required MABM output file is missing. ",
           "Check your choice of `MABM_dir` or use `update = TRUE`.")
  } else {
    MABMreportr:::setup_MABM_reports(export_dir = MABM_dir)
  }

  # Set up temporary directory
  tmps <- tempfile("MABM", fileext = rep(".RDS", 4))

  # Get list of completed routes (stations may host >1 route)
  routes <- readxl::read_excel(file.path(MABM_dir, "MABM_routes.xlsx"))
  routes <- routes %>%
    dplyr::select(station = ORGNAME,
                  site = Site_Name,
                  site_notes = Loc_Notes,
                  len_mi = Rte_length,
                  lat = Centroid_Y_Coord,
                  lon = Centroid_X_Coord,
                  state = State)
  station_routes <- dplyr::select(routes, station, site)

  # Pick a station, any station
  menu_items <- routes$station %>% unique() %>% sort()
  if (is.null(station)) {
    station <- utils::select.list(menu_items, title="Select one or more MABM stations.",
                                   multiple = TRUE, graphics = TRUE)
    if (length(station) == 0) stop("You must select or provide a MABM station.")
  } else {
    if (!(all(station %in% routes$station))) stop("At least one unrecognized MABM station.")
  }

  ### Make the report
  make_report <- function(station) {
    message("Processing report for ", station, "\n")
    routes <- routes[routes$station == station, ] %>% dplyr::arrange(site)
    station <- routes$station %>% unique() %>%
      sub("NWR", "National Wildlife Refuge", .) %>%
      sub("SERVICES", "Services", .) %>%
      Cap()

    # Store in temporary file
    saveRDS(routes, file = tmps[1])

    survey_info <- readxl::read_excel(file.path(MABM_dir, "MABM_survey_details.xlsx"))
    survey_info <- survey_info %>%
      dplyr::select(site = dplyr::starts_with("Site Name"),
                    surv_date = dplyr::starts_with("Date Start"),
                    gps = dplyr::starts_with("GPS"),
                    complete = dplyr::starts_with("Rt Compl"),
                    notes = Notes) %>%
      dplyr::filter(as.integer(format(surv_date, "%Y")) == year)

    # Pause and calculate some station summary for report
    current_stations <- dplyr::left_join(dplyr::select(survey_info, site),
                                         station_routes, by = "site") %>%
      dplyr::select(station) %>% unique()
    n_nwr <- sum(grepl("NWR", current_stations$station))
    n_es <- sum(grepl("ECOLOG", current_stations$station))

    # Carry on...
    survey_info <- survey_info %>%
      dplyr::filter(site %in% routes$site) %>%
      dplyr::mutate(gps = as.logical(gps),
                    complete = as.logical(complete),
                    notes = ifelse(is.na(notes), "", notes)) %>%
      dplyr::arrange(site, surv_date)
    if (nrow(survey_info) == 0) stop(paste0("No MABM data found at ", station, " in ", year, ".\n"))
    # Store it
    saveRDS(survey_info, file = tmps[2])

    # Get the call data for this station...
    calls <- readxl::read_excel(file.path(MABM_dir, "MABM_calls.xlsx"))
    calls <- calls %>%
      dplyr::select(site = Site_Name,
                    lat = LAT,
                    lon = LONG,
                    surv_date = dplyr::contains("Date Start"),
                    spp = A_SP) %>%
      dplyr::mutate(year = as.integer(format(surv_date, "%Y"))) %>%
      dplyr::filter(site %in% routes$site & year <= year) %>%
      dplyr::arrange(site, surv_date)
    start_yr <- min(calls$year)

    #Store it
    saveRDS(calls, tmps[3])

    # Load species code/common name lookup table and store it...
    spp_info <- readxl::read_excel(file.path(MABM_dir, "MABM_spp_details.xlsx")) %>%
      dplyr::rename(spp = A_SP,
                    spp_cn = CommonName) %>%
      dplyr::select(spp, spp_cn)
    saveRDS(spp_info, tmps[4])

    # Set the output directory
    base_dir <- NULL
    if (distribute) {
      base_dir <- grep(paste0(survey_info$site[1], "$"), list.dirs(MABM_dir), value = TRUE)
      if (length(base_dir) == 1) {
        out_dir <- file.path(dirname(base_dir), "Annual Report")
      } else {
        out_dir <- file.path(MABM_dir, "Annual Reports")
        if (!dir.exists(out_dir)) dir.create(out_dir)
        out_dir <- normalizePath(out_dir)
        if (length(base_dir) == 0) {
          warning("Matching station/route directories not found. ",
                  "Annual report output to ", shQuote(out_dir))
        } else {
          warning("Multiple matching station/route directories found. ",
                  "Annual report output to ", shQuote(out_dir))
        }
      }
    } else {
      out_dir <- file.path(MABM_dir, "Annual Reports")
      if (!dir.exists(out_dir)) dir.create(out_dir)
      out_dir <- normalizePath(out_dir)
    }

    render_MABM(year = year, n_nwr = n_nwr, n_es = n_es, station = station,
                stn_start_yr = start_yr, route_path = tmps[1],
                survey_path = tmps[2], bat_path = tmps[3], spp_path = tmps[4],
                out_dir = out_dir)

    if (interactive) interactive_MABM(routes, calls, spp_info, year, out_dir)

  }

  invisible(lapply(station, purrr::possibly(make_report, NULL)))

}

