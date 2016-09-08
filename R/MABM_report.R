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
#' @param station character string indicating the MABM station (e.g., refuge, ES office) for
#'  which to produce a report.  Default (NULL) prompts the user to select from a list of all
#'  available MABM stations
#' @param year integer indicating the year for which to generate the annual report; defaults
#'  to the current year
#' @param out_dir character string indicating path to directory where final report *.pdf
#'  should be deposited.  Default is the current working directory
#' @param MABM_dir character string indicating path to directory where *.xlsx output of
#'  \code{\link{setup_MABM_reports}} is stored.  Currently, the macro implanted in the MABM
#'  database only outputs to C:/temp, so this argument is not useful at the moment
#' @import ggplot2
#' @export

MABM_report <- function(station = NULL, year = lubridate::year(Sys.Date()),
                        out_dir = ".", MABM_dir = NULL) {

    # Current access macro only allows C:/temp, but for future use...
    if (is.null(MABM_dir)) MABM_dir <- "C:/temp"
    if (!all(file.exists(file.path(MABM_dir, "MABM_calls.xlsx")),
             file.exists(file.path(MABM_dir, "MABM_survey_details.xlsx")),
             file.exists(file.path(MABM_dir, "MABM_spp_details.xlsx")),
             file.exists(file.path(MABM_dir, "MABM_routes.xlsx"))))
        stop("At least one required MABM output file is missing.  Try running MABM_report_setup() again.")

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
        stations <- utils::select.list(menu_items, title="Select one or more MABM stations.",
                                      multiple = TRUE, graphics = TRUE)
        if (length(stations) == 0) stop("You must select or provide a MABM station.")
    } else {
        if (!(all(station %in% routes$station))) stop("At least one unrecognized MABM station.")
    }

    make_report <- function(station) {
      routes <- routes[routes$station == station, ] %>% dplyr::arrange(site)
      station <- routes$station %>% unique() %>%
        sub("NWR", "National Wildlife Refuge", .) %>%
        sub("SERVICE", "Services", .) %>%
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
        dplyr::filter(lubridate::year(surv_date) == year)

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
      if (nrow(survey_info) == 0) stop(paste("No MABM data found at", station, "in", year))
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
        dplyr::mutate(year = lubridate::year(surv_date)) %>%
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

      ### MAKE THE REPORT
      render_MABM(year = year, n_nwr = n_nwr, n_es = n_es, station = station,
                  stn_start_yr = start_yr, route_path = tmps[1],
                  survey_path = tmps[2], bat_path = tmps[3], spp_path = tmps[4],
                  out_dir = out_dir)
    }

    invisible(lapply(stations, dplyr::failwith(NULL, make_report)))

}

