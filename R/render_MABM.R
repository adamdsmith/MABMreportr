render_MABM <- function(out_dir, year, n_nwr, n_es, station, stn_start_yr,
                        route_path, survey_path, bat_path, spp_path) {

  # Need better error catching, but this will do for now...
  if (is.null(station)) stop("You must provide a MABM station.")

  rmd_document <- system.file("extdata", "MABM_report_template.Rmd", package = "MABMreportr")

  station_short <- shorten_station(station)

  fn <- paste("MABM", station_short, year, sep = "_")
  out_file <- paste(fn, "pdf", sep = ".")

  rmarkdown::render(rmd_document, output_dir = out_dir,
                    output_file = out_file,
                    params = list(year = year,
                                  n_nwr = n_nwr, n_es = n_es,
                                  station = station,
                                  stn_start_yr = stn_start_yr,
                                  route_path = route_path,
                                  survey_path = survey_path,
                                  bat_path = bat_path,
                                  spp_path = spp_path),
                    quiet = TRUE)
  message("Created ", year, " MABM annual report for ", station, ":\n    ",
          tools::file_path_as_absolute(file.path(out_dir, out_file)), "\n")
}
