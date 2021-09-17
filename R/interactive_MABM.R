interactive_MABM <- function(station, routes, calls, spp_info, yr, out_dir) {

  req_pckg <- c("htmlwidgets", "htmltools")
  req_pckg <- sapply(req_pckg, requireNamespace, quietly = TRUE)
  req_pckg <- names(req_pckg)[!req_pckg]
  if (length(req_pckg) > 0)
    invisible(lapply(req_pckg, function(pckg) {
      message("The ", shQuote(pckg), " package is needed and will be installed")
      install.packages(pckg)
    }))

  #Create a custom color scale to consistently display species
  bat_fills <- c("orange3", "orange3", "sienna", "red3", "forestgreen", "forestgreen",
                 "gray40", "gray40", "gray40", "gray40", "gray40", "gray40", "royalblue",
                 "gold", "white")
  bat_fill_spp <- c("CORA", "COTO", "EPFU", "LABO", "LACI",
                    "LANO", "MYAU", "MYGR", "MYLE", "MYLU",
                    "MYSE", "MYSO", "NYHU", "PESU", "UNKN")
  bat_fill_df <- data.frame(bat_fills, bat_fill_spp, stringsAsFactors = FALSE) %>%
    left_join(spp_info, by = c("bat_fill_spp" = "spp")) %>%
    arrange(spp_cn)

  sppPal <- leaflet::colorFactor(palette = bat_fill_df$bat_fills, domain = bat_fill_df$spp_cn)

  # Make bat icon list
  mBIL <- utils::getFromNamespace("makeBatIconList", "MABM")
  batIcons <- mBIL()

  for (i in routes$site) {

    i_calls <- calls %>%
      dplyr::left_join(spp_info, by = "spp") %>%
      dplyr::filter(site == i, year == yr,
                    !is.na(lat), !is.na(lon))

    shp_path <- file.path(dirname(out_dir), i)
    route_shp <- grep(pattern = "(?=.*canonical)(?=.*shp$)",
                      list.files(shp_path, full.names = TRUE),
                      perl = TRUE, value = TRUE)

    if (nrow(i_calls) == 0) {
      message(paste(i, yr, "interactive bat detection map not created. No calls georeferenced or recorded."))
      next()
    } else if (length(route_shp) == 0) {
      message(paste(i, yr, "interactive bat detection map not created. No canonical route shapefile found."))
      next()
    }

    route_shp <- sf::st_read(route_shp, quiet = TRUE)
    if (!identical(sf::st_crs(route_shp)$epsg, 4326))
      route_shp <- sf::st_transform(route_shp, 4326)
    route_shp <- as(route_shp, "Spatial")

    p <- leaflet::leaflet() %>%
      # Base map group
      leaflet::addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
                        group = "Terrain") %>%
      leaflet::addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                        group = "Aerial") %>%
      # Add canonical route
      leaflet::addPolylines(data = route_shp, color = "#B2B2B2", opacity = 1)

    # Add georeferenced bat detections by survey date
    surv_dates <- unique(i_calls$surv_date)
    groups <- format(surv_dates, format = "%d %B")

    for (j in seq_along(surv_dates)) {
      grp <- groups[j]; dt <- surv_dates[j]
      p <- p %>%
        leaflet::addMarkers(data = dplyr::filter(i_calls, surv_date == dt),
                            ~lon, ~lat, group = grp,
                            options = leaflet::markerOptions(riseOnHover = TRUE),
                            label = ~paste0(spp, ": ", spp_cn),
                            icon = ~batIcons[spp])
    }

    station_short <- station %>% gsub("national wildlife refuge", "NWR", ., ignore.case = TRUE) %>%
      gsub("ecological services", "ES", ., ignore.case = TRUE)

    # Add species legend and layer control
    mZC <- utils::getFromNamespace("moveZoomControl", "leaflet.extras")
    p <- p %>%
      leaflet::addLegend("topleft", pal = sppPal, values = i_calls$spp_cn,
                         title = paste(paste(strwrap(station_short, 16),
                                             collapse = "<br>&nbsp;"),
                                       paste("Route:", i),
                                       paste("Year:", yr), sep = "<br>"),
                         opacity = 1) %>%
      leaflet::addLayersControl(baseGroups = c("Terrain", "Aerial"),
                                overlayGroups = groups,
                                position = "topleft",
                                options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
      mZC()

    out_tmp <- tempdir()
    out_fn <- paste("MABM", i, yr, "interactive.html", sep = "_")

    htmlwidgets::saveWidget(p, file = file.path(out_tmp, out_fn))

    # Move it
    file.rename(file.path(out_tmp, out_fn), file.path(out_dir, out_fn))
    message(paste(i, yr, "interactive bat detection map created:\n   ",
                   tools::file_path_as_absolute(file.path(out_dir, out_fn))))
  }

  # Attach html files to PDF
  pdf_pattern <- paste(shorten_station(station), paste0(yr, ".pdf$"), sep = "_")
  in_pdf <- list.files(out_dir, pattern = pdf_pattern, full.names = TRUE)
  html_pattern <- paste(paste("MABM", routes$site, yr,  "interactive.html", sep = "_"),
                        collapse = "|")
  in_html <- list.files(out_dir, pattern = html_pattern, full.names = TRUE)

  if (all(sapply(list(in_pdf, in_html), length) >= 1))
    attach_htmls(in_pdf, in_html)

}
