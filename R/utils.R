Cap <- function(string) {
    s <- tolower(string)
    s <- strsplit(s, " ")
    unlist(lapply(s, function(i) {
        paste(toupper(substring(i, 1,1)), substring(i, 2), sep="", collapse=" ")
    }))
}

shift_point_y <- function(df, group, y, shift_prop = 0.1, ymin = NULL, ymax = NULL) {
  df <- as.data.frame(df)
  lu <- data.frame(unique(df[order(df[, group]), group, drop = FALSE]),
                   cutoff =  tapply(df[, y], df[, group], function(x) mean(range(x))),
                   shift = tapply(df[, y], df[, group], function(x)
                     max(utils::combn(c(ymin, ymax, range(x)), 2, diff)) * shift_prop)
#                   shift = tapply(df[, y], df[, group], function(x) range(x)))
  )
  df$adj_y <- ifelse(df[, y, drop = TRUE] < lu$cutoff[match(df[, group, drop = TRUE], lu$spp_cn)],
                     df[, y, drop = TRUE] + lu$shift[match(df[, group, drop = TRUE], lu$spp_cn)],
                     df[, y, drop = TRUE] - lu$shift[match(df[, group, drop = TRUE], lu$spp_cn)])
  df
}

render_MABM <- function(out_dir, year, n_nwr, n_es, station, stn_start_yr,
                        route_path, survey_path, bat_path, spp_path) {

    # Need better error catching, but this will do for now...
    if (is.null(station)) stop("You must provide a MABM station.")

    rmd_document <- system.file("extdata", "MABM_report_template.Rmd", package = "MABMreportr")

    station_short <- station %>% gsub("national wildlife refuge", "NWR", ., ignore.case = TRUE) %>%
        gsub("ecological services", "ES", ., ignore.case = TRUE) %>%
        gsub(" ", "", .)

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

texble <- function (x, digits = getOption("digits"), row_names = NA,
                   col_names = LETTERS[1:ncol(x)], caption = NULL,
                   col_widths = rep(14/ncol(x), ncol(x)), col_units = "cm",
                   col_types = rep("C", ncol(x)),
                   linespace = "3pt", tbl_float = c("h", "t", "b", "p", "H"),
                   longtable = FALSE, centering = TRUE) {

    tbl_float <- match.arg(tbl_float)
    # h "here" - place figure in the text where the figure environment is written, if enough room left on the page
    # t "top" - place it at the top of a page
    # b "bottom" - place it at the bottom of a page
    # p "page" - place it on a page containing only floats
    # H "right here" - force it at this point...the others are suggestions

#    if (!is.null(caption) && !is.na(caption))
#        caption <- paste0(knitr:::create_label("tab:", opts_current$get("label")),
#                         caption)
    if (identical(col_names, NA))
        col_names <- colnames(x)
    if (!is.matrix(x))
        x <- as.data.frame(x)
    m <- ncol(x)
    isn <- sapply(x, is.numeric)
    x[, isn] <- round(x[, isn], digits)
    if (is.na(row_names))
        row_names <- !is.null(rownames(x)) && !identical(rownames(x),
                                                         as.character(seq_len(NROW(x))))
    if (row_names) {
        x <- cbind(` ` = rownames(x), x)
        if (length(col_names) + 1 == ncol(x)) {
            if (!is.null(col_names))
                col_names <- c(" ", col_names)
        }
    }

    if (!identical(length(col_names), length(col_types), length(col_widths), ncol(x)))
        stop(paste("Column names, widths, and types must have same length.",
                    paste0("Names (", length(col_names), "), widths (", length(col_widths),
                           "), and types (", length(col_types), ") do not agree."),
                    "Are you including row names?", sep = "\n"))
    n <- nrow(x)
    x <- base::format(as.matrix(x), trim = TRUE, justify = "none")
    if (!is.matrix(x))
        x <- matrix(x, nrow = n)
    x <- gsub("^\\s*|\\s*$", "", x)
    x <- ifelse(x == "TRUE", "\\checkmark", x)
    x <- gsub("FALSE", "", x)
    colnames(x) <- col_names
    table_env <- "table"

    if (identical(caption, NA))
        caption <- NULL
    env1 <- sprintf("\\begin{%s}[%s]", table_env, tbl_float)
    env2 <- sprintf("\\end{%s}", table_env)
    tabular <- if(longtable) "longtable" else "tabular"

    col_widths <- paste0(col_widths, col_units)
    col_specs <- paste(paste0(col_types, "{", col_widths, "}"), collapse = "")
    header_lu <- data.frame(col_type = c("L", "C", "R"),
                            head_type = c("H", "I", "J"))
    head_types <- c("H", "I", "J")[match(col_types, c("L", "C", "R"))]
    head_specs <- paste0(head_types, "{", col_widths, "}")

    if (!is.null(linespace)) linespace <- paste(c("[", linespace, "]"), collapse = "")

    centering <- if (centering && !is.null(caption)) "\\centering"

    res <- paste(c(if (!longtable) c(env1, sprintf("\\caption{%s}", caption), centering),
                   sprintf("\\begin{%s}{%s}", tabular, col_specs),
                   if(longtable) paste0(sprintf("\\caption{%s}", caption), "\\\\"),
                   "\\toprule",
                   paste0(paste(paste0("\\multicolumn{1}{", head_specs, "}{", colnames(x), "}"),
                                collapse = " & "), "\\\\", linespace),
                   "\\midrule",
                   if(longtable) c("\\endfirsthead",
                                   "\\toprule",
                                   paste0(paste(paste0("\\multicolumn{1}{", head_specs, "}{", colnames(x), "}"),
                                                collapse = " & "), "\\\\", linespace),
                                   "\\midrule",
                                   "\\endhead",
                                   "\\hline",
                                   paste0("\\multicolumn{", length(col_widths), "}{c}{Continued on next page}\\\\"),
                                   "\\bottomrule", "\\endfoot"),
                   if(longtable) c("\\bottomrule", "\\endlastfoot"),
                   paste0(apply(x, 1, paste, collapse = " & "), "\\\\", linespace),
                   if(!longtable) "\\bottomrule",
                   sprintf("\\end{%s}", tabular),
                   if (!longtable) env2),
                 collapse = "\n")

    structure(res, format = "latex", class = "knitr_kable")
}

yesno <- function() {
  ans <- substr(readline(prompt="Are MABM DB exports in (or to be exported to) the current directory (y/n/c)?"), 1L, 1L)
  return(tolower(ans))
}

interactive_MABM <- function(station, routes, calls, spp_info, yr, out_dir) {

  req_pckg <- c("leaflet", "htmlwidgets", "htmltools")
  req_pckg <- sapply(req_pckg, requireNamespace, quietly = TRUE)
  req_pckg <- names(req_pckg)[!req_pckg]
  if (length(req_pckg) > 0)
    invisible(lapply(req_pckg, function(pckg) {
      message("The ", shQuote(pckg), " package is needed and will be installed")
      install.packages(pckg)
    }))

  #Create a custom color scale to consistently display species
  bat_fills <- c("orange3", "orange3", "sienna", "red2", "forestgreen", "forestgreen",
                 "gray40", "gray40", "gray40", "gray40", "gray40", "gray40", "royalblue4",
                 "gold", "white")
  names(bat_fills) <- c("CORA", "COTO", "EPFU", "LABO", "LACI", "LANO", "MYAU",
                        "MYGR", "MYLE", "MYLU", "MYSE", "MYSO", "NYHU", "PESU",
                        "UNKN")
  sppPal <- leaflet::colorFactor(palette = bat_fills, domain = names(bat_fills))

  # Make bat icon list
  batIcons <- MABM:::makeBatIconList()

  for (i in routes$site) {

    i_calls <- calls %>%
      dplyr::left_join(spp_info, by = "spp") %>%
      dplyr::filter(site == i, year == yr,
             !is.na(lat), !is.na(lon))

    shp_path <- file.path(dirname(out_dir), i)
    route_shp <- grep(pattern = "(?=.*canonical)(?=.*shp$)", list.files(shp_path),
                      perl = TRUE, value = TRUE)

    if (nrow(i_calls) == 0) {
      message("No calls georeferenced or recorded. Map not created for ", i, " route.")
    } else if (length(route_shp) == 0) {
      message("No canonical route shapefile found. Map not created for ", i, " route.")
    }

    route_shp <- sf::st_read(file.path(shp_path,route_shp), quiet = TRUE) %>%
      as("Spatial")

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

    # Add species legend and layer control
    p <- p %>%
      leaflet::addLegend("topleft", pal = sppPal, values = i_calls$spp,
                title = "Species", opacity = 1) %>%
      leaflet::addLayersControl(baseGroups = c("Terrain", "Aerial"),
                       overlayGroups = groups,
                       options = leaflet::layersControlOptions(collapsed = FALSE))

    station_short <- station %>% gsub("national wildlife refuge", "NWR", ., ignore.case = TRUE) %>%
      gsub("ecological services", "ES", ., ignore.case = TRUE)

    out_file <- file.path(out_dir,
                          paste("MABM", i, yr, "interactive.html", sep = "_"))

    htmlwidgets::saveWidget(p, file = out_file)
    unlink(sub(".html", "_files", out_file), recursive = TRUE)

    message(paste(strwrap(paste("Created interactive bat detection map for", yr,
            "survey(s) along", station_short, "-", i, "route:\n",
            tools::file_path_as_absolute(out_file))), collapse = "\n"), "\n")

  }

}
