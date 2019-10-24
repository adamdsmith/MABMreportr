#' Export data from MABM database necessary for report preparation
#'
#' This function calls macros inside the MABM database to export the data necessary for
#' creating annual reports for MABM routes.  It needs only to be run a single time after
#' the data for a given survey year has been entered into the database. This function is
#' not likely to be called outside of \code{\link{MABM_report}}
#'
#' @section Recommended export directory:
#' It is strongly recommended (required?) that you direct the MABM database macro
#'  to export needed files to the base directory containing MABM station related data
#'  (i.e., MABM route directories with call files, shapefiles, and annual reports).
#'  See the \code{\link{MABM_report}} documentation for further details.
#'
#' @param access_exe character string indicating the path to the MS Access executable.  Default (NULL)
#'  attempts to find it in a couple of common places and, if unsuccessful, prompts the user to browse
#'  to the executable.
#' @param MABM_access_db character string indicating the path to the MABM MS Access database.
#'  Default (NULL) prompts the user to browse to the file.
#' @param export_dir character string indicating desired location to output *.xlsx file from
#'  Access database; default prompts user to browse to desired directory.  See Details for
#'  recommendations.
#' @return None. This function calls a macro within the MABM database to export
#'  MS Excel spreadsheets to a specified directory for use in annual report creation.

setup_MABM_reports <- function(access_exe = NULL, MABM_access_db = NULL,
                               export_dir = NULL) {

    if (is.null(export_dir)) {
      export_dir <- choose.dir("C:/", caption = "Select folder for MABM database exports.")
      if (is.na(export_dir)) stop("No export directory selected.")
    } else export_dir <- file.path(export_dir)

    # Find access executable
    if (is.null(access_exe)) {
        exe_dir <- dirname(list.files(path = "C:/Program Files (x86)/Microsoft Office",
                                      pattern = "msaccess.exe", full.names = TRUE, recursive = TRUE))
        if (length(exe_dir) == 0) {
            exe_dir <- dirname(list.files(path = "C:/Program Files/Microsoft Office",
                                          pattern = "msaccess.exe", full.names = TRUE, recursive = TRUE))
        }
    }

    if (file.exists(file.path(exe_dir, "msaccess.exe"))) {
        access_exe <- normalizePath(file.path(exe_dir, "msaccess.exe"))
    } else {
        access_exe <- utils::choose.files(default = paste0("C:/", "*.exe"),
                                   caption = "Please select the Microsoft Access executable file.",
                                   multi = FALSE)
        if (length(access_exe) == 0) stop("Function cancelled. No executable selected.")
    }

    if (!file.exists(access_exe)) stop("Microsoft Access executable not found at that location.")

    # Find MABM database
    if (!is.null(MABM_access_db)) {
        MABM_access_db <- normalizePath(MABM_access_db, mustWork = TRUE)
    } else {
        MABM_access_db <- utils::choose.files(default = "*.accdb",
                                          caption = "Please select the MABM database file.",
                                          multi = FALSE)
        if (length(MABM_access_db) == 0) stop("Function cancelled. No database file selected.")
    }

    if (!file.exists(MABM_access_db) && tools::file_ext(MABM_access_db) != "accdb")
        stop("MABM database file not found, or wrong file type.")

    if (!dir.exists(export_dir)) dir.create(export_dir)

    # Execute macro with MABM MS Access database
    # Produces 4 spreadsheets: MABM_calls, MABM_survey_details, MABM_spp_details,
    #                          and MABM_routes
    status <- system(paste(shQuote(access_exe),
                           shQuote(normalizePath(MABM_access_db)),
                           '/x Export_MABM_info /nostartup /cmd',
                           export_dir))
    if (status != 0) stop("MABM Access macro failed.")

    if (all(file.exists(file.path(export_dir, "MABM_calls.xlsx")),
            file.exists(file.path(export_dir, "MABM_survey_details.xlsx")),
            file.exists(file.path(export_dir, "MABM_spp_details.xlsx")),
            file.exists(file.path(export_dir, "MABM_routes.xlsx")))) {
      cat("\nMABM Access database tables successfully exported to", export_dir, "\n\n")
    } else stop("Export from MABM Access database failed.")

}
