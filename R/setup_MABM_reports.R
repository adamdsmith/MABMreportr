#' Export data from MABM database necessary for report preparation
#'
#' This function calls macros inside the MABM database to export data necessary for the
#' creating of annual reports for MABM routes.  It only needs to be run a single time after
#' the data for a given survey year has been entered into the database.
#'
#' @param access_exe character string indicating the path to the MS Access executable.  Default (NULL)
#'  attempts to find it in a couple of common places and, if unsuccessful, prompts the user to browse
#'  to the executable.
#' @param MABM_access_db character string indicating the path to the MABM MS Access database.
#'  Default (NULL) prompts the user to browse to the file.
#' @param export_dir character string indicating desired location to output *.xlsx file from
#'  Access database; currently not used
#' @return None.This function calls a macro within the MABM database to export
#'  MS Excel spreadsheets to a specified directory for use in annual report creation.
#' @export

setup_MABM_reports <- function(access_exe = NULL, MABM_access_db = NULL,
                               export_dir = NULL) {

    ## Currently this does not affect where files go...
    ## The directory is fixed in the Access macro
    if (is.null(export_dir)) export_dir <- "C:/temp"

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
        MABM_access_db <- normalizePath(MABM_access_db)
    } else {
        MABM_access_db <- utils::choose.files(default = "*.accdb",
                                          caption = "Please select the MABM database file.",
                                          multi = FALSE)
        if (length(MABM_access_db) == 0) stop("Function cancelled. No database file selected.")
    }

    if (!file.exists(MABM_access_db) && tools::file_ext(MABM_access_db) != "accdb")
        stop("MABM database file not found, or wrong file type.")

    # Unfortunately, can't specify output location of macro product dynamically; it's fixed in macro
    #root_dir <- dirname(MABM_access_db)
    if (!dir.exists(export_dir)) dir.create(export_dir)

    # Execute macro with MABM MS Access database
    # Produces 3 spreadsheets in C:/temp: MABM_routes.xlsx, MABM_survey_details.xlsx, and MABM_calls.xlsx
    system(paste(shQuote(access_exe),
                 shQuote(normalizePath(MABM_access_db)),
                 '/x Export_MABM_info /nostartup'))

    if (all(file.exists(file.path(export_dir, "MABM_calls.xlsx")),
            file.exists(file.path(export_dir, "MABM_survey_details.xlsx")),
            file.exists(file.path(export_dir, "MABM_spp_details.xlsx")),
            file.exists(file.path(export_dir, "MABM_routes.xlsx"))))
        message("\n Access tables successfully exported to ", export_dir, " \n")

}
