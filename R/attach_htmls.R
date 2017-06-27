attach_htmls <- function (in_pdf, in_html) {
  pdftk_path <- find_pdftk()
  if (length(pdftk_path) == 0) {
    message("PDFtk Server not found on your machine. \n",
            "HTML files will remain separate of the final report.")
    invisible(return(NULL))
  }
  if (!grepl("^[\"']", pdftk_path))
    pdftk_path <- shQuote(pdftk_path)
  if (file.exists(in_pdf)) {
    out_pdf = file.path(dirname(in_pdf), paste("output", basename(in_pdf), sep = "-"))
  } else stop("Final report pdf not found.")

  cmd <- paste(pdftk_path, shQuote(in_pdf), "attach_files",
               paste(shQuote(in_html), collapse = " "), "output",
               shQuote(out_pdf))

  status <- system(cmd)
  if(status != 0) {
    message("HTML files failed to attach.")
  } else {
    if (file.exists(out_pdf)) {
      file.rename(out_pdf, file.path(dirname(out_pdf),
                                     sub("^output-", "", basename(out_pdf))))
      unlink(in_html)
      message(paste(strwrap(paste("Interactive bat detection map(s) successfully",
                                  "attached to the final report. Separate HTML",
                                  "files removed.")), collapse = "\n"))
    }
  }

  invisible(return(NULL))

}
