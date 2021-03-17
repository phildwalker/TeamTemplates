#' Generating an EA Report Automatically
#' @description This function will automatically compile an EA
#'     style report without having to tweak the internals of the yaml header
#' @param toc default is \code{TRUE}
#' @param toc_float default is \code{TRUE}
#' @param df_print default is \code{"paged"}
#' @param highlight default is \code{"tango"}
#' @param code_folding default is \code{"hide"}
#' @param fig_width default is \code{6.5}
#' @param fig_height default is \code{4}
#' @param code_download default is \code{TRUE}
#' @param analyst the analyst name to be used
#' @param email the email to be included in the footer
#' @param draft if "DRAFT" is to be printed
#' @param ... any additional parameters to pass to \code{html_document2}
#' @export
ea_gitbook <- function(toc = TRUE,
                      toc_float = TRUE,
                      df_print = "paged",
                      highlight = "tango",
                      code_folding = "hide",
                      fig_width = 6.5,
                      fig_height = 4,
                      code_download = TRUE,
                      analyst,
                      email,
                      draft = FALSE,
                      ...) {


  if(!all(is.logical(c(toc, toc_float, code_download, draft)))){
    stop("Verify that you have entered TRUE/FALSE for toc, toc_float, code_download, and draft")
  }

  # get the locations of resource files located within the package
  css <- tempdir()
  eastyle::apply_cone_css(css_style = "report", location = css)
  css <- file.path(css, "styles.css")

  before_body_material <- readLines(system.file("rmarkdown/templates/ea_report/resources/ea_header.html",
                                                package = "eatemplates"))

  # Has the logo icon
  in_header<- system.file("rmarkdown/templates/ea_report/resources/in_header.html",
                          package = "eatemplates")

  before_body <- tempfile(fileext = ".html")

  cat(before_body_material, file = before_body)

  if(isTRUE(draft)){
    txt <- '<div class="watermark">DRAFT</div>'
    #body
    cat(txt, file = before_body, append = TRUE)
  }


  # Create footer to write additional text
  footer <- tempfile(fileext = ".html")

  # Clean Up When Function Ends
  #on.exit(unlink(footer))

  foot_text <- write_ea_report_footer(author_email = email, author_name = analyst)

  cat(foot_text, file = footer)

  # call the base html_document function
  bookdown::gitbook(#toc = toc,
                          #toc_float = toc_float,
                           #df_print = df_print,
                           #code_folding = code_folding,
                           #highlight = highlight,
                           #fig_width = fig_width,
                           #fig_height = fig_height,
                           #code_download = code_download,
                           #theme = NULL,
                           css = css,
                           #includes = rmarkdown::includes(in_header = in_header,
                          #                                before_body = before_body,
                          #                                after_body = footer),
                           ...)
}
