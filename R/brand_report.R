#' Generating an EA Report Automatically
#' @description This function will automatically compile an EA
#'     style report without having to tweak the internals of the yaml header
#' @param toc default is \code{TRUE}
#' @param toc_float default is \code{TRUE}
#' @param df_print default is \code{"paged"}
#' @param highlight default is \code{"tango"}
#' @param code_folding default is \code{"hide"}
#' @param fig_width default is \code{10}
#' @param fig_height default is \code{6}
#' @param fig_retina default is \code{1}
#' @param code_download default is \code{TRUE}
#' @param analyst the analyst name to be used
#' @param email the email to be included in the footer
#' @param draft if "DRAFT" is to be printed
#' @param audience the targeted audience for the output one of \code{"internal"}
#'     or \code{"client"}
#' @param ... any additional parameters to pass to \code{html_document2}
#' @export
brand_report <- function(toc = TRUE,
                      toc_float = TRUE,
                      df_print = "paged",
                      highlight = "tango",
                      code_folding = "hide",
                      fig_width = 10,
                      fig_height = 6,
                      fig_retina = 1,
                      code_download = TRUE,
                      analyst,
                      email,
                      draft = FALSE,
                      audience = "internal",
                      ...) {


  if(!all(is.logical(c(toc, toc_float, code_download, draft)))){
    stop("Verify that you have entered TRUE/FALSE for toc, toc_float, code_download, and draft")
  }

  audience <- match.arg(arg = audience, choices = c("internal", "client"))

  # Set Audience Params
  audience_param <- list(
    internal = c(doc_code_folding = code_folding,
                 doc_code_download = code_download),
    client = c(doc_code_folding = "none",
               doc_code_download = FALSE)
  )


  # get the locations of resource files located within the package
  css <- tempdir()
  eastyle::apply_cone_css(css_style = "report", location = css)
  css <- file.path(css, "styles.css")
  #css <- system.file("rmarkdown/templates/ea_handout/resources/style.css",
  #                   package = "eaverse")

  logo <-system.file("logos/ea_logo_transparent.png",
                     package = "eastyle")

  before_body_material <- readLines(system.file("rmarkdown/templates/ea_report/resources/ea_header2.html",
                                                package = this_pkg()), encoding = "UTF-8")

  before_body_material[3] <-glue::glue(before_body_material[3])

  # Has the logo icon
  icon_location<- system.file("favicon.ico", package = this_pkg())

  # Location
  head_text <- glue::glue('<link rel="shortcut icon" href="{icon_location}">')

  in_header <- tempfile(fileext = ".html")

  cat(head_text, file = in_header)

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
    bookdown::html_document2(toc = toc,
                             toc_float = toc_float,
                             df_print = df_print,
                             code_folding = audience_param[[audience]]["doc_code_folding"][[1]],
                             #code_folding = "none",
                             highlight = highlight,
                             fig_width = fig_width,
                             fig_height = fig_height,
                             fig_retina = fig_retina,
                             code_download = as.logical(audience_param[[audience]]["doc_code_download"][[1]]),
                             #theme = NULL,
                             css = css,
                             includes = rmarkdown::includes(in_header = in_header,
                                                            before_body = before_body,
                                                            after_body = footer),
                             ...)

}
