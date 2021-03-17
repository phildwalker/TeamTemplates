#' @title make project template
#' @description build a project template that has the associated directories
#'     commonly used
#' @param full default is T, build the reduced structure or full structure
#' @param location the path you wish to put these documents
#' @param shiny with default of \code{FALSE}
#' @param add_report default is \code{TRUE}
#' @family project template
#' @export
#'

make_project_templates <- function(full = T, shiny = FALSE, location = here::here(), add_report = T){

  if(shiny){
    make_shiny_app_pkg()
    return(invisible(NULL))
  }


  if(!dir.exists(location)){
    warning("Creating directory...")
    dir.create(location)
  }

  if(!all(is.logical(c(full, add_report)))){
    stop("You have not entered valid arguments for `full` and/or `add_report`")
  }

  in_full <- folder_name <- NULL

  directory_structure <- dplyr::tribble(
    ~"folder_name", ~"in_full",
    "src", T,
    "munge", T,
    "data", T,
    "data-raw",T,
    "libs", F,
    "report", T,
    "output", T,
    "tests", F
  )

  directories_to_build <-dplyr::pull(
    dplyr::filter(directory_structure, in_full ==full),
    folder_name)

  # Now build the folders

  purrr::map(purrr::map2(.x = directories_to_build, .y =location, ~file.path(.y,.x)),dir.create)

  # Add a Default report
  if(add_report){
    fs::file_copy(path = system.file("rmarkdown/templates/ea_report/skeleton/skeleton.Rmd",
                                     package = this_pkg()),
                  new_path = file.path(location, "report", "report.Rmd"), overwrite = T)
  }


}
