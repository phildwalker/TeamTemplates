# Retrieve Access to files in the package
pkg_resource <- function(...){
  system.file( ..., package = 'TeamTemplates', mustWork = TRUE)
}

#' Suppress all messages and warnings
#' @param x the item passed to the function, generally a function
#' @export
suppress_all <- function(x){
  suppressWarnings(suppressMessages(x))
}


#' safe installation of packages
#' @description This function checks if a package exists, if it does it installs
#'     it. If it does not find the package in CRAN it will let you know
#' @param x the package you wish to install
#' @param repo the repo you wish to use
#'
#' @export
safe_pkg_install <- function(x, repo = c(CRAN = "http://cran.rstudio.com")){
  if(!suppress_all(requireNamespace(x, character.only = T, quietly = T))){
    install.packages(x, dependencies = T, repos = repo)
  }

  if(!suppress_all(requireNamespace(x, character.only = T, quietly = T))){
    warning(sprintf("%s was not found in CRAN", x))
  }
}

# Safe directory create

safe_dir_create <- function(x, path = getwd()){
  x <- file.path(path, x)
  if(fs::dir_exists(x)){
    message(sprintf("%s already exists", x))
  } else{
    fs::dir_create(x)
  }

}

#' Utilities
#'
#'
#' Try Catch Tool
#' @param code the code which is passed to return a condition
show_condition <- function(code) {
  tryCatch(code,
           error = function(c) "error",
           warning = function(c) "warning",
           message = function(c) "message"
  )
}

#' Helper function to check that at minimum the sql
#' template contains \code{SELECT} and \code{FROM}
#' to verify that a valid query is being submitted
#' @param template the template to check
#' @export

check_sql <- function(template){
  validations <- purrr::map2_lgl(.x = paste(template, collapse = "\n"),
                                 .y = c("SELECT", "FROM"),
                                 ~grepl(pattern = .y, x = .x,
                                        ignore.case = TRUE))
  all(validations)
}


check_sql_no_count <- function(template){
  validations <- purrr::map2_lgl(.x = paste(template, collapse = "\n"),
                                 .y = c("SET", "NOCOUNT"),
                                 ~grepl(pattern = .y, x = .x,
                                        ignore.case = TRUE))
  all(validations)
}

# Safe directory create

safe_dir_create <- function(x){
  if(fs::dir_exists(x)){
    message(sprintf("%s already exists", x))
  } else{
    fs::dir_create(x)
  }

}

# Helper for Open Status

file_opened <- function(path) {
  suppressWarnings(
    "try-error" %in% class(
      try(file(path,
               open = "w+"),
          silent = TRUE
      )
    )
  )
  }
# Test the output format

get_output_format <- function() {
  output <- rmarkdown::parse_yaml_front_matter(
    readLines(knitr::current_input())
  )$output
  if (is.list(output)){
    return(names(output)[1])
  } else {
    return(output[1])
  }
}

#' Set Knitr Chunk Options for Printing
#' @export

set_knitr_chunk_options <- function(){
  if(get_output_format()=="TeamTemplates::team_report"){
    knitr::opts_chunk$set(echo = FALSE,
                          message = FALSE, warning = FALSE, fig.align = 'center')
  }
  invisible(NULL)
}


# set name of package for calling template

this_pkg <- function(){"TeamTemplates"}

pkg_unqo <- function(){TeamTemplates}
