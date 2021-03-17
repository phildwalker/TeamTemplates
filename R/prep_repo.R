#' Prep A Project Repository
#'
#' This function applies good practices and writes several
#' invisible files to a repository in order to ensure that
#' secrets are not shared and that large data files are not
#' stored.
#'
#' @param location a filepath, the location to write the file
#'    with a default of \code{here::here()}
#'
#' @export

prep_repo <- function(location = here::here()){
  write_attributes(location)
  write_dockerignore(location)
  write_gitignore(location)
}

#' Write Attributes
#'
#' Adds a .gitattributes file to a directory
#'
#' @param location a filepath, the location to write the file
#'    with a default of \code{here::here()}
#'
#' @export
write_attributes <- function( location = here::here()){
  txt = 'https://raw.githubusercontent.com/conedatascience/repo-templates/master/.gitattributes'
  txt_in = readLines(txt)

  use_path = file.path(location, ".gitattributes")

  if(!file.exists(use_path)){
    file.create(use_path)
  }

  cat(paste(txt_in, collapse = "\n"), file = use_path, append = TRUE)


}
#' Write Gitignore
#'
#' Adds a .gitignore file to a directory
#'
#' @param location a filepath, the location to write the file
#'    with a default of \code{here::here()}
#'
#' @export
write_gitignore <- function( location = here::here()){
  txt = 'https://raw.githubusercontent.com/conedatascience/repo-templates/master/.gitignore'
  txt_in = readLines(txt)

  use_path = file.path(location, ".gitignore")

  if(!file.exists(use_path)){
    file.create(use_path)
  }

  cat(paste(txt_in, collapse = "\n"), file = use_path, append = TRUE)


}

#' Write Dockerignore
#'
#' Adds a .dockerignore file to a directory
#'
#' @param location a filepath, the location to write the file
#'    with a default of \code{here::here()}
#'
#' @export
write_dockerignore <- function( location = here::here()){
  txt = 'https://raw.githubusercontent.com/conedatascience/repo-templates/master/.dockerignore'
  txt_in = readLines(txt)

  use_path = file.path(location, ".dockerignore")

  if(!file.exists(use_path)){
    file.create(use_path)
  }

  cat(paste(txt_in, collapse = "\n"), file = use_path, append = TRUE)


}
