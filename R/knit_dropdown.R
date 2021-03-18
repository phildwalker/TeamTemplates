#' Add Dropdown Box to Knitr Options
#' @description adds an option to rmarkdown code chunks to have a dropdown.
#'     This will allow users to more easily hide entire code chunks if desired.
#' @param before before option
#' @param options the options option
#' @param envir the environment
#' 
#' @examples \dontrun{# In an Rmarkdown Code Chunk Put the Following:
#' 
#' knitr::knit_hooks$set(dropdown = knit_dropdown)
#' 
#' }
#' 
#' 
#' @export

knit_dropdown <- function(before, options, envir) {
  if (before) {
    
    my_lab <-knitr::opts_current$get()$label
    
    my_lab <- ifelse(is.null(my_lab), 
                     paste0(sample(letters, 5), runif(5, min = 1, 10000)), 
                     my_lab)
    
    glue::glue(
      '<p>',
      '<button class="btn btn-primary collapsed" data-toggle="collapse" data-target="#{my_lab}">',
      '</button>',
      '</p>',
      '<div class="collapse" id="{my_lab}">',
      '<div class="card card-body">',  .sep = "\n")
  } else {
    paste("</div>", "</div>", sep = "\n")
  }
}