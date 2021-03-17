#' Knit Summary Block
#' @param before before the code chunk is executed
#' @param options any additional options
#' @param envir the environment
#' @export

knit_summary <- function(before, options, envir) {
  if (before) {
    
    my_lab <-knitr::opts_current$get()$label
    
    my_lab <- ifelse(is.null(my_lab), 
                     paste0(sample(letters, 5), runif(5, min = 1, 10000)), 
                     my_lab)
    
    glue::glue(
      "div class = 'summaryblock'>",  .sep = "\n")
  } else {
    paste("</div>", sep = "\n")
  }
}

#'Second Generation Change for Block Knitting
#'@param x the input
#'@param options more information on options
#'@export

ea_hooks <- function(x, options) {
  regular_output = hook_chunk(x, options)
  
  # if there is a description
  if (!is.null(options$summary)) {
    # Remove the `r something` pattern and wrap with the glue curly braces
    regular_output <- gsub(pattern = "`r (.[^`]+)`", replacement = "{\\1}", x = regular_output)
    # Wrap in glue to allow evaluation
    regular_output <- glue::glue(regular_output)
    # wrap the summaryblock div
    paste0('<div class = "summaryblock">', regular_output, "</div>") 
    
  } else if (!is.null(options$alert)){
    # Remove the `r something` pattern and wrap with the glue curly braces
    regular_output <- gsub(pattern = "`r (.[^`]+)`", replacement = "{\\1}", x = regular_output)
    # Wrap in glue to allow evaluation
    regular_output <- glue::glue(regular_output)
    # wrap the summaryblock div
    paste0('<div class = "alertblock">', regular_output, "</div>") 
  } else {
    
    # if there isn't a description just return unmodified
    return(regular_output)  # pass to default hook
    
  }
}


# Helper Code for Knitting ea_hooks correctly
hook_chunk = function(x, options) 
{
  fence_char = '`'
  fence = paste(rep(fence_char, 3), collapse = '')
  x = gsub(paste0("[\n]{2,}(", fence, "|    )"), "\n\n\\1", 
           x)
  x = gsub("[\n]+$", "", x)
  x = gsub("^[\n]+", "\n", x)
  if (isTRUE(options$collapse)) {
    x = gsub(paste0("\n([", fence_char, "]{3,})\n+\\1(", 
                    tolower(options$engine), ")?\n"), "\n", x)
  }
  if (is.null(s <- options$indent)) 
    return(x)
  line_prompt(x, prompt = s, continue = s)
}

# Knitr Helper from knitr:::line_prompt

line_prompt <- function (x, prompt = getOption("prompt"), continue = getOption("continue")) 
{
  paste0(prompt, gsub("(?<=\n)(?=.|\n)", continue, x, 
                      perl = TRUE))
}