
# Set the theme for ggplot2 to minimal
.onLoad <- function(libname, pkgname){

  knitr::knit_hooks$set(inline = clean_numbers,
                        dropdown = knit_dropdown,
                        chunk = brand_hooks)

}
