pkgs <- c("fs", "rmarkdown",
          "xfun", "glue",
          "distill", "bookdown",
          "pagedown","magrittr",
          "dplyr",
          'eastyle', 'here', 'knitr', 'lazyeval', 'purrr',
          'readr',  'scales', 'stringr'
)

lapply(pkgs, usethis::use_package)

pkg_suggest <- c('crayon', 'miniUI', 'rstudioapi','shiny', 'shinyFiles',
                 'usethis', 'xaringan', 'eadb', 'eageo', 'crosstalk')

lapply(pkg_suggest, usethis::use_package, type = 'Suggests')
