#' Footer Generation for EA Reports
#'@param location default value of \code{NULL} for where to generate the document
#'@param author_email the email to use for the author. If not specified,
#'    Then the rmarkdown email will be used if available. If not email specified
#'    then the footer will omit this entire line.
#' @keywords internal
#'@export
write_ea_report_footer <- function(location = NULL, author_email = rmarkdown::metadata$email, author_name = NULL){

  logo_location <- system.file("logos/ea_logo_big.png",
                               package = "eastyle")

  if(is.null(author_name)){

  if(is.null(author_email)){
    if(is.null(rmarkdown::metadata$email)){
      footer_text <- glue::glue('
&nbsp;
<hr />
<p style="text-align: center;">A work by Cone Health Enterprise Analytics</a></p>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<p style="text-align: center;">
<img src="{logo_location}" alt="logo" class="center" style="width:30%">
</p>
&nbsp;
      ')
    }} else{
      author_email <- ifelse(is.null(author_email),
                            rmarkdown::metadata$email,
                            author_email)

      footer_text <- glue::glue('
&nbsp;
<hr />
<p style="text-align: center;">A work by Cone Health Enterprise Analytics</a></p>
<p style="text-align: center;"><span style="color: #808080;"><em>Analyst: {author_email}</em></span></p>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<p style="text-align: center;">
<img src="{logo_location}"  alt="logo" class="center" style="width:30%">
</p>
&nbsp;
  ')
    }
  } else{
      author_formatted <- make_proper_list(author_name)

    if(is.null(author_email)){
      if(is.null(rmarkdown::metadata$email)){
        footer_text <- glue::glue('
&nbsp;
<hr />
<p style="text-align: center;">A work by Cone Health Enterprise Analytics</a></p>
<p style="text-align: center;"><span style="color: #808080;"><em>Prepared by: {author_formatted}</em></span></p>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<p style="text-align: center;">
<img src="{logo_location}" alt="logo" class="center" style="width:30%">
</p>
&nbsp;
      ')
      }} else{
        author_email <- ifelse(is.null(author_email),
                               rmarkdown::metadata$email,
                               author_email)

        footer_text <- glue::glue('
&nbsp;
<hr />
<p style="text-align: center;">A work by Cone Health Enterprise Analytics</a></p>
<p style="text-align: center;"><span style="color: #808080;"><em>Prepared by: {author_formatted}</em></span></p>
<p style="text-align: center;"><span style="color: #808080;"><em>Contact: {author_email}</em></span></p>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<p style="text-align: center;">
<img src="{logo_location}"  alt="logo" class="center" style="width:30%">
</p>
&nbsp;
  ')

  }


  footer_text
  }
}

