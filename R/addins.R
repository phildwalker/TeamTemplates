#' Insert a Basic Map
#' @keywords addins
#' @export
#' 

add_ea_map <- function(){
code_chunk <- '
```{r leaflet-mapping, warning=FALSE, message=FALSE, out.width="80%"}
require(leaflet) # If leaflet is not already loaded
leaflet() %>% 
  addProviderTiles() %>% 
  addLayersControl(overlayGroups = c("Show Hospitals"))  %>%
  setView() %>% #center between gboro and burlington, include reidsville
  addAwesomeMarkers(data = hospitals,  label = ~Hospital,
                    icon = hospital_icon(),group = "Show Hospitals")

```

'  

rstudioapi::insertText(code_chunk)
  
}

#' Insert a Summary Code Chunk
#' @keywords addins
#' @export

add_summary_block <- function(){
  code_chunk <- '
```{block, summary = TRUE, echo = TRUE}
The purpose of this section is to give a summary of the ask for the project.

The key takeaways are as follows:

* The first conclusion is this
* Our second conclusion is this

Our data set, `mtcars` has `r nrow(mtcars)` observations and `r ncol(mtcars)` parameters.

Our True North Metrics...

```

'  
  
  rstudioapi::insertText(code_chunk)
}

#' Addin for publish_to_ah for publishing Distill Articles to the Knowledge Repo
#' @keywords addins
#' @export

addin_publish_to_ah <-function(){
  options(shiny.maxRequestSize=30*1024^2)
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Moving a Report to the Knowledge Repository"),
    miniUI::miniContentPanel(
      shinyFiles::shinyFilesButton(id = "inFile",label =  "Select the Appropriate File (often ends in *-distill.html" ,
                       title = "Please select a file:", multiple = FALSE,
                       buttonType = "default", class = NULL),
      
      #shiny::textOutput("txt_file"),     
      shiny::checkboxInput( inputId = "inOverwrite",label =  "Overwrite Existing File If It Exists?", value = TRUE),
      shiny::hr(),
      shiny::fluidRow(shiny::column(4, shiny::verbatimTextOutput("value")))
    )
  )
  
  server <- function(input, output, session) {
    
    ## Your reactive logic goes here.
    
    # Listen for the 'done' event. This event will be fired when a user
    # is finished interacting with your application, and clicks the 'done'
    # button.
    
    volumes = shinyFiles::getVolumes()
    
    #current_wd = 
    
    shinyFiles::shinyFileChoose(input, "inFile", 
                                roots = c(home = getwd(), volumes()), 
                                #roots = volumes(), 
                                defaultPath =  getwd(),
                                session = session)
    
    #output$txt_file <- shiny::reactive({shiny::renderText(as.character(file_selected$datapath))})
    
    shiny::observeEvent(input$done, {
      
      
      
      if(!is.null(input$inFile)){
        #file_selected<-shinyFiles::parseFilePaths(volumes, input$inFile)
        file_selected<-shinyFiles::parseFilePaths(c(home = getwd(), volumes()), input$inFile)
        #print(file_selected)
        #print(as.character(file_selected$datapath))
        publish_to_ah(file_name = as.character(file_selected$datapath),
                      over_write = input$inOverwrite)
        
      }
      
      cat(file_selected$datapath)
      shiny::stopApp()
    })
  }
  
  viewer <- shiny::dialogViewer("Publishing to the Knowledge Repository!", width = 500, height = 450)
  shiny::runGadget(ui, server, viewer = viewer)
}

#' Connection String Default
#' @keywords addins
#' @export
#' 
add_connection_string <- function(){
code_chunk <-
'
```{r establish-connections}
connection_name <- DBI::dbConnect(odbc::odbc(), dsn = "MCCBIEDW1")
```
'  
rstudioapi::insertText(code_chunk)
  
}
