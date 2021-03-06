library(shiny)
library(utils)
library(readxl)
library(xlsx)
################################################################################
#### Functions & constants
################################################################################

# css file
stylesheet <- 'styles.css'
# image (in www folder)
vib_logo <- 'vib_rf_plant_systems_biology_rgb_pos.png'

# functions

transcript_to_gene <- function(transcript){
  # replace _FGT by _FG;
  # e.g. AC148152.3_FGT001 to AC148152.3_FG001
  gene <- gsub("_FGT", "_FG", transcript)
  # extract first part of all gene IDs starting with G
  # e.g. GRMZM2G000014_T01 to GRMZM2G000014
  for (i in 1:length(gene)){
    if (is.na(gene[i]) || is.null(gene[i])){
      # next
      gene[i] <- ""
    } else if (substr(gene[i], 0, 1) == "G") {
      # split at underscore, extract first part of split
      gene[i] <- strsplit(gene[i], "_" )[[1]][1]
    }
  }
  return(gene)
}


################################################################################

# ui <- fluidPage(includeCSS(stylesheet),
#                 navbarPage(title = "RNA-Seq Database",
#                            tab_view,
#                            tab_data,
#                            tab_options,
#                            tab_update,
#                            footer=img(src=vib_logo, class= "footer")
#                 )
# )


# Define UI for data upload app ----
ui <- fluidPage(
  # use CSS
  includeCSS(stylesheet),
  
  # App title ----
 # titlePanel("Maize: transcript to gene codes"),
 navbarPage(title = "Maize: transcript to gene codes",
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      # Sidebar panel for inputs ----
      # Input: Select a file ----
      fileInput(inputId = "file1", 
                label = "Choose file with maize transcript codes:",
                multiple = FALSE
      ),
      
      # Horizontal line ----
      tags$hr(),
      radioButtons("filetype", "File type",
                   choices = c("Excel" = 0,
                               "CSV or plain text" = 1),
                   selected = 1),
      # IF CSV Input: Select separator ----
      conditionalPanel(
        condition = "input.filetype == 1",
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        
        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        
        # Input: Does file have header ----
        checkboxInput("header", "Header", FALSE)
        ), # end if CSV

      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      
      tags$img(src=vib_logo)

    ),
    # Main panel for displaying outputs ----
    mainPanel(
      h3("Preview of the original file:"),
      # Output: Data file ----
      tableOutput("original_file"),
      h3("Preview of the gene codes:"),
      tableOutput("new_file"),
      h3("Download the altered file:"),
      downloadButton("download_button", label = "Download")
    )
  )
)
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  datafile <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        if(input$filetype == 1){
        df <- read.table(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        } else {
          df <- read_excel(input$file1$datapath)
          df <- as.data.frame(df)
        }
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(df)
  })
  
  output$original_file <- renderTable({
    if(input$disp == "head") {
      return(head(datafile()))
    }
    else {
      return(datafile())
    }
  })
  
  gene_codes <- reactive({
    apply(datafile(),2,transcript_to_gene)
  })
  
  output$new_file <- renderTable({
    if(input$disp == "head") {
      return(head(gene_codes()))
    }
    else {
      return(gene_codes())
    }
  })
  
  # download SQL lines
  output$download_button <- downloadHandler(
    filename = function(){
      paste0("gene_codes_", input$file1$name)
    },
    content = function(file) {
      if(input$filetype == 1){
        write.csv(gene_codes(), file, row.names = FALSE)
      } else {
        write.xlsx2(gene_codes(), file, sheetName = "Sheet1",
                    col.names = TRUE, row.names = FALSE, append = FALSE)
      }
    }
  )
  
  # stop when app is closed
  session$onSessionEnded(function(session){
    stopApp()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
