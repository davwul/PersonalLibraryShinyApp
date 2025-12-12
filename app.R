#### Shiny app for personal library ####

## Features


# References --------------------------------------------------------------

# Shiny
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/
# https://mastering-shiny.org/index.html
# rcrossref
# https://docs.ropensci.org/rcrossref/index.html
# reactable table
# https://glin.github.io/reactable/index.html


# Libraries ---------------------------------------------------------------



library(shiny)
library(bslib)
library(DT)
library(rcrossref)
library(stringr)
library(reactable)
library(htmltools)

###


######

df <- read.csv2("library_df.csv")

### Shiny App ###

## UI

ui <- page_sidebar(
  title = "My Library",
  sidebar = sidebar(
    
    # Enter DOI
    textInput("doi", "Enter DOI", placeholder="10.1371/journal.pntd.0012693"),
    
    # Enter tag
    textAreaInput("tag", "Enter tag(s)", placeholder = "E.g. 'author', 'T cells, 'assay'..."),
    
    # Enter notes
    textAreaInput("note", "Enter notes"),
    
    # Button to add entry
    actionButton("add", "Add to Library")
  ),
  card(full_screen = TRUE,
       card_header("Library"),
       
       # Tabs
       navset_tab(
         id = "library_tabs",
         nav_panel("Notes", reactableOutput("main_table")),
         nav_panel("Details", reactableOutput("details_table"))
       )
       )
)

## Server



server <- function(input, output, session) {
  
  # Load initial data
  library_df <- reactiveVal(df)
  
  observeEvent(input$add, {
    req(input$doi)  # Ensure DOI is provided
    
    # Fetch citation
    
    withProgress(message = "Adding paper to library...", value = 0, {
      incProgress(0.5, detail = "Fetching citation...")
      citation <- cr_cn(dois = input$doi, format = "text", style = "apa")
      incProgress(1, detail = "Done")
    })
    
      
    ## Get article data
    
    article_data <- cr_works(dois = input$doi, 
                             format = "text", style = "apa")
    article_data <- article_data$data
    
    #  title
    title <- article_data$title
    
    # date
    published_date <- article_data$created
    
    # Type
    article_type <- article_data$type
    
    # journal
    article_journal <- article_data$short.container.title
    
    # abstract
    abstract <- tryCatch(
      cr_abstract(doi = input$doi),
      error = function(e) {
        "No abstract found"
      }
    )
    abstract <- paste0(substr(abstract, 1, 500), "...")
      
    # URL
    doi_url <- article_data$url
    
    # Create PDF filename from DOI by removing slashes
    pdf_name <- paste0(gsub("/", "", input$doi), ".pdf")
    
    # Current data
    current <- library_df()
    
    # Create new row
    new_row <- data.frame(
      Index = nrow(current) + 1,
      Date = published_date,
      Tag = input$tag,
      Title = title,
      Type = article_type,
      Notes = input$note,
      Journal = article_journal,
      Abstract = abstract,
      Citation = citation,
      URL = doi_url,
      PDF = pdf_name,
      stringsAsFactors = FALSE
    )
    
    # Update reactive data frame
    updated_df <- rbind(current, new_row)
    library_df(updated_df)
    write.csv2(updated_df, "library_df.csv", row.names = FALSE)
    
    # Clear inputs
    updateTextInput(session, "doi", value = "")
    updateTextAreaInput(session, "tag", value = "")
    updateTextAreaInput(session, "note", value = "")
  })
  
  # Tab 1: Notes
  output$main_table <- renderReactable({
    reactable(
      library_df()[, c("Index", "Date", "Tag", "Title", "Type", "Notes"), drop = FALSE],
      columns = list(
        Index = colDef(name = "i", width = 40),
        Date  = colDef(format = colFormat(date = TRUE),
                       name = "Date", width = 120,),
        Tag   = colDef(name = "Tag", width = 100),
        Type = colDef(name = "Type", width = 100),
        Title = colDef(name = "Title", minWidth = 300),
        Notes = colDef(name = "Notes", minWidth = 400)
      ),
      searchable = TRUE,
      filterable = TRUE,
      highlight = TRUE,
      striped = TRUE,
      showSortable = TRUE,
      defaultPageSize = 5,
      resizable = TRUE
    )
  })
  
  # Tab 2: Details
  output$details_table <- renderReactable({
    reactable(
      library_df()[, c("Index", "Journal", "Abstract", "Citation", "URL", "PDF"), drop = FALSE],
      columns = list(
        Index = colDef(name = "i", width = 40),
        Journal = colDef(name = "Journal", width = 100),
        Abstract = colDef(name = "Abstract", minWidth = 350),
        Citation = colDef(name = "Citation", minWidth = 300),
        URL = colDef(name = "DOI",
                     filterable = FALSE,
                     cell = function(value) {
                       tags$a(href = value, target = "_blank", "Open DOI")
                     }),
        PDF = colDef(name = "PDF",
                     filterable = FALSE,
                     cell = function(value) {
                       if (!is.na(value)) tags$a(href = paste0("PDFs/", value),
                                                 target = "_blank", "Open PDF")
                     })
      ),
      searchable = TRUE,
      filterable = TRUE,
      highlight = TRUE,
      striped = TRUE,
      showSortable = TRUE,
      defaultPageSize = 5,
      resizable = TRUE
    )
  })
}


## Run App
addResourcePath("PDFs", "PDFs")

shinyApp(ui = ui, server = server)


###


