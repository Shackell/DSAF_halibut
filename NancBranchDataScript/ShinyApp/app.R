library(shiny)
library(bslib)

# Define UI
ui <- page_sidebar(
  title = "Distribution Shift Indicator Prototype Shiny App",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar_width = "300px",
  
  sidebar = sidebar(
    h4("Select Indicators to Display"),
    
    checkboxGroupInput(
      "selected_indicators",
      "Choose indicators:",
      choices = list(
        "Abundance" = "Abdtrend",
        "Spatial Abundance" = "ShinyAbdMap12",
        "Centre of Gravity" = "COG_Reg_map", 
        "Range Edges" = "Range Edge Map.png",
        "Distance to Border" = "Distmap"
      ),
      selected = "Abdtrend"
    ),
    
    hr(),
    
    actionButton("select_all", "Select All", class = "btn btn-outline-primary btn-sm"),
    actionButton("clear_all", "Clear All", class = "btn btn-outline-secondary btn-sm"),
    
    hr(),
    
    p("This app displays Shift indicators. Select one or more indicators to view them simultaneously.")
  ),
  
  # Dynamic UI for selected indicators
  uiOutput("dynamic_plots")
)

# Define server logic
server <- function(input, output, session) {
  
  # Handle select all / clear all buttons
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session = session,
                             "selected_indicators",
                             selected = c("Abdtrend", "ShinyAbdMap12", "COG_Reg_map", "Range Edge Map.png", "Distmap"))
  })
  
  observeEvent(input$clear_all, {
    updateCheckboxGroupInput(session = session,
                             "selected_indicators",
                             selected = character(0))
  })
  
  # Function to create image HTML with minimal whitespace
  create_image_html <- function(filename, title) {
    # Handle case where filename might be NULL or empty
    if (is.null(filename) || length(filename) == 0 || filename == "") {
      return(tags$div(
        style = "text-align: center; padding: 20px;",
        h5("No filename specified", style = "color: red;"),
        p(paste("Indicator:", title), style = "color: #666; font-size: 12px;")
      ))
    }
    
    file_path <- file.path("www", filename)
    
    # Check if file exists and handle the result properly
    file_exists <- file.exists(file_path)
    
    if (length(file_exists) > 0 && file_exists) {
      tags$div(
        style = "text-align: center; margin: 0; padding: 0;",
        tags$img(
          src = filename,
          alt = paste("Plot:", title),
          style = "width: 100%; height: auto; max-height: 450px; object-fit: contain; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); display: block;"
        )
      )
    } else {
      tags$div(
        style = "text-align: center; padding: 20px;",
        h5("Image file not found", style = "color: red;"),
        p(paste("Looking for:", filename), style = "color: #666; font-size: 12px;"),
        p(paste("Full path:", file_path), style = "color: #666; font-size: 10px;")
      )
    }
  }
  
  # Function to get description for each indicator
  get_description <- function(indicator) {
    switch(indicator,
           "Abdtrend" = "Abundance over time, indicating changes in juvenile stock size. Increasing in both US and Canada but rate of increase has leveled off lately",
           
           "ShinyAbdMap12" = "Distribution of juvenile halibut abundance from 1990-2005 and 2006-2023: Note the increase throughout region including NF",
           
           "COG_Reg_map" = "The Centre of Gravity map shows the geographic center point of the species distribution and how it has shifted over time. Movement of this center point indicates directional changes in species distribution, often related to climate change or habitat modifications.",
           
           "Distmap" = "This distance to border analysis measures how close the species distribution is to management boundaries or geographic limits. Values closer to zero indicate the species is approaching or at the edge of its surveyed range.",
           
           "Range Edge Map.png" = "The range edges map identifies the boundaries of juvenile halibut distribution, showing both the leading edge (expanding) and trailing edge (stable) of the species range. This is crucial for understanding range shifts; the population is expanding more than it is shifting",
           
           "Unknown indicator - please add description for this indicator.")
  }
  
  # Dynamic plot generation based on selected indicators
  output$dynamic_plots <- renderUI({
    if (length(input$selected_indicators) == 0) {
      tags$div(
        style = "text-align: center; padding: 50px;",
        h4("No indicators selected", style = "color: #666;"),
        p("Please select one or more indicators from the sidebar.")
      )
    } else {
      # Create cards for selected indicators with descriptions below images
      cards <- lapply(input$selected_indicators, function(indicator) {
        
        # Map indicator codes to filenames and titles - updated to match your choices
        filename <- switch(indicator,
                           "Abdtrend" = "Abdtrend.png",
                           "ShinyAbdMap12" = "ShinyAbdMap12.png",
                           "COG_Reg_map" = "COG_Reg_map.png",
                           "Distmap" = "Distmap.png", 
                           "Range Edge Map.png" = "Range Edge Map.png",
                           NULL)  # Default case
        
        title <- switch(indicator,
                        "Abdtrend" = "Abundance",
                        "ShinyAbdMap12" = "Spatial Abundance",
                        "COG_Reg_map" = "Centre of Gravity",
                        "Distmap" = "Distance to Border",
                        "Range Edge Map.png" = "Range Edges",
                        paste("Unknown indicator:", indicator))  # Default case
        
        description <- get_description(indicator)
        
        # Custom card with description below image
        tags$div(
          class = "card h-100",
          style = "margin-bottom: 15px;",
          tags$div(
            class = "card-header",
            style = "padding: 12px 16px; background-color: #f8f9fa; border-bottom: 1px solid #dee2e6;",
            h5(title, style = "margin: 0; font-size: 1.1rem; font-weight: 600;")
          ),
          tags$div(
            class = "card-body",
            style = "padding: 10px;",
            # Image first, then description below
            tags$div(
              create_image_html(filename, title),
              tags$p(
                description,
                style = "font-size: 0.9rem; line-height: 1.4; color: #333; text-align: justify; margin-top: 10px; padding: 5px; background-color: #f8f9fa; border-radius: 5px;"
              )
            )
          )
        )
      })
      
      # Arrange cards in responsive grid for side-by-side viewing
      tags$div(
        style = "margin: 0; padding: 0;",
        if (length(input$selected_indicators) == 1) {
          # Single card - full width
          cards[[1]]
        } else if (length(input$selected_indicators) == 2) {
          # Two cards - side by side
          layout_columns(col_widths = c(6, 6), gap_size = "15px", cards[[1]], cards[[2]])
        } else if (length(input$selected_indicators) == 3) {
          # Three cards - one full width, two half width below
          tagList(
            cards[[1]],
            layout_columns(col_widths = c(6, 6), gap_size = "15px", cards[[2]], cards[[3]])
          )
        } else if (length(input$selected_indicators) == 4) {
          # Four cards - 2x2 grid
          layout_columns(
            col_widths = c(6, 6, 6, 6),
            gap_size = "15px",
            cards[[1]], cards[[2]], cards[[3]], cards[[4]]
          )
        } else {
          # More than 4 cards - flexible grid
          layout_columns(
            col_widths = rep(6, length(cards)),
            gap_size = "15px",
            !!!cards
          )
        }
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)