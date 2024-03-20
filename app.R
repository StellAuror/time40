library(bs4Dash)
library(shiny)
library(shinyWidgets)
library(htmltools)
library(htmlwidgets)
library(rvest)
library(plotly)
library(waiter)
library(stringr)
library(wordcloud2)
library(tm)
library(SnowballC)
library(tesseract)
library(magick)
library(qdap)

stopwords <- read.delim("stopwords.txt", header = F, sep = "\n")

main_green_color <- "#cccccc"  # Kolor główny (ciemny zieleń)
accent_green_color <- "#cccccc"  # Kolor akcentu (jasny zieleń)
background_color <- "#2f353a"  # Kolor tła (ciemny)


get_last_page_name <- function(links) {
  last_page_names <- sapply(links, function(link) {
    url_parts <- strsplit(link, "/")[[1]]
    last_page_name <- tail(url_parts, 2)[2]
    return(last_page_name)
  })
  return(last_page_names)
}

get_category <- function(tea, categories) {
  x <- NULL
  i <- 0
  for (t in tea) {
    for (category in names(categories)) {
      if (t %in% categories[[category]]) {
        x <- c(x, category)
      }
    }
    
    i <- i + 1
    if (length(x) < i) x <- c(x, "Others")
  }
  x
}


ui <- bs4Dash::bs4DashPage(
  dark = NULL,
  header = bs4Dash::bs4DashNavbar(),
  body = bs4Dash::bs4DashBody(
    useWaiter(),
    includeCSS("style.css"),
    fluidRow(
      column(
        width = 12,
        box(
          width = 12,
          solidHeader = T,
          status = "gray-dark",
          title = "Comparison of the cheapest and the most expensive alternatives of a specific drink.",
          
          plotlyOutput("plot.pricing"),
          fluidRow(
            actionBttn(
              inputId = "inpt.refresh",
              label = "Clear selection",
              style = "simple",
              size = "xs",
              color = "primary",
              block = T
            ) 
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          width = 12,
          solidHeader = T,
          status = "gray-dark",
          title = "Customer evaluation of a drink quality",
          
          plotlyOutput("plot.rating")
        ) 
      ),
      column(
        width = 6,
        box(
          width = 12,
          solidHeader = T,
          status = "gray-dark",
          title = "Words frequently used to describe a drink",
          
          wordcloud2::wordcloud2Output("plot.word")
        ),
      )
    ),
    fluidRow(
      box(
        width = 12,
        solidHeader = T,
        status = "gray",
        collapsed = F,
        headerBorder = F,
        title = "Scrapped data",
        
        DT::dataTableOutput("outpt.table")
      )
    )
  ),
  sidebar = bs4DashSidebar(
    minified = T,
    expandOnHover = F,
    collapsed = T,
    bs4SidebarMenu(
      id = "filter",
      bs4SidebarMenuItem(
        text = "Home",
        icon = icon("home"),
        tabName = "home"
      ),
      bs4SidebarMenuItem(
        selected = T,
        badgeColor = "success",
        text = "Filter",
        icon = icon("filter"),
        tabName = "filter"
      )
    )
  )
)

server <- function(output, input, session) {
  ### UI
  ToC <- reactive({
    if (input$drink.switch == T) {
      readRDS("tealist.rds")
    } else {
      readRDS("coffeelist.rds")
    }
  })
  
  ToC_C <- reactive({
    if (input$drink.switch == T) {
      readRDS("tea_cat.rds")
    } else {
      readRDS("coffee_cat.rds")
    }
    
  })
  
  output$ui.filter <- renderUI({
    fluidRow(
      p("The more you load the longer it will take! Scrapping algoryithm was designed not to overload scrapped webpage."),
      pickerInput(
        inputId = "inpt.url",
        label = "Choose tea type",
        choices = ToC(),    
        selected = ToC()[[1]][1:3],
        options = list(`actions-box` = TRUE),
        multiple = T,
        width = "98%"
      ),
      numericInput(
        inputId = "inpt.pages",
        label = "How many pages should I go through?",
        value = 1, max = 10, min = 1,
        width = "98%"
      ),
      actionBttn(
        inputId = "inpt.go",
        "Let's see",
        icon = icon("search"),
        style = "simple",
        color = "success",
        size = "sm",
        width = "98%"
      ),
      h6("The data presented herein originates from an external source and is not intended for commercial use."), br(),
      p("source: https://fiveoclock.eu")
    )
  })
  
  
  observeEvent(input$filter, {
    if (input$filter == "filter") {
      showModal(
        modalDialog(
          title = "Find your tea!",
          footer = NULL,
          
          fluidRow(
            column(
              width = 4,
              h5(strong("Caution!")),
            ),
            column(
              width = 4, offset = 4,
              shinyWidgets::switchInput(
                inputId = "drink.switch",
                label = "Or",
                value = T,
                onLabel = "Tea",
                offLabel = "Coffee",
                onStatus = NULL,
                offStatus = NULL,
                size = "small",
                width = NULL
              )
            )
          ),
          uiOutput("ui.filter")
        )
      )
      updateTabItems(session, "filter", "home")
    }
  })
  
  observeEvent(input$inpt.go, {
    removeModal()
  })
  
  
  ### Scrapping script
  raw_scrapped <- reactive({
    if (input$inpt.go < 1) return()
    
    # Waiter
    waiter_show(color = "#292929",
                html = tagList(
                  div(
                    id = 'myDiv', class = 'simpleDiv', 
                    spin_inner_circles()
                  ),
                  h1("Scrapping...\n It's a perfect time for a cup of tea!")
                )
                
    )
    
    # Pre-defined df structure
    tea_data <- data.frame(Name = character(),
                           Type = character(),
                           Price = character(),
                           Rating = numeric(),
                           Description = character(),
                           stringsAsFactors = FALSE)
    
    for (URL in isolate(input$inpt.url)) {
      for (i in 1:isolate(input$inpt.pages)) {
        url <- paste0(URL,"/page/", i)
        
        # If url exists, then get the page
        tryCatch({
          page <- read_html(url)
        }, error = function(e) {
          url <- "error"
        })
        if (url == "error") {
          return(tea_data[seq(1, nrow(tea_data), 2),])
        }
        
        # Get all the data
        products <- page %>%
          html_nodes(".product-small")
        for (product in products) {
          Name <- product %>%
            html_node(".name") %>%
            html_text()
          Price <- product %>%
            html_node(".price") %>%
            html_text()
          Rating <- product %>%
            html_node(".rating") %>%
            html_text() %>% as.numeric
          Description <- product %>%
            html_node(".box-excerpt") %>%
            html_text()
          Picture <- product  %>%
            html_nodes("img.lazy-load") %>%
            html_attr("data-src")
          
          # Adding new records
          tea_data <- 
            rbind(
              tea_data,
              data.frame(
                Name = Name,
                Type = get_last_page_name(URL),
                Price_Min = min(as.numeric(unlist(strsplit(gsub(",", ".", gsub("[^0-9,–]", "", Price)), "–")), na.rm = T)),
                Price_Max = max(as.numeric(unlist(strsplit(gsub(",", ".", gsub("[^0-9,–]", "", Price)), "–")), na.rm = T)),
                Rating = Rating,
                Description = Description,
                Picture = paste0('<img src="',Picture , '"></img>'),
                stringsAsFactors = FALSE
              )
            )
        }
      }
    }
    tea_data %>% unique
  })
  scrapped <- reactive({
    df <- NULL
    if (!is.null(raw_scrapped()))
      df <- raw_scrapped() %>%  mutate(Category = get_category(raw_scrapped()$Type, ToC_C()))
    
    waiter_hide()
    df <- df %>% unique
    scrapped.filter(df)
    df
  })
  
  ### DT with scrapped data
  output$outpt.table <- DT::renderDataTable({
    DT::datatable(
      scrapped.filter(),
      style = "bootstrap4",
      options = list(pageLength = 5, dom = 'tip'),
      escape = F,
      extensions = 'Responsive',
      rownames = F
      
    )
  })
  
  ### Plotly - Drink max vs. min price [Scatter plot]
  output$plot.pricing <- renderPlotly({
    input$inpt.refresh
    req(scrapped())
    scrapped() %>% group_by(Type, Price_Min, Price_Max) %>% summarise(Size = dplyr::n()) %>%
      plot_ly(
        y = ~Price_Min,
        source = "plot.pricing",
        x = ~Price_Max,
        color = ~Type, colors = 'Paired',
        mode = "scatter", 
        marker = list(symbol  = "circle", size = ~Size*3 + 10, opacity = .9)
      ) %>% layout(dragmode = 'lasso') %>%
      config(displayModeBar = F)
  })
  
  scrapped.filter <- reactiveVal(data.frame())
  observeEvent(event_data("plotly_selected", source = "plot.pricing"), {
    
    if (!event_data("plotly_selected", source = "plot.pricing") %>% identical(list())) {
      brushed_points <- event_data("plotly_selected", source = "plot.pricing")
      if (nrow(brushed_points) > 0) {
        scrapped.filter(scrapped() %>% filter(Price_Max %in% brushed_points$x, Price_Min %in% brushed_points$y))
      } else {
        scrapped.filter(scrapped())
      } 
    }
  })
  
  observeEvent(input$inpt.refresh, {
    scrapped.filter(scrapped())
  })
  
  ### Plotly - Customers' rating [Sunburst plot]
  output$plot.rating <- renderPlotly({
    req(scrapped())
    
    df <- scrapped.filter() %>% mutate(Rating = paste0("Avg. Rate: ", as.character(round(Rating, 1))))
    df.h1 <- df %>% group_by(Category) %>% summarise(n = dplyr::n())
    df.h2 <- df %>% group_by(Category, Type) %>% summarise(n = dplyr::n()) 
    df.h3 <- df %>% group_by(Category, Type, Rating) %>% summarise(n = dplyr::n())
    
    plot_ly(
      labels = c(df.h1$Category, df.h2$Type, df.h3$Rating),
      parents = c(rep("", nrow(df.h1)), df.h2$Category, df.h3$Type),
      values = c(df.h1$n, df.h2$n, df.h3$n),
      type = 'sunburst',
      branchvalues = 'total'
    )
  })
  
  ### Wordcloud2 - wordcloud with drink description
  output$plot.word <- wordcloud2::renderWordcloud2({
    req(scrapped())
    
    df <- paste(scrapped.filter()$Description, collapse = " ") %>%
      bracketX() %>%
      replace_abbreviation() %>%
      replace_contraction() %>%
      replace_symbol() %>%
      tolower() %>%
      removePunctuation() %>%
      removeNumbers() %>%
      stripWhitespace() %>%
      removeWords(stopwords[[1]])
    
    df <- table(strsplit(df, " "))
    df <- data.frame(word = names(df), freq = as.numeric(df)) 
    df <- df %>% arrange(-freq) %>% head(20)
    
    wordcloud2(df, size = 1.5, widgetsize = "100%")
  })
}

shinyApp(ui = ui, server = server)
