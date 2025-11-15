# Load required packages
library(shiny)
library(leaflet)
library(googlesheets4)
library(dplyr)
library(shinyjs)
library(DT)
library(tidyr)

# Authenticate and load sheet
gs4_auth(path = "...")  
sheet_id <- "..."

# loading authorized businesses
get_authorized_business <- function() {
  tryCatch({
    data <- read_sheet(sheet_id, sheet = "Business")
    required_cols <- c("ReferenceNumber", "Phone", "Region", "Place")
    missing_cols <- setdiff(required_cols, names(data))
    for (col in missing_cols) data[[col]] <- ""
    mutate(data, Phone = as.character(Phone))
  }, error = function(e) {
    message("Failed to load business data: ", e$message)
    tibble(ReferenceNumber = character(), Phone = character(), Region = character(), Place = character())
  })
}

# Loading dictionary here :
get_dictionary_data <- function() {
  tryCatch({
    df <- read_sheet(sheet_id, sheet = "Dictionary")
    
    required_cols <- c("Category", "Name", "Location", "Price", "TimesOpen", "Website", "Contact",
                       "Excellent", "Good", "Satisfactory", "Poor")
    for (col in required_cols) {
      if (!col %in% names(df)) {
        df[[col]] <- ifelse(col %in% c("Category", "Name", "Location", "Price", "TimesOpen", "Website", "Contact"), "", 0)
      }
    }
    
    df <- df %>%
      mutate(across(c("Excellent", "Good", "Satisfactory", "Poor"), ~ as.numeric(replace_na(as.character(.), "0"))))
    
    return(df)
  }, error = function(e) {
    message("Failed to load dictionary data: ", e$message)
    data.frame()
  })
}

# For review review
sheet_append_vote <- function(business_name, category, vote_type, user_id, comment) {
  tryCatch({
    vote_data <- data.frame(
      Timestamp = Sys.time(),
      Name = business_name,
      Category = category,
      Vote = vote_type,
      Comment = comment,
      UserID = user_id
    )
    sheet_append(ss = sheet_id, data = vote_data, sheet = "Votes")
  }, error = function(e) {
    message("Failed to append vote: ", e$message)
  })
}

# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Lucian Dictionary Search"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Category:", choices = c("Loading...")),
      textInput("search", "Search:", ""),
      uiOutput("results"),
      hr(),
      h4("Submit a Review"),
      uiOutput("review_buttons"),
      textAreaInput("commentInput", "Your Comment:", "", rows = 3),
      actionButton("submitReview", "Submit Review"),
      textOutput("ratingOutput"),
      hr(),
      h4("Authorized Business Access"),
      textInput("refNumber", "Reference Number:"),
      textInput("phoneInput", "Phone Number:"),
      actionButton("checkAuth", "Authorize"),
      uiOutput("authUI")
    ),
    mainPanel(
      h3("Details"),
      verbatimTextOutput("details"),
      uiOutput("website"),
      verbatimTextOutput("review_average"),
      
      tags$div(
        tags$style(HTML(".green-label { color: green; }")),
        HTML("<p class='green-label'>JSur copyright ©2023</p> Sponsors:"),
        p("Silver Sponsor: JnBaptiste Foundation", br(), "Gold sponsor: AGM Network Solution Advance Caribbean Businesses"), 
        br(),
        p(strong("Contact me if you want to include your business information: jsurvey758@gmail.com"))
        
        
        
      )  
    )
  )
)


get_votes_data <- function() {
  tryCatch({
    read_sheet(sheet_id, sheet = "Votes")
  }, error = function(e) {
    message("Failed to load votes data: ", e$message)
    data.frame()
  })
}


# Server
server <- function(input, output, session) {
  votes_data <- reactiveVal(get_votes_data())
  
  dictionary_data <- reactiveVal(get_dictionary_data())
  get_votes_data <- function() {
    tryCatch({
      read_sheet(sheet_id, sheet = "Votes")
    }, error = function(e) {
      message("Failed to load votes data: ", e$message)
      data.frame()
    })
  }
  
  authorized_data <- reactiveVal(get_authorized_business())
  selected_item <- reactiveVal(NULL)
  user_votes <- reactiveValues(voted = list())
  user_id <- paste0(sample(letters, 8, TRUE), collapse = "")
  auth_success <- reactiveVal(FALSE)
  vote_choice <- reactiveVal(NULL)
  
  observe({
    data <- dictionary_data()
    req(nrow(data) > 0)
    updateSelectInput(session, "category", choices = unique(data$Category))
  })
  
  filteredResults <- reactive({
    data <- dictionary_data()
    req(input$category, input$search)
    data_cat <- data[data$Category == input$category, ]
    search <- tolower(input$search)
    if (search == "") data_cat$Name else data_cat$Name[grepl(search, tolower(data_cat$Name))]
  })
  
  output$results <- renderUI({
    results <- filteredResults()
    if (length(results) > 0) {
      tagList(lapply(results, function(result) {
        actionLink(inputId = paste0("link_", result), label = result)
      }))
    } else {
      h5("No results found")
    }
  })
  
  calculate_average_rating <- function(row) {
    votes <- votes_data()
    if (is.null(row) || nrow(row) == 0 || nrow(votes) == 0) return("No reviews yet")
    
    matched <- votes %>% filter(Name == row$Name & Category == row$Category)
    if (nrow(matched) == 0) return("No reviews yet")
    
    score_map <- c("Excellent" = 4, "Good" = 3, "Satisfactory" = 2, "Poor" = 1)
    scores <- score_map[matched$Vote]
    avg <- mean(as.numeric(scores), na.rm = TRUE)
    paste0(round(avg, 2), " / 5 stars")
  }
  
  
  update_reviews <- function(result) {
    data <- dictionary_data()
    row <- data[data$Name == result & data$Category == input$category, ]
    selected_item(row)
    
    output$details <- renderText({
      paste("Location:", row$Location, "\n",
            "Price:", row$Price, "\n",
            "Reviews:", calculate_average_rating(row), "\n",
            "Times Open:", row$TimesOpen)
    })
    output$review_average <- renderText({
      paste("Average Review:", calculate_average_rating(row))
    })
    output$website <- renderUI({
      if (!is.null(row$Website) && row$Website != "")
        tags$a(href = row$Website, "Visit Website", target = "_blank")
    })
    output$review_buttons <- renderUI({
      tagList(
        actionButton("thumbsUp", HTML("<span style='color:brown;'>👍</span>")),
        actionButton("thumbsDown", HTML("<span style='color:brown;'>👎</span>"))
      )
    })
  }
  
  observe({
    results <- filteredResults()
    lapply(results, function(result) {
      local({
        res <- result
        observeEvent(input[[paste0("link_", res)]], {
          update_reviews(res)
        })
      })
    })
  })
  
  observeEvent(input$thumbsUp, {
    vote_choice("Excellent")
  })
  observeEvent(input$thumbsDown, {
    vote_choice("Poor")
  })
  
  observeEvent(input$submitReview, {
    row <- selected_item()
    if (is.null(row)) return()
    key <- paste0(row$Name, "_", row$Category)
    if (!is.null(user_votes$voted[[key]])) return()
    vote_type <- vote_choice()
    if (is.null(vote_type)) return()
    sheet_append_vote(row$Name, row$Category, vote_type, user_id, input$commentInput)
    user_votes$voted[[key]] <- TRUE
    dictionary_data(get_dictionary_data())
    get_votes_data <- function() {
      tryCatch({
        read_sheet(sheet_id, sheet = "Votes")
      }, error = function(e) {
        message("Failed to load votes data: ", e$message)
        data.frame()
      })
    }
    
    votes_data(get_votes_data())
    
    update_reviews(row$Name)
  })
  
  output$ratingOutput <- renderText({
    row <- selected_item()
    if (is.null(row)) return("")
    key <- paste0(row$Name, "_", row$Category)
    if (!is.null(user_votes$voted[[key]])) {
      "Thank you for your rating!"
    } else {
      "Please rate this business."
    }
  })
  
  observeEvent(input$checkAuth, {
    refData <- authorized_data()
    is_auth <- any(refData$ReferenceNumber == input$refNumber & refData$Phone == input$phoneInput)
    auth_success(is_auth)
  })
  
  output$authUI <- renderUI({
    if (auth_success()) {
      tagList(
        h4("Authorized: Add Listing"),
        textInput("newCategory", "Category:"),
        textInput("newName", "Business Name:"),
        textInput("newLocation", "Location:"),
        textInput("newPrice", "Price:"),
        textInput("newTime", "Times Open:"),
        textInput("newWebsite", "Website URL:"),
        actionButton("submitNew", "Submit")
      )
    }
  })
  
  observeEvent(input$submitNew, {
    tryCatch({
      new_entry <- data.frame(
        Category = input$newCategory,
        Name = input$newName,
        Location = input$newLocation,
        Price = input$newPrice,
        Reviews = NA,
        TimesOpen = input$newTime,
        Website = input$newWebsite,
        Contact = input$phoneInput,
        Excellent = 0,
        Good = 0,
        Satisfactory = 0,
        Poor = 0
      )
      sheet_append(sheet_id, new_entry, sheet = "Dictionary")
      dictionary_data(get_dictionary_data())
      
      
      votes_data(get_votes_data())
      
    }, error = function(e) {
      message("Failed to append new business: ", e$message)
    })
  })
}

shinyApp(ui = ui, server = server)
