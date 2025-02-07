library(shiny)
library(bslib)
library(rvest)
library(httr2)
library(dplyr)
library(stringr)

# --- Helper Functions ---

fetch_page <- function(url) {
  tryCatch({
    req_obj <- request(url)
    resp <- req_perform(req_obj)
    if (resp_status(resp) >= 400) {
      stop("Error: Unable to fetch the URL. HTTP status code: ", resp_status(resp))
    }
    read_html(resp_body_string(resp))
  }, error = function(e) {
    NULL
  })
}

extract_links <- function(page, base_url, css_selector) {
  tryCatch({
    page %>%
      html_elements(css_selector) %>%
      html_attr("href") %>%
      url_absolute(base_url) %>%
      unique()
  }, error = function(e) {
    NULL
  })
}

filter_internal_links <- function(links, base_url) {
  domain <- sub("^.+://([^/]+).*$", "\\1", base_url)
  links[str_detect(links, fixed(domain))]
}

# --- UI ---

ui <- fluidPage(
  title = "Outlinks Explorer",
  tags$head(
    tags$style(HTML("
        /* Fixed navigation bar */
        .nav-container {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            background: white;
            padding: 15px 20px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            z-index: 1000;
        }
        /* Container for all content below nav */
        .content-container {
            margin-top: 140px;
            padding: 0 20px;
        }
        /* URL and CSS input styling in nav */
        .url-controls {
            display: flex;
            gap: 10px;
            margin: 0 auto;
            padding: 10px 0;
        }
        /* Remove extra bottom margin from Shiny input containers */
        .url-controls .shiny-input-container {
            margin-bottom: 0;
            padding: 0;
            width: auto;
        }
        /* Enforce a 2:1 width ratio */
        .url-input {
            flex: 2;
        }
        .css-selector-input {
            flex: 1;
        }
        /* Force consistent height and full width for inputs */
        .url-controls .form-control {
            height: 38px;
            padding: 6px 12px;
            width: 100%;
        }
        /* Adjust buttons so they align with the inputs */
        .url-controls .btn {
            align-self: flex-end;
            margin-top: 0;
        }
        /* Links list styling */
        .link-list {
            max-width: 1200px;
            margin: 0 auto;
            padding: 10px 0;
        }
        .link-list h4 {
            margin: 0 0 15px 0;
        }
        .link-item {
            margin: 2px 0;
            padding: 4px 8px;
            display: flex;
            align-items: center;
        }
        .link-item:hover {
            background-color: #f8f9fa;
        }
        .link-icon {
            cursor: pointer;
            color: #007bff;
            margin-right: 10px;
            flex-shrink: 0;
        }
        .link-icon:hover {
            color: #0056b3;
        }
        .link-item a {
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
        }
        .error-message {
            color: red;
            margin-top: 10px;
        }
        /* Title styling */
        .app-title {
            margin: 0 0 15px 0;
            color: #2c3e50;
        }
        /* Debug info styling */
        .debug-info {
            padding: 10px;
            margin-bottom: 20px;
            background-color: #f8f9fa;
            border-radius: 4px;
        }
    ")),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
    )
  ),
  
  # Fixed navigation bar
  div(
    class = "nav-container",
    h1(class = "app-title h2", "Outlinks Explorer"),
    div(
      class = "url-controls",
      div(
        class = "url-input",
        textInput("url", label = "URL of the page to explore")
      ),
      div(
        class = "css-selector-input",
        textInput("css_selector", "CSS Selector", value = "")
      ),
      actionButton("explore",
        HTML('<i class="fas fa-search"></i> Explore Links'),
        class = "btn btn-primary"
      ),
      actionButton("back",
        HTML('<i class="fas fa-arrow-left"></i> Back'),
        class = "btn btn-secondary"
      )
    )
  ),
  
  # Scrollable content below nav bar
  div(
    class = "content-container",
    div(
      class = "debug-info",
      verbatimTextOutput("debugInfo")
    ),
    uiOutput("links")
  )
)

# --- Server ---

server <- function(input, output, session) {
  url_stack <- reactiveVal(character(0))
  
  explore_links <- function(url) {
    req(url)
    
    page <- fetch_page(url)
    if (is.null(page)) {
      output$links <- renderUI({
        HTML("<p class='error-message'>Unable to fetch the URL</p>")
      })
      return()
    }
    
    css_selector <- if (nzchar(trimws(input$css_selector))) {
      paste(trimws(input$css_selector), "a")
    } else {
      "a"
    }
    
    links <- extract_links(page, url, css_selector)
    if (is.null(links)) {
      output$links <- renderUI({
        HTML("<p>No links found on the page.</p>")
      })
      return()
    }
    
    links_filtered <- filter_internal_links(links, url)
    if (length(links_filtered) == 0) {
      output$links <- renderUI({
        HTML("<p>No internal links found.</p>")
      })
      return()
    }
    
    output$exploredUrl <- renderUI({
      HTML(paste("<p>Exploring:", url, "</p>"))
    })
    
    output$links <- renderUI({
      tagList(
        tags$ol(
          lapply(seq_along(links_filtered), function(i) {
            tags$li(
              actionLink(
                inputId = paste0("link_", i),
                label = links_filtered[i],
                onclick = sprintf("Shiny.setInputValue('clicked_link', '%s');", links_filtered[i])
              )
            )
          })
        )
      )
    })
  }
  
  observeEvent(input$explore, {
    explore_links(input$url)
    url_stack(c(url_stack(), input$url))
  })
  
  observeEvent(input$clicked_link, {
    updateTextInput(session, "url", value = input$clicked_link)
    explore_links(input$clicked_link)
    url_stack(c(url_stack(), input$clicked_link))
  })
  
  observeEvent(input$back, {
    stack <- url_stack()
    if (length(stack) > 1) {
      stack <- stack[-length(stack)]
      previous_url <- tail(stack, 1)
      url_stack(stack)
      updateTextInput(session, "url", value = previous_url)
      explore_links(previous_url)
    }
  })
}

shinyApp(ui = ui, server = server)
