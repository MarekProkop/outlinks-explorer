library(shiny)
library(bslib)
library(rvest)
library(httr2)
library(dplyr)
library(stringr)

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
            margin-top: 140px;  /* Adjust based on nav height */
            padding: 0 20px;
        }

        /* URL input and buttons styling */
        .url-controls {
            display: flex;
            gap: 10px;
            align-items: start;
            max-width: 1200px;
            margin: 0 auto;
            padding: 10px 0;
        }

        .url-input {
            flex: 1;
            min-width: 0;
        }

        /* Override Shiny's default form-group width limitation */
        .url-input .form-group {
            width: 100%;
            max-width: none;
        }

        .url-input .form-control {
            width: 100%;
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
        div(
          textInput("url",
            label = NULL,
            placeholder = "Enter URL of the page to explore..."
          )
        )
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
  div(
    textInput("css_selector", "Optional CSS Selector to limit links from", value = "")
  ),

  # Scrollable content
  div(
    class = "content-container",
    div(
      class = "debug-info",
      verbatimTextOutput("debugInfo")
    ),
    uiOutput("links")
  )
)

server <- function(input, output, session) {
  # LIFO stack to store URL history
  url_stack <- reactiveVal(character(0))

  # Event to explore links using the URL from the input field
  explore_links <- function(url) {
    req(url)

    # Fetch the page
    page <- tryCatch(
      {
        req <- request(url)
        resp <- req_perform(req)
        if (resp_status(resp) >= 400) {
          stop("Error: Unable to fetch the URL. HTTP status code: ", resp_status(resp))
        }
        read_html(resp_body_string(resp))
      },
      error = function(e) {
        return(NULL)
      }
    )

    if (is.null(page)) {
      output$links <- renderUI({
        HTML("<p>Unable to fetch the URL</p>")
      })
      return()
    }

    # Extract links based on the CSS selector (if provided)
    css_selector <- paste(input$css_selector, "a") |> trimws()
    links <- tryCatch(
      {
        page |>
          html_elements(css_selector) |>
          html_attr("href") |>
          url_absolute(url) |>
          unique()
      },
      error = function(e) {
        return(NULL)
      }
    )

    if (is.null(links)) {
      output$links <- renderUI({
        HTML("<p>No links found on the page.</p>")
      })
      return()
    }

    # filter internal links
    domain <- str_extract(url, ".+://([^/]+)", group = 1)
    links_filtered <- links[str_detect(links, domain)]

    # Return the links as a numbered list of clickable links
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
                onclick = sprintf(
                  "Shiny.setInputValue('clicked_link', '%s');", links_filtered[i]
                )
              )
            )
          })
        )
      )
    })
  }

  observeEvent(input$explore, {
    explore_links(input$url)

    # Save the URL to the stack before exploring
    url_stack(c(url_stack(), input$url))
  })

  # Observe link clicks and update the URL input, then trigger exploration
  observeEvent(input$clicked_link, {
    updateTextInput(session, "url", value = input$clicked_link)
    explore_links(input$clicked_link)
    # Save the URL to the stack before exploring
    url_stack(c(url_stack(), input$clicked_link))
  })

  # Back button event to go to the previous URL
  observeEvent(input$back, {
    stack <- url_stack()
    if (length(stack) > 1) {
      # Remove the current URL from the stack
      stack <- stack[-length(stack)]

      # Get the previous URL
      previous_url <- tail(stack, 1)
      # Update the stack
      url_stack(stack)
      # Update the URL input and explore the previous URL
      updateTextInput(session, "url", value = previous_url)
      explore_links(previous_url)
    }
  })
}

shinyApp(ui = ui, server = server)
