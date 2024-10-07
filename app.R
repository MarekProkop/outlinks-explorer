library(shiny)
library(rvest)
library(httr2)
library(dplyr)
library(stringr)

ui <- fluidPage(
  titlePanel("Internal Link Explorer"),
  sidebarLayout(
    sidebarPanel(
      textInput("url", "URL of the page to explore", value = "https://www.hotely.cz/ceska-lipa"),
      textInput("css_selector", "Optional CSS Selector to limit links from", value = ""),
      actionButton("explore", "Explore Links"),
      actionButton("back", "Back")
    ),
    mainPanel(
      uiOutput("links")
    )
  )
)

server <- function(input, output, session) {
  # LIFO stack to store URL history
  url_stack <- reactiveVal(character(0))

  # Event to explore links using the URL from the input field
  explore_links <- function(url) {
    req(url)

    # Save the current URL to the stack before exploring
    url_stack(c(url_stack(), url))

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
  })

  # Observe link clicks and update the URL input, then trigger exploration
  observeEvent(input$clicked_link, {
    updateTextInput(session, "url", value = input$clicked_link)
    explore_links(input$clicked_link)
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
