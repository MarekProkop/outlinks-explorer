# Outlink Explorer Shiny App

This Shiny application allows users to explore outlinks on a given web page. By providing a URL and an optional CSS selector, users can view a list of links pointing from the page to another pages. The app also allows navigation between pages using clickable links and a back button to return to previously explored pages.

## Features

- **Input URL**: Enter the URL of the page to explore.
- **Optional CSS Selector**: Filter the links by providing a CSS selector. This allows users to focus on specific sections of the page, such as links within a particular div, table, or other HTML element, making the exploration more targeted and efficient.
- **Explore Links Button**: Fetch the links from the given page.
- **Clickable Links**: Click on any link to explore that page.
- **Back Button**: Return to the previous page that was explored.

## Installation

To run the app locally, follow these steps:

1. Clone the repository:

   ```sh
   git clone https://github.com/MarekProkop/outlink-explorer.git
   ```

2. Set up the required R packages:

   ```r
   install.packages(c("shiny", "rvest", "httr2", "dplyr", "purrr"))
   ```

3. Run the app:

   ```r
   shiny::runApp("outlink-explorer")
   ```

## Usage

1. Enter the URL of the page you want to explore.
2. Optionally, provide a CSS selector to filter the links on the page.
3. Click the **Explore Links** button to view the list of outlinks.
4. Click on any link in the list to explore further.
5. Use the **Back** button to return to the previous page.



## Requirements

- R (version 4.0 or higher)
- The following R packages: `shiny`, `rvest`, `httr2`, `dplyr`, `purrr`

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contributing

Feel free to fork this repository and submit pull requests. All contributions are welcome!

## Author

Created by [Marek Prokop](https://github.com/MarekProkop).

## Acknowledgments

- [RStudio](https://www.rstudio.com/) for providing a great IDE for R.
- [Shiny](https://shiny.rstudio.com/) for making web application development in R easy and accessible.

