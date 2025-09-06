## app-061-line-chart-pws-1a
## Choose country to display WPFI for all available years
## Using ggplotly()
## @cnj-061-ine-chart-pws-1a

suppressWarnings(suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(ggplot2)
    library(dplyr)
    library(plotly)
}))

rwb <- readRDS(gzcon(url("https://raw.githubusercontent.com/petzi53/rwb-book/master/data/chap011/rwb/rwb.rds")))

theme_set(theme_bw())

ui <- page_sidebar(
    titlePanel("Evolution of the World Press Freedom Index (WPFI) 2013-2025"),
    sidebar = sidebar(
        selectInput(
            inputId = "country",
            label = "Country",
            choices = unique(rwb$country_en)
        )
    ),
    card(
        card_header((textOutput("card_title"))),
        plotlyOutput("p")                              # (1)
    )
)

server <- function(input, output, session) {
    output$card_title <-  renderText({
        paste("World Prees Freedom Index for", input$country)
    })

    output$p <- renderPlotly({                         # (2)
        rwb <- rwb |>
            select(year_n, country_en, score) |>
            filter(country_en == input$country) |>
            na.omit() |>
            ggplot(aes(year_n, score)) +
            geom_line() +
            geom_point()
        ggplotly(rwb)
    })
}

shinyApp(ui, server)
