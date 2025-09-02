## app-061-line-chart-pws-1b
## Choose country to display WPFI for all available years
## @cnj-061-ine-chart-pws-1b


library(shiny)
library(bslib)
library(ggplot2)
library(munsell)
library(dplyr)
library(plotly)

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
        plotlyOutput("p")
    )
)

server <- function(input, output, session) {
    output$card_title <-  renderText({
        paste("World Prees Freedom Index for", input$country)
    })

    output$p <- renderPlotly({
        rwb |>
            select(year_n, country_en, score) |>
            filter(country_en == input$country) |>
            na.omit() |>
            plot_ly(
                x = ~year_n,
                y = ~score,
                type = 'scatter',
                mode = 'lines+markers')
    })
}

shinyApp(ui, server)
