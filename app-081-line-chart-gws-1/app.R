## app-081-line-chart-gws-1
## Choose country to display WPFI for all available years (with ggplot)
## @cnj-081-gws-line-chart-1


library(shiny)
library(bslib)
library(ggplot2)
library(munsell)
library(dplyr)

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
        plotOutput("p")
    )
)

server <- function(input, output, session) {
    output$card_title <-  renderText({
        paste("World Prees Freedom Index for", input$country)
    })

    output$p <- renderPlot({
        rwb |>
            select(year_n, country_en, score) |>
            filter(country_en == input$country) |>
            na.omit() |>
            ggplot(aes(year_n, score)) +
            geom_line() +
            geom_point()
    })
}

shinyApp(ui, server)
