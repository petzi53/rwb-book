## app-061-line-chart-pws-3
## Choose several countries to display WPFI for all available years
## @cnj-061-ine-chart-pws-3


suppressWarnings(suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(dplyr)
    library(plotly)
}))

rwb <- readRDS(gzcon(url("https://raw.githubusercontent.com/petzi53/rwb-book/master/data/chap011/rwb/rwb.rds")))

ui <- page_sidebar(
    titlePanel("Evolution of the World Press Freedom Index (WPFI) 2013-2025"),
    sidebar = sidebar(
        selectInput(
            inputId = "country",
            label = "Country",
            choices = unique(rwb$country_en),
            multiple = TRUE
        ),
        actionButton("go", "Go!", class = "btn-success")
    ),
    card(
        card_header((textOutput("card_title"))),
        plotlyOutput("my_chart")
    )
)

server <- function(input, output, session) {

    output$card_title <-  renderText({
        my_countries <- filter(countries(), country_en %in% input$country)
        txt <- unique(my_countries$country_en)
        s = paste("World Press Freedom Index for", txt[1])
        for (i in 2:length(txt)) {
            s <- paste(s, txt[i], sep = ", ")
        }
        s
    })

    countries <- eventReactive(input$go, {
        rwb |>
            select(year_n, score, country_en) |>
            filter(country_en %in% input$country) |>
            arrange(year_n) |>
            na.omit() |>
            droplevels()
    })

    output$my_chart <- renderPlotly({
        plotly::plot_ly(
            data = countries(),
            x = ~year_n,
            y = ~score,
            color = ~country_en,
            colors = RColorBrewer::brewer.pal(12, "Paired"),
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(size = 10)
        )
    })
}

shinyApp(ui, server)


