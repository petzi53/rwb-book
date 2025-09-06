## app-061-line-chart-pws-2
## Choose several countries to display WPFI for all available years
## @cnj-061-ine-chart-pws-2

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
            label = "Choose countries",
            choices = unique(rwb$country_en),
            multiple = TRUE
        )
    ),
    card(
        card_header((textOutput("card_title"))),
        plotlyOutput("p")
    )
)

server <- function(input, output, session) {

    output$card_title <-  renderText({
        my_countries <- filter(countries(), country_en %in% input$country)
        txt <- unique(my_countries$country_en)
        s = paste("World Press Freedom Index:", txt[1])
        if (length(txt) > 1) {
            for (i in 2:length(txt)) {
                s <- paste(s, txt[i], sep = ", ")
            }
        }
        s
    })

    countries <- reactive({
        req(input$country)
        rwb |>
            select(year_n, score, country_en) |>
            filter(country_en %in% input$country) |>
            arrange(year_n) |>
            na.omit() |>
            droplevels()
    })

    output$p <- renderPlotly({
        req(countries())
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


