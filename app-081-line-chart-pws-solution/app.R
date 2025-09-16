## app-081-line-chart-pws-solution
## Choose several countries to display WPFI for all available years
## @cnj-081-line-chart-pws-solution

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

    pal = RColorBrewer::brewer.pal(12, "Paired")                # added

    countries <- reactive({
        req(input$country)

        # transferred and simplified #######################
        output$card_title <-  renderText({
            s = paste(
                "World Press Freedom Index for",
                input$country[1]
                )
            if (length(input$country)  > 1) {
                for (i in 2:length(input$country)) {
                    s <- paste(s, input$country[i], sep = ", ")
                }
            }
            s
        })
        #####################################################

        rwb |>
            select(year_n, score, country_en) |>
            filter(country_en %in% input$country) |>
            arrange(year_n) |>
            na.omit() |>
            droplevels()
    })

    output$p <- renderPlotly({
        req(countries())
        length(pal) <- length(input$country)                    # added
        pal <- setNames(pal, input$country)                     # added
        plotly::plot_ly(
            data = countries(),
            x = ~year_n,
            y = ~score,
            color = ~country_en,
            colors = pal,                                       # changed
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(size = 10)
        )
    })
}

shinyApp(ui, server)


