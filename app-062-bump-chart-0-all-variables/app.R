## app-062-bump-chart-0-all-variables
## get global scores and ranks with all components

suppressWarnings(suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(dplyr)
    library(plotly)
}))


rwb <- readRDS(gzcon(url("https://raw.githubusercontent.com/petzi53/rwb-book/master/data/chap011/rwb/rwb.rds")))

ui <- page_sidebar(
    titlePanel("World Press Freedom Indices (WPFI)"),
    sidebar = sidebar(
        selectInput(
            inputId = "var",
            label = "Score or Rank Type",
            choices = list(
                `Press Freedom Score` = list(
                    "Global score" = "score",
                    "Political Context" = "political_context",
                    "Economic Context" = "economic_context",
                    "Legal Framework" = "legal_context",
                    "Sociocultural Context" = "social_context",
                    "Safety" = "safety"),
                `Press Freedom Rank` = list(
                    "Global Rank" = "rank",
                    "Political Rank" = "rank_pol",
                    "Economic Rank" = "rank_eco",
                    "Legal Rank" = "rank_leg",
                    "Sociocultural Rank" = "rank_soc",
                    "Safety Rank" = "rank_saf")
            )
        ),
        selectInput(
            inputId = "country",
            label = "Countries",
            choices = unique(rwb$country_en),
            multiple = TRUE
        )
    ),
    card(
        card_header((textOutput("card_title"))),
        plotlyOutput("p")
    )
)

server <-  function(input, output) {

    pal = RColorBrewer::brewer.pal(12, "Paired")

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
            select(year_n, input$var, country_en) |>
            filter(country_en %in% input$country) |>
            arrange(year_n) |>
            na.omit() |>
            droplevels()
    })

    output$p <- renderPlotly({
        req(countries())
        length(pal) <- length(input$country)
        pal <- setNames(pal, input$country)
        plotly::plot_ly(
            data = countries(),
            x = ~year_n,
            y = ~get(input$var),
            color = ~country_en,
            colors = pal,
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(size = 10)
        )
    })
}

shinyApp(ui, server)
