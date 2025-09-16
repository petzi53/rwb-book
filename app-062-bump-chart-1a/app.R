## app-081-bump-chart-1a
## line & bump chart with iso3 country names

suppressWarnings(suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(dplyr)
    library(plotly)
    library(ggplot2)
    library(ggbump)
}))


rwb <- readRDS(gzcon(url("https://raw.githubusercontent.com/petzi53/rwb-book/master/data/chap011/rwb/rwb.rds")))

ui <- page_sidebar(
    titlePanel("World Press Freedom Indices (WPFI)"),
    sidebar = sidebar(
        selectInput(
            inputId = "var",
            label = "Score or Rank Type",
            choices = c(
                    "Global score" = "score",
                    "Global Rank" = "rank"
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
        # card_header((textOutput("card_title"))),
        plotlyOutput("p")
    )
)

server <-  function(input, output) {

    pal = RColorBrewer::brewer.pal(12, "Paired")

    countries <- reactive({
        req(input$country)

        # transferred and simplified #######################
        # output$card_title <-  renderText({
        #     s = paste(
        #         "World Press Freedom Index for",
        #         input$country[1]
        #     )
        #     if (length(input$country)  > 1) {
        #         for (i in 2:length(input$country)) {
        #             s <- paste(s, input$country[i], sep = ", ")
        #         }
        #     }
        #     s
        # })
        #####################################################

        rwb |>
            select(year_n, input$var, country_en, iso) |>
            filter(country_en %in% input$country) |>
            arrange(year_n) |>
            na.omit() |>
            droplevels()
    })

    output$p <- renderPlotly({
        req(countries())
        length(pal) <- length(input$country)
        pal <- setNames(pal, input$country)
        if (input$var == "score") {
            plot <- plotly::plot_ly(
                    data = countries(),
                    x = ~year_n,
                    y = ~score,
                    color = ~country_en,
                    colors = pal,
                    type = 'scatter',
                    mode = 'lines+markers',
                    line = list(width = 4),
                    marker = list(size = 20)
                )
        }
        if (input$var == "rank") {
            plot <- ggplot(countries(),
               aes(x = year_n,
                   y = rank,
                   color = country_en)
               ) +
                geom_bump(linewidth = 1.0) +
                geom_point(size = 5) +
                ggimage::geom_flag(data = countries() |>  filter(year_n == min(year_n)),
                          aes(year_n = country_en), size = .05) +
                          # aes(label = ~country_en), nudge_x = -1,
                          # size = 5, color = "black",
                          # hjust = 1) +
                geom_text(data = countries() |> filter(year_n == max(year_n)),
                          aes(label = iso), nudge_x = 1,
                          size = 5, color = "black",
                          hjust = 0) +
                theme_bw() +
                theme(legend.position = "none") +
                scale_y_reverse(
                    breaks = waiver(),
                    n.breaks = 25) +
                scale_colour_manual(values = pal)
            plot <- ggplotly(plot)
        }
        plot

    })
}

shinyApp(ui, server)
