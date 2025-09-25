## path: app-071-conditional-dashboard/app.R
## MS book: header/position
## my book: listing
## implement score and rank charts into conditional sidebar dashboard


rwb <- readRDS(gzcon(url("https://raw.githubusercontent.com/petzi53/rwb-book/master/data/chap011/rwb/rwb.rds")))

library(shiny)
library(bslib)

ui <- page_fluid(
    navset_card_pill(
        title = "World Press Freedom Indices (WPFI)",
        id = "nav",
        sidebar = sidebar(
            # conditionalPanel(
            #     "input.nav == 'Map'",
            #     "Map controls"
            # ),
            conditionalPanel(
                "input.nav == 'Score/Rank'",
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
                ),
            ),
            # conditionalPanel(
            #     "input.nav == 'Components'",
            #     "Components"
            # ),
            conditionalPanel(
                "input.nav == 'Country'",
                selectInput(
                    inputId = "my_country",
                    label = "Choose a country",
                    choices = unique(rwb$country_en),
                    multiple = FALSE
                ),
            )
        ),
        card(
            card_header((textOutput("card_title"))),
            plotlyOutput("p")
        ),
        # nav_panel("Map", "Display the world map"),
        # nav_panel("Score/Rank", "Display score or rank distribution"),
        # nav_panel("Components", "Display score/rank components"),
        # nav_panel("Country", "Display country information")
        nav_panel("Map"),
        nav_panel("Score/Rank"),
        nav_panel("Components"),
        nav_panel("Country")


    )
)

server <- function(input, output) {
    # Reactive logic can access the selected tab via input$nav
    observe({
        if (input$nav == "Score/Rank") {
            pal = RColorBrewer::brewer.pal(12, "Paired")

            countries <- reactive({
                req(input$country)

                output$card_title <-  renderText({
                    if (input$var == "score") {
                        s = "Global Score for"
                    }
                    if (input$var == "rank") {
                        s = "Global Rank for"
                    }
                    s = paste(s, input$country[1])
                    if (length(input$country)  > 1) {
                        for (i in 2:length(input$country)) {
                            s <- paste(s, input$country[i], sep = ", ")
                        }
                    }
                    s
                })

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
                        geom_text(data = countries() |>  filter(year_n == min(year_n)),
                                  aes(label = iso), nudge_x = -1,
                                  size = 5, color = "black",
                                  hjust = 1) +
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
    })

}

shinyApp(ui = ui, server = server)


