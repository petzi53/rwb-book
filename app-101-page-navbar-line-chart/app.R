## path: app-071-page-navbar-line-chart/app.R
## my book: @sec-071-page-navbar-line-chart

library(shiny)
library(shinythemes)
library(bslib)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggbump)


rwb <- readRDS(gzcon(url("https://raw.githubusercontent.com/petzi53/rwb-book/master/data/chap011/rwb/rwb.rds")))


sidebar_map <- page_sidebar(
    sidebar = "Controls for map"
)

sidebar_chart <- list(
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
)


sidebar_country <- page_sidebar(
    sidebar = "Control for country"
)

cards <- list(
    card(
        full_screen = TRUE,
        card_header("This is a map"),
    ),
    card(
        full_screen = TRUE,
        card_header((textOutput("card_title"))),
        plotlyOutput("p")
    ),
    card(
        full_screen = TRUE,
        card_header("This is country information")
    )
)

ui <- page_navbar(
    theme = bs_theme(5, bootswatch = "cosmo"),
    title = "Reporters Without Borders",
    id = "nav",
    sidebar = sidebar(
        conditionalPanel(
            "input.nav === 'Map'",
            "Map controls"
        ),
        conditionalPanel(
            "input.nav === 'Chart'",
            sidebar_chart
        ),
        conditionalPanel(
            "input.nav === 'Country'",
            "Country controls"
        )
    ),
    nav_spacer(),
    nav_panel("Map", cards[[1]]),
    nav_panel("Chart", cards[[2]]),
    nav_panel("Country", cards[[3]]),
    nav_item(tags$a("About",
                    href = "https://rsf.org/en/index",
                    target = "_blank"))
)

server <-  function(input, output) {
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

shinyApp(ui, server)
