## app-071-functions
## convert (complex) reactive into function
## Convert UI elements into function(s)

suppressWarnings(suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(dplyr)
    library(plotly)
    library(ggplot2)
    library(ggbump)
}))


rwb <- readRDS(gzcon(url("https://raw.githubusercontent.com/petzi53/rwb-book/master/data/chap011/rwb/rwb.rds")))
pal = RColorBrewer::brewer.pal(12, "Paired")

df_chart <- function(df, var, country) {
    df |>
        select(year_n, all_of(var), country_en, iso) |>
        filter(country_en %in% country) |>
        arrange(year_n) |>
        na.omit() |>
        droplevels()
}

card_title <-  function(var, country) {
    if (var == "score") {
        s = "Global Score for"
    }
    if (var == "rank") {
        s = "Global Rank for"
    }
    s = paste(s, country[1])
    if (length(country)  > 1) {
        for (i in 2:length(country)) {
            s <- paste(s, country[i], sep = ", ")
        }
    }
    s
}

select_rwb_var <- function() {
    selectInput(
        inputId = "var",
        label = "Score or Rank Type",
        choices = c(
            "Global score" = "score",
            "Global Rank" = "rank"
        )
    )
}

select_rwb_country <- function() {
    selectInput(
        inputId = "country",
        label = "Countries",
        choices = unique(rwb$country_en),
        multiple = TRUE
    )
}


##############################################################
ui <- page_sidebar(
    titlePanel("World Press Freedom Indices (WPFI)"),
    sidebar = sidebar(
        select_rwb_var(),
        select_rwb_country()
    ),
    card(
        card_header((textOutput("card_title"))),
        plotlyOutput("p")
    )
)

##############################################################
server <-  function(input, output) {

    countries <- reactive({
        req(input$country)

        output$card_title <-  renderText({
            card_title(input$var, input$country)
        })

        df_chart(rwb, input$var, input$country)
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
