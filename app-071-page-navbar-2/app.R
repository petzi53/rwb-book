## path: app-071-page-navbar/app.R
## MS book: header/position
## my book: listing
## implement score and rank charts into dashboard


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

cards <- list(
    card(
        full_screen = TRUE,
        card_header((textOutput("card_title"))),
        plotlyOutput("p")
    ),
    card(
        full_screen = TRUE,
        card_header("Bill depth"),
        plotOutput("bill_depth")
    ),
    card(
        full_screen = TRUE,
        card_header("Body Mass"),
        plotOutput("body_mass")
    )
)

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

color_by <- varSelectInput(
    "color_by", "Color by",
    penguins[c("species", "island", "sex")],
    selected = "species"
)

ui <- page_navbar(
    title = "Penguins dashboard: Multipage standard",
    sidebar = color_by,
    nav_spacer(),
    nav_panel("Bill Length", cards[[1]]),
    nav_panel("Bill Depth", cards[[2]]),
    nav_panel("Body Mass", cards[[3]]),
    nav_item(tags$a("Posit", href = "https://posit.co"))
)


server <- function(input, output) {
    gg_plot <- reactive({
        ggplot(penguins) +
            geom_density(aes(fill = !!input$color_by), alpha = 0.2) +
            theme_bw(base_size = 16) +
            theme(axis.title = element_blank())
    })

    output$bill_length <- renderPlot(gg_plot() + aes(bill_length_mm))
    output$bill_depth <- renderPlot(gg_plot() + aes(bill_depth_mm))
    output$body_mass <- renderPlot(gg_plot() + aes(body_mass_g))
}

shinyApp(ui, server)
