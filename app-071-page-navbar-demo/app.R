## path: app-071-page-navbar-demo/app.R
## my book: @sec-071-page-navbar-demo

## suppressWarnings(suppressPackageStartupMessages({
    library(shiny)
    library(shinythemes)
    library(bslib)
## })) # these suppresion does not work with  shinylive-r

sidebar_map <- layout_sidebar(
    sidebar = "Controls for map"
)

sidebar_chart <- layout_sidebar(
    sidebar = "Control for chart"
)

sidebar_country <- layout_sidebar(
    sidebar = "Control for country"
)

cards <- list(
    card(
        full_screen = TRUE,
        card_header("This is a map"),
    ),
    card(
        full_screen = TRUE,
        card_header("This is a line chart")
    ),
    card(
        full_screen = TRUE,
        card_header("This is country information")
    )
)

ui <- page_navbar(
    ## choose a bootstrap version and theme
    ## only themes from {shinythemes} are allowed
        ## cerulean, cosmo, cyborg, darkly, flatly, journal,
        ## lumen, paper, readable, sandstone, simplex, slate,
        ## spacelab, superhero, united, and yeti.
    theme = bs_theme(5, bootswatch = "cosmo"),
    ## next line does not work with shinylive-r
    ## navbar_options = navbar_options(class = "bg-primary", theme = "dark"),
    title = "Reporters Without Borders",
    id = "nav",
    sidebar = sidebar(
        conditionalPanel(
            "input.nav === 'Map'",
            "Map controls"
        ),
        conditionalPanel(
            "input.nav === 'Chart'",
            "Chart controls"
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
    nav_item(tags$a("About", href = "https://peter.baumgartner.name", target="_blank"))
)

server <-  function(input, output) {}

shinyApp(ui, server)
