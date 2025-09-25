## app-071-conditional-sidebar-pills

# rwb <- readRDS(gzcon(url("https://raw.githubusercontent.com/petzi53/rwb-book/master/data/chap011/rwb/rwb.rds")))

library(shiny)
library(bslib)

ui <- page_fluid(
    navset_card_pill(
        title = "World Press Freedom Indices (WPFI)",
        id = "nav",
        sidebar = sidebar(
            conditionalPanel(
                "input.nav == 'Map'",
                "Map controls"
            ),
            conditionalPanel(
                "input.nav == 'Score/Rank'",
                "Global values"
            ),
            conditionalPanel(
                "input.nav == 'Components'",
                "Components"
            ),
            conditionalPanel(
                "input.nav == 'Country'",
                "Country"
            )
        ),
        nav_panel("Map", "Display the world map"),
        nav_panel("Score/Rank", "Display score or rank distribution"),
        nav_panel("Components", "Display score/rank components"),
        nav_panel("Country", "Display country information")
    )
)

server <- function(input, output) {
    # Reactive logic can access the selected tab via input$tab
}

shinyApp(ui = ui, server = server)


