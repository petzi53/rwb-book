## app-071-variable-countries


library(shiny)
library(bslib)

rwb <- readRDS(gzcon(url("https://raw.githubusercontent.com/petzi53/rwb-book/master/data/chap011/rwb/rwb.rds")))

ui <- page_sidebar(
    titlePanel("World Press Freedom Indices (WPFI)"),
    sidebar = sidebar(
        selectInput(
            inputId = "variable",
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
    )
)

server <-  function(input, output) {}

shinyApp(ui, server)
