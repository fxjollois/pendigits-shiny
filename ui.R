library(shiny)
library(shinythemes)

shinyUI(navbarPage(
    "pendigits",
    theme = shinytheme("yeti"),
    tabPanel(
        "Données",
        tabsetPanel(
            tabPanel(
                "Tracés d'origine",
                plotOutput("trace")
            ),
            tabPanel(
                "Premier tracé",
                plotOutput("premier")
            ),
            tabPanel(
                "Tracé moyen",
                plotOutput("moyen")
            )
        )
    ),
    tabPanel(
        "Visualisation",
        tabsetPanel(
            tabPanel(
                "ACP",
                plotOutput("acp")
            ),
            tabPanel(
                "ACP découpée",
                plotOutput("acp.decoup")
            )
        )
    ),
    tabPanel(
        "Classification",
        sidebarLayout(
            sidebarPanel(
                width = 2,
                selectInput("chiffre", "Chiffre", choices = 0:9),
                radioButtons("critere", "Critère", choices = c("BIC", "ICL"))
            ),
            mainPanel(
                width = 10,
                tabsetPanel(
                    tabPanel(
                        "Critère",
                        plotOutput("evolution")
                    ),
                    tabPanel(
                        "Résultat",
                        plotOutput("classif")
                    )
                )
            )
        )
    )
))