library(shiny)

# Define UI for application
fluidPage(
  # Application title
  titlePanel("Twitchalyst"),

  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      titlePanel("Video Games Overview"),


      conditionalPanel(
        condition = "input.tabs != 'Graph per game'",
        sliderInput("percentage", "Percentages of results to show:",
                    min = 1, max = 100, value = 15),
        selectInput("week", "Pick a week:",
                    choices = c(17, 18, 19, 20, 21, 22, 26, 28)),
        selectInput("metric", "Pick a metric:",
                    choices = c("Average Viewers", "Average Channels", "Hours Watched"), selected = "Average Viewers")
        ),
      # Conditional panel for displaying gameSelect only on the "Graphique par jeu" tab
      conditionalPanel(
        condition = "input.tabs == 'Graph per game'",
        uiOutput("gameSelect") # Nouvel élément UI pour le choix du jeu
      )
    ),
    # Show plots
    mainPanel(
      tabsetPanel(
        id = "tabs", # Add an id for the tabsetPanel
        tabPanel("Main graphs",
                 plotOutput("graph"),
                 plotOutput("treemap")),
        tabPanel("Graph per game",
                 plotOutput("gamePlot")),
        tabPanel("Full data",
                 tableOutput("dataTable"))
      )
    )
  )
)
