library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(treemapify)

load_data <- function() {
  games_combined <- read_csv("games_combined.csv")
  top20_17 <- read_csv("get_top100_17.csv")
  top20_18 <- read_csv("get_top100_18.csv")
  top20_19 <- read_csv("get_top100_19.csv")
  top20_20 <- read_csv("get_top100_20.csv")
  top20_21 <- read_csv("get_top100_21.csv")
  top20_22 <- read_csv("get_top100_22.csv")
  top20_26 <- read_csv("get_top100_26.csv")
  top20_28 <- read_csv("get_top100_28.csv")


  # Ajouter une colonne semaine pour chaque dataframe
  top20_17$Week <- 17
  top20_18$Week <- 18
  top20_19$Week <- 19
  top20_20$Week <- 20
  top20_21$Week <- 21
  top20_22$Week <- 22
  top20_26$Week <- 26
  top20_28$Week <- 28


  # Combiner toutes les données en un seul dataframe
  all_data <- bind_rows(top20_17, top20_18, top20_19, top20_20, top20_21, top20_22, top20_26, top20_28)

  # Supprimer les doublons et créer une liste des labels uniques
  unique_labels <- unique(all_data$Name)

  return(unique_labels)
}

# Define server logic
shinyServer(function(input, output) {
  games_combined <- read_csv("games_combined.csv")
  top20_17 <- read_csv("get_top100_17.csv")
  top20_18 <- read_csv("get_top100_18.csv")
  top20_19 <- read_csv("get_top100_19.csv")
  top20_20 <- read_csv("get_top100_20.csv")
  top20_21 <- read_csv("get_top100_21.csv")
  top20_22 <- read_csv("get_top100_22.csv")
  top20_26 <- read_csv("get_top100_26.csv")
  top20_28 <- read_csv("get_top100_28.csv")


  # Ajouter une colonne semaine pour chaque dataframe
  top20_17$Week <- 17
  top20_18$Week <- 18
  top20_19$Week <- 19
  top20_20$Week <- 20
  top20_21$Week <- 21
  top20_22$Week <- 22
  top20_26$Week <- 26
  top20_28$Week <- 28


  # Combiner toutes les données
  all_data <- bind_rows(top20_17, top20_18, top20_19, top20_20, top20_21, top20_22, top20_26, top20_28)

  unique_labels <- load_data()

  output$gameSelect <- renderUI({
    unique_labels <- load_data()
    selectInput("game", "Pick game(s):", choices = unique_labels, multiple = TRUE)
  })

  # Fonction pour obtenir une couleur aléatoire pour chaque label unique
  get_random_color <- function() {
    return(sprintf("#%06X", sample(0:(2^24 - 1), 1)))
  }

  # Associez chaque label unique à une couleur aléatoire
  label_colors <- setNames(replicate(length(unique_labels), get_random_color()), unique_labels)

  # Fonction pour obtenir une couleur basée sur le label
  get_color <- function(label) {
    return(label_colors[[label]])
  }

  # Créer le treemap
  output$treemap <- renderPlot({
    selected_week <- input$week
    week_data <- all_data %>% filter(Week == selected_week)
    num_rows <- nrow(week_data)
    top_data <- week_data %>%
      arrange(desc(get(input$metric))) %>%
      slice(1:round(num_rows * input$percentage / 100)) %>%
      distinct(Name, .keep_all = TRUE)

    ggplot(top_data, aes(area = !!sym(input$metric), fill = Name, label = Name)) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
      scale_fill_manual(values = sapply(top_data$Name, get_color)) +
      labs(title = paste("Treemap of games per", input$metric, "- Week", selected_week))
  })

  # Fonction pour créer les graphiques
  output$graph <- renderPlot({
    selected_week <- input$week
    week_data <- all_data %>% filter(Week == selected_week)
    num_rows <- nrow(week_data)
    top_data <- week_data %>%
      arrange(desc(get(input$metric))) %>%
      slice(1:round(num_rows * input$percentage / 100))

    top_data <- distinct(top_data, Name, .keep_all = TRUE)

    top_data$Name <- factor(top_data$Name, levels = top_data$Name[order(top_data[[input$metric]], decreasing = TRUE)])

    ggplot(top_data, aes(x = reorder(Name, !!sym(input$metric), FUN = median), y = !!sym(input$metric), fill = Name)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = sapply(top_data$Name, get_color)) +
      labs(x = "Jeu", y = "Mean viewers") +
      ggtitle(paste("Top games per", input$metric, "- Week", selected_week)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # Créer le graphique pour les jeux sélectionnés
  output$gamePlot <- renderPlot({
    selected_games <- input$game

    game_data <- all_data %>% filter(Name %in% selected_games)

    # Trouver la valeur maximale de l'axe y dans les données
    max_y_value <- max(game_data[[input$metric]])

    ggplot(game_data, aes(x = Week, y = !!sym(input$metric), color = Name)) +
      geom_line() +
      labs(x = "Week ", y = input$metric) +
      ggtitle("Weekly comparison") +
      scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, max_y_value + max_y_value/10))
  })

  # Afficher les données en table
  output$dataTable <- renderTable({
    selected_week <- input$week
    week_data <- all_data %>% filter(Week == selected_week)
    num_rows <- nrow(week_data)
    top_data <- week_data %>%
      arrange(desc(get(input$metric))) %>%
      slice(1:round(num_rows * input$percentage / 100)) %>%
      distinct(Name, .keep_all = TRUE)

    top_data
  })
})


