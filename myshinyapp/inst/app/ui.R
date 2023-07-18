ui <- fluidPage(
  selectInput("paragraphe", "Paragraphe à générer", choices  = c("Chômage", "Emploi", "Demandeurs d'emploi", "Créations d'entreprises")),
  
  # Titre
  titlePanel("Perroquet"),
  
  
  # Légende
  sidebarLayout(
    sidebarPanel(
      textInput("annee", label="Année"),
      sliderInput("trimestre", "Trimestre :", min = 1, max = 4,  value = 1),
      sliderInput("horizon", "Horizon :", min = 1, max = 50,  value = 1),
      actionButton("submit_info", "Valider")),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Commentaires & Graphique", textOutput("commentaires"), plotlyOutput("graphique1"), plotlyOutput("graphique2")),
        tabPanel("Tableau", tableOutput("tableau"))
      )
    ),
  ),
)
