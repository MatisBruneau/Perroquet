server <- function(input, output){
  observeEvent(input$submit_info, {
    if (input$paragraphe == "Chômage"){
      observeEvent(input$submit_info, {
        source("myshinyapp/R/generateurs.R")
        output$graphique1 <- renderPlotly(
          generateur_figure4(as.numeric(input$annee), as.numeric(input$trimestre), as.numeric(input$horizon)))
        output$graphique2 <- renderPlotly(NULL)
        output$commentaires <- renderText({
          generateur_chômage(input$annee, input$trimestre)})
        output$tableau <- renderTable({
          generateur_table_chomage(as.numeric(input$annee), as.numeric(input$trimestre), max(as.numeric(input$horizon),2))}, caption = "Taux de chômage au sens du BIT - Ensemble")
      })
    }else if (input$paragraphe == "Emploi"){
      observeEvent(input$submit_info, {
        source("myshinyapp/R/generateurs.R")
        output$graphique1 <- renderPlotly({
          ggplotly(generateur_figure2(as.numeric(input$annee), as.numeric(input$trimestre), as.numeric(input$horizon)))
          })
        output$graphique2 <- renderPlotly({
          ggplotly(generateur_figure3(as.numeric(input$annee), as.numeric(input$trimestre), as.numeric(input$horizon)))
        })
        output$commentaires <- renderText({
          generateur_emplois(input$annee, input$trimestre)})
        output$tableau <- renderTable({
          generateur_table_emploi(as.numeric(input$annee), as.numeric(input$trimestre), max(as.numeric(input$horizon),2))}, caption="Nombre d'emplois salariés en fin de trimestre")
      })
    }else if (input$paragraphe == "Demandeurs d'emploi") {
      observeEvent(input$submit_info, {
        source("myshinyapp/R/generateurs.R")
        output$commentaires <- renderText({
          generateur_demandeurs_d_emploi(input$annee, input$trimestre)})
        output$tableau <- renderTable({
          generateur_table_demandeurs_d_emploi(as.numeric(input$annee), as.numeric(input$trimestre), max(as.numeric(input$horizon),2))}, caption="Nombre de demandeurs d'emploi", digits = 0)
        output$graphique1 <- renderPlotly(NULL)
        output$graphique2 <- renderPlotly(NULL)
    })
    } else if (input$paragraphe == "Créations d'entreprises") {
      observeEvent(input$submit_info, {
        source("myshinyapp/R/generateurs.R")
        output$commentaires <- renderText({
          generateur_creation_entreprise(input$annee, input$trimestre)})
        output$tableau <- renderTable({
          generateur_table_creation_entreprise(as.numeric(input$annee), as.numeric(input$trimestre), max(as.numeric(input$horizon),2))}, caption="Nombre de créations d'entreprises", digits = 0)
        output$graphique1 <- renderPlotly(generateur_figure5(as.numeric(input$annee), as.numeric(input$trimestre), as.numeric(input$horizon)))
        output$graphique2 <- renderPlotly(NULL)
    })
}
})
}
