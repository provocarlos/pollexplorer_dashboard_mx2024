#Personajes----
##Línea de tiempo----
###Evaluación----
output$graf_personaje_LT <- renderHighchart({
  df <- perso_nombre |> 
    filter(
      segmento == "bandera",
      personaje == input$selActor,
      filtro == "Opinión")
  titulo <- str_c(unique(df$filtro)," sobre ", input$selActor)
  
  graf_pos_neg_lt(df, titulo)
})

###Conocimiento----
output$graf_personaje_conocimiento_LT <- renderHighchart({
  df <- perso_nombre |> 
    filter(
      segmento == "bandera",
      personaje == input$selActor,
      filtro == "Conocimiento")
  titulo <- str_c(unique(df$filtro)," de ", input$selActor)
  
  graf_tasa_lt(df, titulo)
})

###Saldo----
output$graf_personaje_saldo_LT <- renderHighchart({
  df <- perso_nombre |> 
    filter(
      segmento == "bandera",
      personaje == input$selActor,
      filtro == "Saldo")
  titulo <- str_c(unique(df$filtro)," de ", input$selActor)
  
  graf_tasa_lt(df, titulo)
})

##Puntual----
##revisarDatosPersonaje----
# observeEvent(input$selActor, {
#   #levantamientos <- perso_nombre[perso_nombre$personaje == input$selActor, "medicion"]
#   levantamientos <- rev((perso_nombre |> filter(personaje == input$selActor))$medicion)
#   updateSelectizeInput(session, "selLevantamiento", choices = levantamientos)
# })

###Evaluación----
output$graf_personaje <-renderHighchart({
  df <- perso_nombre |> 
    filter(filtro == "Opinión",
           medicion == input$selLevantamiento,
           segmento == input$selSegmentoPersonajes,
           personaje == input$selActor)
  df$var <- factor(df$var, levels = niveles)
  titulo <- str_c(unique(df$filtro)," sobre ", input$selActor)
  subtitulo = input$selLevantamiento
  
  graf_pos_neg(df, titulo, subtitulo)
})

###Conocimiento----
output$graf_personaje_conocimiento <- renderHighchart({
  df <- perso_nombre |>
    filter(filtro == "Conocimiento",
           medicion == input$selLevantamiento,
           segmento == input$selSegmentoPersonajes,
           personaje == input$selActor)
  titulo <- str_c(unique(df$filtro)," sobre ", input$selActor)
  subtitulo = input$selLevantamiento
  
  graf_tasa(df, titulo, subtitulo)
})

###Saldo----
output$graf_personaje_saldo <- renderHighchart({
  df <- perso_nombre |> 
    filter(filtro == "Saldo",
           medicion == input$selLevantamiento,
           segmento == input$selSegmentoPersonajes,
           personaje == input$selActor) 
  titulo <- str_c(unique(df$filtro)," de ", input$selActor)
  subtitulo = input$selLevantamiento
  
  graf_tasa(df, titulo, subtitulo)
})