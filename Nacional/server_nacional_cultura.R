#Cultura Política----
##Línea de tiempo----
###Simpatía partidista----
output$graf_simpatia_lt <- renderHighchart({
  df <- df_simpatia |> 
    filter (segmento == "bandera")|> 
    rename(opinion = cultupol_simpatia)
  titulo <- "Actualmente, ¿con cuál partido político simpatiza más?"
  
  graf_partidos_lt(df, titulo)
})

###Rechazo partidista----
output$graf_rechazo_lt <- renderHighchart({
  df <- df_rechazo |> 
    filter(segmento == "bandera")|> 
    rename(opinion = cultupol_rechazomarca)
  titulo <- "¿Por cuál partido nunca votaría?"
  
  graf_partidos_lt(df, titulo)
})

###Voto MC----
output$graf_voto_MC_cult_lt <- renderHighchart({
  df <- df_techomc |> 
    filter(segmento == "bandera")
  titulo <- "Votaría por Movimiento Ciudadano para la presidencia en 2024"
  
  graf_sino_lt(df, titulo)
})

###Voto Morena 2024----
output$graf_voto_Morena_lt <- renderHighchart({
  df <- df_votoMorena |> 
    filter(segmento == "bandera")
  titulo <- "Votaría por MORENA para la presidencia en 2024"
  
  graf_sino_lt(df, titulo)
})

###Voto PAN 2024----
output$graf_voto_PAN_lt <- renderHighchart({
  df <- df_votoPAN |> 
    filter(segmento == "bandera")
  titulo <- "Votaría por PAN para la presidencia en 2024"
  
  graf_sino_lt(df, titulo)
})   

###Voto PRI 2024----
output$graf_voto_PRI_lt <- renderHighchart({
  df <- df_votoPRI |> 
    filter(segmento == "bandera")
  titulo <- "Votaría por PRI para la presidencia en 2024"
  
  graf_sino_lt(df, titulo)
})

###Evaluación MC ----
output$graf_eval_MC_cult_lt <- renderHighchart({
  df <- df_partidos |> 
    filter(segmento == "bandera",
           partido == "MC") |> 
    rename(opinion = evaluacion)
  titulo <- "Evaluación a Movimiento Ciudadano"
  
  graf_pos_neg_lt(df, titulo)
})

###Evaluación Morena ----
output$graf_eval_Morena_lt <- renderHighchart({
  df <- df_partidos |> 
    filter(segmento == "bandera",
           partido == "Morena") |> 
    rename(opinion = evaluacion)
  titulo <- "Evaluación a Morena"
  
  graf_pos_neg_lt(df, titulo)
})

###Evaluación PAN ----
output$graf_eval_PAN_lt <- renderHighchart({
  df <- df_partidos |> 
    filter(segmento == "bandera",
           partido == "PAN") |> 
    rename(opinion = evaluacion)
  titulo <- "Evaluación a PAN"
  
  graf_pos_neg_lt(df, titulo)
})   

###Evaluación PRI ----
output$graf_eval_PRI_lt <- renderHighchart({
  df <- df_partidos |> 
    filter(segmento == "bandera",
           partido == "PRI") |> 
    rename(opinion = evaluacion)
  titulo <- "Evaluación a PRI"
  
  graf_pos_neg_lt(df, titulo)
})

###Internos----
output$graf_internos_lt <- renderHighchart({
  df <- df_interno |> 
    filter (segmento == "bandera",
            interno == input$selInterno)
  titulo <- str_c("De estos cuatro partidos: PAN, PRI, MORENA, MOVIMIENTO CIUDADANO, ¿cuál cree que ",input$selInterno,"?")
  
  graf_partidos_lt(df, titulo)
})

###Alianza Morena----
output$graf_Alianzas_Morena_lt <-renderHighchart({
  df <- df_evalAlian |> 
    filter(segmento == "bandera",
           alianza == "MORENA-PVEM-PT")
  
  titulo <- str_c("Opinión sobre alianza ", unique(df$alianza))
  
  graf_pos_neg_lt(df, titulo)
})

###Alianza PRIAN----
output$graf_Alianzas_PRIAN_lt <-renderHighchart({
  df <- df_evalAlian |> 
    filter(segmento == "bandera",
           alianza == "PAN-PRI-PRD")
  
  titulo <- str_c("Opinión sobre alianza ", unique(df$alianza))
  
  graf_pos_neg_lt(df, titulo)
})


###Lo mejor para México----
output$graf_lo_mejor_lt <- renderHighchart({
  df <- df_mejor |> 
    filter(segmento == "bandera") |> 
    rename(opinion = cultupol_mejormexico)
  titulo <- "¿Qué sería lo mejor para México en 2024?"
  
  graf_cuatro_partidos_lt(df, titulo)
})

###Lo peor para México----
output$graf_lo_peor_lt <- renderHighchart({
  df <- df_peor |> 
    filter(segmento == "bandera") |> 
    rename(opinion = cultupol_peormexico)
  titulo <- "¿Qué sería lo peor para México en 2024?"
  
  graf_cuatro_partidos_lt(df, titulo)
})

##Puntual----
##revisarDatosCultura----
observe({
  medicionesCultura <-  levantamientoPorPregunta[input$tabCultura] %||% character(0)
  updateSelectizeInput(session, "selLevantamiento_cultura", choices = medicionesCultura)
})

###Simpatía partidista----
output$graf_simpatia <- renderHighchart({
  df <- df_simpatia |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura)|> 
    rename(opinion = cultupol_simpatia)
  titulo <- "Actualmente, ¿con cuál partido político simpatiza más?"
  subtitulo = input$selLevantamiento_cultura
  
  graf_partidos(df, titulo, subtitulo)
})
###Rechazo partidista----
output$graf_rechazo <- renderHighchart({
  df <- df_rechazo |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura)|> 
    rename(opinion = cultupol_rechazomarca)
  titulo <- "¿Por cuál partido nunca votaría?"
  subtitulo = input$selLevantamiento_cultura
  
  graf_partidos(df, titulo, subtitulo)
})

###Voto MC 2024----
output$graf_voto_MC_cult <- renderHighchart({
  df <- df_techomc |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura)
  titulo <- "Votaría por Movimiento Ciudadano para la presidencia en 2024"
  subtitulo = input$selLevantamiento_cultura
  
  graf_sino(df, titulo, subtitulo)
})

###Voto Morena 2024----
output$graf_voto_Morena <- renderHighchart({
  df <- df_votoMorena |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura)
  titulo <- "Votaría por MORENA para la presidencia en 2024"
  subtitulo = input$selLevantamiento_cultura
  
  graf_sino(df, titulo, subtitulo)
})

###Voto PAN 2024----
output$graf_voto_PAN <- renderHighchart({
  df <- df_votoPAN |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura)
  titulo <- "Votaría por PAN para la presidencia en 2024"
  subtitulo = input$selLevantamiento_cultura
  
  graf_sino(df, titulo, subtitulo)
})   

###Voto PRI 2024----
output$graf_voto_PRI <- renderHighchart({
  df <- df_votoPRI |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura)
  titulo <- "Votaría por PRI para la presidencia en 2024"
  subtitulo = input$selLevantamiento_cultura
  
  graf_sino(df, titulo, subtitulo)
})

###Evaluación MC ----
output$graf_eval_MC_cult <- renderHighchart({
  df <- df_partidos |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura,
            partido == "MC")|> 
    rename(opinion = evaluacion)
  titulo <- "Evaluación a Movimiento Ciudadano"
  subtitulo = input$selLevantamiento_cultura
  
  graf_pos_neg(df, titulo, subtitulo)
})

###Evaluación Morena ----
output$graf_eval_Morena <- renderHighchart({
  df <- df_partidos |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura,
            partido == "Morena")|> 
    rename(opinion = evaluacion)
  titulo <- "Evaluación a Morena"
  subtitulo = input$selLevantamiento_cultura
  
  graf_pos_neg(df, titulo, subtitulo)
})

###Evaluación PAN ----
output$graf_eval_PAN <- renderHighchart({
  df <- df_partidos |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura,
            partido == "PAN")|> 
    rename(opinion = evaluacion)
  titulo <- "Evaluación a PAN"
  subtitulo = input$selLevantamiento_cultura
  
  graf_pos_neg(df, titulo, subtitulo)
})   

###Evaluación PRI ----
output$graf_eval_PRI <- renderHighchart({
  df <- df_partidos |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura,
            partido == "PRI")|> 
    rename(opinion = evaluacion)
  titulo <- "Evaluación a PRI"
  subtitulo = input$selLevantamiento_cultura
  
  graf_pos_neg(df, titulo, subtitulo)
})

###Internos----
output$graf_internos <- renderHighchart({
  df <- df_interno |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura,
            interno == input$selInterno)
  titulo <- str_c("De estos cuatro partidos: PAN, PRI, MORENA, MOVIMIENTO CIUDADANO, ¿cuál cree que ",input$selInterno,"?")
  subtitulo = input$selLevantamiento_cultura
  
  graf_partidos(df, titulo, subtitulo)
})

###Alianza Morena----
output$graf_Alianzas_Morena <-renderHighchart({
  df <- df_evalAlian |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura,
            alianza == "MORENA-PVEM-PT")
  titulo <- str_c("Opinión sobre alianza ", unique(df$alianza))
  subtitulo = input$selLevantamiento_cultura
  
  graf_pos_neg(df, titulo, subtitulo)
})

###Alianza PRIAN----
output$graf_Alianzas_PRIAN <-renderHighchart({
  df <- df_evalAlian |> 
    filter (segmento == input$selSegmento_cultura,
            medicion == input$selLevantamiento_cultura,
            alianza == "PAN-PRI-PRD")
  titulo <- str_c("Opinión sobre alianza ", unique(df$alianza))
  subtitulo = input$selLevantamiento_cultura
  
  graf_pos_neg(df, titulo, subtitulo)
})

###Lo mejor para México----
output$graf_lo_mejor <- renderHighchart({
  df <- df_mejor |> 
    filter(segmento == input$selSegmento_cultura,
           medicion == input$selLevantamiento_cultura) |> 
    rename(opinion = cultupol_mejormexico)
  titulo <- "¿Qué sería lo mejor para México en 2024?"
  subtitulo = input$selLevantamiento_cultura
  
  graf_cuatro_partidos(df, titulo, subtitulo)
})
###Lo peor para México----
output$graf_lo_peor <- renderHighchart({
  df <- df_peor |> 
    filter(segmento == input$selSegmento_cultura,
           medicion == input$selLevantamiento_cultura) |> 
    rename(opinion = cultupol_peormexico)
  titulo <- "¿Qué sería lo peor para México en 2024?"
  subtitulo = input$selLevantamiento_cultura
  
  graf_cuatro_partidos(df, titulo, subtitulo)
})
