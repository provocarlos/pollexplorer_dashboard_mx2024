#Gobierno Federal----
##Línea de tiempo----
###Aprobación AMLO----
output$graf_aprobacion_AMLO_lt <-renderHighchart({
  df <- df_evalAMLO |> 
    filter(segmento == "bandera") |> 
    mutate(opinion = evalgob_amlo)
  titulo <- "Evaluación del Gobierno de AMLO"
  
  graf_pos_neg_lt(df, titulo)
})

###Temas AMLO----
output$graf_temas_AMLO_lt <-renderHighchart({
  df <- df_temasAMLO |> 
    filter(segmento == "bandera",
           tema == input$selTemaAMLO)
  titulo <- str_c("De esta lista puede indicarme, ¿usted aprueba o desaprueba la labor del presidente López Obrador para atender el problema de ",
                  input$selTemaAMLO, "?")
  
  graf_pos_neg_lt(df, titulo)
})

###Beneficiario de programas sociales----
output$graf_beneficiarios_lt <- renderHighchart({
  df <- df_benef |> 
    filter(segmento == "bandera")
  titulo <- "Usted o algún integrante de su hogar recibe algún apoyo del Gobierno Federal?"
  
  graf_sino_lt(df, titulo)
})

##Puntual----
##revisarDatosEval----
observe({
  medicionesEval <- levantamientoPorPregunta[input$tabEval] %||% character(0)
  updateSelectizeInput(session, "selLevantamiento_eval",
                       choices = medicionesEval)
})

###Aprobación AMLO----
output$graf_aprobacion_AMLO <-renderHighchart({
  df <- df_evalAMLO |> 
    filter(segmento == input$selSegmento_eval,
           medicion == input$selLevantamiento_eval) |> 
    mutate(opinion = evalgob_amlo)
  titulo <- "Evaluación del Gobierno de AMLO"
  subtitulo = input$selLevantamiento_eval
  
  graf_pos_neg(df, titulo, subtitulo)
})

###Temas AMLO----
output$graf_temas_AMLO <-renderHighchart({
  df <- df_temasAMLO |> 
    filter(segmento == input$selSegmento_eval,
           medicion == input$selLevantamiento_eval,
           tema == input$selTemaAMLO)
  titulo <- str_c("De esta lista puede indicarme, ¿usted aprueba o desaprueba la labor del presidente López Obrador para atender el problema de ",
                  input$selTemaAMLO, "?")
  subtitulo = input$selLevantamiento_eval
  
  graf_pos_neg(df, titulo, subtitulo)
})

###Beneficiario de programas sociales----
output$graf_beneficiarios <- renderHighchart({
  df <- df_benef |> 
    filter(segmento == input$selSegmento_eval,
           medicion == input$selLevantamiento_eval)
  titulo <- "Usted o algún integrante de su hogar recibe algún apoyo del Gobierno Federal?"
  subtitulo = input$selLevantamiento_eval
  
  graf_sino(df, titulo, subtitulo)
})
