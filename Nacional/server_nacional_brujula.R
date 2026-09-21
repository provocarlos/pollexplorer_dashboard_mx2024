#Brújula----
##Línea de tiempo----
###Seguridad----
output$graf_seguridad_lt <-renderHighchart({
  df <- df_seguridad |> 
    filter(segmento == "bandera",
           ambito == input$selAmbito)
  titulo <- str_c("En una escala de seguro o inseguro, ¿qué tan seguro se siente ", input$selAmbito, "?")
  
  graf_seguridad_lt(df, titulo)
})

###Temas progres----
output$graf_temasprogres_lt <-renderHighchart({
  df <- df_temasprogres |> 
    filter(segmento == "bandera",
           tema == input$selTemaBrujula)
  titulo <- str_c("¿Estás a favor o en contra de ", input$selTemaBrujula, "?")
  
  graf_favorcontra_lt(df, titulo)
})

###Apoyaría a un candidato que ----
output$graf_apoya_lt <-renderHighchart({
  df <- df_apoyariacandy |> 
    filter(segmento == "bandera",
           tema == input$selApoyaBrujula)
  titulo <- str_c("¿Apoyaría a un candidato que fomente ", input$selApoyaBrujula, "?")
  
  graf_apoyocandy_lt(df, titulo)
})

###Camino país----
output$graf_camino_pais_lt <-renderHighchart({
  df <- df_caminopais |> 
    filter(segmento == "bandera") |> 
    mutate(opinion = brujula_caminopais)
  titulo <- "¿Cómo califica la ruta que lleva el país?"
  
  graf_camino_lt(df, titulo)
})

###Economía----
output$graf_economia_lt <-renderHighchart({
  df <- df_economia |> 
    filter(segmento == "bandera") |> 
    mutate(opinion = brujula_economia)
  titulo <- "En los últimos cinco años, ¿considera que su economía ha mejorado o empeorado?"
  
  graf_economia_lt(df, titulo)
})

# #Puntual----
# #revisarDatosBrujula----
# observe({
#   medicionesBrujula <- levantamientoPorPregunta[input$tabBrujula] %||% character(0)
#   updateSelectizeInput(session, "selLevantamiento_brujula", choices = medicionesBrujula)
# 
# })
# ###Seguridad----
output$graf_seguridad <-renderHighchart({
  df <- df_seguridad |> 
    filter(segmento == input$selSegmento_brujula,
           medicion == input$selLevantamiento_brujula,
           ambito == input$selAmbito)
  titulo <- str_c("En una escala de seguro o inseguro, ¿qué tan seguro se siente en ", input$selAmbito, "?")
  subtitulo = input$selLevantamiento_brujula
  
  graf_seguridad(df, titulo, subtitulo)
})

###Temas progres----
output$graf_temasprogres <-renderHighchart({
  df <- df_temasprogres |> 
    filter(segmento == input$selSegmento_brujula,
           medicion == input$selLevantamiento_brujula,
           tema == input$selTemaBrujula)
  titulo <- str_c("¿Estás a favor o en contra de ", input$selTemaBrujula, "?")
  subtitulo = input$selLevantamiento_brujula
  
  graf_favorcontra(df, titulo, subtitulo)
})

###Apoyaría a un candidato que ----
output$graf_apoya <-renderHighchart({
  df <- df_apoyariacandy |> 
    filter(segmento == input$selSegmento_brujula,
           medicion == input$selLevantamiento_brujula,
           tema == input$selApoyaBrujula)
  titulo <- str_c("¿Apoyaría a un candidato que fomente ", input$selApoyaBrujula, "?")
  subtitulo = input$selLevantamiento_brujula
  
  graf_apoya(df, titulo, subtitulo)
})

###Camino país----
output$graf_camino_pais <-renderHighchart({
  df <- df_caminopais |> 
    filter(segmento == input$selSegmento_brujula,
           medicion == input$selLevantamiento_brujula) |> 
    mutate(opinion = brujula_caminopais)
  titulo <- "¿Cómo califica la ruta que lleva el país?"
  subtitulo = input$selLevantamiento_brujula
  
  graf_pos_neg(df, titulo, subtitulo)
})

###Economía----
output$graf_economia <-renderHighchart({
  df <- df_economia |> 
    filter(segmento == input$selSegmento_brujula,
           medicion == input$selLevantamiento_brujula) |> 
    mutate(opinion = brujula_economia)
  titulo <- "En los últimos cinco años, ¿considera que su economía ha mejorado o empeorado?"
  subtitulo = input$selLevantamiento_brujula
  
  graf_economia(df, titulo, subtitulo)
})
