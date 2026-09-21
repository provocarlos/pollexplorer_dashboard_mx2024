#Elección presidencial----
##Línea de tiempo----
###Careos----
output$graf_careos_lt <- renderHighchart({
  df <- df_careosPresi |> 
    select(-c(opinion)) |> 
    filter (segmento == "bandera") |> 
    rename(opinion = elec_careo)
  titulo <- "Voto por candidatos para la presidencia de México"
  
  graf_careos_lt(df, titulo)
})

### Segunda opción----
output$graf_careos_segunda_lt <- renderHighchart({
  df <- df_elecsegop |>
    filter (segmento == "bandera") |>
    mutate(opinion = elec_segop)
  titulo <- "Segunda opción para la presidencia de México"
  
  graf_careos_lt(df, titulo)
})

###Voto por alianza----
output$graf_voto_alianza_lt <- renderHighchart({
  df <- df_presiAlian |> 
    filter (segmento == "bandera") |> 
    mutate(opinion = cultupol_votoalian)
  titulo <- "Voto por alianza para la presidencia de México"
  
  graf_alianza_lt(df, titulo)
})

###Seguridad de voto ----
output$graf_seguridad_voto_lt <- renderHighchart({
  df <- df_seguvoto |>
    filter (segmento == "bandera") |>
    mutate(opinion = escala_seguridad_voto)
  titulo <- "Seguridad de voto"
  
  graf_seguvoto_lt(df, titulo)
})

###Seguridad de voto area----
output$graf_seguridad_voto_alt_lt <- renderHighchart({
  
  df <- df_seguvoto |>
    filter (segmento == "bandera") |>
    mutate (opinion = escala_seguridad_voto,
            opinion = case_match(as.character(opinion),
                                 c("10","9","8") ~ "Muy seguro",
                                 c("7", "6", "5") ~ "Algo seguro",
                                 c("4", "3", "2", "1") ~ "Poco seguro",
                                 .default = opinion)
    ) |> 
    group_by(medicion, opinion) |> 
    summarize(por = sum(por), .groups = 'drop')
  
  titulo <- "Seguridad de voto"
  
  graf_seguvoto_alt_lt(df, titulo)
})

### Expectativa de triunfo ----
output$graf_careos_expectativa_lt <- renderHighchart({
  df <- df_epext |> 
    filter (segmento == "bandera") |> 
    mutate(opinion = elec_expectativa)
  titulo <- "Expectativa de triunfo para la presidencia de México"
  
  graf_careos_lt(df, titulo)
})

### Elección definida ----
output$graf_definida_lt <- renderHighchart({
  df <- df_definida |> 
    filter (segmento == "bandera")
  titulo <- "¿Considera que la elección ya está definida?"
  
  graf_sino_lt(df, titulo)
})

### Cualidades de los candidatos ----
output$graf_cualidades_lt <- renderHighchart({
  df <- df_cualicandy |> 
    filter (segmento == "bandera",
            tema == input$selTemaCualidades)
  titulo <- "Cualidades de los candidatos para la presidencia de México"
  
  graf_careos_cand_lt(df, titulo)
})

# ###Voto por marca
# output$graf_voto_marca_lt <- renderHighchart({
#   df <- df_presiMarca |> 
#     filter (segmento == "bandera") |> 
#     mutate(opinion = cultupol_votomarca)
#   titulo <- "Voto por partido para la presidencia de México"
#   
#   graf_partidos_lt(df, titulo)
# })

# ###Segunda opción
# output$graf_segunda_marca_lt <- renderHighchart({
#   df <- df_presiSeguOpc |> 
#     filter (segmento == "bandera") |> 
#     mutate(opinion = cultupol_segundaalian)
#   titulo <- "Segunda opción para la presidencia de México"
#   
#   graf_alianza_lt(df, titulo)
# })

##Puntual----
##revisarDatosCareos----
# observe({
#   medicionesCareos <- levantamientoPorPregunta[input$tabCareos] %||% character(0)
#   updateSelectizeInput(session, "selLevantamiento_careo", choices = medicionesCareos)
# })

###Careos----
output$graf_careos <- renderHighchart({
  df <- df_careosPresi |> 
    select(-c(opinion)) |> 
    filter (segmento == input$selSegmento_careo,
            medicion == input$selLevantamiento_careo) |> 
    mutate(opinion = elec_careo)
  titulo <- "Voto por candidatos para la presidencia de México"
  subtitulo = input$selLevantamiento_careo
  
  graf_careos(df, titulo, subtitulo)
})

### Segunda opción----
output$graf_careos_segunda <- renderHighchart({
  df <- df_elecsegop |>
    filter (segmento == input$selSegmento_careo,
            medicion == input$selLevantamiento_careo) |>
    mutate(opinion = elec_segop)
  titulo <- "Segunda opción para la presidencia de México"
  subtitulo = input$selLevantamiento_careo
  
  graf_careos(df, titulo, subtitulo)
})

###Seguridad de voto----
output$graf_seguridad_voto <- renderHighchart({
  df <- df_seguvoto |>
    filter (segmento == input$selSegmento_careo,
            medicion == input$selLevantamiento_careo) |>
    mutate(opinion = escala_seguridad_voto)
  titulo <- "Seguridad de voto"
  subtitulo = input$selLevantamiento_careo
  
  graf_seguvoto(df, titulo, subtitulo)
})

###Seguridad de voto alt----
output$graf_seguridad_voto_alt <- renderHighchart({
  df <- df_seguvoto |>
    filter (segmento == input$selSegmento_careo,
            medicion == input$selLevantamiento_careo) |>
    mutate(opinion = escala_seguridad_voto,
           opinion = case_match(as.character(opinion),
                                c("10","9","8") ~ "Muy seguro",
                                c("7", "6", "5") ~ "Algo seguro",
                                c("4", "3", "2", "1") ~ "Poco seguro",
                                "Ns/Nc" ~ "Ns/Nc")
    ) |> 
    group_by(medicion, var, opinion) |> 
    summarize(por = sum(por), .groups = 'drop')
  titulo <- "Seguridad de voto"
  subtitulo = input$selLevantamiento_careo
  
  graf_seguvoto_alt(df, titulo, subtitulo)
})

###Voto por alianza----
output$graf_voto_alianza <- renderHighchart({
  df <- df_presiAlian |> 
    filter (segmento == input$selSegmento_careo,
            medicion == input$selLevantamiento_careo) |> 
    mutate(opinion = cultupol_votoalian)
  titulo <- "Voto por alianza para la presidencia de México"
  subtitulo = input$selLevantamiento_careo
  
  graf_alianza(df, titulo, subtitulo)
})

### Expectativa de triunfo ----
output$graf_careos_expectativa <- renderHighchart({
  df <- df_epext |>
    filter (segmento == input$selSegmento_careo,
            medicion == input$selLevantamiento_careo) |>
    mutate(opinion = elec_expectativa)
  titulo <- "Expectativa de triunfo para la presidencia de México"
  subtitulo = input$selLevantamiento_careo
  
  graf_careos(df, titulo, subtitulo)
})

### Elección definida ----
output$graf_definida <- renderHighchart({
  df <- df_definida |>
    filter (segmento == input$selSegmento_careo,
            medicion == input$selLevantamiento_careo)
  titulo <- "¿Considera que la elección ya está definida?"
  subtitulo = input$selLevantamiento_careo
  
  graf_sino(df, titulo, subtitulo)
})

### Cualidades de los candidatos ----
output$graf_cualidades <- renderHighchart({
  df <- df_cualicandy |>
    filter (segmento == input$selSegmento_careo,
            medicion == input$selLevantamiento_careo,
            tema = input$selTemaCualidades)
  titulo <- output$graf_careos_expectativa <- renderHighchart({
    df <- df_epext |>
      filter (segmento == input$selSegmento_careo,
              medicion == input$selLevantamiento_careo) |>
      mutate(opinion = elec_expectativa)
    titulo <- "Expectativa de triunfo para la presidencia de México"
    subtitulo = input$selLevantamiento_careo
    
    graf_careos(df, titulo, subtitulo)
  })
  subtitulo = input$selLevantamiento_careo
  
  graf_careos_cand(df, titulo, subtitulo)
})
