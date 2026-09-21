pacman::p_load(tidyverse, readxl)

# Leer datos
data <- read_excel("www/Microdatos/NL_sept_15_2024.xlsx")
data2 <- read_excel("www/Microdatos/NL_oct_15_2024.xlsx") |>
  mutate(Ponderador_Compuesto = as.double(Ponderador_Compuesto))

calcular_conocimiento <- function(data, fecha) {
  # Columnas que representan los personajes
  personajes_cols <- names(data) |> 
    str_subset("^T_PER_NOM_")
  
  # Función para calcular el conocimiento ponderado
  calcular_porcentaje <- function(df, segmento_nombre, var_col = NULL) {
    df |>
      select(all_of(personajes_cols), Ponderador_Compuesto, all_of(var_col)) |>
      pivot_longer(cols = all_of(personajes_cols), 
                   names_to = "personaje", 
                   values_to = "conocimiento") |>
      filter(conocimiento == "Si") |>
      group_by(personaje, !!sym(var_col)) |>
      summarise(
        por = sum(Ponderador_Compuesto, na.rm = TRUE) / sum(df$Ponderador_Compuesto, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        opinion = "Conocimiento",
        filtro = "Conocimiento",
        segmento = segmento_nombre,
        medicion = as.Date(fecha),
        personaje = str_remove(personaje, "^T_PER_NOM_"),
        var = !!sym(var_col)
      )
  }
  
  # Calcular para total
  resultados_totales <- data |>
    select(all_of(personajes_cols), Ponderador_Compuesto) |>
    pivot_longer(cols = all_of(personajes_cols), 
                 names_to = "personaje", 
                 values_to = "conocimiento") |>
    filter(conocimiento == "Si") |>
    group_by(personaje) |>
    summarise(
      por = sum(Ponderador_Compuesto, na.rm = TRUE) / sum(data$Ponderador_Compuesto, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      opinion = "Conocimiento",
      filtro = "Conocimiento",
      var = "Nuevo León",
      segmento = "Total",
      medicion = as.Date(fecha),
      personaje = str_remove(personaje, "^T_PER_NOM_")
    )
  
  # Calcular por NSE
  resultados_nse <- data |>
    group_split(nse_clasificacion) |>
    map_dfr(~ calcular_porcentaje(.x, "nse", "nse"))
  
  # Calcular por generaciones
  resultados_generaciones <- data |>
    group_split(GENERACIONES_UPDATED_MORA) |>
    map_dfr(~ calcular_porcentaje(.x, "generaciones", "GENERACIONES_UPDATED_MORA"))
  
  # Calcular por sexo
  resultados_sexo <- data |>
    group_split(sexo) |>
    map_dfr(~ calcular_porcentaje(.x, "sexo", "sexo"))
  
  # Calcular por escolaridad
  resultados_escolaridad <- data |>
    group_split(socioeconomicos_escolaridad) |>
    map_dfr(~ calcular_porcentaje(.x, "escolaridad", "socioeconomicos_escolaridad"))
  
  # Combinar todos los resultados
  bind_rows(resultados_totales, resultados_nse, resultados_generaciones, resultados_sexo, resultados_escolaridad) |>
    mutate(var = as.factor(var),
           opinion = as.factor(opinion)) |> 
    select(medicion, var, opinion, por, personaje, segmento, filtro)
}

# Aplicar la función
resultados <- calcular_conocimiento(data, "2024-09-15")
resultados2 <- calcular_conocimiento(data2, "2024-10-15")

# Verificar resultados
head(resultados)


calcular_opinion <- function(data, fecha) {
  # Columnas que representan las opiniones de personajes
  opinion_cols <- names(data) |> 
    str_subset("^T_OpinionPersonajes_")
  
  # Función para calcular la opinión ponderada
  calcular_porcentaje_opinion <- function(df, segmento_nombre, var_col = NULL) {
    df |>
      select(all_of(opinion_cols), Ponderador_Compuesto, all_of(var_col)) |>
      pivot_longer(cols = all_of(opinion_cols), 
                   names_to = "personaje", 
                   values_to = "opinion_original") |>
      mutate(
        opinion = case_when(
          opinion_original %in% c("Muy buena", "Buena") ~ "Positiva",
          opinion_original == "Regular ( NO LEER)" ~ "Regular",
          opinion_original %in% c("Mala", "Muy mala") ~ "Negativa",
          opinion_original == "Ns/Nc" ~ "Ns/Nc",
          TRUE ~ NA_character_
        )
      ) |>
      filter(!is.na(opinion)) |>
      group_by(personaje, opinion, !!sym(var_col)) |>
      summarise(
        por = sum(Ponderador_Compuesto, na.rm = TRUE) / sum(df$Ponderador_Compuesto, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        filtro = "Opinión",
        segmento = segmento_nombre,
        medicion = as.Date(fecha),
        personaje = str_remove(personaje, "^T_OpinionPersonajes_"),
        var = !!sym(var_col)
      )
  }
  
  # Calcular para total
  resultados_totales <- data |>
    select(all_of(opinion_cols), Ponderador_Compuesto) |>
    pivot_longer(cols = all_of(opinion_cols), 
                 names_to = "personaje", 
                 values_to = "opinion_original") |>
    mutate(
      opinion = case_when(
        opinion_original %in% c("Muy buena", "Buena") ~ "Positiva",
        opinion_original == "Regular ( NO LEER)" ~ "Regular",
        opinion_original %in% c("Mala", "Muy mala") ~ "Negativa",
        opinion_original == "Ns/Nc" ~ "Ns/Nc",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(opinion)) |>
    group_by(personaje, opinion) |>
    summarise(
      por = sum(Ponderador_Compuesto, na.rm = TRUE) / sum(data$Ponderador_Compuesto, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      filtro = "Opinión",
      var = "Nuevo León",
      segmento = "Total",
      medicion = as.Date(fecha),
      personaje = str_remove(personaje, "^T_OpinionPersonajes_")
    )
  
  # Calcular por NSE
  resultados_nse <- data |>
    group_split(nse_clasificacion) |>
    map_dfr(~ calcular_porcentaje_opinion(.x, "nse", "nse"))
  
  # Calcular por generaciones
  resultados_generaciones <- data |>
    group_split(GENERACIONES_UPDATED_MORA) |>
    map_dfr(~ calcular_porcentaje_opinion(.x, "generaciones", "GENERACIONES_UPDATED_MORA"))
  
  # Calcular por sexo
  resultados_sexo <- data |>
    group_split(sexo) |>
    map_dfr(~ calcular_porcentaje_opinion(.x, "sexo", "sexo"))
  
  # Calcular por escolaridad
  resultados_escolaridad <- data |>
    group_split(socioeconomicos_escolaridad) |>
    map_dfr(~ calcular_porcentaje_opinion(.x, "escolaridad", "socioeconomicos_escolaridad"))
  
  # Combinar todos los resultados
  bind_rows(resultados_totales, resultados_nse, resultados_generaciones, resultados_sexo, resultados_escolaridad) |>
    mutate(var = as.factor(var),
           opinion = as.factor(opinion)) |> 
    select(medicion, var, opinion, por, personaje, segmento, filtro)
}

# Aplicar la función
resultados_opinion <- calcular_opinion(data, "2024-09-15")
resultados_opinion2 <- calcular_opinion(data2,  "2024-10-15")

# Verificar resultados
head(resultados_opinion)

fecha = "2024-12-12"
calcular_saldo <- function(data, fecha) {
  # Columnas que representan las opiniones de personajes
  opinion_cols <- names(data) |> 
    str_subset("^T_OpinionPersonajes_")
  
  # Función para calcular el saldo ponderado
  calcular_porcentaje_saldo <- function(df, segmento_nombre, var_col = NULL) {
    df <- df |> 
      mutate(var_col_value = if (!is.null(var_col)) df[[var_col]] else "Nuevo León")
    
    df |>
      select(all_of(opinion_cols), Ponderador_Compuesto, var_col_value) |>
      pivot_longer(
        cols = all_of(opinion_cols), 
        names_to = "personaje", 
        values_to = "opinion_original"
      ) |>
      mutate(
        opinion = case_when(
          opinion_original %in% c("Muy buena", "Buena") ~ "Positiva",
          opinion_original %in% c("Mala", "Muy mala") ~ "Negativa",
          opinion_original == "Ns/Nc" ~ "Ns/Nc",
          TRUE ~ NA_character_
        )
      ) |>
      filter(!is.na(opinion)) |>
      group_by(personaje, opinion, var_col_value) |>
      summarise(
        por = sum(Ponderador_Compuesto, na.rm = TRUE) / sum(df$Ponderador_Compuesto, na.rm = TRUE),
        .groups = "drop"
      ) |>
      ungroup() |>
      complete(personaje, opinion = c("Positiva", "Negativa"), fill = list(por = 0)) |>
      group_by(personaje, var_col_value) |>
      summarise(
        Positiva = sum(por[opinion == "Positiva"]),
        Negativa = sum(por[opinion == "Negativa"]),
        .groups = "drop"
      ) |>
      mutate(
        por = Positiva - Negativa,
        filtro = "Saldo",
        segmento = segmento_nombre,
        medicion = as.Date(fecha),
        personaje = str_remove(personaje, "^T_OpinionPersonajes_"),
        opinion = "Tasa",
        var = var_col_value
      ) |>
      select(medicion, var, opinion, por, personaje, segmento, filtro)
  }
  
  # Calcular para total
  resultados_totales <- calcular_porcentaje_saldo(data, "Total")
  
  # Calcular por NSE
  resultados_nse <- data |>
    group_split(nse_clasificacion) |>
    map_dfr(~ calcular_porcentaje_saldo(.x, "nse", "nse_clasificacion"))
  
  # Calcular por generaciones
  resultados_generaciones <- data |>
    group_split(GENERACIONES_UPDATED_MORA) |>
    map_dfr(~ calcular_porcentaje_saldo(.x, "generaciones", "GENERACIONES_UPDATED_MORA"))
  
  # Calcular por sexo
  resultados_sexo <- data |>
    group_split(sexo) |>
    map_dfr(~ calcular_porcentaje_saldo(.x, "sexo", "sexo"))
  
  # Calcular por escolaridad
  resultados_escolaridad <- data |>
    group_split(socioeconomicos_escolaridad) |>
    map_dfr(~ calcular_porcentaje_saldo(.x, "escolaridad", "socioeconomicos_escolaridad"))
  
  # Combinar todos los resultados
  bind_rows(resultados_totales, resultados_nse, resultados_generaciones, resultados_sexo, resultados_escolaridad) |>
    mutate(var = as.factor(var),
           opinion = as.factor(opinion)) |> 
    select(medicion, var, opinion, por, personaje, segmento, filtro)
}

# Aplicar la función
resultados_saldo <- calcular_saldo(data, "2024-09-15")
resultados_saldo2 <- calcular_saldo(data2, "2024-10-15")

# Verificar resultados
head(resultados_saldo)

df_perso_nl <- bind_rows(resultados, resultados_opinion, resultados_saldo,
                         resultados2, resultados_opinion2, resultados_saldo2)


save(df_perso_nl, file = "www/Microdatos/DF - NL/df_perso_nl.Rda")
