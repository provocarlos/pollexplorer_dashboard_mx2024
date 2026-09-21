pacman::p_load(tidyverse, readxl)

# Leer datos
data <- read_excel("www/Microdatos/NL_sept_15_2024.xlsx")
data2 <- read_excel("www/Microdatos/NL_oct_15_2024.xlsx") |>
  mutate(Ponderador_Compuesto = as.double(Ponderador_Compuesto))

calcular_eval <- function(data, fecha) {
  # Identificar dinámicamente las columnas que comienzan con "EVAL_"
  eval_cols <- names(data) %>% str_subset("^EVAL_")
  
  # Función para calcular el saldo y porcentajes por cada segmento
  calcular_porcentaje_eval <- function(df, segmento_nombre, var_col = NULL) {
    if (!is.null(var_col)) {
      df <- df %>% mutate(var_col_value = .[[var_col]])
    } else {
      df <- df %>% mutate(var_col_value = "Nuevo León")
    }
    
    df %>%
      select(all_of(eval_cols), Ponderador_Compuesto, var_col_value) %>%
      pivot_longer(cols = all_of(eval_cols), 
                   names_to = "personaje", 
                   values_to = "evaluacion_original") %>%
      mutate(
        opinion = case_when(
          evaluacion_original %in% c("Muy buena", "Buena") ~ "Positiva",
          evaluacion_original == "Regular ( NO LEER)" ~ "Regular",
          evaluacion_original %in% c("Mala", "Muy mala") ~ "Negativa",
          TRUE ~ NA_character_
        )) |> 
      filter(!is.na(opinion)) |>
      group_by(personaje, opinion, var_col_value) |>
      summarise(
        por = sum(Ponderador_Compuesto, na.rm = TRUE) /
              sum(df$Ponderador_Compuesto, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        filtro = "Aprobación",
        segmento = segmento_nombre,
        medicion = as.Date(fecha),
        personaje = str_remove(personaje, "^EVAL_"),
        var = var_col_value
      ) %>%
      select(medicion, var, opinion, por, personaje, segmento, filtro)
  }
  
  # Calcular para total
  resultados_totales <- calcular_porcentaje_eval(data, "Total")
  
  # Calcular por NSE
  resultados_nse <- data %>%
    group_split(nse_clasificacion) %>%
    map_dfr(~ calcular_porcentaje_eval(.x, "nse", "nse"))
  
  # Calcular por generaciones
  resultados_generaciones <- data %>%
    group_split(GENERACIONES_UPDATED_MORA) %>%
    map_dfr(~ calcular_porcentaje_eval(.x,
                                       "generaciones",
                                       "GENERACIONES_UPDATED_MORA"))
  
  # Calcular por sexo
  resultados_sexo <- data %>%
    group_split(sexo) %>%
    map_dfr(~ calcular_porcentaje_eval(.x, "sexo", "sexo"))
  
  # Calcular por escolaridad
  resultados_escolaridad <- data %>%
    group_split(socioeconomicos_escolaridad) %>%
    map_dfr(~ calcular_porcentaje_eval(.x,
                                       "escolaridad",
                                       "socioeconomicos_escolaridad"))
  
  # Combinar todos los resultados
  bind_rows(resultados_totales, resultados_nse, resultados_generaciones,
            resultados_sexo, resultados_escolaridad) %>%
    mutate(var = as.factor(var),
           opinion = as.factor(opinion)) |> 
    select(medicion, var, opinion, por, personaje, segmento, filtro)
}

# Aplicar la función a los datos
resultados_eval <- calcular_eval(data, "2024-09-15")
resultados_eval2 <- calcular_eval(data2, "2024-10-15")






fecha = "2024-12-12"

calcular_aprob <- function(df, fecha) {
  # Identificar dinámicamente las columnas que comienzan con "EVAL_"
  eval_cols <- names(df) %>% str_subset("^EVAL_")
  
  # Función para calcular el saldo y porcentajes por cada segmento
  calcular_porcentaje_aprob <- function(df, segmento_nombre, var_col = NULL) {
    if (!is.null(var_col)) {
      df <- df %>% mutate(var_col_value = .[[var_col]])
    } else {
      df <- df %>% mutate(var_col_value = "Nuevo León")
    }
    
    df %>%
      select(all_of(eval_cols), Ponderador_Compuesto, var_col_value) %>%
      pivot_longer(cols = all_of(eval_cols), 
                   names_to = "personaje", 
                   values_to = "evaluacion_original") %>%
      mutate(
        opinion = case_when(
          evaluacion_original =="Aprueba" ~ "Aprueba",
          evaluacion_original == "Desaprueba" ~ "Desaprueba",
          evaluacion_original == "Ns/Nc" ~ "Ns/Nc",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(opinion)) |>
      group_by(personaje, opinion, var_col_value) |>
      summarise(
        por = sum(Ponderador_Compuesto,na.rm = TRUE) /
              sum(df$Ponderador_Compuesto, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        filtro = "Evaluación",
        segmento = segmento_nombre,
        medicion = as.Date(fecha),
        personaje = str_remove(personaje, "^EVAL_"),
        var = var_col_value
      ) %>%
      select(medicion, var, opinion, por, personaje, segmento, filtro)
  }
  
  # Calcular para total
  resultados_totales <- calcular_porcentaje_aprob(df, "Total")
  
  # Calcular por NSE
  resultados_nse <- df %>%
    group_split(nse_clasificacion) %>%
    map_dfr(~ calcular_porcentaje_aprob(.x, "nse", "nse"))
  
  # Calcular por generaciones
  resultados_generaciones <- df %>%
    group_split(GENERACIONES_UPDATED_MORA) %>%
    map_dfr(~ calcular_porcentaje_aprob(.x,
                                        "generaciones",
                                        "GENERACIONES_UPDATED_MORA"))
  
  # Calcular por sexo
  resultados_sexo <- df %>%
    group_split(sexo) %>%
    map_dfr(~ calcular_porcentaje_aprob(.x, "sexo", "sexo"))
  
  # Calcular por escolaridad
  resultados_escolaridad <- df %>%
    group_split(socioeconomicos_escolaridad) %>%
    map_dfr(~ calcular_porcentaje_aprob(.x,
                                        "escolaridad",
                                        "socioeconomicos_escolaridad"))
  
  # Combinar todos los resultados
  bind_rows(resultados_totales, resultados_nse, resultados_generaciones,
            resultados_sexo, resultados_escolaridad) %>%
    mutate(var = as.factor(var),
           opinion = as.factor(opinion)) |> 
    select(medicion, var, opinion, por, personaje, segmento, filtro) |> 
    filter(personaje != "FC_SEGURIDAD_NL",
         personaje != "GOB_NL")
}

resultados_aprob <- calcular_aprob(data, "2024-09-15")
resultados_aprob2 <- calcular_aprob(data2, "2024-10-15")

df_aprob_nl <- bind_rows(resultados_aprob, resultados_aprob2) |> 
  filter(personaje != "FC_SEGURIDAD_NL",
         personaje != "GOB_NL")

df_eval_nl <- bind_rows(resultados_aprob, resultados_eval,
                        resultados_aprob2, resultados_eval2)

save(df_eval_nl, file = "www/Microdatos/DF - NL/df_eval_nl.Rda")
