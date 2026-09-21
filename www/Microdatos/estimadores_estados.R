rm(list = ls())
# Setup----
pacman::p_load(tidyverse, scales, purrr, lubridate, janitor, zoo,  htmltools, viridis, yaml, rpostgis, googlesheets4, sp, ggthemes, 
               htmlwidgets, stringr, survey, MetBrewer, srvyr, readxl, sysfonts, rlang,
               shiny, shinyWidgets, showtext)

Sys.setlocale(locale = "es_ES.UTF-8")

path_output <- "www/Microdatos/DF - Estados/"

segmentos <- c("bandera", "nse", "generaciones", "sexo", "zona_euzen", "socio_educacion", "cultupol_votoalian")

read_rds(paste0(path_output, "df_edos.rds")) -> df_raw

df_raw |> 
  mutate(cultupol_votoalian = fct_recode(cultupol_votoalian,
                                         "Indeciso" = "No lo ha decidido aún",
                                         "Indeciso" = "Ns/Nc",
                                         "MORENA-PT-PVEM" = "MORENA-PT-Partido Verde"),
         bandera = "NACIONAL") -> df_acum

df_acum|>
  srvyr::as_survey_design(ids = sbj_num,
                          probs = NULL, 
                          # fpc = f_exp_nal,
                          weights = ponderador) -> temp

aux_segmentos <- c(unique(df_acum$bandera),
                   levels(df_acum$nse),
                   levels(df_acum$generaciones),
                   unique(df_acum$sexo),
                   unique(df_acum$zona_euzen),
                   levels(df_acum$socio_educacion),
                   levels(df_acum$cultupol_votoalian))

# Cultura Política ----
## Opinión Personajes ----

personajes <- names(df_acum[26:34])

map(segmentos,
    function(x){
      print(x)
      map(personajes,
          function(y){
            print(y)
            
            # x <- segmentos[1]
            # y <- personajes[1]
            temp|>
              # mutate(!!sym(y) := fct_recode(!!sym(y), !!!evaluacion)) |>
              group_by(medicion, estado,!!sym(x), !!sym(y)) |>
              summarise(por = survey_mean(vartype = "cv")) |>
              mutate(personaje = y,
                     segmento = x,
                     filtro = "Opinión") |>
              rename(var = !!sym(x),
                     opinion  = !!sym(y)) 
          }
      )
    }
)  |> 
  reduce(bind_rows) |>
  select(-por_cv)  -> df_opinion


df_opinion  |>
  pivot_wider(names_from = opinion, values_from = por) |> 
  rowwise() |> 
  mutate(across(Positiva:Negativa, ~replace_na(., 0)),
         Conocimiento = Positiva + Negativa,
         filtro = "Conocimiento") |> 
  select(-7:-10) |> 
  gather("opinion", "por", -c(1:6)) ->  df_conocimiento

df_opinion |>
  pivot_wider(names_from = opinion, values_from = por) |> 
  mutate(across(Positiva:Negativa, ~ replace_na(., 0)),
         Tasa = Positiva - Negativa,
         filtro = "Saldo") |>   # Realmente es saldo
  select(-7:-10) |> 
  gather("opinion", "por", -c(1:6)) -> df_tasa

df_opinion |> 
  bind_rows(df_conocimiento)|>
  bind_rows(df_tasa) |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc")) |> 
  mutate(opinion = factor(opinion, levels = c("Positiva", "Ns/Nc", "Negativa", "Conocimiento", "Tasa")),
         var = factor(var, levels = aux_segmentos)
  ) -> df_perso
# rename(personaje = pregunta) 

save(df_perso, file=paste0(path_output, "df_perso.Rda"))



## Simpatía partidista ----


map(segmentos,
    function(x){
      print(x)
      temp|>
        mutate(cultupol_simpatia =
                 recode_factor(cultupol_simpatia,
                               "No simpatiza con ninguno en particular" = "Apartidista",
                               "Ns/Nc" = "Apartidista",
                               "Otro" = "Apartidista"
                 ),
               cultupol_simpatia = factor(cultupol_simpatia,
                                          levels = c("MOVIMIENTO CIUDADANO",
                                                     "MORENA",
                                                     "PAN",
                                                     "PRI",
                                                     "PARTIDO VERDE",
                                                     "PRD",
                                                     "PT",
                                                     "Apartidista"
                                                     
                                          ))
        ) |>
        group_by(medicion, !!sym(x), cultupol_simpatia) |>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = x) |>
        rename(var = !!sym(x))
    }
) |>
  reduce(full_join) |>
  mutate(var = ifelse(is.na(var), "NACIONAL", as.character(var)),
         var = factor(var, levels = aux_segmentos)) |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var))-> df_simpatia


save(df_simpatia, file = paste0(path_output, "df_simpatia.Rda"))


## Evaluación ----


partidos <- c("cultupol_evalmarca_pri", "cultupol_evalmarca_pan", "cultupol_evalmarca_mc", "cultupol_evalmarca_mor")



map(segmentos,
    function(x) {
      print(x)
      map(partidos,
          function(y) {
            # x <- segmentos[1]
            # y <- partidos[1]
            
            temp |>
              group_by(medicion, !!sym(x), !!sym(y)) |>
              summarise(por = survey_mean(vartype = "cv"),
                        partido = case_match(y,
                                             "cultupol_evalmarca_pri" ~ "PRI",
                                             "cultupol_evalmarca_pan" ~ "PAN",
                                             "cultupol_evalmarca_mor" ~ "Morena",
                                             "cultupol_evalmarca_mc" ~ "MC"),
                        partido = factor(partido, c("MC", "Morena", "PAN", "PRI"))) |>
              mutate(segmento = ifelse(x == "", "nacional", x)) |>
              rename("evaluacion" = !!sym(y),
                     var = !!sym(x))
            
          }
      )
      
    }
)  |> 
  reduce(bind_rows) |> 
  mutate(evaluacion = factor(evaluacion, levels = c("Positiva", "Ns/Nc", "Negativa")),
         var = factor(var, levels = aux_segmentos),
  ) |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var)) -> df_partidos

save(df_partidos, file=paste0(path_output, "df_partidos.Rda"))


## Rechazo Partidista ---- 


map(segmentos,
    function(x){
      print(x)
      temp|>
        mutate(cultupol_rechazomarca =
                 recode_factor(cultupol_rechazomarca,
                               "Otro" = "Ns/Nc"
                 ),
               cultupol_rechazomarca = factor(cultupol_rechazomarca,
                                              levels = c("MOVIMIENTO CIUDADANO",
                                                         "MORENA",
                                                         "PAN",
                                                         "PRI",
                                                         "PARTIDO VERDE",
                                                         "PRD",
                                                         "PT", 
                                                         "Ns/Nc"
                                              ))) |>
        group_by(medicion, !!sym(x), cultupol_rechazomarca)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = x) |>
        rename(var = !!sym(x))
    }
) |>
  reduce(full_join) |>
  mutate(var = ifelse(is.na(var), "NACIONAL", as.character(var)),
         var = factor(var, levels = aux_segmentos),
  ) |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var)) -> df_rechazo

save(df_rechazo, file=paste0(path_output, "df_rechazo.Rda"))



## Voto marca presidente ---- 


map(segmentos,
    function(x){
      print(x)
      temp|>
        mutate(cultupol_votomarca =
                 recode_factor(cultupol_votomarca,
                               "Anulará su voto" = "Ns/Nc",
                               "No lo ha decidido aún" = "Ns/Nc",
                               "Otro… ¿cuál?" = "Ns/Nc",
                 ),
               cultupol_votomarca = factor(cultupol_votomarca,
                                           levels = c("MOVIMIENTO CIUDADANO",
                                                      "MORENA",
                                                      "PAN",
                                                      "PRI",
                                                      "PARTIDO VERDE",
                                                      "PT",
                                                      "PRD",
                                                      "INDEPENDIENTE",
                                                      "Ns/Nc"
                                           ))) |>
        group_by(medicion, !!sym(x), cultupol_votomarca)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = x) |>
        rename(var = !!sym(x))
    }
) |>
  reduce(full_join) |>
  mutate(var = ifelse(is.na(var), "NACIONAL", as.character(var)),
         var = factor(var, levels = aux_segmentos),
  ) |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var)) -> df_presiMarca

save(df_presiMarca, file=paste0(path_output, "df_presiMarca.Rda"))


## Voto alianza presidente ---- 


map(segmentos[-7],
    function(x){
      print(x)
      temp|>
        mutate(
          cultupol_votoalian = factor(cultupol_votoalian,
                                      levels = c("MORENA-PT-PVEM",
                                                 "MOVIMIENTO CIUDADANO",
                                                 "PAN-PRI-PRD",
                                                 "Anulará su voto",
                                                 "Indeciso")
          )
        ) |>
        group_by(medicion, !!sym(x), cultupol_votoalian)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = x) |>
        rename(var = !!sym(x))
    }
) |>
  reduce(full_join) |>
  mutate(var = ifelse(is.na(var), "NACIONAL", as.character(var)),
         var = factor(var, levels = aux_segmentos),
  ) |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var)) -> df_presiAlian

save(df_presiAlian, file=paste0(path_output, "df_presiAlian.Rda"))



## Segunda opción presidente ---- 

# 
# map(segmentos,
#     function(x){
#       print(x)
#       temp|>
#         rename(cultupol_segundamarca = cultupol_segundamarca) |> 
#         filter(cultupol_segundamarca!="-1.0") |>
#         mutate(cultupol_segundamarca =
#                  recode_factor(cultupol_segundamarca,
#                                "Otro… ¿cuál?" = "Ns/Nc",
#                                "Anulará su voto" = "Ns/Nc",
#                                "No votaría (espontáneo)" = "Ns/Nc"
#                  ),
#                cultupol_segundamarca = factor(cultupol_segundamarca,
#                                             levels = c("MOVIMIENTO CIUDADANO",
#                                                        "MORENA",
#                                                        "PAN",
#                                                        "PRI",
#                                                        "PRD",
#                                                        "PT",
#                                                        "PARTIDO VERDE",
#                                                        "Ns/Nc"
#                                             ))) |>
#         group_by(medicion, !!sym(x), cultupol_segundamarca)|>
#         summarise(por = survey_mean(vartype = "cv")) |>
#         mutate(segmento =x) |>
#         rename(var = !!sym(x))
#     }
# ) |>
#   reduce(full_join) |>
#   mutate(var = ifelse(is.na(var), "NACIONAL", as.character(var)),
#          var = factor(var, levels = aux_segmentos),
#   ) |> 
#   filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
#          !is.na(var)) -> df_presiSeguOpc
# 
# save(df_presiSeguOpc, file=paste0(path_output, "df_presiSeguOpc.Rda"))

## Expectativa triunfo alianza presidente ---- 


map(segmentos,
    function(x){
      print(x)
      temp|>
        mutate(cultupol_expectalian =
                 recode_factor(cultupol_expectalian,
                               "Ninguno (espontáneo)" = "Ns/Nc"
                 ),
               cultupol_expectalian = factor(cultupol_expectalian,
                                             levels = c("MORENA-PT-PVEM",
                                                        "MOVIMIENTO CIUDADANO",
                                                        "PAN-PRI-PRD",
                                                        "Ns/Nc"
                                             ))) |>
        group_by(medicion, !!sym(x), cultupol_expectalian)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = x) |>
        rename(var = !!sym(x))
    }
) |>
  reduce(full_join) |>
  mutate(var = ifelse(is.na(var), "NACIONAL", as.character(var)),
         var = factor(var, levels = aux_segmentos),
  ) |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var),
         !is.na(cultupol_expectalian)) -> df_triunfAlian

save(df_triunfAlian, file=paste0(path_output, "df_triunfAlian.Rda"))

## Internos de los partidos ---- 

internos <- paste0("cultupol_interno", seq(1,8))
interno_c <- "cultupol_interno"

map(segmentos,
    function(x){
      print(x)
      map(internos,
          function(y){
            print(y)
            temp|>
              group_by(medicion,!!sym(x), !!sym(y)) |>
              summarise(por = survey_mean(vartype = "cv")) |>
              mutate(segmento = ifelse(x == "", "nacional", x),
                     interno = y) |>
              rename(var = !!sym(x),
                     opinion  = !!sym(y)) -> df_temp
          }
      )
    }
)  -> temp_internos


# Falta identificar cada interno, hasta tener diccionario 

temp_internos |> 
  reduce(bind_rows) |>
  mutate(var = ifelse(is.na(var), "NACIONAL", var),
         var = factor(var, levels = aux_segmentos),
         interno = case_match(interno,
                              str_c(interno_c, "1") ~  "Atenderá la violencia hacia las mujeres",
                              str_c(interno_c, "2") ~  "Protegerá el medio ambiente",
                              str_c(interno_c, "3") ~  "Trabajará por los jóvenes",
                              str_c(interno_c, "4") ~  "Mejorará la salud",
                              str_c(interno_c, "5") ~ "Mejorará la economía de las familias",
                              str_c(interno_c, "6") ~ "Trabajará para resolver el problema de inseguridad",
                              str_c(interno_c, "7") ~  "Combatirá la corrupción",
                              str_c(interno_c, "8") ~ "Apoya más a la educación",
                              str_c(interno_c, "9") ~ "Representa algo nuevo",
                              str_c(interno_c, "10") ~ "Atraer inversión extranjera",
                              str_c(interno_c, "11") ~ "Apoyar a los emprendedores"),
  ) |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var),
         !is.na(opinion)) -> df_interno


save(df_interno, file=paste0(path_output, "df_interno.Rda"))


## Evaluación de alianzas ---- 

alianzas <- c("cultupol_alianmor", "cultupol_alianprian")
evaluacion <- c(Positivo = "Muy bien", Positivo = "Bien", Negativo = "Muy mal", Negativo = "Mal", `Ns/Nc` = "Ns/Nc")

map(segmentos,
    function(x){
      print(x)
      map(alianzas,
          function(y){
            print(y)
            temp|>
              
              mutate(!!sym(y) := fct_recode(!!sym(y), !!!evaluacion)) |>
              group_by(medicion, !!sym(x), !!sym(y)) |>
              summarise(por = survey_mean(vartype = "cv")) |>
              mutate(segmento = x,
                     alianza = y) |>
              rename(var = !!sym(x),
                     opinion  = !!sym(y)) -> df_temp
          }
      )
    }
)  -> temp_evalAlian


temp_evalAlian |> 
  reduce(bind_rows) |>
  mutate(alianza = case_when(alianza == "cultupol_alianmor" ~ "MORENA-PVEM-PT", 
                             T ~ "PAN-PRI-PRD"),
         var = factor(var, levels = aux_segmentos),
  )  |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var)) -> df_evalAlian

save(df_evalAlian, file=paste0(path_output, "df_evalAlian.Rda"))


# Movimiento Ciudadano ----


# VOTO Movimiento Ciudadano ----

# "viabilidad_voto_presidencia_mc_nacional"                        
# "razon_voto_positivo_mc_nacional"                                
# "razon_voto_negativo_mc_nacional"

map(segmentos,
    function(x){
      print(x)
      temp|>
        group_by(medicion, !!sym(x), cultupol_techo_mc)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = x) |>
        rename(var = !!sym(x))
    }
) |> 
  reduce(full_join) |>
  mutate(var = factor(var, levels = aux_segmentos)) |> 
  rename(opinion = cultupol_techo_mc) -> df_techomc

save(df_techomc, file=paste0(path_output, "df_votoMC.Rda"))



# Partidos políticos ----
## VOTO Morena -----


map(segmentos,
    function(x){
      print(x)
      temp|>
        filter(!(cultupol_techo_mor%in%c("-1.0", "#NULL!"))) |>
        group_by(medicion, !!sym(x), cultupol_techo_mor)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = x) |>
        rename(var = !!sym(x))
    }
) |> 
  reduce(full_join) |>
  mutate(var = factor(var, levels = aux_segmentos),
         nivel = "voto") |> 
  rename(opinion = cultupol_techo_mor) |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var)) -> df_votoMorena

save(df_votoMorena, file=paste0(path_output, "df_votoMorena.Rda"))


## VOTO PAN -----

map(segmentos,
    function(x){
      print(x)
      temp|>
        filter(!(cultupol_techo_pan%in%c("-1", "#NULL!"))) |>
        group_by(medicion, !!sym(x), cultupol_techo_pan)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = x) |>
        rename(var = !!sym(x))
    }
) |> 
  reduce(full_join) |>
  mutate(var = factor(var, levels = aux_segmentos),
         nivel = "voto") |> 
  rename(opinion = cultupol_techo_pan) -> df_votoPAN

save(df_votoPAN, file=paste0(path_output, "df_votoPAN.Rda"))



## VOTO PRI -----

map(segmentos,
    function(x){
      print(x)
      temp|>
        group_by(medicion, !!sym(x), cultupol_techo_pri)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = x) |>
        rename(var = !!sym(x))
    }
) |> 
  reduce(full_join) |>
  mutate(var =factor(var, levels = aux_segmentos),
         nivel = "voto") |> 
  rename(opinion = cultupol_techo_pri) -> df_votoPRI

save(df_votoPRI, file=paste0(path_output, "df_votoPRI.Rda"))


# CAREOS ----


map(segmentos,
    function(x){
      print(x)
      temp|>
        group_by(medicion, !!sym(x), careo1) |>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = x) |>
        rename(var = !!sym(x)) -> df_temp
    }
) |> 
  reduce(bind_rows) |> 
  mutate(
    var = factor(var, levels = aux_segmentos),
    opinion = "Careo #1"
  ) |> 
  filter(!is.na(careo1))-> df_careosPresi

save(df_careosPresi, file=paste0(path_output, "df_careosPresi.Rda"))

# Evaluación ----

## Aprobación AMLO -----

map(segmentos,
    function(x){
      print(x)
      temp|>
        mutate(evalgob_amlo := fct_recode(evalgob_amlo, 
                                          !!!c(Positivo = "Muy buena", 
                                               Positivo = "Buena", 
                                               Negativo = "Muy mala", 
                                               Negativo = "Mala", 
                                               `Ns/Nc` = "Ns/Nc"))) |> 
        group_by(medicion, !!sym(x), evalgob_amlo)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = ifelse(x == "", "nacional", x)) |>
        rename(var = !!sym(x))
    }
) |> 
  reduce(full_join) |>
  mutate(var = factor(var, levels = aux_segmentos)) |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var)) -> df_evalAMLO

save(df_evalAMLO, file=paste0(path_output, "df_evalAMLO.Rda"))


## Aprobación temas de gobierno ----

temasAMLO <-  paste0("evalgob_tema", c(seq(1, 5), seq(7,9)))
temasAMLO_c <- "evalgob_tema"

map(segmentos,
    function(x){
      print(x)
      map(temasAMLO,
          function(y){
            print(y)
            temp|>
              group_by(medicion, !!sym(x), !!sym(y)) |>
              summarise(por = survey_mean(vartype = "cv")) |>
              mutate(segmento = x,
                     tema = y) |>
              rename(var = !!sym(x),
                     opinion  = !!sym(y)) -> df_temp
          }
      )
    }
)  |> 
  reduce(bind_rows) |> 
  mutate(var = factor(var, levels = aux_segmentos), 
         tema = case_match(tema,
                           str_c(temasAMLO_c, 1) ~ "Atención a niños con cáncer",
                           str_c(temasAMLO_c, 2) ~ "Abasto de medicamentos",
                           str_c(temasAMLO_c, 3) ~ "Inseguridad",
                           str_c(temasAMLO_c, 4) ~ "Violencia contra las mujeres",
                           str_c(temasAMLO_c, 5) ~ "Búsqueda de desaparecidos",
                           str_c(temasAMLO_c, 7) ~ "Precio de las gasolinas",
                           str_c(temasAMLO_c, 8) ~ "Disminución de la pobreza",
                           str_c(temasAMLO_c, 9) ~ "Combate a la corrupción"
         )
  ) |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var))-> df_temasAMLO

# save(df_temasAMLO, file=paste0(path_output, "df_temasAMLO.Rda"))
# 
# 
# ## Sensación de inseguridad ---- 
# 
# seguridad <- paste0("evalgob_seg", seq(1,6))
# seguridad_c <- "evalgob_seg"
# 
# map(segmentos,
#     function(x){
#       print(x)
#       map(seguridad,
#           function(y){
#             print(y)
#             temp|>
#               group_by(medicion, !!sym(x), !!sym(y)) |>
#               summarise(por = survey_mean(vartype = "cv")) |>
#               mutate(segmento = x,
#                      ambito = y) |>
#               rename(var = !!sym(x),
#                      respuesta  = !!sym(y)) -> df_temp
#           }
#       )
#     }
# )  -> temp_seguridad
# 
# temp_seguridad |> 
#   reduce(bind_rows) |>
#   mutate(var = ifelse(is.na(var), "NACIONAL", var),
#          var = factor(var, levels = aux_segmentos),
#          ambito = case_match(ambito,
#                              str_c(seguridad_c, "1") ~ "En el país en general",
#                              str_c(seguridad_c, "2") ~ "En el estado",
#                              str_c(seguridad_c, "3") ~ "En su municipio",
#                              str_c(seguridad_c, "4") ~ "En su colonia",
#                              str_c(seguridad_c, "5") ~ "En la calle en la que vive",
#                              str_c(seguridad_c, "6") ~ "En su propia casa"
#          )) |> 
#   filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
#          !is.na(var)) -> df_seguridad
# 
# 
# save(df_seguridad, file=paste0(path_output, "df_seguridad.Rda"))




# Economía ----


map(segmentos,
    function(x){
      print(x)
      temp|>
        group_by(medicion, !!sym(x), brujula_economia)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = ifelse(x == "", "nacional", x)) |>
        rename(var = !!sym(x))
    }
) |>
  reduce(full_join) |>
  mutate(var = ifelse(is.na(var), "NACIONAL", as.character(var)),
         var = factor(var, levels = aux_segmentos),
  )  |>
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var),
         !is.na(brujula_economia))-> df_economia

save(df_economia, file=paste0(path_output, "df_economia.Rda"))



# Camino del país ----


map(segmentos,
    function(x){
      print(x)
      temp|>
        group_by(medicion, !!sym(x), brujula_caminopais)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = x) |>
        rename(var = !!sym(x))
    }
) |>
  reduce(full_join) |>
  mutate(var = factor(var, levels = aux_segmentos),
  )  |>
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var),
         !is.na(brujula_caminopais)) -> df_caminopais

save(df_caminopais, file=paste0(path_output, "df_caminopais.Rda"))

# Beneficiarios ----


map(segmentos,
    function(x){
      print(x)
      temp|>
        group_by(medicion, !!sym(x), socio_progsoc)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = ifelse(x == "", "nacional", x)) |>
        rename(var = !!sym(x))
    }
) |> 
  reduce(full_join) |>
  mutate(var = factor(var, levels = aux_segmentos),
  )  |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var))-> df_benef

save(df_benef, file=paste0(path_output, "df_benef.Rda"))



## Sensación de inseguridad ---- 

temasprogres <- paste0("brujula_temaspol", seq(1,6))
temasprogres_c <- "brujula_temaspol"

map(segmentos,
    function(x){
      print(x)
      map(temasprogres,
          function(y){
            print(y)
            temp|>
              mutate(!!sym(y) := fct_recode(!!sym(y),
                                            "Ns/Nc" = "Le da igual (espontáneo)")) |> 
              group_by(medicion, !!sym(x), !!sym(y)) |>
              summarise(por = survey_mean(vartype = "cv")) |>
              mutate(segmento = x,
                     tema = y) |>
              rename(var = !!sym(x),
                     respuesta  = !!sym(y)) -> df_temp
          }
      )
    }
)  -> temp_temasprogres

temp_temasprogres |> 
  reduce(bind_rows) |>
  mutate(var = factor(var, levels = aux_segmentos),
         tema = case_match(tema,
                           str_c(temasprogres_c, 1) ~ "Militarización",
                           str_c(temasprogres_c, 2) ~ "Legalización de la marihuana",
                           str_c(temasprogres_c, 3) ~ "Matrimonio igualitario",
                           str_c(temasprogres_c, 4) ~ "Adopción homoparental",
                           str_c(temasprogres_c, 5) ~ "Despenalización del aborto",
                           str_c(temasprogres_c, 6) ~ "Discriminación como impedimento",
                           str_c(temasprogres_c, 7) ~ "Educación universitaria para mobilidad social",
                           str_c(temasprogres_c, 8) ~ "Inmigración desmedida",
                           str_c(temasprogres_c, 9) ~ "Organizaciones criminales más violentas",
                           str_c(temasprogres_c, 10) ~ "Meritocracia"
                           
         )) |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var),
         !is.na(respuesta)) -> df_temasprogres


save(df_temasprogres, file=paste0(path_output, "df_temasprogres.Rda"))


# cultupol_mejormexico ----

map(segmentos,
    function(x){
      print(x)
      temp|>
        group_by(medicion, !!sym(x), cultupol_mejormexico)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = ifelse(x == "", "nacional", x)) |>
        rename(var = !!sym(x))
    }
) |> 
  reduce(full_join) |>
  mutate(var = factor(var, levels = aux_segmentos),
  )  |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var),
         !is.na(cultupol_mejormexico)) -> df_mejor

save(df_mejor, file=paste0(path_output, "df_mejor.Rda"))


# cultupol_peormexico ----

map(segmentos,
    function(x){
      print(x)
      temp|>
        group_by(medicion, !!sym(x), cultupol_peormexico)|>
        summarise(por = survey_mean(vartype = "cv")) |>
        mutate(segmento = ifelse(x == "", "nacional", x)) |>
        rename(var = !!sym(x))
    }
) |> 
  reduce(full_join) |>
  mutate(var = factor(var, levels = aux_segmentos),
  )  |> 
  filter(!(segmento == "socio_educacion" & var == "Ns/Nc"),
         !is.na(var),
         !is.na(cultupol_peormexico)) -> df_peor

save(df_peor, file=paste0(path_output, "df_peor.Rda"))
