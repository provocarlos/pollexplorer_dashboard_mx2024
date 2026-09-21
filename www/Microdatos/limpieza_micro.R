# Setup----
pacman::p_load(tidyverse, scales, purrr, lubridate, janitor, zoo, googlesheets4, 
               stringr, survey, srvyr, readxl, sysfonts, rlang,
               shiny, shinyWidgets, showtext)


Sys.setlocale(locale = "es_ES.UTF-8")

path_output <- "www/Microdatos/"
path_microdatos <- "~/Downloads/"


# Primer medición ----

read_excel(paste0(path_microdatos, 
                  "ESTUDIO NACIONAL EUZEN_SM_PONDERADORES.xlsx"), guess_max = 15000) |>
  clean_names()|> 
  rename_with(.cols = contains("nacional"), ~gsub("_?nacional_?", "", .)) |> 
  rename_with(~str_replace(., "partidosl", "partidos")) |> 
  rename(
    nombre_amlo             = t_personalidadesandres_manuel_lopez_obrador,
    nombre_me               = t_personalidadesmarcelo_ebrard,
    nombre_cs               = t_personalidadesclaudia_sheinbaum,
    nombre_sg               = t_personalidadessamuel_garcia,
    nombre_ldc              = t_personalidadesluis_donaldo_colosio,
    nombre_mr               = t_personalidadesmariana_rodriguez,
    nombre_dd               = t_personalidadesdante_delgado,
    nombre_xg               = t_personalidadesxochitl_galvez,
    nombre_jam              = t_personalidades2_jorge_alvarez_maynez,
    cultupol_calenelect     = conocimiento_elecciones_presidenciales,
    cultupol_simpatia       = simpatia_partidista,
    cultupol_rechazomarca   = rechazo_partidista,
    cultupol_evalmarca_pan  = t_evaluacion_imagen_partidos_1,
    cultupol_evalmarca_pri  = t_evaluacion_imagen_partidos_2,
    cultupol_evalmarca_prd  = t_evaluacion_imagen_partidos_3,
    cultupol_evalmarca_mc   = t_evaluacion_imagen_partidos_4,
    cultupol_evalmarca_mor  = t_evaluacion_imagen_partidos_5,
    cultupol_evalmarca_pvem = t_evaluacion_imagen_partidos_8,
    cultupol_evalmarca_pt   = t_evaluacion_imagen_partidos_7,
    cultupol_votomarca      = intencion_votox_partido_presidencia,
    cultupol_mejormexico    = lo_mejorpodriapasarle_mexico,
    cultupol_peormexico     = lo_peorpodriapasarle_mexico,
    cultupol_techo_mc       = viabilidad_voto_presidencia_mc,
    cultupol_techo_pri      = viabilidad_voto_presidencia_pri,
    cultupol_techo_mor      = viabilidad_voto_presidencia_morena,
    cultupol_techo_pan      = viabilidad_voto_presidencia_pan,
    cultupol_alianmor       = t_imagen_alianzas1,
    cultupol_alianprian     = t_imagen_alianzas2,
    cultupol_frentemor      = opositor_enfrenta_mejor,
    cultupol_votoalian      = intencion_voto_alianza_presidencial,
    cultupol_expectalian    = percepcion_triunfo_alianza_presidencial,
    evalgob_amlo            = calificacion_labor_presidente_mx,
    brujula_caminopais      = buen_caminomal_camino,
    brujula_economia        = situacion_econimicafamiliar,
    brujula_temaspol1       = t_opinion_polarizada_favor1,  # Militares
    brujula_temaspol2       = t_opinion_polarizada_favor2,  # Marihuana
    brujula_temaspol3       = t_opinion_polarizada_favor3,  # Matrimonio gay
    brujula_temaspol4       = t_opinion_polarizada_favor4,  # Adopción gay
    brujula_temaspol5       = t_opinion_polarizada_favor5,  # Carcel por abortar
    brujula_temaspol6       = t_opinion_polarizada_favor6,  # Discriminación
    socio_progsoc           = beneficiario_de_al_menos_1_programa_social,
    socio_educacion         = socioeconomicos_escolaridad,
    cultupol_interno1       = t_internos_partidos1, # Violencia mujeres
    cultupol_interno2       = t_internos_partidos2, # Medio Ambiente
    cultupol_interno3       = t_internos_partidos3, # Trabajar x los jóvenes
    cultupol_interno4       = t_internos_partidos6, # Salud
    cultupol_interno5       = t_internos_partidos10, # Economia familiar
    cultupol_interno6       = t_internos_partidos12, #Inseguridad
    cultupol_interno7       = t_internos_partidos9, # Corrupción
    cultupol_interno8       = t_internos_partidos13, # Educación
    evalgob_tema1           = t_aprueba_desaprueba_acciones_presidente_mx1, # Niños con cancer
    evalgob_tema2           = t_aprueba_desaprueba_acciones_presidente_mx2, # Medicamentos
    evalgob_tema3           = t_aprueba_desaprueba_acciones_presidente_mx3, # Inseguridad
    evalgob_tema4           = t_aprueba_desaprueba_acciones_presidente_mx5, #Violencia mujeres
    evalgob_tema5           = t_aprueba_desaprueba_acciones_presidente_mx7, # Desaparecidos
    evalgob_tema7           = t_aprueba_desaprueba_acciones_presidente_mx9, # Gasolina
    evalgob_tema8           = t_aprueba_desaprueba_acciones_presidente_mx10, # Pobreza
    evalgob_tema9           = t_aprueba_desaprueba_acciones_presidente_mx12 # Corrupción
  ) |> 
  select(1:26,
         contains("nombre_"),
         contains("cultupol_"), 
         contains("brujula_"),
         contains("socio_"),
         contains("evalgob_"),
         ponderador) |> 
  relocate(contains("cultupol_interno"), .after = cultupol_techo_pan) |> 
  relocate(contains("evalgob_tema"), .after = evalgob_amlo) |> 
  mutate(
    across(.cols = c(cultupol_calenelect:socio_educacion), ~factor(.)),
    nse = fct_relevel(nse, c("A/B", "C+", "C", "C-", "D+", "D", "E")),
    socio_educacion = fct_recode(socio_educacion,
                                 "Bachillerato o símil" ="Preparatoria o carrera técnica o comercial"),
    across(.cols = nombre_amlo:nombre_jam,
           ~gsub("No lo conoce/NS/NC", "Ns/Nc", .)),
    across(.cols = nombre_amlo:nombre_jam, 
           ~factor(case_when(. == "Muy Bien" ~ "Positiva",
                             . == "Bien" ~ "Positiva",
                             . == "Mal" ~ "Negativa",
                             . == "Muy Mal" ~ "Negativa",
                             T ~ "Ns/Nc"),
                   levels = c("Positiva", "Ns/Nc", "Negativa"))),
    across(.cols = cultupol_evalmarca_pan:cultupol_evalmarca_pt, 
           ~factor(case_when(. == "Muy Buena" ~ "Positiva",
                             . == "Buena" ~ "Positiva",
                             . == "Mala" ~ "Negativa",
                             . == "Muy Mala" ~ "Negativa",
                             T ~ "Ns/Nc"),
                   levels = c("Positiva", "Ns/Nc", "Negativa"))),
    generaciones = factor(
      case_when(
        (year(today()) - as.numeric(edad_abierta)) >= 1995 & (year(today()) - as.numeric(edad_abierta)) <= 2010 ~ "Generación Z (18-28)",
        (year(today()) - as.numeric(edad_abierta)) >= 1981 & (year(today()) - as.numeric(edad_abierta)) <= 1994 ~ "Millennials (29-42)",
        (year(today()) - as.numeric(edad_abierta)) >= 1965 & (year(today()) - as.numeric(edad_abierta)) <= 1980 ~ "Generación X (43-58)", 
        (year(today()) - as.numeric(edad_abierta)) >= 1956 & (year(today()) - as.numeric(edad_abierta)) <= 1964 ~ "Baby Boomers (59-67)", 
        .default = "Silenciosos (67 o +)"
      ),
      levels = c(
        "Generación Z (18-28)", 
        "Millennials (29-42)", 
        "Generación X (43-58)", 
        "Baby Boomers (59-67)",
        "Silenciosos (67 o +)"
      )),
    medicion = as.Date("2023-06-19")
  ) |> 
  filter(tipo_muestra == "NACIONAL") |> 
  select(-tipo_muestra)-> df_nal1

names(df_nal1)

# Segunda medición ----- 

read_excel(str_c(path_microdatos, 
                 "MICRODATOS NACIONAL 2.0.xlsx"), guess_max = 15000) |>
  clean_names()|>
  rename_with(~ str_replace(., "t_internos_partidos_1_", "t_internos_partidos")) |> 
  rename_with(.cols = contains("nacional"), ~gsub("_?nacional_?", "", .)) |> 
  rename(
    gps_lat = gps_inicial_lat,
    gps_lon = gps_inicial_lon, 
    nombre_amlo = t_personalidadesamlo,
    nombre_me = t_personalidadesme,
    nombre_cs = t_personalidadescs,
    nombre_sg = t_personalidadessgs,
    nombre_ldc = t_personalidadesldc,
    nombre_mr = t_personalidadesmrc,
    nombre_dd = t_personalidadesdd,
    nombre_xg = t_personalidadesxg,
    nombre_jam = t_personalidadesjam,
    cultupol_calenelect =  conocimiento_elecciones_presidenciales,
    cultupol_simpatia =  simpatia_partidista,
    cultupol_evalmarca_pan = t_evaluacion_imagen_partidos_1,
    cultupol_evalmarca_pri = t_evaluacion_imagen_partidos_2,
    cultupol_evalmarca_prd =  t_evaluacion_imagen_partidos_10,
    cultupol_evalmarca_mc =  t_evaluacion_imagen_partidos_3,
    cultupol_evalmarca_mor =  t_evaluacion_imagen_partidos_4,
    cultupol_evalmarca_pvem =  t_evaluacion_imagen_partidos_5,
    cultupol_evalmarca_pt =  t_evaluacion_imagen_partidos_6,
    cultupol_rechazomarca =  rechazo_partidista,
    cultupol_votomarca =  presidente_republica_por_partido2024,
    cultupol_mejormexico =  lo_mejor_para_mexico,
    cultupol_peormexico =  lo_peor_para_mexico,
    cultupol_techo_mc = voto_para_presidencia_mc,
    cultupol_techo_pri = viabilidad_voto_presidencia_pri,
    cultupol_techo_mor =  voto_presidencia_morena,
    cultupol_techo_pan = viabilidad_voto_presidencia_pan,
    cultupol_alianmor =  t_partidos_agrupados_en_coalicion_1,
    cultupol_alianprian =  t_partidos_agrupados_en_coalicion_2,
    cultupol_frentemor =  opositor_enfrenta_mejor,
    cultupol_votoalian =  voto_por_partido_presidente,
    cultupol_segundaalian =  segunda_opcion_voto_alianzas,
    cultupol_rechazoalian =  partido_alianza_voto,
    cultupol_expectalian = partido_alianza_ganara_pre_rep,
    evalgob_amlo = calificacion_labor_presidente_mx,
    brujula_caminopais = buen_caminomal_camino,
    brujula_economia =situacion_economicafamiliar,
    brujula_temaspol1 = t_opinion_polarizada_favor1,  # Militares
    brujula_temaspol2 = t_opinion_polarizada_favor2,  # Marihuana
    brujula_temaspol3 = t_opinion_polarizada_favor3,  # Matrimonio gay
    brujula_temaspol4 = t_opinion_polarizada_favor4,  # Adopción gay
    brujula_temaspol5 = t_opinion_polarizada_favor5,  # Carcel por abortar
    socio_progsoc = beneficiario_de_al_menos_1_programa_social,
    socio_educacion = socioeconomicos_escolaridad,
    cultupol_interno1 = t_internos_partidos1, # Violencia mujeres
    cultupol_interno2 = t_internos_partidos2, # Medio Ambiente
    cultupol_interno3 = t_internos_partidos3, # Trabajar x los jóvenes
    cultupol_interno4 = t_internos_partidos4, # Salud
    cultupol_interno5 = t_internos_partidos5, # Economía familias
    cultupol_interno6 = t_internos_partidos6, #Inseguridad
    cultupol_interno7 = t_internos_partidos7, # Corrupción
    cultupol_interno8 = t_internos_partidos8, # Educación
    evalgob_tema1 = t_aprueba_desaprueba_acciones_presidente_mx1, # Niños con cancer
    evalgob_tema2 = t_aprueba_desaprueba_acciones_presidente_mx2, # Medicamentos
    evalgob_tema3= t_aprueba_desaprueba_acciones_presidente_mx3, # Inseguridad
    evalgob_tema4 = t_aprueba_desaprueba_acciones_presidente_mx4, #Violencia mujeres
    evalgob_tema5 = t_aprueba_desaprueba_acciones_presidente_mx5, # Desaparecidos
    # evalgob_tema6 = t_aprueba_desaprueba_acciones_presidente_mx6, # Economía
    evalgob_tema7 = t_aprueba_desaprueba_acciones_presidente_mx7, # Gasolina
    evalgob_tema8 = t_aprueba_desaprueba_acciones_presidente_mx8, # Pobreza
    evalgob_tema9 = t_aprueba_desaprueba_acciones_presidente_mx9, # Corrupción
    evalgob_tema10 = t_aprueba_desaprueba_acciones_presidente_mx10, # Corrupción
    evalgob_tema11 = t_aprueba_desaprueba_acciones_presidente_mx11, # Salud
    # "f_exp_nal"
  ) |> 
  select(1:25, 
         contains("nombre_"),
         contains("cultupol_"), 
         contains("brujula_"),
         contains("socio_"),
         contains("evalgob_"),
         ponderador) |> 
  relocate(contains("cultupol_evalmarca"), .after = cultupol_evalmarca_prd) |> 
  mutate(across(.cols = c(cultupol_calenelect:socio_educacion), ~factor(.)),
         nse = fct_relevel(nse, c("A/B", "C+", "C", "C-", "D+", "D", "E")),
         socio_educacion = fct_recode(socio_educacion,
                    "Bachillerato o símil" ="Preparatoria o carrera técnica o comercial"),
         across(.cols = nombre_amlo:nombre_jam,
                ~gsub("No lo conoce/NS/NC", "Ns/Nc", .)),
         across(.cols = contains("cultupol_evalmarca"), 
                ~factor(case_when(. == "Muy Buena" ~ "Positiva",
                                  . == "Buena" ~ "Positiva",
                                  . == "Mala" ~ "Negativa",
                                  . == "Muy Mala" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         across(.cols = nombre_amlo:nombre_jam, 
                ~factor(case_when(. == "Muy Bien" ~ "Positiva",
                                  . == "Bien" ~ "Positiva",
                                  . == "Mal" ~ "Negativa",
                                  . == "Muy Mal" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         generaciones = factor(
           case_when(
             (year(today()) - as.numeric(edad_abierta)) >= 1995 & (year(today()) - as.numeric(edad_abierta)) <= 2010 ~ "Generación Z (18-28)",
             (year(today()) - as.numeric(edad_abierta)) >= 1981 & (year(today()) - as.numeric(edad_abierta)) <= 1994 ~ "Millennials (29-42)",
             (year(today()) - as.numeric(edad_abierta)) >= 1965 & (year(today()) - as.numeric(edad_abierta)) <= 1980 ~ "Generación X (43-58)", 
             (year(today()) - as.numeric(edad_abierta)) >= 1956 & (year(today()) - as.numeric(edad_abierta)) <= 1964 ~ "Baby Boomers (59-67)", 
             .default = "Silenciosos (67 o +)"
           ),
           levels = c(
             "Generación Z (18-28)", 
             "Millennials (29-42)", 
             "Generación X (43-58)", 
             "Baby Boomers (59-67)",
             "Silenciosos (67 o +)"
           )),
         medicion = as.Date("2023-10-08")) -> df_nal2


names(df_nal2)


# Tercera medición ----- 

read_excel(str_c(path_microdatos, 
                 "Nacional 3_0 Noviembre 2023 EUZEN Microdatos.xlsx"), guess_max = 15000) |>
  clean_names()|>
  filter(referenica_muestra %in% c("ORIGINAL", "ORIGINAL SE VA")) |> 
  rename_with(~ str_replace(., "t_internos_partidos_1_", "t_internos_partidos")) |> 
  rename_with(.cols = contains("nacional"), ~gsub("_?nacional_?", "", .)) |> 
  rename(
    zona_euzen = zona_pais,
    gps_lat = gps_inicial_la,
    gps_lon = gps_inicial_lo, 
    nombre_amlo = t_personalidadesamlo,
    nombre_me = t_personalidadesme,
    nombre_cs = t_personalidadescs,
    nombre_sg = t_personalidadessgs,
    nombre_ldc = t_personalidadesldc,
    nombre_mr = t_personalidadesmrc,
    nombre_dd = t_personalidadesdd,
    nombre_xg = t_personalidadesxg,
    nombre_jam = t_personalidadesjam,
    cultupol_calenelect =  conocimiento_elecciones_presidenciales,
    cultupol_simpatia =  simpatia_partidista,
    cultupol_evalmarca_pan = t_evaluacion_imagen_partidos_1,
    cultupol_evalmarca_pri = t_evaluacion_imagen_partidos_2,
    cultupol_evalmarca_prd =  t_evaluacion_imagen_partidos_10,
    cultupol_evalmarca_mc =  t_evaluacion_imagen_partidos_3,
    cultupol_evalmarca_mor =  t_evaluacion_imagen_partidos_4,
    cultupol_evalmarca_pvem =  t_evaluacion_imagen_partidos_5,
    cultupol_evalmarca_pt =  t_evaluacion_imagen_partidos_6,
    cultupol_rechazomarca =  rechazo_partidista,
    cultupol_votomarca =  presidente_republica_por_partido2024,
    # cultupol_mejormexico =  lo_mejor_para_mexico,
    # cultupol_peormexico =  lo_peor_para_mexico,
    cultupol_techo_mc = voto_para_presidencia_mc,
    cultupol_techo_pri = viabilidad_voto_presidencia_pri,
    cultupol_techo_mor =  voto_presidencia_morena,
    cultupol_techo_pan = viabilidad_voto_presidencia_pan,
    cultupol_alianmor =  t_partidos_agrupados_en_coalicion_1,
    cultupol_alianprian =  t_partidos_agrupados_en_coalicion_2,
    cultupol_frentemor =  opositor_enfrenta_mejor,
    cultupol_votoalian =  voto_por_partido_presidente,
    cultupol_segundaalian =  segunda_opcion_voto_alianzas,
    cultupol_rechazoalian =  partido_alianza_voto,
    cultupol_expectalian = partido_alianza_ganara_pre_rep,
    evalgob_amlo = calificacion_labor_presidente_mx,
    # socio_progsoc = beneficiario_de_al_menos_1_programa_social,
    socio_educacion = socioeconomicos_escolaridad,
    cultupol_interno1 = t_internos_partidos1, # Violencia mujeres
    cultupol_interno2 = t_internos_partidos2, # Medio Ambiente
    cultupol_interno3 = t_internos_partidos3, # Trabajar x los jóvenes
    cultupol_interno4 = t_internos_partidos4, # Salud
    cultupol_interno5 = t_internos_partidos5, # Economía familias
    cultupol_interno6 = t_internos_partidos6, #Inseguridad
    cultupol_interno7 = t_internos_partidos7, # Corrupción
    cultupol_interno8 = t_internos_partidos8, # Educación
    cultupol_interno9 = t_internos_partidos9, # Algo nuevo
    cultupol_interno10 = t_internos_partidos10, # Inversión
    factor = factor_de_expansion
  ) |> 
  select(1:23, 
         contains("nombre_"),
         contains("cultupol_"), 
         contains("brujula_"),
         contains("socio_"),
         contains("evalgob_"),
         ponderador) |> 
  relocate(contains("cultupol_evalmarca"), .after = cultupol_evalmarca_prd) |> 
  mutate(across(.cols = c(cultupol_calenelect:socio_educacion), ~factor(.)),
         nse = fct_relevel(nse, c("A/B", "C+", "C", "C-", "D+", "D", "E")),
         socio_educacion = fct_recode(socio_educacion,
                                      "Bachillerato o símil" ="Preparatoria o carrera técnica o comercial"),
         across(.cols = contains("nombre_"),
                ~gsub("No lo conoce/NS/NC", "Ns/Nc", .)),
         across(.cols = contains("cultupol_evalmarca"), 
                ~factor(case_when(. == "Muy Buena" ~ "Positiva",
                                  . == "Buena" ~ "Positiva",
                                  . == "Mala" ~ "Negativa",
                                  . == "Muy Mala" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         across(.cols = contains("nombre_"), 
                ~factor(case_when(. == "Muy Bien" ~ "Positiva",
                                  . == "Bien" ~ "Positiva",
                                  . == "Mal" ~ "Negativa",
                                  . == "Muy Mal" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         generaciones = factor(
           case_when(
             (year(today()) - as.numeric(edad_abierta)) >= 1995 & (year(today()) - as.numeric(edad_abierta)) <= 2010 ~ "Generación Z (18-28)",
             (year(today()) - as.numeric(edad_abierta)) >= 1981 & (year(today()) - as.numeric(edad_abierta)) <= 1994 ~ "Millennials (29-42)",
             (year(today()) - as.numeric(edad_abierta)) >= 1965 & (year(today()) - as.numeric(edad_abierta)) <= 1980 ~ "Generación X (43-58)", 
             (year(today()) - as.numeric(edad_abierta)) >= 1956 & (year(today()) - as.numeric(edad_abierta)) <= 1964 ~ "Baby Boomers (59-67)", 
             .default = "Silenciosos (67 o +)"
           ),
           levels = c(
             "Generación Z (18-28)", 
             "Millennials (29-42)", 
             "Generación X (43-58)", 
             "Baby Boomers (59-67)",
             "Silenciosos (67 o +)"
           )),
         medicion = as.Date("2023-11-05")) -> df_nal3


names(df_nal3)

# Cuarta medición 

read_excel(str_c(path_microdatos, 
                 "Nacional 4_0 - Diciembre 2023 EUZEN.xlsx"), guess_max = 15000) |>
  clean_names()|>
  rename_with(.cols = contains("nacional"), ~gsub("_?nacional_?", "", .)) |> 
  rename(
    zona_euzen = zona_pais,
    gps_lat = gps_inicial_la,
    gps_lon = gps_inicial_lo, 
    nombre_amlo = evaluacion_personalidades_amlo,
    nombre_me = evaluacion_personalidades_me,
    nombre_cs = evaluacion_personalidades_cs,
    nombre_sg = evaluacion_personalidades_sg,
    # nombre_ldc = t_personalidadesldc,
    nombre_mr = evaluacion_personalidades_mr,
    nombre_dd = evaluacion_personalidades_dd,
    nombre_xg = evaluacion_personalidades_xg,
    nombre_jam = evaluacion_personalidades_jam,
    nombre_ev = evaluacion_personalidades_ev,
    cultupol_calenelect =  conocimiento_elecciones_presidenciales,
    cultupol_simpatia =  identificacion_partidista,
    cultupol_evalmarca_pan = t_evaluacion_imagen_partidos_1,
    cultupol_evalmarca_pri = t_evaluacion_imagen_partidos_2,
    cultupol_evalmarca_prd =  t_evaluacion_imagen_partidos_10,
    cultupol_evalmarca_mc =  t_evaluacion_imagen_partidos_3,
    cultupol_evalmarca_mor =  t_evaluacion_imagen_partidos_4,
    cultupol_evalmarca_pvem =  t_evaluacion_imagen_partidos_5,
    cultupol_evalmarca_pt =  t_evaluacion_imagen_partidos_6,
    cultupol_rechazomarca =  rechazo_partidista,
    cultupol_votomarca =  presidente_republica_por_partido2024,
    # cultupol_mejormexico =  lo_mejor_para_mexico,
    # cultupol_peormexico =  lo_peor_para_mexico,
    cultupol_techo_mc = voto_para_presidencia_mc,
    cultupol_techo_pri = viabilidad_voto_presidencia_pri,
    cultupol_techo_mor =  voto_presidencia_morena,
    cultupol_techo_pan = viabilidad_voto_presidencia_pan,
    cultupol_alianmor =  t_partidos_agrupados_en_coalicion_1,
    cultupol_alianprian =  t_partidos_agrupados_en_coalicion_2,
    cultupol_frentemor =  opositor_enfrenta_mejor,
    cultupol_votoalian =  voto_por_partido_presidente, # Pedir cambiar
    cultupol_segundaalian =  segunda_opcion_voto_alianzas,
    cultupol_rechazoalian =  partido_alianza_voto, # Pedir cambiar
    cultupol_expectalian = partido_alianza_ganara_pre_rep,
    evalgob_amlo = calificacion_labor_presidente_mx,
    # socio_progsoc = beneficiario_de_al_menos_1_programa_social,
    socio_educacion = socioeconomicos_escolaridad,
    cultupol_interno1 = t_internos_partidos_1_1, # Violencia mujeres
    cultupol_interno2 = t_internos_partidos_1_2, # Medio Ambiente
    cultupol_interno3 = t_internos_partidos_1_3, # Trabajar x los jóvenes
    cultupol_interno4 = t_internos_partidos_1_4, # Salud
    cultupol_interno5 = t_internos_partidos_1_5, # Economía familias
    cultupol_interno6 = t_internos_partidos_1_6, #Inseguridad
    cultupol_interno7 = t_internos_partidos_1_7, # Corrupción
    cultupol_interno8 = t_internos_partidos_1_8, # Educación
    cultupol_interno9 = t_internos_partidos_1_9, # Algo nuevo
    cultupol_interno10 = t_internos_partidos_1_10, # Inversión
    cultupol_interno11 = t_internos_partidos_1_11, # Apoyar a los emprendedores
    brujula_temaspol1 = t_esta_afavor_o_encontra_de_las_frases_1,  # Militares
    brujula_temaspol2 = t_esta_afavor_o_encontra_de_las_frases_2,  # Marihuana
    brujula_temaspol3 = t_esta_afavor_o_encontra_de_las_frases_3,  # Matrimonio gay
    brujula_temaspol4 = t_esta_afavor_o_encontra_de_las_frases_4,  # Adopción gay
    brujula_temaspol5 = t_esta_afavor_o_encontra_de_las_frases_5,  # Carcel por abortar
    brujula_temaspol6 = t_esta_afavor_o_encontra_de_las_frases_6,  # Discriminación
    brujula_temaspol7 = t_esta_afavor_o_encontra_de_las_frases_7,  # Universidad
    brujula_temaspol8 = t_esta_afavor_o_encontra_de_las_frases_8,  # Inmigración
    brujula_temaspol9 = t_esta_afavor_o_encontra_de_las_frases_9,  # Grupos criminales
    brujula_temaspol10 = t_esta_afavor_o_encontra_de_las_frases_10,  # Meritocracia
    elec_careo = care_presi_jamchxg,
    ponderador = ponderador4_0,
    f_exp_nal = factor_de_expansion
  ) |> 
  select(1:23, 
         contains("nombre_"),
         contains("cultupol_"), 
         contains("brujula_"),
         contains("socio_"),
         contains("evalgob_"),
         ponderador) |> 
  relocate(contains("cultupol_evalmarca"), .after = cultupol_evalmarca_prd) |> 
  mutate(across(.cols = c(cultupol_calenelect:socio_educacion), ~factor(.)),
         nse = fct_relevel(nse, c("A/B", "C+", "C", "C-", "D+", "D", "E")),
         socio_educacion = fct_recode(socio_educacion,
                                      "Bachillerato o símil" ="Preparatoria o carrera técnica o comercial"),
         across(.cols = contains("nombre_"),
                ~gsub("No lo conoce/NS/NC", "Ns/Nc", .)),
         across(.cols = contains("cultupol_evalmarca"), 
                ~factor(case_when(. == "Muy Buena" ~ "Positiva",
                                  . == "Buena" ~ "Positiva",
                                  . == "Mala" ~ "Negativa",
                                  . == "Muy Mala" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         across(.cols = contains("nombre_"), 
                ~factor(case_when(. == "Muy Bien" ~ "Positiva",
                                  . == "Bien" ~ "Positiva",
                                  . == "Mal" ~ "Negativa",
                                  . == "Muy Mal" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         generaciones = factor(
           case_when(
             (year(today()) - as.numeric(edad_abierta)) >= 1995 & (year(today()) - as.numeric(edad_abierta)) <= 2010 ~ "Generación Z (18-28)",
             (year(today()) - as.numeric(edad_abierta)) >= 1981 & (year(today()) - as.numeric(edad_abierta)) <= 1994 ~ "Millennials (29-42)",
             (year(today()) - as.numeric(edad_abierta)) >= 1965 & (year(today()) - as.numeric(edad_abierta)) <= 1980 ~ "Generación X (43-58)", 
             (year(today()) - as.numeric(edad_abierta)) >= 1956 & (year(today()) - as.numeric(edad_abierta)) <= 1964 ~ "Baby Boomers (59-67)", 
             .default = "Silenciosos (67 o +)"
           ),
           levels = c(
             "Generación Z (18-28)", 
             "Millennials (29-42)", 
             "Generación X (43-58)", 
             "Baby Boomers (59-67)",
             "Silenciosos (67 o +)"
           )),
         medicion = as.Date("2023-12-15")) -> df_nal4


names(df_nal4)


# Quinta medición 

read_excel(str_c(path_microdatos, 
                 "Estudio Nacional 5.0.xlsx"), guess_max = 15000) |>
  clean_names()|>
  rename_with(.cols = contains("nacional"), ~gsub("_?nacional_?", "", .)) |> 
  rename(
    zona_euzen = zona_pais,
    gps_lat = gps_inicial_la,
    gps_lon = gps_inicial_lo, 
    nombre_amlo = t_per_nom_amlo,
    # nombre_me = evaluacion_personalidades_me,
    nombre_cs = t_per_nom_cs,
    nombre_sg = t_per_nom_sg,
    # nombre_ldc = t_personalidadesldc,
    nombre_mr = t_per_nom_mr,
    nombre_dd = t_per_nom_dd,
    nombre_xg = t_per_nom_xg,
    nombre_jam = t_per_nom_jam,
    # nombre_ev = evaluacion_personalidades_ev,
    cultupol_calenelect =  conocimiento_elecciones_presidenciales,
    cultupol_simpatia =  identificacion_partidista,
    cultupol_evalmarca_pan = t_evaluacion_imagen_partidos_1,
    cultupol_evalmarca_pri = t_evaluacion_imagen_partidos_2,
    cultupol_evalmarca_prd =  t_evaluacion_imagen_partidos_10,
    cultupol_evalmarca_mc =  t_evaluacion_imagen_partidos_3,
    cultupol_evalmarca_mor =  t_evaluacion_imagen_partidos_4,
    cultupol_evalmarca_pvem =  t_evaluacion_imagen_partidos_5,
    cultupol_evalmarca_pt =  t_evaluacion_imagen_partidos_6,
    # cultupol_rechazomarca =  rechazo_partidista,
    # cultupol_votomarca =  presidente_republica_por_partido2024,
    # cultupol_mejormexico =  lo_mejor_para_mexico,
    # cultupol_peormexico =  lo_peor_para_mexico,
    cultupol_techo_mc = voto_para_presidencia_mc,
    cultupol_techo_pri = voto_para_presidencia_pri,
    cultupol_techo_mor =  voto_presidencia_morena,
    cultupol_techo_pan = voto_para_presidencia_pan,
    cultupol_alianmor =  t_partidos_agrupados_en_coalicion_1,
    cultupol_alianprian =  t_partidos_agrupados_en_coalicion_2,
    cultupol_frentemor =  opositor_enfrenta_mejor,
    cultupol_votoalian =  voto_por_partido_presidente, # Pedir cambiar
    cultupol_segundaalian =  segunda_opcion_voto_alianzas,
    cultupol_rechazoalian =  partido_alianza_voto, # Pedir cambiar
    cultupol_expectalian = partido_alianza_ganara_pre_rep,
    evalgob_amlo = eval_gob_presi,
    # socio_progsoc = beneficiario_de_al_menos_1_programa_social,
    socio_educacion = socioeconomicos_escolaridad,
    cultupol_interno1 = t_internos_partidos_1, # Violencia mujeres
    cultupol_interno2 = t_internos_partidos_2, # Medio Ambiente
    cultupol_interno3 = t_internos_partidos_3, # Trabajar x los jóvenes
    cultupol_interno4 = t_internos_partidos_4, # Salud
    cultupol_interno5 = t_internos_partidos_5, # Economía familias
    cultupol_interno6 = t_internos_partidos_6, #Inseguridad
    cultupol_interno7 = t_internos_partidos_7, # Corrupción
    cultupol_interno8 = t_internos_partidos_8, # Educación
    # cultupol_interno9 = t_internos_partidos_1_9, # Algo nuevo
    cultupol_interno10 = t_internos_partidos_9, # Inversión
    # cultupol_interno11 = t_internos_partidos_1_11, # Apoyar a los emprendedores
    cultupol_interno12 = t_internos_partidos_10, # Reducir pobreza
    cultupol_interno13 = t_internos_partidos_11, # Trabajar con visión de futuro
    brujula_caminopais = cree_pais_va_camino_correcto_equivocado,
    brujula_temaspol1 = t_esta_afavor_o_encontra_de_las_frases_1,  # Militares
    brujula_temaspol2 = t_esta_afavor_o_encontra_de_las_frases_2,  # Marihuana
    brujula_temaspol3 = t_esta_afavor_o_encontra_de_las_frases_3,  # Matrimonio gay
    brujula_temaspol4 = t_esta_afavor_o_encontra_de_las_frases_4,  # Adopción gay
    brujula_temaspol5 = t_esta_afavor_o_encontra_de_las_frases_5,  # Carcel por abortar
    # brujula_temaspol6 = t_esta_afavor_o_encontra_de_las_frases_6,  # Discriminación
    # brujula_temaspol7 = t_esta_afavor_o_encontra_de_las_frases_7,  # Universidad
    brujula_temaspol8 = t_esta_afavor_o_encontra_de_las_frases_6,  # Inmigración
    # brujula_temaspol9 = t_esta_afavor_o_encontra_de_las_frases_9,  # Grupos criminales
    # brujula_temaspol10 = t_esta_afavor_o_encontra_de_las_frases_10,  # Meritocracia
    evalgob_tema1 = t_aprobacion_labor_amlo_1, # Niños con cancer
    # evalgob_tema2 = t_aprueba_desaprueba_acciones_presidente_mx2, # Medicamentos
    evalgob_tema3= t_aprobacion_labor_amlo_2, # Inseguridad
    evalgob_tema4 = t_aprobacion_labor_amlo_3, #Violencia mujeres
    # evalgob_tema5 = t_aprueba_desaprueba_acciones_presidente_mx7, # Desaparecidos
    evalgob_tema7 = t_aprobacion_labor_amlo_4, # Gasolina
    evalgob_tema8 = t_aprobacion_labor_amlo_5, # Pobreza
    evalgob_tema9 = t_aprobacion_labor_amlo_6, # Corrupción
    evalgob_tema10 = t_aprobacion_labor_amlo_7, # Migración
    evalgob_tema11 = t_aprobacion_labor_amlo_8, # Educación
    evalgob_tema12 = t_aprobacion_labor_amlo_9, # Servicios de salud y medicamentos
    elec_careo = care_presi_jamcsxg,
    elec_segop = voto_por_candidato_presidente,
    elec_rechazo = rechazo_candidato_jamcsxg,
    elec_expectativa = candidato_ganara_pre_rep,
    elec_interno_1 = t_interno_jamcsxg_1, # Está más preparada
    elec_interno_2 = t_interno_jamcsxg_2, # Tiene ideas nuevas
    elec_interno_3 = t_interno_jamcsxg_3, # Representa un cambio
    elec_interno_4 = t_interno_jamcsxg_4, # Es más líder
    elec_interno_5 = t_interno_jamcsxg_5, # Es más honesto
    elec_interno_6 = t_interno_jamcsxg_6, # Es más trabajador
    elec_interno_7 = t_interno_jamcsxg_7, # Es más cercano
    elec_interno_8 = t_interno_jamcsxg_8, # Miente más
    elec_interno_9 = t_interno_jamcsxg_9, # Es más corrupto
    elec_interno_10 = t_interno_jamcsxg_10, #Tiene más perfil de presidente
    ponderador = ponderador5_0,
    f_exp_nal = factor_de_expansion
  ) |> 
  select(1:25,
         contains("nombre_"),
         contains("cultupol_"), 
         contains("brujula_"),
         contains("socio_"),
         contains("evalgob_"),
         contains("elec_"),
         ponderador) |> 
  relocate(contains("cultupol_evalmarca"), .after = cultupol_evalmarca_prd) |> 
  mutate(across(.cols = c(cultupol_calenelect:socio_educacion), ~factor(.)),
         nse = fct_relevel(nse, c("A/B", "C+", "C", "C-", "D+", "D", "E")),
         socio_educacion = fct_recode(socio_educacion,
                                      "Bachillerato o símil" ="Preparatoria o carrera técnica o comercial"),
         across(.cols = contains("nombre_"),
                ~gsub("No lo conoce/NS/NC", "Ns/Nc", .)),
         across(.cols = contains("nombre_"), 
                ~factor(case_when(. == "Muy Bien" ~ "Positiva",
                                  . == "Bien" ~ "Positiva",
                                  . == "Mal" ~ "Negativa",
                                  . == "Muy Mal" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         across(.cols = contains("cultupol_evalmarca"), 
                ~factor(case_when(. == "Muy Buena" ~ "Positiva",
                                  . == "Buena" ~ "Positiva",
                                  . == "Mala" ~ "Negativa",
                                  . == "Muy Mala" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         generaciones = factor(
           case_when(
             (year(today()) - as.numeric(edad_abierta)) >= 1995 & (year(today()) - as.numeric(edad_abierta)) <= 2010 ~ "Generación Z (18-28)",
             (year(today()) - as.numeric(edad_abierta)) >= 1981 & (year(today()) - as.numeric(edad_abierta)) <= 1994 ~ "Millennials (29-42)",
             (year(today()) - as.numeric(edad_abierta)) >= 1965 & (year(today()) - as.numeric(edad_abierta)) <= 1980 ~ "Generación X (43-58)", 
             (year(today()) - as.numeric(edad_abierta)) >= 1956 & (year(today()) - as.numeric(edad_abierta)) <= 1964 ~ "Baby Boomers (59-67)", 
             .default = "Silenciosos (67 o +)"
           ),
           levels = c(
             "Generación Z (18-28)", 
             "Millennials (29-42)", 
             "Generación X (43-58)", 
             "Baby Boomers (59-67)",
             "Silenciosos (67 o +)"
           )),
         medicion = as.Date("2024-03-12"),
         elec_careo = fct_recode(elec_careo,
                             "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO" = "Jorge Álvarez Maynez + MOVIMIENTO CIUDADANO" )
         ) -> df_nal5


names(df_nal5)



# Sexta medición 

read_excel(str_c(path_microdatos, 
                 "Nacional 6 Abril 1200.xlsx")) |>
  clean_names()|>
  rename_with(.cols = contains("nacional"), ~gsub("_?nacional_?", "", .)) |> 
  rename(
    zona_euzen = zona_pais,
    gps_lat = gps_inicial_la,
    gps_lon = gps_inicial_lo, 
    nombre_amlo = t_per_nom_amlo,
    nombre_cs = t_per_nom_cs,
    nombre_sg = t_per_nom_sg,
    nombre_mr = t_per_nom_mr,
    nombre_dd = t_per_nom_dd,
    nombre_xg = t_per_nom_xg,
    nombre_jam = t_per_nom_jam,
    cultupol_calenelect =  conocimiento_elecciones_presidenciales,
    cultupol_simpatia =  identificacion_partidista,
    cultupol_evalmarca_pan = t_evaluacion_imagen_partidos_1,
    cultupol_evalmarca_pri = t_evaluacion_imagen_partidos_2,
    cultupol_evalmarca_prd =  t_evaluacion_imagen_partidos_10,
    cultupol_evalmarca_mc =  t_evaluacion_imagen_partidos_3,
    cultupol_evalmarca_mor =  t_evaluacion_imagen_partidos_4,
    cultupol_evalmarca_pvem =  t_evaluacion_imagen_partidos_5,
    cultupol_evalmarca_pt =  t_evaluacion_imagen_partidos_6,
    cultupol_techo_mc = voto_para_presidencia_mc,
    cultupol_alianmor =  t_partidos_agrupados_en_coalicion_1,
    cultupol_alianprian =  t_partidos_agrupados_en_coalicion_2,
    cultupol_frentemor =  opositor_enfrenta_mejor,
    cultupol_votoalian =  voto_por_partido_presidente, 
    cultupol_segundaalian =  segunda_opcion_voto_alianzas,
    cultupol_esquirol =  frase_acerca_mas_postura_mc,
    evalgob_amlo = eval_gob_presi,
    socio_educacion = socioeconomicos_escolaridad,
    evalgob_tema1 = t_aprobacion_labor_amlo_1, # Niños con cancer
    evalgob_tema3= t_aprobacion_labor_amlo_2, # Inseguridad
    evalgob_tema4 = t_aprobacion_labor_amlo_3, #Violencia mujeres
    evalgob_tema7 = t_aprobacion_labor_amlo_4, # Gasolina
    evalgob_tema8 = t_aprobacion_labor_amlo_5, # Pobreza
    evalgob_tema9 = t_aprobacion_labor_amlo_6, # Corrupción
    evalgob_tema10 = t_aprobacion_labor_amlo_7, # Migración
    evalgob_tema11 = t_aprobacion_labor_amlo_8, # Educación
    evalgob_tema12 = t_aprobacion_labor_amlo_9, # Servicios de salud y medicamentos
    elec_careo = care_presi_jamcsxg,
    elec_segop = voto_por_candidato_presidente,
    elec_rechazo = rechazo_candidato_jamcsxg,
    elec_expectativa = candidato_ganara_pre_rep,
    elec_definida = eleccionesta_definida,
    elec_interno_1 = t_interno_jamcsxg_1, # Está más preparada
    elec_interno_2 = t_interno_jamcsxg_2, # Tiene ideas nuevas
    elec_interno_3 = t_interno_jamcsxg_3, # Representa un cambio
    elec_interno_4 = t_interno_jamcsxg_4, # Es más líder
    elec_interno_5 = t_interno_jamcsxg_5, # Es más honesto
    elec_interno_6 = t_interno_jamcsxg_6, # Es más trabajador
    elec_interno_7 = t_interno_jamcsxg_7, # Es más cercano
    elec_interno_8 = t_interno_jamcsxg_8, # Miente más
    elec_interno_9 = t_interno_jamcsxg_9, # Es más corrupto
    elec_interno_10 = t_interno_jamcsxg_10, #Tiene más perfil de presidente
    brujula_apoyaria1 = t_esta_afavor_o_encontra_de_las_frases_1,  # Militares
    brujula_apoyaria2 = t_esta_afavor_o_encontra_de_las_frases_2,  # Marihuana
    brujula_apoyaria3 = t_esta_afavor_o_encontra_de_las_frases_3,  # Matrimonio gay
    brujula_apoyaria4 = t_esta_afavor_o_encontra_de_las_frases_4,  # Adopción gay
    brujula_apoyaria5 = t_esta_afavor_o_encontra_de_las_frases_5,  # Interrupción embarazo
    brujula_apoyaria6 = t_esta_afavor_o_encontra_de_las_frases_6,  # Más cárceles
    brujula_apoyaria7 = t_esta_afavor_o_encontra_de_las_frases_7,  # Legalización de todas las drogas
    brujula_apoyaria8 = t_esta_afavor_o_encontra_de_las_frases_8,  # Refinerias
    ponderador = ponderador6
    # f_exp_nal = factor_de_expansion
  ) |> 
  select(1:25, 
         contains("nombre_"),
         contains("cultupol_"), 
         contains("brujula_"),
         contains("socio_"),
         contains("evalgob_"),
         contains("elec_"),
         ponderador) |> 
  relocate(contains("cultupol_evalmarca"), .after = cultupol_evalmarca_prd) |> 
  mutate(across(.cols = c(cultupol_calenelect:elec_interno_10), ~factor(.)),
         nse = fct_relevel(nse, c("A/B", "C+", "C", "C-", "D+", "D", "E")),
         socio_educacion = fct_recode(socio_educacion,
                                      "Bachillerato o símil" ="Preparatoria o carrera técnica o comercial"),
         across(.cols = contains("nombre_"),
                ~gsub("No lo conoce/NS/NC", "Ns/Nc", .)),
         across(.cols = contains("nombre_"), 
                ~factor(case_when(. == "Muy Bien" ~ "Positiva",
                                  . == "Bien" ~ "Positiva",
                                  . == "Mal" ~ "Negativa",
                                  . == "Muy Mal" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         across(.cols = contains("cultupol_evalmarca"), 
                ~factor(case_when(. == "Muy Buena" ~ "Positiva",
                                  . == "Buena" ~ "Positiva",
                                  . == "Mala" ~ "Negativa",
                                  . == "Muy Mala" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         generaciones = factor(
           case_when(
             (year(today()) - as.numeric(edad_abierta)) >= 1995 & (year(today()) - as.numeric(edad_abierta)) <= 2010 ~ "Generación Z (18-28)",
             (year(today()) - as.numeric(edad_abierta)) >= 1981 & (year(today()) - as.numeric(edad_abierta)) <= 1994 ~ "Millennials (29-42)",
             (year(today()) - as.numeric(edad_abierta)) >= 1965 & (year(today()) - as.numeric(edad_abierta)) <= 1980 ~ "Generación X (43-58)", 
             (year(today()) - as.numeric(edad_abierta)) >= 1956 & (year(today()) - as.numeric(edad_abierta)) <= 1964 ~ "Baby Boomers (59-67)", 
             .default = "Silenciosos (67 o +)"
           ),
           levels = c(
             "Generación Z (18-28)", 
             "Millennials (29-42)", 
             "Generación X (43-58)", 
             "Baby Boomers (59-67)",
             "Silenciosos (67 o +)"
           )),
         medicion = as.Date("2024-04-04"),
         elec_careo = case_when(
                            str_detect(elec_careo, "Claudia") ~ "Claudia Sheinbaum + MORENA-PT-VERDE", 
                            str_detect(elec_careo, "Jorge") ~ "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO", 
                            str_detect(elec_careo, "Xóchitl") ~ "Xóchitl Gálvez + PAN-PRI-PRD", 
                            .default = "Ns/Nc")
  ) -> df_nal6


names(df_nal6)


# Séptima medición 
read_excel(str_c(path_microdatos, 
                 "Estudio Nacional 7.0 - 21 Abril 2024 EUZEN.xlsx")) |>
  clean_names()|> 
  rename_with(.cols = contains("nacional"), ~gsub("_?nacional_?", "", .)) |> 
  rename(
    zona_euzen = zona_pais,
    gps_lat = gps_inicial_la,
    gps_lon = gps_inicial_lo, 
    nombre_amlo = t_per_nom_amlo,
    nombre_cs = t_per_nom_cs,
    nombre_sg = t_per_nom_sg,
    nombre_mr = t_per_nom_mr,
    nombre_dd = t_per_nom_dd,
    nombre_xg = t_per_nom_xg,
    nombre_jam = t_per_nom_jam,
    cultupol_calenelect =  conocimiento_elecciones_presidenciales,
    cultupol_simpatia =  identificacion_partidista,
    cultupol_evalmarca_pan = t_evaluacion_imagen_partidos_1,
    cultupol_evalmarca_pri = t_evaluacion_imagen_partidos_2,
    cultupol_evalmarca_prd =  t_evaluacion_imagen_partidos_10,
    cultupol_evalmarca_mc =  t_evaluacion_imagen_partidos_3,
    cultupol_evalmarca_mor =  t_evaluacion_imagen_partidos_4,
    cultupol_evalmarca_pvem =  t_evaluacion_imagen_partidos_5,
    cultupol_evalmarca_pt =  t_evaluacion_imagen_partidos_6,
    cultupol_techo_mc = voto_para_presidencia_mc,
    cultupol_alianmor =  t_partidos_agrupados_en_coalicion_1,
    cultupol_alianprian =  t_partidos_agrupados_en_coalicion_2,
    cultupol_frentemor =  opositor_enfrenta_mejor,
    cultupol_votoalian =  voto_por_partido_presidente, # Pedir cambiar
    cultupol_segundaalian =  segunda_opcion_voto_alianzas,
    cultupol_esquirol = frase_acerca_mas_postura_mc,
    evalgob_amlo = eval_gob_presi,
    socio_educacion = socioeconomicos_escolaridad,
    evalgob_tema1 = t_aprobacion_labor_amlo_1, # Niños con cancer
    evalgob_tema3= t_aprobacion_labor_amlo_1, # Inseguridad
    evalgob_tema4 = t_aprobacion_labor_amlo_2, #Violencia mujeres
    evalgob_tema7 = t_aprobacion_labor_amlo_3, # Gasolina
    evalgob_tema8 = t_aprobacion_labor_amlo_4, # Pobreza
    evalgob_tema9 = t_aprobacion_labor_amlo_5, # Corrupción
    evalgob_tema10 = t_aprobacion_labor_amlo_6, # Migración
    evalgob_tema11 = t_aprobacion_labor_amlo_7, # Educación
    evalgob_tema12 = t_aprobacion_labor_amlo_8, # Servicios de salud y medicamentos
    elec_careo = care_presi_mx_jamcsxg, #care_presi_jamcsxg -> sin boleta
    elec_segop = voto_por_candidato_presidente,
    elec_rechazo = rechazo_candidato_jamcsxg,
    elec_expectativa = candidato_ganara_pre_rep,
    elec_definida = eleccionesta_definida,
    elec_interno_1 = t_interno_jamcsxg_1, # Está más preparada
    elec_interno_2 = t_interno_jamcsxg_2, # Tiene ideas nuevas
    elec_interno_4 = t_interno_jamcsxg_3, # Es más líder
    elec_interno_5 = t_interno_jamcsxg_4, # Es más honesto
    elec_interno_6 = t_interno_jamcsxg_5, # Es más trabajador
    elec_interno_7 = t_interno_jamcsxg_6, # Es más cercano
    elec_interno_9 = t_interno_jamcsxg_7, # Es más corrupto
    elec_interno_11 = t_interno_jamcsxg_8, # Administra mejor
    elec_interno_12 = t_interno_jamcsxg_9, # Combatirá corrupción 
    ponderador = ponderador_nl
  ) |> 
  select(1:25, 
         contains("nombre_"),
         contains("cultupol_"), 
         contains("brujula_"),
         contains("socio_"),
         contains("evalgob_"),
         contains("elec_"),
         ponderador) |> 
  relocate(contains("cultupol_evalmarca"), .after = cultupol_evalmarca_prd) |> 
  mutate(across(.cols = c(cultupol_calenelect:socio_educacion), ~factor(.)),
         nse = fct_relevel(nse, c("A/B", "C+", "C", "C-", "D+", "D", "E")),
         socio_educacion = fct_recode(socio_educacion,
                                      "Bachillerato o símil" ="Preparatoria o carrera técnica o comercial"),
         across(.cols = contains("nombre_"),
                ~gsub("No lo conoce/NS/NC", "Ns/Nc", .)),
         across(.cols = contains("nombre_"), 
                ~factor(case_when(. == "Muy Bien" ~ "Positiva",
                                  . == "Bien" ~ "Positiva",
                                  . == "Mal" ~ "Negativa",
                                  . == "Muy Mal" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         across(.cols = contains("cultupol_evalmarca"), 
                ~factor(case_when(. == "Muy Buena" ~ "Positiva",
                                  . == "Buena" ~ "Positiva",
                                  . == "Mala" ~ "Negativa",
                                  . == "Muy Mala" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         generaciones = factor(
           case_when(
             (year(today()) - as.numeric(edad_abierta)) >= 1995 & (year(today()) - as.numeric(edad_abierta)) <= 2010 ~ "Generación Z (18-28)",
             (year(today()) - as.numeric(edad_abierta)) >= 1981 & (year(today()) - as.numeric(edad_abierta)) <= 1994 ~ "Millennials (29-42)",
             (year(today()) - as.numeric(edad_abierta)) >= 1965 & (year(today()) - as.numeric(edad_abierta)) <= 1980 ~ "Generación X (43-58)", 
             (year(today()) - as.numeric(edad_abierta)) >= 1956 & (year(today()) - as.numeric(edad_abierta)) <= 1964 ~ "Baby Boomers (59-67)", 
             .default = "Silenciosos (67 o +)"
           ),
           levels = c(
             "Generación Z (18-28)", 
             "Millennials (29-42)", 
             "Generación X (43-58)", 
             "Baby Boomers (59-67)",
             "Silenciosos (67 o +)"
           )),
         medicion =as.Date("2024-04-20"),
         elec_careo = case_when(
           str_detect(elec_careo, "Claudia") ~ "Claudia Sheinbaum + MORENA-PT-VERDE", 
           str_detect(elec_careo, "Jorge") ~ "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO", 
           str_detect(elec_careo, "Xóchitl") ~ "Xóchitl Gálvez + PAN-PRI-PRD", 
           .default = "Ns/Nc")
  ) -> df_nal7

names(df_nal7)


# Octava medición 

read_excel(str_c(path_microdatos,
  "Nacional 8 Mayo EUZEN.xlsx")) |>
  clean_names() |> 
  rename_with(.cols = contains("nacional"), ~gsub("_?nacional_?", "", .)) |> 
  rename(
    zona_euzen = zona_pais,
    gps_lat = gps_inicial_la,
    gps_lon = gps_inicial_lo, 
    nombre_amlo = t_per_nom_amlo,
    # nombre_me = evaluacion_personalidades_me,
    nombre_cs = t_per_nom_cs,
    nombre_sg = t_per_nom_sg,
    nombre_ldc = t_per_nom_ldc,
    nombre_mr = t_per_nom_mr,
    nombre_dd = t_per_nom_dd,
    nombre_xg = t_per_nom_xg,
    nombre_jam = t_per_nom_jam,
    # nombre_ev = evaluacion_personalidades_ev,
    cultupol_calenelect =  conocimiento_elecciones_presidenciales,
    cultupol_simpatia =  identificacion_partidista,
    cultupol_evalmarca_pan = t_evaluacion_imagen_partidos_1,
    cultupol_evalmarca_pri = t_evaluacion_imagen_partidos_2,
    cultupol_evalmarca_prd =  t_evaluacion_imagen_partidos_10,
    cultupol_evalmarca_mc =  t_evaluacion_imagen_partidos_3,
    cultupol_evalmarca_mor =  t_evaluacion_imagen_partidos_4,
    cultupol_evalmarca_pvem =  t_evaluacion_imagen_partidos_5,
    cultupol_evalmarca_pt =  t_evaluacion_imagen_partidos_6,
    cultupol_techo_mc = voto_para_presidencia_mc,
    cultupol_techo_mor =  voto_presidencia_morena,
    cultupol_alianmor =  t_partidos_agrupados_en_coalicion_1,
    cultupol_alianprian =  t_partidos_agrupados_en_coalicion_2,
    cultupol_frentemor =  opositor_enfrenta_mejor,
    cultupol_votoalian =  voto_por_partido_presidente, # Pedir cambiar
    cultupol_segundaalian =  segunda_opcion_voto_alianzas,
    socio_educacion = socioeconomicos_escolaridad,
    elec_careo = care_presi_mx_jamcsxg, #care_presi_jamcsxg -> sin boleta
    elec_segop = voto_para_senado,
    elec_rechazo = rechazo_candidato_jamcsxg,
    elec_expectativa = candidato_ganara_pre_rep,
    elec_definida = elecciones_estan_definidas,
    elec_interno_1 = t_interno_jamcsxg_1, # Está más preparada
    elec_interno_2 = t_interno_jamcsxg_2, # Tiene ideas nuevas
    elec_interno_4 = t_interno_jamcsxg_3, # Es más líder
    elec_interno_5 = t_interno_jamcsxg_4, # Es más honesto
    elec_interno_6 = t_interno_jamcsxg_5, # Es más trabajador
    elec_interno_7 = t_interno_jamcsxg_6, # Es más cercano
    elec_interno_9 = t_interno_jamcsxg_7, # Es más corrupto
    elec_interno_11 = t_interno_jamcsxg_8, # Administra mejor
    elec_interno_12 = t_interno_jamcsxg_9, # Combatirá corrupción 
    ponderador = ponderador_nl
  ) |> 
  select(1:25, 
         contains("nombre_"),
         contains("cultupol_"), 
         contains("brujula_"),
         contains("socio_"),
         contains("evalgob_"),
         contains("elec_"),
         ponderador) |> 
  relocate(contains("cultupol_evalmarca"), .after = cultupol_evalmarca_prd) |> 
  mutate(across(.cols = c(cultupol_calenelect:socio_educacion), ~factor(.)),
         nse = fct_relevel(nse, c("A/B", "C+", "C", "C-", "D+", "D", "E")),
         socio_educacion = fct_recode(socio_educacion,
                                      "Bachillerato o símil" ="Preparatoria o carrera técnica o comercial"),
         across(.cols = contains("nombre_"),
                ~gsub("No lo conoce/NS/NC", "Ns/Nc", .)),
         across(.cols = contains("nombre_"), 
                ~factor(case_when(. == "Muy Bien" ~ "Positiva",
                                  . == "Bien" ~ "Positiva",
                                  . == "Mal" ~ "Negativa",
                                  . == "Muy Mal" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         across(.cols = contains("cultupol_evalmarca"), 
                ~factor(case_when(. == "Muy Buena" ~ "Positiva",
                                  . == "Buena" ~ "Positiva",
                                  . == "Mala" ~ "Negativa",
                                  . == "Muy Mala" ~ "Negativa",
                                  T ~ "Ns/Nc"),
                        levels = c("Positiva", "Ns/Nc", "Negativa"))),
         generaciones = factor(
           case_when(
             (year(today()) - as.numeric(edad_abierta)) >= 1995 & (year(today()) - as.numeric(edad_abierta)) <= 2010 ~ "Generación Z (18-28)",
             (year(today()) - as.numeric(edad_abierta)) >= 1981 & (year(today()) - as.numeric(edad_abierta)) <= 1994 ~ "Millennials (29-42)",
             (year(today()) - as.numeric(edad_abierta)) >= 1965 & (year(today()) - as.numeric(edad_abierta)) <= 1980 ~ "Generación X (43-58)", 
             (year(today()) - as.numeric(edad_abierta)) >= 1956 & (year(today()) - as.numeric(edad_abierta)) <= 1964 ~ "Baby Boomers (59-67)", 
             .default = "Silenciosos (67 o +)"
           ),
           levels = c(
             "Generación Z (18-28)", 
             "Millennials (29-42)", 
             "Generación X (43-58)", 
             "Baby Boomers (59-67)",
             "Silenciosos (67 o +)"
           )),
         medicion =as.Date("2024-05-09"),
         elec_careo = case_when(
           str_detect(elec_careo, "Claudia") ~ "Claudia Sheinbaum + MORENA-PT-VERDE", 
           str_detect(elec_careo, "Jorge") ~ "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO", 
           str_detect(elec_careo, "Xóchitl") ~ "Xóchitl Gálvez + PAN-PRI-PRD", 
           .default = "Ns/Nc"),
        elec_segop = case_when(
          str_detect(elec_segop, "Claudia") ~ "Claudia Sheinbaum + MORENA-PT-VERDE", 
          str_detect(elec_segop, "Jorge") ~ "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO", 
          str_detect(elec_segop, "Xóchitl") ~ "Xóchitl Gálvez + PAN-PRI-PRD", 
          .default = "Ns/Nc")
  ) -> df_nal8


names(df_nal8)


# Novena medición 

df_nal9 <- read_excel(str_c(path_microdatos,
  "Nacional 24 de mayo.xlsx")) |>
  clean_names() |>
  rename_with(.cols = contains("nacional"), ~gsub("_?nacional_?", "", .)) |> 
  rename(
    zona_euzen = zona_pais,
    gps_lat = gps_inicial_la,
    gps_lon = gps_inicial_lo, 
    nombre_amlo = t_per_nom_amlo,
    nombre_cs = t_per_nom_cs,
    nombre_dd = t_per_nom_dd,
    nombre_xg = t_per_nom_xg,
    nombre_jam = t_per_nom_jam,
    cultupol_calenelect =  conocimiento_elecciones_presidenciales,
    cultupol_simpatia =  identificacion_partidista,
    cultupol_evalmarca_pan = t_evaluacion_imagen_partidos_1,
    cultupol_evalmarca_pri = t_evaluacion_imagen_partidos_2,
    cultupol_evalmarca_prd =  t_evaluacion_imagen_partidos_10,
    cultupol_evalmarca_mc =  t_evaluacion_imagen_partidos_3,
    cultupol_evalmarca_mor =  t_evaluacion_imagen_partidos_4,
    cultupol_evalmarca_pvem =  t_evaluacion_imagen_partidos_5,
    cultupol_evalmarca_pt =  t_evaluacion_imagen_partidos_6,
    cultupol_techo_mc = voto_para_presidencia_mc,
    cultupol_techo_mor =  voto_presidencia_morena,
    cultupol_alianmor =  t_partidos_agrupados_en_coalicion_1,
    cultupol_alianprian =  t_partidos_agrupados_en_coalicion_2,
    cultupol_frentemor =  mejor_opositor,
    cultupol_votoalian =  voto_por_partido_presidente, # Pedir cambiar
    socio_educacion = socioeconomicos_escolaridad,
    elec_careo = careo_tradicional_presidente_mx_jamcsxg, #care_presi_jamcsxg -> sin boleta
    elec_segop = segunda_opcion_voto_presidente_tradicional,
    elec_rechazo = rechazo_candidato_jamcsxg,
    elec_expectativa = candidato_ganara_pre_rep,
    elec_definida = percepcion_definicion_eleccion,
    elec_interno_1 = t_interno_jamcsxg_1, # Está más preparado
    elec_interno_2 = t_interno_jamcsxg_2, # Tiene ideas nuevas
    elec_interno_4 = t_interno_jamcsxg_3, # Es más líder
    elec_interno_5 = t_interno_jamcsxg_4, # Es más honesto
    elec_interno_6 = t_interno_jamcsxg_5, # Es más trabajador
    elec_interno_7 = t_interno_jamcsxg_6, # Es más cercano
    elec_interno_9 = t_interno_jamcsxg_7, # Es más corrupto
    elec_interno_11 = t_interno_jamcsxg_8, # Administra mejor
    elec_interno_12 = t_interno_jamcsxg_9, # Combatirá corrupción 
    ponderador = ponderador_nl
  ) |> 
  select(1:25, 
    contains("nombre_"),
    contains("cultupol_"), 
    contains("brujula_"),
    contains("socio_"),
    contains("evalgob_"),
    contains("elec_"),
    ponderador) |> 
  relocate(contains("cultupol_evalmarca"), .after = cultupol_evalmarca_prd) |> 
  mutate(across(.cols = c(cultupol_calenelect:socio_educacion), ~factor(.)),
    nse = fct_relevel(nse, c("A/B", "C+", "C", "C-", "D+", "D", "E")),
    socio_educacion = fct_recode(socio_educacion,
      "Bachillerato o símil" ="Preparatoria o carrera técnica o comercial"),
    across(.cols = contains("nombre_"),
      ~gsub("No lo conoce/NS/NC", "Ns/Nc", .)),
    across(.cols = contains("nombre_"), 
      ~factor(case_when(. == "Muy Bien" ~ "Positiva",
        . == "Bien" ~ "Positiva",
        . == "Mal" ~ "Negativa",
        . == "Muy Mal" ~ "Negativa",
        T ~ "Ns/Nc"),
        levels = c("Positiva", "Ns/Nc", "Negativa"))),
    across(.cols = contains("cultupol_evalmarca"), 
      ~factor(case_when(. == "Muy Buena" ~ "Positiva",
        . == "Buena" ~ "Positiva",
        . == "Mala" ~ "Negativa",
        . == "Muy Mala" ~ "Negativa",
        T ~ "Ns/Nc"),
        levels = c("Positiva", "Ns/Nc", "Negativa"))),
    generaciones = factor(
      case_when(
        (year(today()) - as.numeric(edad_abierta)) >= 1995 & (year(today()) - as.numeric(edad_abierta)) <= 2010 ~ "Generación Z (18-28)",
        (year(today()) - as.numeric(edad_abierta)) >= 1981 & (year(today()) - as.numeric(edad_abierta)) <= 1994 ~ "Millennials (29-42)",
        (year(today()) - as.numeric(edad_abierta)) >= 1965 & (year(today()) - as.numeric(edad_abierta)) <= 1980 ~ "Generación X (43-58)", 
        (year(today()) - as.numeric(edad_abierta)) >= 1956 & (year(today()) - as.numeric(edad_abierta)) <= 1964 ~ "Baby Boomers (59-67)", 
        .default = "Silenciosos (67 o +)"
      ),
      levels = c(
        "Generación Z (18-28)", 
        "Millennials (29-42)", 
        "Generación X (43-58)", 
        "Baby Boomers (59-67)",
        "Silenciosos (67 o +)"
      )),
    medicion =as.Date("2024-05-24"),
    elec_careo = case_when(
      str_detect(elec_careo, "Claudia") ~ "Claudia Sheinbaum + MORENA-PT-VERDE", 
      str_detect(elec_careo, "Jorge") ~ "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO", 
      str_detect(elec_careo, "Xóchitl") ~ "Xóchitl Gálvez + PAN-PRI-PRD", 
      .default = "Ns/Nc"),
    elec_segop = case_when(
      str_detect(elec_segop, "Claudia") ~ "Claudia Sheinbaum + MORENA-PT-VERDE", 
      str_detect(elec_segop, "Jorge") ~ "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO", 
      str_detect(elec_segop, "Xóchitl") ~ "Xóchitl Gálvez + PAN-PRI-PRD", 
      .default = "Ns/Nc")
  ) -> df_nal9


names(df_nal9)

df_nal1 |> 
  plyr::rbind.fill(df_nal2) |> 
  plyr::rbind.fill(df_nal3) |> 
  plyr::rbind.fill(df_nal4) |> 
  plyr::rbind.fill(df_nal5) |> 
  plyr::rbind.fill(df_nal6) |> 
  plyr::rbind.fill(df_nal7) |> 
  plyr::rbind.fill(df_nal8) |> 
  plyr::rbind.fill(df_nal9) |> 
  mutate(ponderador = as.numeric(ponderador),
         medicion_f =  format(medicion, format = "%d-%m-%y"),
         medicion = factor(medicion_f, levels = unique(medicion_f[order(medicion)])),
  )-> df_acum


rm(list=ls()[grep("df_nal", ls())])

saveRDS(df_acum, paste0(path_output, "DF - Nacional/df_acum.rds"))

