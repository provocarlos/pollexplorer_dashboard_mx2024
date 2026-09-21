# Setup----
pacman::p_load(tidyverse, scales, purrr, lubridate, janitor, zoo, googlesheets4, 
               stringr, survey, srvyr, readxl, sysfonts, rlang,
               shiny, shinyWidgets, showtext)


Sys.setlocale(locale = "es_ES.UTF-8")

path_output <- "www/Microdatos/DF - Estados/"
path_microdatos <- "~/Downloads/"

lookup <- c(
  "nombre_amlo",
  "nombre_me",
  "nombre_cs",
  "nombre_sg",
  "nombre_ldc",
  "nombre_mr",
  "nombre_dd",
  "nombre_xg",
  "nombre_jam",
  "nombre_ev",
  "cultupol_calenelect",
  "cultupol_simpatia",
  "cultupol_evalmarca_pan",
  "cultupol_evalmarca_pri",
  "cultupol_evalmarca_prd",
  "cultupol_evalmarca_mc",
  "cultupol_evalmarca_mor",
  "cultupol_evalmarca_pvem",
  "cultupol_evalmarca_pt",
  "cultupol_rechazomarca",
  "cultupol_votomarca",
  "cultupol_segundamarca",
  "cultupol_mejormexico",
  "cultupol_peormexico",
  "cultupol_techo_mc",
  "cultupol_techo_pri",
  "cultupol_techo_mor",
  "cultupol_techo_pan",
  "cultupol_alianmor",
  "cultupol_alianprian",
  "cultupol_frentemor",
  "cultupol_votoalian",
  "cultupol_segundaalian",
  "cultupol_rechazoalian",
  "cultupol_expectalian",
  "candi_cs_techo",
  "candi_xg_techo",
  "candi_sg_techo",
  "careo1", "careo2", "careo3",
  "candi_rechazo",
  "evalgob_amlo",
  "brujula_caminopais",
  "brujula_economia",
  "brujula_temaspol1",
  "brujula_temaspol2",
  "brujula_temaspol3",
  "brujula_temaspol4",
  "brujula_temaspol5",
  "brujula_temaspol6",
  "socio_progsoc",
  "socio_educacion",
  "ponderador",
  "f_exp_nal"
)


# Primer medición ----

read_excel(paste0(path_microdatos, 
                  "ESTUDIO NACIONAL EUZEN_SM_PONDERADORES.xlsx"), guess_max = 15000) |>
  clean_names()|> 
  rename_with(.cols = contains("nacional"), ~gsub("_?nacional_?", "", .)) |> 
  rename_with(~str_replace(., "partidosl", "partidos")) |> 
  filter(tipo_muestra != "NACIONAL") |> 
  rename(
    nombre_amlo = t_personalidadesandres_manuel_lopez_obrador,
    nombre_me = t_personalidadesmarcelo_ebrard,
    nombre_cs = t_personalidadesclaudia_sheinbaum,
    nombre_sg = t_personalidadessamuel_garcia,
    nombre_ldc = t_personalidadesluis_donaldo_colosio,
    nombre_mr = t_personalidadesmariana_rodriguez,
    nombre_dd = t_personalidadesdante_delgado,
    nombre_xg = t_personalidadesxochitl_galvez,
    nombre_jam = t_personalidades2_jorge_alvarez_maynez,
    cultupol_calenelect =  conocimiento_elecciones_presidenciales,
    cultupol_simpatia =  simpatia_partidista,
    cultupol_evalmarca_pan = t_evaluacion_imagen_partidos_1,
    cultupol_evalmarca_pri = t_evaluacion_imagen_partidos_2,
    cultupol_evalmarca_prd =  t_evaluacion_imagen_partidos_3,
    cultupol_evalmarca_mc =  t_evaluacion_imagen_partidos_4,
    cultupol_evalmarca_mor =  t_evaluacion_imagen_partidos_5,
    cultupol_evalmarca_pvem =  t_evaluacion_imagen_partidos_8,
    cultupol_evalmarca_pt =  t_evaluacion_imagen_partidos_7,
    cultupol_rechazomarca =  rechazo_partidista,
    cultupol_votomarca =  intencion_votox_partido_presidencia,
    cultupol_mejormexico =  lo_mejorpodriapasarle_mexico,
    cultupol_peormexico =  lo_peorpodriapasarle_mexico,
    cultupol_techo_mc = viabilidad_voto_presidencia_mc,
    cultupol_techo_pri = viabilidad_voto_presidencia_pri,
    cultupol_techo_mor =  viabilidad_voto_presidencia_morena,
    cultupol_techo_pan = viabilidad_voto_presidencia_pan,
    cultupol_alianmor =  t_imagen_alianzas1,
    cultupol_alianprian =  t_imagen_alianzas2,
    cultupol_frentemor =  opositor_enfrenta_mejor,
    cultupol_votoalian =  intencion_voto_alianza_presidencial,
    cultupol_expectalian = percepcion_triunfo_alianza_presidencial,
    evalgob_amlo = calificacion_labor_presidente_mx,
    brujula_caminopais = buen_caminomal_camino,
    brujula_economia = situacion_econimicafamiliar,
    brujula_temaspol1 = t_opinion_polarizada_favor1,  # Militares
    brujula_temaspol2 = t_opinion_polarizada_favor2,  # Marihuana
    brujula_temaspol3 = t_opinion_polarizada_favor3,  # Matrimonio gay
    brujula_temaspol4 = t_opinion_polarizada_favor4,  # Adopción gay
    brujula_temaspol5 = t_opinion_polarizada_favor5,  # Carcel por abortar
    brujula_temaspol6 = t_opinion_polarizada_favor6,  # Discriminación
    socio_progsoc = beneficiario_de_al_menos_1_programa_social,
    socio_educacion = socioeconomicos_escolaridad,
    cultupol_interno1 = t_internos_partidos1, # Violencia mujeres
    cultupol_interno2 = t_internos_partidos2, # Medio Ambiente
    cultupol_interno3 = t_internos_partidos3, # Trabajar x los jóvenes
    cultupol_interno4 = t_internos_partidos6, # Salud
    cultupol_interno5 = t_internos_partidos10, # Economia familiar
    cultupol_interno6 = t_internos_partidos12, #Inseguridad
    cultupol_interno7 = t_internos_partidos9, # Corrupción
    cultupol_interno8 = t_internos_partidos13, # Educación
    evalgob_tema1 = t_aprueba_desaprueba_acciones_presidente_mx1, # Niños con cancer
    evalgob_tema2 = t_aprueba_desaprueba_acciones_presidente_mx2, # Medicamentos
    evalgob_tema3= t_aprueba_desaprueba_acciones_presidente_mx3, # Inseguridad
    evalgob_tema4 = t_aprueba_desaprueba_acciones_presidente_mx5, #Violencia mujeres
    evalgob_tema5 = t_aprueba_desaprueba_acciones_presidente_mx7, # Desaparecidos
    evalgob_tema7 = t_aprueba_desaprueba_acciones_presidente_mx9, # Gasolina
    evalgob_tema8 = t_aprueba_desaprueba_acciones_presidente_mx10, # Pobreza
    evalgob_tema9 = t_aprueba_desaprueba_acciones_presidente_mx12 # Corrupción
    
  ) |> 
  select(1:26, any_of(lookup),
         contains("cultupol_interno"), 
         contains("evalgob_tema")) |> 
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
    medicion = as.yearmon(as.Date("2023-06-01"))
  ) |> 
  select(-tipo_muestra) -> df_mayo23


df_mayo23 |> 
  mutate(ponderador = as.numeric(ponderador)) -> df_edos

saveRDS(df_edos, paste0(path_output, "df_edos.rds"))


