pacman::p_load(tidyverse)

#Personajes----
load("www/Microdatos/DF - Nacional/df_perso.Rda")
listaLevantamiento <- unique(df_perso$medicion)
perso_nombre <- df_perso |> 
  mutate(personaje = case_match(personaje, 
    "nombre_amlo" ~ "Andrés Manuel López Obrador",
    "nombre_cs" ~ "Claudia Sheinbaum",
    "nombre_xg" ~ "Xóchitl Gálvez",
    "nombre_jam" ~ "Jorge Álvarez Máynez",
    "nombre_sg" ~ "Samuel García",
    "nombre_dd" ~ "Dante Delgado",
    "nombre_mr" ~ "Mariana Rodríguez", 
    "nombre_ldc" ~ "Luis Donaldo Colosio",
    "nombre_me" ~ "Marcelo Ebrard"
  ))

load("www/Microdatos/DF - Nacional/df_perso_2025.Rda")

perso_nombre_2025 <- df_perso_2025 |> 
  mutate(personaje = case_match(personaje,
    "T_OpinionPersonajes_AMLO" ~ "Andrés Manuel López Obrador",
    "T_OpinionPersonajes_CS" ~ "Claudia Sheinbaum",
    "T_OpinionPersonajes_XG" ~ "Xóchitl Gálvez",
    "T_OpinionPersonajes_JAM" ~ "Jorge Álvarez Máynez",
    "T_OpinionPersonajes_SG" ~ "Samuel García",
    "T_OpinionPersonajes_MR" ~ "Mariana Rodríguez",
    "T_OpinionPersonajes_LDC" ~ "Luis Donaldo Colosio",
    "T_OpinionPersonajes_ME" ~ "Marcelo Ebrard"
  ))

perso_nombre <- perso_nombre |> 
  bind_rows(perso_nombre_2025) |> 
  mutate(
    Color = case_when(
      personaje == "Andrés Manuel López Obrador" ~ "#761d01",
      personaje == "Claudia Sheinbaum" ~ "#761d01",
      personaje == "Xóchitl Gálvez" ~ "#0062ff",
      personaje == "Jorge Álvarez Máynez" ~ "#f77b09",
      personaje == "Samuel García" ~ "#f77b09",
      personaje == "Dante Delgado" ~ "#f77b09",
      personaje == "Mariana Rodríguez" ~ "#f77b09",
      personaje == "Luis Donaldo Colosio" ~ "#f77b09",
      personaje == "Marcelo Ebrard" ~ "#761d01"
    )
  )|> 
  drop_na() |> 
  filter(por != 0.0)

perso_nombre$var <- factor(perso_nombre$var, levels = niveles)

opcion_actores <- c(
  "Jorge Álvarez Máynez",
  "Andrés Manuel López Obrador",
  "Claudia Sheinbaum",
  "Xóchitl Gálvez",
  "Samuel García",
  "Dante Delgado",
  "Mariana Rodríguez",
  "Luis Donaldo Colosio",
  "Marcelo Ebrard"
)

opcion_actores_nl <- c(
  "Andrés Manuel López Obrador",
  "Claudia Sheinbaum",
  "Samuel García",
  "Mariana Rodríguez",
  "Luis Donaldo Colosio",
  "Adrián De La Garza",
  "Miguel “Mike” Flores",
  "Lorena De La Garza",
  "Waldo Fernández"
)

#Cultura política----
load("www/Microdatos/DF - Nacional/df_simpatia.Rda")
opcion_partidos <- unique(df_simpatia$cultupol_simpatia)
load("www/Microdatos/DF - Nacional/df_rechazo.Rda")
load("www/Microdatos/DF - Nacional/df_votoMorena.Rda")
load("www/Microdatos/DF - Nacional/df_votoPAN.Rda")
load("www/Microdatos/DF - Nacional/df_votoPRI.Rda")
load("www/Microdatos/DF - Nacional/df_partidos.Rda")
load("www/Microdatos/DF - Nacional/df_interno.Rda")
internos <- unique(df_interno$interno)
load("www/Microdatos/DF - Nacional/df_evalAlian.Rda")
load("www/Microdatos/DF - Nacional/df_mejor.Rda")
load("www/Microdatos/DF - Nacional/df_peor.Rda")

#Techo electoral----
load("www/Microdatos/DF - Nacional/df_votoMC.Rda")

#Elección presidencial----
load("www/Microdatos/DF - Nacional/df_careosPresi.Rda")
load("www/Microdatos/DF - Nacional/df_elecsegop.Rda") 
df_elecsegop <- df_elecsegop |>   
mutate(elec_segop = case_match(elec_segop,
    "Jorge Álvarez Maynez + MOVIMIENTO CIUDADANO" ~ "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO",
    .default = elec_segop))
load("www/Microdatos/DF - Nacional/df_epext.Rda")
df_epext <- df_epext |>   
  mutate(elec_expectativa = case_match(elec_expectativa,
                                 "Jorge Álvarez Maynez + MOVIMIENTO CIUDADANO" ~ "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO",
                                 "Jorge Maynez + MOVIMIENTO CIUDADANO" ~ "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO",
                                 .default = elec_expectativa))
load("www/Microdatos/DF - Nacional/df_presiMarca.Rda")
load("www/Microdatos/DF - Nacional/df_presiSeguOpc.Rda")
load("www/Microdatos/DF - Nacional/df_presiAlian.Rda")
load("www/Microdatos/DF - Nacional/df_seguvoto.Rda")
load("www/Microdatos/DF - Nacional/df_cualicandy.Rda")
df_cualicandy <- df_cualicandy |> 
  mutate(opinion = case_match(opinion,
    "Jorge Maynez" ~ "Jorge Máynez",
    "Jorge Álvarez Maynez" ~ "Jorge Máynez",
    .default = opinion)) |> 
  filter(!is.na(tema))
load("www/Microdatos/DF - Nacional/df_definida.Rda")
df_definida <- df_definida |> 
  mutate(opinion = case_match(elec_definida,
    "Puede cambiar" ~ "No",
    "Ya está definida" ~ "Sí",
    .default = elec_definida
  ))
echart_careosPresi <- df_careosPresi |>
  filter (segmento == "bandera") |>
  mutate(opinion = elec_careo)

#Evaluación----
load("www/Microdatos/DF - Nacional/df_evalAMLO.Rda")
load("www/Microdatos/DF - Nacional/df_temasAMLO.Rda")
load("www/Microdatos/DF - Nacional/df_benef.Rda")
df_benef <- df_benef |> 
  mutate(socio_progsoc = if_else(socio_progsoc == "SI", "Sí", socio_progsoc),
    socio_progsoc = if_else(socio_progsoc == "Si", "Sí", socio_progsoc),
    socio_progsoc = if_else(socio_progsoc == "NO", "No", socio_progsoc),
    opinion = socio_progsoc)|> 
  drop_na()

#Brújula----
load("www/Microdatos/DF - Nacional/df_seguridad.Rda")
df_seguridad <- df_seguridad |> 
  mutate(opinion = respuesta)
load("www/Microdatos/DF - Nacional/df_apoyariacandy.Rda")

load("www/Microdatos/DF - Nacional/df_temasprogres.Rda")
df_temasprogres <- df_temasprogres |> 
  mutate(respuesta = if_else(respuesta == "En contra.", "En contra", respuesta),
    opinion = respuesta)

load("www/Microdatos/DF - Nacional/df_caminopais.Rda")
load("www/Microdatos/DF - Nacional/df_economia.Rda")


#Estados----
load("www/Microdatos/DF - Estados/df_perso.Rda")


# Nuevo León ----
load("www/Microdatos/DF - NL/df_perso_nl.Rda")
perso_nombre_nl <- df_perso_nl |> 
  mutate(personaje = case_match(personaje, 
                                "AMLO" ~ "Andrés Manuel López Obrador",
                                "CS" ~ "Claudia Sheinbaum",
                                "ADLG" ~ "Adrián De La Garza",
                                "MF" ~ "Miguel “Mike” Flores",
                                "SGS" ~ "Samuel García",
                                "LDLG" ~ "Lorena De La Garza",
                                "MR" ~ "Mariana Rodríguez", 
                                "LDCR" ~ "Luis Donaldo Colosio",
                                "WF" ~ "Waldo Fernández"
  ),
  Color = case_when(
    personaje == "Andrés Manuel López Obrador" ~ "#761d01",
    personaje == "Claudia Sheinbaum" ~ "#761d01",
    personaje == "Adrián De La Garza" ~ "#ff1d25",
    personaje == "Miguel “Mike” Flores" ~ "#f77b09",
    personaje == "Samuel García" ~ "#f77b09",
    personaje == "Lorena De La Garza" ~ "#ff1d25",
    personaje == "Mariana Rodríguez" ~ "#f77b09",
    personaje == "Luis Donaldo Colosio" ~ "#f77b09",
    personaje == "Waldo Fernández" ~ "#761d01"
  )) |> 
  drop_na() |> 
  filter(por != 0.0)

load("www/Microdatos/DF - NL/df_eval_nl.Rda")
load("www/Microdatos/DF - NL/df_presidencia30_nl.Rda")
df_presidencia30_nl <- df_presidencia30_nl |> 
  mutate(opinion = case_match(opinion,
                              "Si, estaría de acuerdo" ~ "Aprueba",
                              "No estaría de acuerdo" ~ "Desaprueba"))


#Listas----
listaSegmentos <- list(
  "Nacional" = "bandera",
  "NSE" = "nse",
  "Generaciones" = "generaciones",
  "Sexo" = "sexo",
  #"Zona" = "zona_euzen",
  "Nivel educativo" = "socio_educacion"
  #"Intención de voto" = "cultupol_votoalian"
)

levantamientoPorPregunta <- list(
  #Cultura
  marcas = rev(unique(df_simpatia$medicion)),
  voto = rev(unique(df_votoMorena$medicion)),
  internos = rev(unique(df_interno$medicion)),
  alianzas = rev(unique(df_evalAlian$medicion)),
  mejor_peor = rev(unique(df_mejor$medicion)),
  evaluacion = rev(unique(df_partidos$medicion)),
  #Careos
  careos = rev(unique(df_careosPresi$medicion)),
  careos_marca = rev(unique(df_presiMarca$medicion)),
  segunda_opcion = rev(unique(df_presiSeguOpc$medicion)),
  careos_alianza = rev(unique(df_presiAlian$medicion)),
  #Evaluación
  aprobacion = rev(unique(df_evalAMLO$medicion)),
  temasAMLO = rev(unique(df_temasAMLO$medicion)),
  programas = rev(unique(df_benef$medicion)),
  #Brújula
  seguridad = rev(unique(df_seguridad$medicion)),
  temasCont = rev(unique(df_temasprogres$medicion)),
  rumbo = rev(unique(df_caminopais$medicion)),
  economia = rev(unique(df_economia$medicion)),
  #Estados
  estados = unique(df_perso$estado)
)
# 
# listaLevantamientoTemasNL <- unique(problemas_NL$medicion)
# listaSegmentoTemasNL <- unique(problemas_NL$segmento)

