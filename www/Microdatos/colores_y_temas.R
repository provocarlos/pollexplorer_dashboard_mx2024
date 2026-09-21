#Funciones
pacman::p_load(tidyverse, extrafont, sysfonts, showtext)
loadfonts()

font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()


#Cambiar tamaño de guides (fill, color, etc)
#theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(1, "cm"))



text_size <- function(Size_texto,
                      fuente = "Montserrat",
                      Mapa = FALSE) {
  
  Size_texto <- ifelse(Size_texto < 4, 
                         4, Size_texto)
  if (Mapa == FALSE) {
    return(
      theme(text = element_text(size = Size_texto, family = fuente,  face = "bold",
                                lineheight = 0.7),
            legend.text = element_text(size = Size_texto, family = fuente,
                                       lineheight = 0.7,),
            axis.text =  element_text(size = Size_texto, family = fuente),
            axis.title = element_text(size = Size_texto, family = fuente),
            plot.title = element_text(size = Size_texto + 4, family = fuente),
            plot.subtitle = element_text(size = Size_texto, family = fuente),
            strip.text = element_text(size = Size_texto + 3, family = fuente),
            plot.caption = element_text(size = Size_texto - 4, family = fuente))
      )
  }
  
  else {
    return(
      theme_void() +
      theme(text = element_text(size = Size_texto, family = fuente,  face = "bold",
                                lineheight = 0.7),
            legend.text = element_text(size = Size_texto, family = fuente,
                                       lineheight = 0.7),
            axis.text =  element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(size = Size_texto + 4, family = fuente),
            plot.subtitle = element_text(size = Size_texto, family = fuente),
            strip.text = element_text(size = Size_texto + 3, family = fuente),
            plot.caption = element_text(size = Size_texto - 10, family = fuente))
    )
  }
}





    #Colores para Partidos Políticos #####
col_finder <- function(Partido) {
  
  Partido <- toupper(Partido) 
  
  a <- case_when(
    #MC y variantes
    Partido %in% c("MC", "MOVIMIENTO CIUDADANO", "CONVERGENCIA") ~ "#f77b09",
    
    
    #Partidos "Tradicionales"
    Partido == "PAN"     ~ "#0062ff",
    Partido == "PRI"     ~ "#ff1d25",
    Partido == "PRD"     ~ "#ffd400",
    
    #Morena y "aliados"
    Partido == "MORENA"  ~ "#761d01",
    Partido == "PVEM"    ~ "#51ce00",
    Partido == "PT"      ~ "#a50000",
    
    #Partidos minoritarios (nacionales)
    Partido == "FXM" ~ "#f14fc9",
    Partido == "RSP" ~ "#d5a6bd",
    Partido == "PES" ~ "#9600ca",
    Partido == "PNA" ~ "#41e3e3",
    Partido == "PH"  ~ "#800080",
    
    #Partidos minoritarios (Locales) 
    
    
    #Coaliciones Agregadas
    Partido == "COALICIÓN MORENA" ~ "#d97b64",
    Partido %in% c("COALICIÓN PAN-PRI-PRD", "COALICIÓN PRI-PAN-PRD") ~ "#6b9ff1",
    
    #Otras Coaliciones
    Partido == "PRI-PVEM-PNA" ~ "forestgreen",
    Partido == "PAN-PRD-Convergencia" ~ "#6C9FBF",
    Partido == "PAN-PRD-PNA" ~ "#234F81",
    
    #Independientes y otros
    Partido %in% c("INDEPENDIENTE", "INDEPENDIENTES",
                   paste0("CAND_IND", c(1:80)), paste0("CI", c(1:80)), paste0("INDEP-", c(1:5)))  ~ "gray60",
    Partido == "OTROS"  ~ "black",
    
    #
    T ~ "white"
    )
  return(a)
}




Partidos_agrupados <- function(Partido, 
                               Otros_Partidos = NULL) #Vector para agregar partidos No considerados
  {
  Partido <- toupper( str_replace_all(Partido, "_", "-"))
  Partido <- str_replace(Partido, "C-", "")
  a <-
    case_when(
      Partido %in% c( "INDEPENDIENTE", "INDEPENDIENTES", paste0("CI", c(1:65))) ~  "Independiente",
      
      Partido == "MC" ~  "Movimiento Ciudadano",
      
      Partido %in% c("MORENA",  "PAN",  "PRD",  "PRI", "PT", "PVEM",
                     Otros_Partidos)                                ~  Partido,
      
      str_detect(Partido, "-MORENA") | str_detect(Partido, "MORENA-") ~  "Coalición MORENA",
      
      str_detect(Partido, "-PAN") |  str_detect(Partido, "PAN-") |
        str_detect(Partido, "-PRI") |  str_detect(Partido, "PRI-") |
        str_detect(Partido, "-PRD") |  str_detect(Partido, "PRD-") ~  "Coalición PAN-PRI-PRD",
      
      T ~ Partido )
  
  return(a)
}


#Listas de 
ID <- c(1:32)

Minusculas_A <- 
  c(#A, B, C, D, 
    "Aguascalientes",
    "Baja California", "Baja California Sur",
    "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
    "Durango",
    #G, H, I, J,
    "Guanajuato", "Guerrero",
    "Hidalgo",
    "Jalisco",
    #M, N, O, P,
    "México", "Michoacán", "Morelos", 
    "Nayarit", "Nuevo León",
    "Oaxaca", "Puebla",
    #Q, S, T,
    "Querétaro", "Quintana Roo",
    "San Luis Potosí", "Sinaloa", "Sonora",
    "Tabasco", "Tamaulipas", "Tlaxcala",
    #V, Y, Z
    "Veracruz",
    "Yucatán",
    "Zacatecas")

Mayusculas_N_A <-
  c("AGUASCALIENTES",
    "BAJA CALIFORNIA",
    "BAJA CALIFORNIA SUR",
    "CAMPECHE",
    "COAHUILA",
    "COLIMA",
    "CHIAPAS",
    "CHIHUAHUA",
    "CIUDAD DE MEXICO",
    "DURANGO",
    "GUANAJUATO",
    "GUERRERO",
    "HIDALGO",
    "JALISCO",
    "MEXICO",
    "MICHOACAN",
    "MORELOS",
    "NAYARIT",
    "NUEVO LEON",
    "OAXACA",
    "PUEBLA",
    "QUERETARO",
    "QUINTANA ROO",
    "SAN LUIS POTOSI",
    "SINALOA",
    "SONORA",
    "TABASCO",
    "TAMAULIPAS",
    "TLAXCALA",
    "VERACRUZ",
    "YUCATAN",
    "ZACATECAS")

Mayusculas_A <-
  c("AGUASCALIENTES",
    "BAJA CALIFORNIA",
    "BAJA CALIFORNIA SUR",
    "CAMPECHE",
    "COAHUILA",
    "COLIMA",
    "CHIAPAS",
    "CHIHUAHUA",
    "CIUDAD DE MÉXICO",
    "DURANGO",
    "GUANAJUATO",
    "GUERRERO",
    "HIDALGO",
    "JALISCO",
    "MÉXICO",
    "MICHOACÁN",
    "MORELOS",
    "NAYARIT",
    "NUEVO LEÓN",
    "OAXACA",
    "PUEBLA",
    "QUERÉTARO",
    "QUINTANA ROO",
    "SAN LUIS POTOSÍ",
    "SINALOA",
    "SONORA",
    "TABASCO",
    "TAMAULIPAS",
    "TLAXCALA",
    "VERACRUZ",
    "YUCATáN",
    "ZACATECAS")
Z_Catalogo_Nombres_Estados <- as.data.frame( cbind(ID, Minusculas_A, Mayusculas_N_A, Mayusculas_A))
rm(list = c("ID", "Minusculas_A", "Mayusculas_N_A", "Mayusculas_A") )


#Otros #####
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

Quitar_tilde <- function(x) {
  return(chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", x))
}



