personajes <- source("Nacional/personajes.R", local = T)$value
cultura <- source("Nacional/cultura.R", local = T)$value
careos <- source("Nacional/eleccion.R", local = T)$value
evaluacion <- source("Nacional/evaluacion.R", local = T)$value
brujula <- source("Nacional/brujula.R", local = T)$value

navbarMenu("Nacional",
  tabPanel(title = "Personajes", value = "personajes", personajes),
  tabPanel(title = "Cultura Política", value = "cultura", cultura),
  tabPanel(title = "Elección presidencial", value = "careos", careos),
  tabPanel(title = "Evaluación de gobierno", value = "evaluacion", evaluacion),
  tabPanel(title = "Brújula Ideológica", value = "brujula", brujula)
)