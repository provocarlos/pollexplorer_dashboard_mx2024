listaLevantamiento <- unique(perso_nombre$medicion)

fluidPage(style = "padding-top: 50px",
  fluidRow( style = "padding-top: 50px",
    column(7,
      h2("Personajes"),
    ),
    column(5,
      img(src="iconos/personajes.png", align = "right", width = "150px")
    )
  ),
  fluidRow( style = "padding-top: 50px", height = "100vh",
    column(3,
      radioButtons(
        inputId = "Tipo",
        label = "Tipo de medición",
        choices = list("Línea de tiempo", "Período"),
        selected = "Línea de tiempo"
      ),
      selectizeInput(
        inputId = "selActor",
        label = "Personaje",
        choices = opcion_actores
      ),
      conditionalPanel(
        condition = "input.Tipo == 'Período'",
        selectizeInput(
          inputId = "selLevantamiento",
          label = "Levantamiento",
          choices = listaLevantamiento
        )
      ),
      conditionalPanel(
        condition = "input.Tipo == 'Período'",
        selectizeInput(inputId = "selSegmentoPersonajes",
          label = "Segmento",
          selected = "generaciones",
          choices = listaSegmentos
        ),
      )
    ),
    column(9, class= "grafica",
      tabBox(width = "100%",
        tabPanel("Evaluación", style = "margin-top: 20px",
          conditionalPanel(
            condition = "input.Tipo == 'Línea de tiempo'",
            highchartOutput("graf_personaje_LT")
          ),
          conditionalPanel(
            condition = "input.Tipo == 'Período'",
            highchartOutput("graf_personaje")
          )
        ),
        tabPanel("Conocimiento", style = "margin-top: 20px",
          conditionalPanel(
            condition = "input.Tipo == 'Línea de tiempo'",
            highchartOutput("graf_personaje_conocimiento_LT")
          ),
          conditionalPanel(
            condition = "input.Tipo == 'Período'",
            highchartOutput("graf_personaje_conocimiento")
          )
        ),
        tabPanel("Saldo", style = "margin-top: 20px",
          conditionalPanel(
            condition = "input.Tipo == 'Línea de tiempo'",
            highchartOutput("graf_personaje_saldo_LT")
          ),
          conditionalPanel(
            condition = "input.Tipo == 'Período'",
            highchartOutput("graf_personaje_saldo")
          )
        )
      )
    )
  )
)
