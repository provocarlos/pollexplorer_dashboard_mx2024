fluidPage(style = "padding-top: 50px",
  fluidRow( style = "padding-top: 50px",
    column(7,
      h2("Elección presidencial"),
    ),
    column(5,
      img(src="iconos/careos.png", align = "right", width = "150px")
    )
  ),
  fluidRow(
    column(3,
      radioButtons(
        inputId = "Tipo_Careo",
        label = "Tipo de medición",
        choices = list("Línea de tiempo", "Período"),
        selected = "Línea de tiempo"
      ),
      conditionalPanel(
        condition = "input.Tipo_Careo == 'Período'",
        selectizeInput(
          inputId = "selLevantamiento_careo",
          label = "Levantamiento",
          choices = listaLevantamiento
        )
      ),
      conditionalPanel(
        condition = "input.Tipo_Careo == 'Período'",
        selectizeInput(inputId = "selSegmento_careo",
          label = "Segmento",
          selected = "generaciones",
          choices = listaSegmentos
        ),
      )
    ),
    column(9, class= "grafica",
      tabBox(width= "100%", id = "tabCareos",
        tabPanel("Careos",
          value = "careos",
          conditionalPanel(
            condition = "input.Tipo_Careo == 'Línea de tiempo'",
            highchartOutput("graf_careos_lt"),
            p("Segunda opción por careo"),
            highchartOutput("graf_careos_segunda_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_Careo == 'Período'",
            highchartOutput("graf_careos"),
            p("Segunda opción por careo"),
            highchartOutput("graf_careos_segunda")
          )
        ),
        tabPanel("Por alianza",
          value = "careos_alianza",
          conditionalPanel(
            condition = "input.Tipo_Careo == 'Línea de tiempo'",
            highchartOutput("graf_voto_alianza_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_Careo == 'Período'",
            highchartOutput("graf_voto_alianza")
          )
        ),
        tabPanel("Seguridad de voto",
          value = "seguridad_voto",
          conditionalPanel(
            condition = "input.Tipo_Careo == 'Línea de tiempo'",
            #highchartOutput("graf_seguridad_voto_lt"),
            highchartOutput("graf_seguridad_voto_alt_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_Careo == 'Período'",
            #highchartOutput("graf_seguridad_voto"),
            highchartOutput("graf_seguridad_voto_alt")
          )
        ),
        tabPanel("Expectativa de triunfo",
          value = "expectativa",
          conditionalPanel(
            condition = "input.Tipo_Careo == 'Línea de tiempo'",
            highchartOutput("graf_careos_expectativa_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_Careo == 'Período'",
            highchartOutput("graf_careos_expectativa")
          )
        ),
        tabPanel("Cualidades",
          value = "cualidades",
          selectizeInput("selTemaCualidades",
            width = "100%",
            label = "Tema",
            choices = unique(df_cualicandy$tema)
          ),
          conditionalPanel("input.Tipo_Careo == 'Línea de tiempo'",
            highchartOutput("graf_cualidades_lt"),
          ),
          conditionalPanel("input.Tipo_Careo == 'Período'",
            highchartOutput("graf_cualidades"))
        ),
        tabPanel("Definición",
          value = "definicion",
          conditionalPanel(
            condition = "input.Tipo_Careo == 'Línea de tiempo'",
            highchartOutput("graf_definida_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_Careo == 'Período'",
            highchartOutput("graf_definida")
          )
        ),
      )
    )
  )
)
