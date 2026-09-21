fluidPage( style = "padding-top: 50px",
  fluidRow( style = "padding-top: 50px",
    column(7,
      h2("Evaluación Gobierno Federal")
    ),
    column(5,
      img(src="iconos/eval.png", align = "right", width = "150px")
    )
  ),
  fluidRow(
    column(3,
      radioButtons(
        inputId = "Tipo_eval",
        label = "Tipo de medición",
        choices = list("Línea de tiempo", "Período")
      ),
      conditionalPanel(
        condition = "input.Tipo_eval == 'Período'",
        selectizeInput(
          inputId = "selLevantamiento_eval",
          label = "Levantamiento",
          choices = listaLevantamiento
        )
      ),
      conditionalPanel(
        condition = "input.Tipo_eval == 'Período'",
        selectizeInput(inputId = "selSegmento_eval",
          label = "Segmento",
          selected = "generaciones",
          choices = listaSegmentos
        ),
      )
    ),
    column(9, class = "grafica",
      tabBox(width="100%", id = "tabEval",
        tabPanel("Aprobación",
          value = "aprobacion",
          conditionalPanel(
            condition = "input.Tipo_eval == 'Línea de tiempo'",
            highchartOutput("graf_aprobacion_AMLO_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_eval == 'Período'",
            highchartOutput("graf_aprobacion_AMLO")
          )
        ),
        tabPanel("Temas",
          value = "temasAMLO",
          selectizeInput(inputId = "selTemaAMLO",
            label = "Tema",
            choices = unique(df_temasAMLO$tema)
          ),
          conditionalPanel(
            condition = "input.Tipo_eval == 'Línea de tiempo'",
            highchartOutput("graf_temas_AMLO_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_eval == 'Período'",
          highchartOutput("graf_temas_AMLO")
          )
        ),
        tabPanel("Programas sociales",
          value = "programas",
          conditionalPanel(
            condition = "input.Tipo_eval == 'Línea de tiempo'",
            highchartOutput("graf_beneficiarios_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_eval == 'Período'",
            highchartOutput("graf_beneficiarios")
          )
        )
      )
    )
  )
)