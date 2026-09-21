fluidPage( style = "padding-top: 50px",
  fluidRow( style = "padding-top: 50px",
    column(7,
      h2("Cultura Política")
    ),
    column(5,
      img(src="iconos/brujula.png", align = "right", width = "150px")
    )
  ),
  fluidRow( style = "padding-top: 50px", height = "100vh",
    column(3,
      radioButtons(
        inputId = "Tipo_cultura",
        label = "Tipo de medición",
        choices = list("Línea de tiempo", "Período"),
        selected = "Línea de tiempo"
      ),
      conditionalPanel(
        condition = "input.Tipo_cultura == 'Período'",
        selectizeInput(
          inputId = "selLevantamiento_cultura",
          label = "Levantamiento",
          choices = NULL
        )
      ),
      conditionalPanel(
        condition = "input.Tipo_cultura == 'Período'",
        selectizeInput(inputId = "selSegmento_cultura",
          label = "Segmento",
          selected = "generaciones",
          choices = listaSegmentos
        ),
      )
    ),
    column(9, class= "grafica",
      tabBox(width = "100%", id = "tabCultura",
        tabPanel("Simpatía", style = "margin-top: 20px",
          value = "marcas",
          conditionalPanel(
            condition = "input.Tipo_cultura == 'Línea de tiempo'",
            highchartOutput("graf_simpatia_lt"),
            div(style = "margin-top: 60px"),
            highchartOutput("graf_rechazo_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_cultura == 'Período'",
            highchartOutput("graf_simpatia"),
            div(style = "margin-top: 60px"),
            highchartOutput("graf_rechazo")
          )
        ),
        tabPanel("Voto", style = "margin-top: 20px",
          value = "voto",
          conditionalPanel(
            condition = "input.Tipo_cultura == 'Línea de tiempo'",
            highchartOutput("graf_voto_MC_cult_lt"),
            highchartOutput("graf_voto_Morena_lt"),
            highchartOutput("graf_voto_PAN_lt"),
            highchartOutput("graf_voto_PRI_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_cultura == 'Período'",
            highchartOutput("graf_voto_MC_cult"),
            highchartOutput("graf_voto_Morena"),
            highchartOutput("graf_voto_PAN"),
            highchartOutput("graf_voto_PRI")
          )
        ),
        tabPanel("Evaluación", style = "margin-top: 20px",
          value = "evaluacion",
          conditionalPanel(
            condition = "input.Tipo_cultura == 'Línea de tiempo'",
            highchartOutput("graf_eval_MC_cult_lt"),
            highchartOutput("graf_eval_Morena_lt"),
            highchartOutput("graf_eval_PAN_lt"),
            highchartOutput("graf_eval_PRI_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_cultura == 'Período'",
            highchartOutput("graf_eval_MC_cult"),
            highchartOutput("graf_eval_Morena"),
            highchartOutput("graf_eval_PAN"),
            highchartOutput("graf_eval_PRI")
          )
        ),
        tabPanel("Internos", style = "margin-top: 20px",
          value = "internos",
          selectizeInput(
            inputId = "selInterno",
            label = "Elige interno",
            choices = internos
          ),
          conditionalPanel(
            condition = "input.Tipo_cultura == 'Línea de tiempo'",
            highchartOutput("graf_internos_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_cultura == 'Período'",
            highchartOutput("graf_internos")
          )
        ),
        tabPanel("Alianzas", style = "margin-top: 20px",
          value = "alianzas",
          conditionalPanel(
            condition = "input.Tipo_cultura == 'Línea de tiempo'",
            highchartOutput("graf_Alianzas_Morena_lt"),
            highchartOutput("graf_Alianzas_PRIAN_lt")
          ),
          conditionalPanel(
            condition = "input.Tipo_cultura == 'Período'",
            highchartOutput("graf_Alianzas_Morena"),
            highchartOutput("graf_Alianzas_PRIAN")
          )
        ),
        # tabPanel("Lo mejor y peor", style = "margin-top: 20px",
        #   value = "mejor_peor",
        #   conditionalPanel(
        #     condition = "input.Tipo_cultura == 'Línea de tiempo'",
        #     highchartOutput("graf_lo_mejor_lt"),
        #     highchartOutput("graf_lo_peor_lt")
        #   ),
        #   conditionalPanel(
        #     condition = "input.Tipo_cultura == 'Período'",
        #     highchartOutput("graf_lo_mejor"),
        #     highchartOutput("graf_lo_peor")
        #   )
        # )
      )
    )
  )
)