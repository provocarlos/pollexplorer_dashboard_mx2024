fluidPage(style = "padding-top: 50px",
          fluidRow(style = "padding-top: 50px",
                   column(7,
                          h2("Brújula ideológica")
                   ),
                   column(5,
                          img(src="iconos/brujula.png", align = "right", width = "150px")
                   )
          ),
          fluidRow(
            column(3,
                   radioButtons(
                     inputId = "Tipo_brujula",
                     label = "Tipo de medición",
                     choices = list("Línea de tiempo", "Período")
                   ),
                   conditionalPanel(
                     condition = "input.Tipo_brujula == 'Período'",
                     selectizeInput(
                       inputId = "selLevantamiento_brujula",
                       label = "Levantamiento",
                       choices = listaLevantamiento
                     )
                   ),
                   conditionalPanel(
                     condition = "input.Tipo_brujula == 'Período'",
                     selectizeInput(inputId = "selSegmento_brujula",
                                    label = "Segmento",
                                    selected = "generaciones",
                                    choices = listaSegmentos
                     ),
                   )
            ),
            column(9, class = "grafica",
                   tabBox(width="100%", id = "tabBrujula",
                          tabPanel("Rumbo del país",
                                   value = "rumbo",
                                   conditionalPanel(
                                     condition = "input.Tipo_brujula == 'Línea de tiempo'",
                                     highchartOutput("graf_camino_pais_lt")
                                   ),
                                   conditionalPanel(
                                     condition = "input.Tipo_brujula == 'Período'",
                                     highchartOutput("graf_camino_pais")
                                   )
                          ),
                          tabPanel("Temas controversiales",
                                   value = "temasCont",
                                   selectizeInput("selTemaBrujula",
                                                  width = "100%",
                                                  label = "Tema",
                                                  choices = unique(df_temasprogres$tema)
                                   ),
                                   conditionalPanel(
                                     condition = "input.Tipo_brujula == 'Línea de tiempo'",
                                     highchartOutput("graf_temasprogres_lt")
                                   ),
                                   conditionalPanel(
                                     condition = "input.Tipo_brujula == 'Período'",
                                     highchartOutput("graf_temasprogres")
                                   ),
                          ),
                          tabPanel("Apoyaría a un candidato que...",
                                   value = "Apoyaria",
                                   selectizeInput("selApoyaBrujula",
                                                  width = "100%",
                                                  label = "Tema",
                                                  choices = unique(df_apoyariacandy$tema)
                                   ),
                                   conditionalPanel(
                                     condition = "input.Tipo_brujula == 'Línea de tiempo'",
                                     highchartOutput("graf_apoya_lt")
                                   ),
                                   conditionalPanel(
                                     condition = "input.Tipo_brujula == 'Período'",
                                     highchartOutput("graf_apoya")
                                   )
                          ),
                          tabPanel("Seguridad",
                                   value = "seguridad",
                                   selectizeInput("selAmbito",
                                                  width = "100%",
                                                  label = "Ámbito",
                                                  choices = unique(df_seguridad$ambito),
                                   ),
                                   conditionalPanel(
                                     condition = "input.Tipo_brujula == 'Línea de tiempo'",
                                     highchartOutput("graf_seguridad_lt")
                                   ),
                                   conditionalPanel(
                                     condition = "input.Tipo_brujula == 'Período'",
                                     highchartOutput("graf_seguridad")
                                   )
                          ),
                          tabPanel("Economía",
                            value = "economia",
                            conditionalPanel(
                              condition = "input.Tipo_brujula == 'Línea de tiempo'",
                              highchartOutput("graf_economia_lt")
                            ),
                            conditionalPanel(
                              condition = "input.Tipo_brujula == 'Período'",
                              highchartOutput("graf_economia")
                            )
                          )
                   )
            )
          )
)