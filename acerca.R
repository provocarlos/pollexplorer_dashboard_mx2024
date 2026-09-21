tabPanel(title = "Acerca de", value = "acerca",
  fluidPage( style = "margin-top: 100px",
    fluidRow(
      column(12,  class = "izquierda",
        tabBox(width="100%",
          tabPanel("Metodología",
            h2("Metodología"),
            h3("Universo muestra"),
            p("Adultos con credencial de elector en la selección electoral 
              seleccionada del país a nivel nacional."),
           
            h3("Fechas de levantamiento"),
            tags$ol(
              tags$li("Del 2 de mayo al 19 de junio del 2023"),
              tags$li("Del 1 al 8 de octubre del 2023"),
              tags$li("Del 31 de octubre al 5 de noviembre del 2023"),
              tags$li("Del 13 al 15 de diciembre del 2023"),
              tags$li("Del 9 al 12 de marzo del 2023"),
              tags$li("Del 1 al 4 de abril del 2023"),
              tags$li("Del 17 al 20 de abril del 2023")
              
              
            ),
            h3("Tamaño y diseño de muestras"),
            tags$ol(
              tags$li("6,000 casos válidos. 95% de confianza y error 
                medio de las estimaciones de +- 1.3%. La selección de la muestra 
                se hizo de forma sistemática con arranque aleatorio mediante algoritmo."),
              tags$li("1,800 casos válidos. 95% de confianza y error 
                medio de las estimaciones de +- 2.3%."),
              tags$li("3,000 casos válidos. 95% de confianza y 
                error medio de las estimaciones de +- 1.8%."),
              tags$li("1,200 casos válidos. 95% de confianza y 
                error medio de las estimaciones de +- 2.8%."),
              tags$li("1,200 casos válidos. 95% de confianza y 
                error medio de las estimaciones de +- 2.8%."),
              tags$li("1,200 casos válidos. 95% de confianza y 
                error medio de las estimaciones de +- 2.8%."),
              tags$li("1,200 casos válidos. 95% de confianza y 
                error medio de las estimaciones de +- 2.8%.")
            ),
            h3("Unidades de muestreo"),
            p("UNIDAD PRIMARIA DE MUESTREO: Sección electoral."),
            p("UNIDAD SECUNDARIA DE MUESTREO: Vivienda."),
            p("UNIDAD TERCIARIA DE MUESTREO: Individuo perteneciente al universo 
              muestral, un informante por vivienda."),
            h3("Consideraciones"),
            tags$ul(
              tags$li("100% de la muestra geo-referenciada en tiempo real."),
              tags$li("100% captura en dispositivos móviles."),
              tags$li("100% de la muestra supervisada en ex-post.")
            )
          ),
          tabPanel("Zonas del país",
          #   h2("Sociodemográficos"),
          #   h3("Segmentos"),
          #   p("La segmentación del estudio se realizó considerando cinco elementos: sexo, generación, NSE, escoolaridad y
          #     zona territorial. Para conseguir una segmentación más precisa se utilizaron datos estadísticos de población
          #     del censo nacional INEGI 2020."),
          #   h4("Sexo"),
          #   p("Segmentación por hombres y mujeres."),
          #   imageOutput("segmento_sexo"),
          #   h4("Generación"),
          #   p("Segmentación por edades según el porcentaje de población nacional."),
          #   imageOutput("segmento_generaciones"),
          #   h4("Nivel socioeconómico"),
          #   p("egmentación por nivel socioeconómico a nivel nacional."),
          #   imageOutput("segmento_NSE"),
            h4("Zona"),
            p("Se clasificaron en cuatro regiones a los estados según factores territorales, culturales y socioeconómicos:"),
            tags$ul(
              tags$li("Centro"),
              tags$li("Norte"),
              tags$li("Occidente"),
              tags$li("Sureste")
            ),
            imageOutput("zonas_pais")
          #   imageOutput("segmento_zonas"),
          #   p("Escolaridad: segmentación por nivel educativo."),
          #   imageOutput("segmento_escolaridad"),
          #   p("Además, se registran dos segmentos generales para facilitar la interpretación estadística:"),
          #   h4("Nacional"),
          #   p("Promedio de la encuesta en todo el país para ofrecer un resumen general."),
          #   h4("Opinión MC"),
          #   p("Segmentación según la valoración hacia Movimiento Ciudadano para medir el público objetivo 
          #     principal.")
          #   
          )
        )
      )
    )
  )
)
