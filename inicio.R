tabPanel(title = "Inicio", value = "inicio", id = "inicio",
  fluidPage(
    fluidRow(class = "login",
      box(class = "intro", width = "100%",
        column(6,
          h1("Explorador de encuestas"),
          p(class ="subtitulo", "Estudio Nacional")
        ),
        column(6,
          box(style = "min-height = 500px", width = "100%",
            p(style = "text-align: justify; font-size: 1.5rem", "Este informe 
              interactivo permite visualizar los resultados de las encuestas 
              nacionales levantadas por una encuestadora privada para el proceso electoral 2024. La intención 
              de este informe es presentar los datos de las encuestas en sus 
              distintas desagregaciones de una forma intuitiva, visual, 
              explicativa e interactiva, y así generar conocimiento accesible y 
              aplicable a estrategias y campañas políticas. 
              Los datos mostrados en esta versión son falsos y solo tienen fines de demostración. ")
          )
        )
      )
    ),
    fluidRow(height= "150px", style = "background: #F0F0F0; padding-top: 50px", class = "secciones",
             column(12,
                    h2("Nacional")
                    ),
      column(4,
        tags$a(onclick = "customHref('personajes')",
          box(width = "100%", class = "seccion",
            img(src="iconos/personajes.png", width = "48px", class = "icono"),
            div(
            h3("Personajes"),
            p(class = "enlace", "Encuentra y compara los personajes de la política nacional"))
          )
        )
      ),
      column(4,
        tags$a(onclick = "customHref('cultura')",
          box(width = "100%", class = "seccion",
            img(src="iconos/partidos.png", width = "48px", class = "icono"),
            div(
          h3("Cultura política"),
          p(class = "enlace", "Evalúa y descubre la fortalezas y debilidades de los contrincantes"))
          )
        )
      )
    ),
    fluidRow(height= "150px",  style = "background: #F0F0F0; padding-bottom: 50px", class = "secciones",
      column(4,
        tags$a(onclick = "customHref('careos')",
          box(width = "100%", class = "seccion",
            img(src="iconos/careos.png", width = "48px", class = "icono"),
            div(
          h3("Elección presidencial"),
          p(class = "enlace", "Examina y contraste los posibles escenarios de la elección presidencial"))
          )
        )
      ),
      column(4,
        tags$a(onclick = "customHref('evaluacion')",
          box(width = "100%", class = "seccion",
            img(src="iconos/eval.png", width = "48px", class = "icono"),
            div(
          h3("Evaluación"),
          p(class = "enlace", "Conoce la evaluación del presidente y su desempeño en distintos temas"))
          )
        )
      ),
      column(4,
        tags$a(onclick = "customHref('brujula')",
          box(width = "100%", class = "seccion",
          img(src="iconos/brujula.png", width = "48px", class = "icono"),
            div(
          h3("Brújula"),
          p(class = "enlace", "Descubre las posturas y creencias de la población o segmentos"))
          )
        )
      )
    )
  )
)