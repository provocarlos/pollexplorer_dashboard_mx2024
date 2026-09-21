navbarPage("General",
           tabPanel(
             fluidPage(
               style = "margin: 5%",
               titlePanel("Inicio"),
               fluidRow(
                 column(4,
                        h3("Inicio"),
                        h3("Nacional"),
                        h4("Personajes"),
                        h4("Cultura política"),
                        h4("Elección presidencial"),
                        h4("Evaluación de gobierno"),
                        h4("Brújula política")
                 ),
                 column(4,
                        h3("Nuevo León"),
                        h4("Personajes"),
                        h4("Evaluación de gobierno"),
                        h4("Temas estatales")
                 )
               )
             )
           )
)