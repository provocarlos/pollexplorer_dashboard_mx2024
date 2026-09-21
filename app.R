pacman::p_load(DBI, highcharter, RSQLite, shiny, leaflet, shinyauthr,
  shinydashboard, shinyWidgets, tidyverse)

source("MyloginServer.R")
source("global.R")
source("www/Microdatos/dataframes.R")
menu <- source("Menu.R")
inicio <- source("inicio.R", local = T)$value
nacional <- source("Nacional/nacional.R", local = T)$value
# nuevo_leon <- source("NL/nuevo_leon.R", local = T)$value
acerca <- source("acerca.R", local = T)$value

login <- tabPanel(
  title = icon("lock"),
  value = "login",
  fluidPage(
    fluidRow(class="video", width = "100%", height = "100%", padding = "-15px",
      column(12,
        tags$video(id="intro", type="video/mp4",
          src="video_bg.mp4", autoplay = "autoplay",
          loop = "loop", muted = "muted")
      )
    ),
    fluidRow(
      box(class = "centrado", width = "100%",
        column(4, offset = -1,
          loginUI("login",
            title = div(style = "display: block; overflow: hidden",
              img(src="iconos/logo.png",
                style = "max-width: 150px;")),
            user_title    = "Usuario",
            pass_title    = "Contraseña",
            login_title   = "Entrar",
            error_message = "Usuario o contraseña inválido",
            additional_ui = p(
              style = "text-align: center; margin-top: 10px; color: #cccccc;",
              "Demo pública — Usuario: ", tags$b("demo"),
              " · Contraseña: ", tags$b("demo123")
            )
          )
        ),
        column(6, offset = -1, class = "login",
          h1("Explorador de encuestas"),
          p(class="titular", "Estudio Nacional")
        )
      )
    )
  )
)

ui <-  tagList(
  tags$head(
    tags$link(rel = "shortcut icon", href = "logo.png"),
    tags$link(rel = "stylesheet", type ="text/css", href = "custom.css"),
    tags$script(type = "text/javascript", src = "index.js"),
    tags$title("Explorador de encuestas")
  ),
  
  navbarPage(title = img(src = "iconos/logo_invertido.png", width = "80px"),
    windowTitle = "Explorador de encuestas",
    id          = "nav",
    collapsible = TRUE,
    theme       = "mytheme.css",
    position    = c("fixed-top"),
    login
  )
)

server <- function(input, output, session) {
  
  # Inicio de sesión ----
  conn <- dbConnect(SQLite(), "ID.sqlite")
  users_df <- dbGetQuery(conn, "SELECT user, pw FROM users")
  
  credentials <- loginServer(
    id            = "login",
    data          = users_df,
    user_col      = user,
    pwd_col       = pw,
    sodium_hashed = TRUE
  )
  
  session$onSessionEnded(function() dbDisconnect(conn))
  
  output$logo <- renderImage({
    list(src   = "www/logo.png",
      width    = "128px",
      height   = "128px"
    )
  }, deleteFile = F)
  
  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth) {
      removeTab("nav", "login")
      appendTab("nav", inicio, select = TRUE)
      appendTab("nav", nacional, select = FALSE)
      # appendTab("nav", nuevo_leon, select = FALSE)
      appendTab("nav", acerca, select = FALSE)
      # appendTab("nav", menu, select = TRUE)
    }
  })
  
  # Nacional ----
  ## Personajes ----
  source("Nacional/server_nacional_personajes.R", local = TRUE)
  
  ## Cultura política ----
  source("Nacional/server_nacional_cultura.R", local = TRUE)
  
  ## Elección presidencial ----
  source("Nacional/server_nacional_eleccion.R", local = TRUE)
  
  ## Evaluación del gobierno federal ----
  source("Nacional/server_nacional_evaluacion.R", local = TRUE)
  
  ## Brújula política ----
  source("Nacional/server_nacional_brujula.R", local = TRUE)
  
  # # Nuevo León ----
  # ## Personajes ----
  # source("NL/server_nl_personajes.R", local = TRUE)
  # 
  # ## Evaluación del gobierno estatal ----
  # source("NL/server_nl_evaluacion.R", local = TRUE)
  # 
  # ## Temas estatales ----
  # source("NL/server_nl_temas.R", local = TRUE)
}

shinyApp(ui = ui, server = server)
