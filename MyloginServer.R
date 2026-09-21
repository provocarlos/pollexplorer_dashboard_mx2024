## shinyauthr loginServer Customizing function
pacman::p_load(DBI, sodium)

### package needed : dplyr, shiny, shinyjs, DBI


# Customize shinyauthr::loginUI

custom_loginUI <- function (id, title = "Please log in", user_title = "User Name",
  pass_title = "Password", login_title = "Log in", error_message = "Invalid username or password!",
  additional_ui = NULL, cookie_expiry = 7)
{
  
  ns <- shiny::NS(id)
  shinyjs::hidden(shiny::div(id = ns("panel"), style = "width: inherit; margin: inherit; padding: 20px;  font-family: promptlight",
    shiny::wellPanel(shinyjs::useShinyjs(), jscookie_script(),
      shinyjs::extendShinyjs(text = js_cookie_to_r_code(ns("jscookie"),
        expire_days = cookie_expiry), functions = c("getcookie",
          "setcookie", "rmcookie")), shinyjs::extendShinyjs(text = js_return_click(ns("password"),
            ns("button")), functions = c()), shiny::tags$h2(title,
              class = "text-center", style = "padding-top: 0;"),
      shiny::textInput(ns("user_name"), user_title),
      shiny::passwordInput(ns("password"), pass_title),
      shiny::div(style = "text-align: right;", shiny::actionButton(ns("button"),
        login_title, class = "btn-default", style = "color: white;")),
      additional_ui, shinyjs::hidden(shiny::div(id = ns("error"),
        shiny::tags$p(error_message, style = "color: red; font-weight: bold; padding-top: 5px;",
          class = "text-center"))))))
}

environment(custom_loginUI) <- asNamespace('shinyauthr')
assignInNamespace("loginUI", custom_loginUI, ns = "shinyauthr")
