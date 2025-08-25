# R/modules/auth_module.R
auth_ui <- function(id) {
  ns <- NS(id)
  
  # Menggunakan UI login baru sesuai referensi Anda
  div(
    id = "loginpage", style = "width: 450px; max-width: 100%; margin: 0 auto;",
    
    # ====================================================================
    # PERUBAHAN DI SINI: Menambahkan logo.png di atas form
    # ====================================================================
    tags$div(style = "text-align: center; margin-bottom: 20px;",
             tags$img(src = "logo.png", width = "300")
    ),
    # ====================================================================
    
    wellPanel(
      style = "background: #ECECEC; border-radius: 15px; box-shadow: 0 5px 15px rgba(0,0,0,0.2);",
      tags$h2("LOGIN",
              class = "text-center",
              style = "padding-top: 0;color:#333; font-family: san fransisco, sans-serif; font-size: 25px; font-weight:bold;"
      ),
      tags$h5("*) Bagi user yang tidak bisa Login,", class = "text-center", style = "font-weight:bold;"),
      tags$h5("silahkan input username dengan NIK LENGKAP anda:", class = "text-center", style = "font-weight:bold;"),
      tags$br(),
      textInput(ns("userName"), placeholder = "Username", label = tagList(icon("user"), "Username")),
      passwordInput(ns("passwd"), placeholder = "Password", label = tagList(icon("unlock-alt"), "Password")),
      tags$br(),
      div(
        style = "text-align: center;",
        actionButton(ns("login"), "LOG IN",
                     style = "color: white; background-color:#4B4A48; padding: 10px 15px; width: 120px; cursor: pointer;
                   font-family: san fransisco, sans-serif; font-size: 15px; font-weight: 400; border-radius: 6px;"
        ),
        div(class = "shiny-text-output text-danger mt-2", id = ns("login_error_placeholder"))
      )
    )
  )
}

auth_server <- function(id, conn) {
  # Server auth tetap sama, tidak perlu diubah
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    auth_data <- reactiveValues(is_logged_in = FALSE, user_info = NULL)
    observe({
      shinyjs::runjs(sprintf("
        $('#%s').on('keyup', function(e) {
          if (e.keyCode === 13) { $('#%s').click(); }
        });
      ", ns("passwd"), ns("login")))
    })
    observeEvent(input$login, {
      req(input$userName, input$passwd)
      query <- "SELECT user_id, email, nama, role FROM operator WHERE email = $1 AND password = $2"
      user <- dbGetQuery(conn, query, params = list(input$userName, input$passwd))
      if (nrow(user) > 0) {
        auth_data$is_logged_in <- TRUE
        auth_data$user_info <- as.list(user[1, ])
      } else {
        output$login_error_placeholder <- renderText("Username atau password salah.")
      }
    })
    return(
      list(
        is_logged_in = reactive(auth_data$is_logged_in),
        user_info = reactive(auth_data$user_info)
      )
    )
  })
}