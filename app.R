# app.R
source("global.R")

# ==============================================================================
# -- UI (USER INTERFACE) --
# ==============================================================================
ui <- dashboardPage(
  skin = "black", title = "QC APP V1.0",
  
  dashboardHeader(

    title = tags$img(src = "logo.png", height = '30', style = "padding: 5px;"),
    titleWidth = 250,
    uiOutput("header_ui")
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      uiOutput("sidebar_ui")
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    setBackgroundImage(src = "bc3.jpg", shinydashboard = TRUE),
    tags$head(tags$style(HTML("
      .shiny-notification {
        position:fixed;
        top: calc(50%);
        left: calc(50%);
        transform: translate(-50%, -50%);
        width: 350px;
        font-size: 18px;
        font-weight: bold;
        box-shadow: 0 0 20px rgba(0,0,0,0.5);
      }
      .content-wrapper {
        overflow-y: auto !important;
      }
    "))),
    uiOutput("body_ui")
  )
)

# ==============================================================================
# -- SERVER (LOGIC) --
# ==============================================================================
server <- function(input, output, session) {
  
  conn <- get_db_conn()
  onStop(function() { dbDisconnect(conn) })
  
  auth <- auth_server("auth", conn = conn)
  
  output$body_ui <- renderUI({
    if (isTRUE(auth$is_logged_in())) {
      operator_qc_ui("operator_qc")
    } else {
      div(style = "display: flex; justify-content: center; align-items: center; height: 70vh;",
          auth_ui("auth")
      )
    }
  })
  
  output$sidebar_ui <- renderUI({
    if (isTRUE(auth$is_logged_in())) {
      menuItem("Operator QC", tabName = "operator_qc", icon = icon("check-square"))
    }
  })
  
  output$header_ui <- renderUI({
    if (isTRUE(auth$is_logged_in())) {
      tags$li(class = "dropdown",
              div(style = "padding: 15px; color: white;",
                  strong(auth$user_info()$nama),
                  actionButton("logout_btn", "Logout", icon = icon("sign-out-alt"), class = "btn-danger btn-sm", style="margin-left: 15px;")
              )
      )
    }
  })
  
  observeEvent(input$logout_btn, {
    session$reload()
  })
  
  observe({
    if (isTRUE(auth$is_logged_in())) {
      operator_qc_server("operator_qc",
                         conn = conn,
                         user_info = auth$user_info
      )
    }
  })
}

shinyApp(ui = ui, server = server)