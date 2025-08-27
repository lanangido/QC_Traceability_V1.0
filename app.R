# app.R (Versi Final & Sinkron)
source("global.R")

# UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = tags$img(src = "logo.png", height = '30', style = "padding: 5px;"), titleWidth = 250, uiOutput("header_ui")),
  dashboardSidebar(width = 250, sidebarMenu(id = "tabs", menuItem("Operator QC", tabName = "operator_qc", icon = icon("check-square")))),
  dashboardBody(
    useShinyjs(),
    setBackgroundImage(src = "bc3.jpg", shinydashboard = TRUE),
    tags$head(tags$style(HTML("
      /* Custom Notification Position */
      .shiny-notification { position:fixed; top: calc(50%); left: calc(50%); transform: translate(-50%, -50%); width: 350px; font-size: 18px; font-weight: bold; box-shadow: 0 0 20px rgba(0,0,0,0.5); }
      .content-wrapper { overflow-y: auto !important; }
    "))),
    uiOutput("body_ui")
  )
)

# SERVER
server <- function(input, output, session) {
  conn <- get_db_conn(); onStop(function() { dbDisconnect(conn) })
  
  auth <- auth_server("auth", conn = conn)
  selected_product_info <- reactiveVal(NULL)
  
  output$body_ui <- renderUI({
    if (!isTRUE(auth$is_logged_in())) {
      div(style = "display: flex; justify-content: center; align-items: center; height: 70vh;", auth_ui("auth"))
    } else if (is.null(selected_product_info())) {
      product_choices <- dbGetQuery(conn, "SELECT product_id, product_name FROM products ORDER BY product_name")
      choices <- setNames(product_choices$product_id, product_choices$product_name)
      div(style = "display: flex; justify-content: center; align-items: center; height: 70vh;",
          box(title = "Pilih Produk", solidHeader = TRUE, width = 4,
              selectInput("product_select_input", "Pilih Produk untuk Diinspeksi:", choices = choices),
              actionButton("product_select_btn", "Lanjutkan", class = "btn-success w-100")
          )
      )
    } else {
      operator_qc_ui("operator_qc")
    }
    
  })
  source("operator_qc_module.R", local=TRUE)
  
  output$header_ui <- renderUI({
    if (isTRUE(auth$is_logged_in())) {
      tags$li(class = "dropdown",
              div(style = "padding: 15px; color: white;",
                  strong(auth$user_info()$nama),
                  if(!is.null(selected_product_info())) tags$span(paste(" | Produk:", selected_product_info()$name), style="font-weight:normal; margin-left: 15px;"),
                  actionButton("logout_btn", "Logout", icon = icon("sign-out-alt"), class = "btn-danger btn-sm", style="margin-left: 15px;")
              )
      )
    }
  })
  observeEvent(input$logout_btn, { session$reload() })
  observeEvent(input$product_select_btn, {
    req(input$product_select_input)
    product_name <- dbGetQuery(conn, "SELECT product_name FROM products WHERE product_id = $1", params=list(input$product_select_input))$product_name
    selected_product_info(list(id = input$product_select_input, name = product_name))
  })
  observe({
    if (!is.null(selected_product_info())) {
      operator_qc_server("operator_qc",
                         conn = conn,
                         user_info = auth$user_info,
                         product_info = selected_product_info
      )
    }
  })
  source("operator_qc_module.R", local=TRUE)
}
shinyApp(ui = ui, server = server)