# R/modules/operator_qc_module.R

operator_qc_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 4,
           box(
             title = "Kontrol Inspeksi",
             solidHeader = TRUE,
             width = NULL,
             selectInput(ns("tahap"), "Pilih Tahap Produksi:", choices = NULL),
             textInput(ns("part_number"), "Scan Barcode Part (Tekan Enter)"),
             actionButton(ns("refresh_btn"), "Refresh Form", icon = icon("sync"), class = "btn-custom-black w-100")
           ),
           uiOutput(ns("rework_status_ui")),
           box(
             title = "Gambar Referensi",
             solidHeader = TRUE,
             width = NULL,
             collapsible = TRUE,
             uiOutput(ns("reference_image_ui"))
           )
    ),
    column(width = 8,
           box(
             title = "Input Pengukuran",
             solidHeader = TRUE,
             width = NULL,
             uiOutput(ns("measurement_ui"))
           )
    )
  )
}

operator_qc_server <- function(id, conn, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    standards_data <- reactiveVal(NULL)
    recap_data <- reactiveVal(NULL)
    rework_info <- reactiveVal(NULL)
    reset_ui_signal <- reactiveVal(0)
    
    observe({
      tahap_choices <- dbGetQuery(conn, "SELECT DISTINCT tahap FROM qc_standart ORDER BY tahap")
      updateSelectInput(session, "tahap", choices = tahap_choices$tahap)
    })
    
    main_trigger <- eventReactive(input$part_number, {
      req(input$tahap, input$part_number != "")
      input$part_number
    })
    
    observeEvent(main_trigger(), {
      pn <- main_trigger()
      tahap <- input$tahap
      query <- "SELECT qc_id, hasil, tahap FROM qc_inspeksi WHERE part_number = $1 ORDER BY waktu_inspeksi DESC LIMIT 1"
      existing_part <- dbGetQuery(conn, query, params = list(pn))
      
      if(nrow(existing_part) > 0) {
        last_tested_stage <- existing_part$tahap
        if (last_tested_stage == tahap) {
          rework_info(list(qc_id = existing_part$qc_id, last_result = existing_part$hasil, is_rework = TRUE))
          showModal(modalDialog(
            title = "Peringatan: Part Sudah Pernah Diinspeksi",
            paste0("Part '", pn, "' sudah diuji di tahap ini dengan hasil: ", existing_part$hasil, "."),
            footer = tagList(modalButton("Batalkan"), actionButton(ns("proceed_btn"), "Ya, Lakukan Rework", class = "btn-warning"))
          ))
        } else {
          rework_info(list(is_rework = FALSE))
          showModal(modalDialog(
            title = "Informasi: Part Memiliki Histori",
            paste0("Part '", pn, "' terakhir kali diuji di tahap '", last_tested_stage, "' dengan hasil: ", existing_part$hasil, "."),
            footer = tagList(modalButton("Batalkan"), actionButton(ns("proceed_btn"), "Ya, Lanjutkan", class = "btn-success"))
          ))
        }
      } else {
        rework_info(NULL)
        build_measurement_ui()
      }
    })
    
    observeEvent(input$proceed_btn, {
      removeModal()
      build_measurement_ui()
    })
    
    build_measurement_ui <- function() {
      tahap <- input$tahap
      pn <- input$part_number
      query <- "SELECT * FROM qc_standart WHERE tahap = $1 ORDER BY standart_id"
      standards <- dbGetQuery(conn, query, params = list(tahap))
      standards_data(standards)
      
      output$rework_status_ui <- renderUI({
        info <- rework_info()
        if(!is.null(info) && isTRUE(info$is_rework)) {
          div(class = "alert alert-warning", role = "alert", strong("Mode Rework:"), paste("Menginspeksi ulang part", pn))
        } else { NULL }
      })
      
      output$reference_image_ui <- renderUI({
        mts_stages <- c("MTS-Roller Matic", "MTS-Wire EDM", "Poles Finish", "MTS-Poles 2 Finish")
        
        if (tahap == "OTS") {
          div(id = ns("ots_image_wrapper"), style = "cursor: pointer;",
              div(h4("OTS Measurement"), img(src = "ots_drawing.jpg", width = "70%", class = "img-fluid"))
          )
        } else if (tahap %in% mts_stages) {
          div(id = ns("mts_image_wrapper"), style = "cursor: pointer;",
              div(h4("MTS Measurement"), img(src = "mts_drawing.jpg", width = "70%", class = "img-fluid"))
          )
        }
      })
      
      output$measurement_ui <- renderUI({
        tagList(
          h4(paste("Measurement for:", pn)),
          lapply(1:nrow(standards), function(i) {
            std <- standards[i, ]
            div(class = "measurement-row", style="padding: 10px; border-bottom: 1px solid #eee;",
                fluidRow(
                  column(12, strong(std$parameter), p(em(paste("Alat Ukur:", std$alat_ukur, "| Standar:", ifelse(is.na(std$batas_min), "N/A", paste(std$batas_min, "-", std$batas_maks))))))),
                fluidRow(
                  column(3, numericInput(ns(paste0("val_", std$standart_id, "_0")), "Nilai 0°", value = NA)),
                  column(3, numericInput(ns(paste0("val_", std$standart_id, "_45")), "Nilai 45°", value = NA)),
                  column(3, numericInput(ns(paste0("val_", std$standart_id, "_90")), "Nilai 90°", value = NA)),
                  column(3, numericInput(ns(paste0("val_", std$standart_id, "_135")), "Nilai 135°", value = NA))
                )
            )
          }),
          actionButton(ns("recap_btn"), "Simpan & Tampilkan Rekap", class = "btn-success btn-lg mt-3 w-100")
        )
      })
    }
    
    observeEvent(input$recap_btn, {
      standards <- standards_data()
      req(standards)
      tryCatch({
        is_overall_pass <- TRUE
        pengukuran_results <- list()
        recap_ui_list <- list()
        for (i in 1:nrow(standards)) {
          std <- standards[i, ]
          sid <- std$standart_id
          vals <- c(input[[paste0("val_", sid, "_0")]], input[[paste0("val_", sid, "_45")]], input[[paste0("val_", sid, "_90")]], input[[paste0("val_", sid, "_135")]])
          statuses <- sapply(vals, function(v) validate_measurement(v, std$batas_min, std$batas_maks))
          is_param_pass <- all(statuses == "Pass")
          if (!is_param_pass) is_overall_pass <- FALSE
          pengukuran_results[[i]] <- list(standart_id = sid, nilai_0_deg = vals[1], nilai_45_deg = vals[2], nilai_90_deg = vals[3], nilai_135_deg = vals[4], status_0_deg = statuses[1], status_45_deg = statuses[2], status_90_deg = statuses[3], status_135_deg = statuses[4], overall_status = ifelse(is_param_pass, "Pass", "Fail"))
          recap_ui_list[[i]] <- div(style="margin-bottom: 10px;",
                                    tags$b(std$parameter),
                                    div(
                                      lapply(1:4, function(j) {
                                        angle <- c("0°", "45°", "90°", "135°")[j]
                                        val <- vals[j]
                                        status <- statuses[j]
                                        style <- if(status == "Fail") "color: red; font-weight: bold;" else ""
                                        display_val <- if(is.na(val)) "" else val
                                        span(style=style, paste0(angle, ": ", display_val), " (", status, ")", style="margin-right: 15px;")
                                      })
                                    )
          )
        }
        final_result <- ifelse(is_overall_pass, "PASS", "NOT GOOD")
        recap_data(list(results = pengukuran_results, final_status = final_result))
        showModal(modalDialog(
          title = "Rekapitulasi & Konfirmasi Inspeksi",
          tagList(
            h4(paste("Part Number:", input$part_number)),
            p("Silakan periksa kembali data di bawah ini."),
            hr(), recap_ui_list, hr(),
            h3(paste("Hasil Akhir:", final_result), style = if(final_result == "NOT GOOD") "color: red;" else "color: green;")
          ),
          footer = tagList(modalButton("Batal"), actionButton(ns("confirm_save_btn"), "Konfirmasi & Simpan", class = "btn-success")),
          easyClose = TRUE,
          size = "l"
        ))
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 8)
      })
    })
    
    observeEvent(input$confirm_save_btn, {
      req(recap_data())
      removeModal()
      dbBegin(conn)
      tryCatch({
        data_to_save <- recap_data()
        info <- rework_info()
        if (!is.null(info) && isTRUE(info$is_rework)) {
          qc_id_to_update <- info$qc_id
          update_inspeksi_query <- "UPDATE qc_inspeksi SET hasil = $1, nama_operator = $2, waktu_inspeksi = NOW() WHERE qc_id = $3"
          dbExecute(conn, update_inspeksi_query, params = list(data_to_save$final_status, user_info()$nama, qc_id_to_update))
          dbExecute(conn, "DELETE FROM qc_pengukuran WHERE qc_id = $1", params = list(qc_id_to_update))
          new_qc_id <- qc_id_to_update
        } else {
          inspeksi_query <- "INSERT INTO qc_inspeksi (part_number, tahap, nama_operator, hasil) VALUES ($1, $2, $3, $4) RETURNING qc_id"
          # ====================================================================
          # PERBAIKAN DI SINI
          # ====================================================================
          res <- dbGetQuery(conn, inspeksi_query, params = list(input$part_number, input$tahap, user_info()$nama, data_to_save$final_status))
          # ====================================================================
          new_qc_id <- res$qc_id[1]
        }
        for (result in data_to_save$results) {
          pengukuran_query <- "INSERT INTO qc_pengukuran (qc_id, standart_id, nilai_0_deg, nilai_45_deg, nilai_90_deg, nilai_135_deg, status_0_deg, status_45_deg, status_90_deg, status_135_deg, overall_status) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)"
          dbExecute(conn, pengukuran_query, params = unlist(c(qc_id = new_qc_id, result), use.names = FALSE))
        }
        dbCommit(conn)
        showNotification(paste("Data berhasil disimpan. Hasil:", data_to_save$final_status), type = "message", duration = 5)
        reset_all_inputs()
      }, error = function(e){
        dbRollback(conn)
        showNotification(paste("Gagal menyimpan ke database:", e$message), type = "error", duration = 8)
      })
    })
    
    reset_all_inputs <- function() {
      updateTextInput(session, "part_number", value = "")
      output$measurement_ui <- renderUI(NULL)
      output$reference_image_ui <- renderUI(NULL)
      output$rework_status_ui <- renderUI(NULL)
      standards_data(NULL)
      recap_data(NULL)
      rework_info(NULL)
    }
    
    observeEvent(input$refresh_btn, {
      reset_all_inputs()
      showNotification("Aplikasi disegarkan.", type = "message", duration = 3)
    })
    
    shinyjs::onclick("ots_image_wrapper", {
      showModal(modalDialog(
        title = "Gambar Referensi OTS",
        img(src = "ots_drawing.jpg", width = "100%"),
        footer = modalButton("Tutup"),
        size = "l",
        easyClose = TRUE
      ))
    })
    
    shinyjs::onclick("mts_image_wrapper", {
      showModal(modalDialog(
        title = "Gambar Referensi MTS",
        img(src = "mts_drawing.jpg", width = "100%"),
        footer = modalButton("Tutup"),
        size = "l",
        easyClose = TRUE
      ))
    })
    
    return(list(
      reset_ui = reset_ui_signal
    ))
  })
}