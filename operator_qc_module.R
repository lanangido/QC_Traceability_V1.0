# operator_qc_module.R (Versi Final dengan Report Format Long)

operator_qc_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 4,
           box(title = "Kontrol Inspeksi", solidHeader = TRUE, width = NULL,
               selectInput(ns("tahap"), "Pilih Tahap Inspeksi:", choices = NULL),
               textInput(ns("part_number"), "Scan Barcode Part (Tekan Enter)"),
               actionButton(ns("refresh_btn"), "Refresh Form", icon = icon("sync"), class = "btn-custom-black w-100")
           ),
           uiOutput(ns("rework_status_ui")),
           box(title = "Gambar Referensi", solidHeader = TRUE, width = NULL, collapsible = TRUE, uiOutput(ns("reference_image_ui")))
    ),
    column(width = 8,
           box(title = "Input Pengukuran", solidHeader = TRUE, width = NULL,
               uiOutput(ns("measurement_ui"))
           )
    )
  )
}

validate_measurement <- function(value, min_val, max_val) {
  value <- as.numeric(value); min_val <- as.numeric(min_val); max_val <- as.numeric(max_val)
  if (is.na(value)) return("Fail")
  if (is.na(min_val) || is.na(max_val)) return("Pass")
  if (value >= min_val && value <= max_val) return("Pass") else return("Fail")
}

operator_qc_server <- function(id, conn, user_info, product_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    standards_data <- reactiveVal(NULL); recap_data <- reactiveVal(NULL); rework_info <- reactiveVal(NULL)
    historical_report_data <- reactiveVal(NULL); previous_measurements <- reactiveVal(NULL)
    
    observe({
      req(product_info())
      
      # Ambil tahapan dari DB dan langsung urutkan berdasarkan kolom baru
      query <- "SELECT DISTINCT tahap, urutan_tahap FROM qc_standart ORDER BY urutan_tahap, tahap"
      tahap_choices <- dbGetQuery(conn, query)
      
      updateSelectInput(session, "tahap", choices = tahap_choices$tahap)
    })
    
    main_trigger <- eventReactive(input$part_number, { req(input$tahap, input$part_number != ""); input$part_number })
    
    observeEvent(main_trigger(), {
      pn <- main_trigger(); tahap <- input$tahap; prod_id <- product_info()$id
      query <- "SELECT qc_id, hasil, tahap FROM qc_inspeksi WHERE part_number = $1 AND product_id = $2 ORDER BY waktu_inspeksi DESC LIMIT 1"
      existing_part <- dbGetQuery(conn, query, params = list(pn, prod_id))
      
      if(nrow(existing_part) > 0) {
        last_tested_stage <- existing_part$tahap
        if (last_tested_stage == tahap) {
          rework_info(list(qc_id = existing_part$qc_id, last_result = existing_part$hasil, is_rework = TRUE))
          showModal(modalDialog(title = "Peringatan Rework", paste0("Part '", pn, "' sudah diuji di tahap ini dengan hasil: ", existing_part$hasil, ". Data sebelumnya akan dimuat."), footer = tagList(modalButton("Batalkan"), actionButton(ns("proceed_btn"), "Ya, Lakukan Rework", class = "btn-warning"))))
        } else {
          rework_info(list(is_rework = FALSE))
          showModal(modalDialog(title = "Informasi", paste0("Part '", pn, "' terakhir kali diuji di tahap '", last_tested_stage, "' dengan hasil: ", existing_part$hasil, "."), footer = tagList(modalButton("Batalkan"), actionButton(ns("proceed_btn"), "Ya, Lanjutkan", class = "btn-success"))))
        }
      } else {
        rework_info(NULL); build_measurement_ui()
      }
    })
    
    observeEvent(input$proceed_btn, { removeModal(); build_measurement_ui() })
    
    # Ganti seluruh fungsi build_measurement_ui dengan kode final ini
    build_measurement_ui <- function() {
      tahap <- input$tahap; pn <- input$part_number; info <- rework_info()
      if (!is.null(info) && isTRUE(info$is_rework)) {
        prev_data_query <- "SELECT * FROM qc_pengukuran_detail WHERE inspeksi_id = $1"
        prev_data <- dbGetQuery(conn, prev_data_query, params = list(info$qc_id))
        previous_measurements(prev_data)
      } else {
        previous_measurements(NULL)
      }
      query <- "SELECT * FROM qc_standart WHERE tahap = $1 ORDER BY standart_id"
      standards <- dbGetQuery(conn, query, params = list(tahap))
      standards_data(standards)
      
      output$rework_status_ui <- renderUI({
        info <- rework_info()
        if(!is.null(info) && isTRUE(info$is_rework)) {
          div(class = "alert alert-warning", role = "alert", style="font-weight:bold;", icon("wrench"), " MODE REWORK:", paste("Menginspeksi ulang part", pn))
        } else { NULL }
      })
      
      output$reference_image_ui <- renderUI({
        mts_stages <- c("MTS-Roller Matic", "MTS-Wire EDM", "Poles Finish", "MTS-Poles 2 Finish")
        if (tahap == "OTS") {
          div(id = ns("ots_image_wrapper"), style = "cursor: pointer;", div(h4("OTS Measurement"), img(src = "ots_drawing.jpg", width = "70%", class = "img-fluid")))
        } else if (tahap %in% mts_stages) {
          div(id = ns("mts_image_wrapper"), style = "cursor: pointer;", div(h4("MTS Measurement"), img(src = "mts_drawing.jpg", width = "70%", class = "img-fluid")))
        }
      })
      
      output$measurement_ui <- renderUI({
        prev_data <- previous_measurements()
        collapsible_boxes <- lapply(1:nrow(standards), function(i) {
          std <- standards[i, ]; panel_title <- std$alat_ukur; counts <- 1:std$measure_count
          degrees <- if (!is.na(std$measure_degrees)) as.numeric(unlist(strsplit(std$measure_degrees, ","))) else NA
          positions <- if (!is.na(std$measure_positions)) unlist(strsplit(std$measure_positions, ",")) else NA
          points <- if (!is.na(std$measure_points)) unlist(strsplit(std$measure_points, ",")) else NA
          
          panel_content <- tagList(
            lapply(counts, function(c) {
              div(class="measurement-group", style="margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                  if(length(counts) > 1) tags$h5(paste("Pengukuran Ke-", c), style="font-weight:bold;"),
                  if (!is.na(degrees[1]) && !is.na(positions[1])) {
                    lapply(degrees, function(d) {
                      fluidRow(tags$b(paste("Sudut", d, "°"), style="margin-left: 10px; margin-bottom: 5px;"), lapply(positions, function(p) {
                        input_id <- ns(paste("val", std$standart_id, c, d, p, sep="_"))
                        default_val <- NA
                        if(!is.null(prev_data)) {
                          val_row <- prev_data %>% filter(standart_id == std$standart_id, urutan_pengukuran == c, derajat == d, posisi == p)
                          if(nrow(val_row) > 0) default_val <- val_row$nilai[1]
                        }
                        column(4, div(class="validatable-input", `data-min`=std$batas_min, `data-max`=std$batas_maks, numericInput(input_id, p, value = default_val)))
                      }))
                    })
                  } else if (!is.na(degrees[1])) {
                    fluidRow(lapply(degrees, function(d) {
                      input_id <- ns(paste("val", std$standart_id, c, d, sep="_"))
                      default_val <- NA
                      if(!is.null(prev_data)) {
                        val_row <- prev_data %>% filter(standart_id == std$standart_id, urutan_pengukuran == c, derajat == d)
                        if(nrow(val_row) > 0) default_val <- val_row$nilai[1]
                      }
                      column(3, div(class="validatable-input", `data-min`=std$batas_min, `data-max`=std$batas_maks, numericInput(input_id, paste(d, "°"), value = default_val)))
                    }))
                  } else if (!is.na(points[1])) {
                    fluidRow(lapply(points, function(pt) {
                      input_id <- ns(paste("val", std$standart_id, c, gsub(" ", "_", pt), sep="_"))
                      default_val <- NA
                      if(!is.null(prev_data)) {
                        val_row <- prev_data %>% filter(standart_id == std$standart_id, urutan_pengukuran == c, poin_ukur == pt)
                        if(nrow(val_row) > 0) default_val <- val_row$nilai[1]
                      }
                      column(6, div(class="validatable-input", `data-min`=std$batas_min, `data-max`=std$batas_maks, numericInput(input_id, pt, value = default_val)))
                    }))
                  } else {
                    input_id <- ns(paste("val", std$standart_id, c, sep="_"))
                    default_val <- NA
                    if(!is.null(prev_data)) {
                      val_row <- prev_data %>% filter(standart_id == std$standart_id, urutan_pengukuran == c, is.na(derajat), (is.na(posisi) | posisi == ''), (is.na(poin_ukur) | poin_ukur == ''))
                      if(nrow(val_row) > 0) default_val <- val_row$nilai[1]
                    }
                    div(class="validatable-input", `data-min`=std$batas_min, `data-max`=std$batas_maks, numericInput(input_id, "Nilai", value = default_val))
                  }
              )
            })
          )
          shinydashboardPlus::box(
            title = tagList(
              panel_title,
              tags$small(style = "display:block; font-weight:normal; font-style:italic;", paste("Standar:", ifelse(is.na(std$batas_min), "N/A", paste(std$batas_min, "-", std$batas_maks))))
            ),
            width = 12, solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE, panel_content
          )
        })
        tagList(
          h4(paste("Measurement for:", pn)),
          collapsible_boxes,
          actionButton(ns("recap_btn"), "Simpan & Tampilkan Rekap", class = "btn-success btn-lg mt-3 w-100")
        )
      })
      
      # [KODE FINAL DEFINITIF] JavaScript dengan logika yang disempurnakan
      js_code <- "
        // Pisahkan logika validasi menjadi fungsi sendiri agar bisa dipakai ulang
        function validate(element) {
            var input_element = $(element);
            var value_str = String(input_element.val()).replace(',', '.');
            var value = parseFloat(value_str);
            
            var wrapper = input_element.closest('.validatable-input');
            var min_str = String(wrapper.data('min')).replace(',', '.');
            var min = parseFloat(min_str);
            var max_str = String(wrapper.data('max')).replace(',', '.');
            var max = parseFloat(max_str);
            
            var error_style = {'border-color': '#e94560', 'background-color': '#fff0f0', 'box-shadow': '0 0 5px rgba(233, 69, 96, 0.5)'};
            var normal_style = {'border-color': '', 'background-color': '', 'box-shadow': ''};

            if (isNaN(value)) {
                input_element.css(normal_style);
                return;
            }
            
            if (isNaN(min) || isNaN(max)) {
                input_element.css(error_style); // Jadi merah jika standar tidak ada tapi nilai ada
                return;
            }

            if (value < min || value > max) {
                input_element.css(error_style);
            } else {
                input_element.css(normal_style);
            }
        }

        setTimeout(function(){ 
            // 1. Jalankan validasi untuk SEMUA input yang ada saat form dimuat
            $('.validatable-input input[type=\"number\"]').each(function() {
                validate(this);
            });

            // 2. Pasang listener untuk validasi saat input manual
            $(document).off('input change keyup', '.validatable-input input[type=\"number\"]');
            $(document).on('input change keyup', '.validatable-input input[type=\"number\"]', function() {
                validate(this);
            });
        }, 500);
      "
      shinyjs::runjs(js_code)
    }
    
    # [DIUBAH] Logika Tombol Rekap disederhanakan
    observeEvent(input$recap_btn, {
      standards <- standards_data(); req(standards)
      tryCatch({
        all_results <- list(); is_overall_pass <- TRUE; recap_ui_list <- list()
        # Proses validasi input saat ini (tidak berubah)
        for (i in 1:nrow(standards)) {
          std <- standards[i, ]; is_param_pass <- TRUE; param_results_display <- list()
          counts <- 1:std$measure_count
          degrees <- if (!is.na(std$measure_degrees)) as.numeric(unlist(strsplit(std$measure_degrees, ","))) else NA
          positions <- if (!is.na(std$measure_positions)) unlist(strsplit(std$measure_positions, ",")) else NA
          points <- if (!is.na(std$measure_points)) unlist(strsplit(std$measure_points, ",")) else NA
          for (c in counts) {
            if (!is.na(degrees[1]) && !is.na(positions[1])) {
              for (d in degrees) { for (p in positions) { input_id <- paste("val", std$standart_id, c, d, p, sep="_"); value <- input[[input_id]]; if (is.null(value) || is.na(value)) stop(paste("Nilai untuk", std$parameter, "| Pengukuran", c, "|", d, "° |", p, "harus diisi.")); status <- validate_measurement(value, std$batas_min, std$batas_maks); if (status == 'Fail') is_param_pass <- FALSE; all_results <- append(all_results, list(list(sid=std$standart_id, c=c, d=d, p=p, pt=NA, val=value, stat=status))); param_results_display <- append(param_results_display, list(paste0("P",c,"-",d,"°/",p,": ", value, " (",status,")"))) }}
            } else if (!is.na(points[1])) {
              for (pt in points) { input_id <- paste("val", std$standart_id, c, gsub(" ", "_", pt), sep="_"); value <- input[[input_id]]; if (is.null(value) || is.na(value)) stop(paste("Nilai untuk", std$parameter, "| Pengukuran", c, "|", pt, "harus diisi.")); status <- validate_measurement(value, std$batas_min, std$batas_maks); if (status == 'Fail') is_param_pass <- FALSE; all_results <- append(all_results, list(list(sid=std$standart_id, c=c, d=NA, p=NA, pt=pt, val=value, stat=status))); param_results_display <- append(param_results_display, list(paste0(pt,": ", value, " (",status,")"))) }
            } else { input_id <- paste("val", std$standart_id, c, sep="_"); value <- input[[input_id]]; if (is.null(value) || is.na(value)) stop(paste("Nilai untuk", std$parameter, "| Pengukuran", c, "harus diisi.")); status <- validate_measurement(value, std$batas_min, std$batas_maks); if (status == 'Fail') is_param_pass <- FALSE; all_results <- append(all_results, list(list(sid=std$standart_id, c=c, d=NA, p=NA, pt=NA, val=value, stat=status))); param_results_display <- append(param_results_display, list(paste0("Nilai: ", value, " (",status,")"))) }
          }
          if (!is_param_pass) is_overall_pass <- FALSE
          recap_ui_list[[i]] <- div(tags$b(std$alat_ukur), p(paste(param_results_display, collapse=" | ")))
        }
        final_result <- ifelse(is_overall_pass, "PASS", "NOT GOOD")
        recap_data(list(results = all_results, final_status = final_result))
        
        # Logika baru untuk mengambil data historis dalam format LONG
        current_pn <- input$part_number
        query <- "
          SELECT
              p.product_name, i.tahap, i.part_number, i.waktu_inspeksi, i.nama_operator,
              s.parameter, s.alat_ukur, s.batas_min, s.batas_maks,
              d.nilai, d.status, d.urutan_pengukuran, d.derajat, d.posisi, d.poin_ukur
          FROM qc_inspeksi i
          JOIN qc_pengukuran_detail d ON i.qc_id = d.inspeksi_id
          JOIN qc_standart s ON d.standart_id = s.standart_id
          JOIN products p ON i.product_id = p.product_id
          WHERE i.part_number = $1
        "
        historical_data <- dbGetQuery(conn, query, params = list(current_pn))
        
        if (nrow(historical_data) > 0) {
          stage_order <- c("OTS", "MTS-Roller Matic", "MTS-Wire EDM", "MTS - Poles Finish", "MTS-Poles 2 Finish")
          
          final_report <- historical_data %>%
            mutate(tahap = factor(tahap, levels = stage_order, ordered = TRUE)) %>%
            arrange(tahap, waktu_inspeksi, parameter, urutan_pengukuran, derajat, posisi)
          
          historical_report_data(final_report)
        } else {
          historical_report_data(NULL)
        }
        
        # Menampilkan modal
        showModal(modalDialog(
          title = "Rekapitulasi & Konfirmasi Inspeksi",
          tagList(h4(paste("Part Number:", input$part_number)), hr(), recap_ui_list, hr(), h3(paste("Hasil Akhir:", final_result), style = if(final_result == "NOT GOOD") "color: red;" else "color: green;")),
          footer = tagList(
            modalButton("Batal"),
            downloadButton(ns("download_historical_report"), "Unduh Report Historis (CSV)", class = "btn-info"),
            actionButton(ns("confirm_save_btn"), "Konfirmasi & Simpan", class="btn-success")
          )
        ))
        
      }, error = function(e) {
        showNotification(paste("Peringatan:", e$message), type = "warning", duration = 5)
      })
    })
    
    # observeEvent untuk confirm_save_btn tidak berubah
    observeEvent(input$confirm_save_btn, {
      req(recap_data()); removeModal(); dbBegin(conn)
      tryCatch({
        data_to_save <- recap_data(); info <- rework_info(); prod_id <- product_info()$id
        if (!is.null(info) && isTRUE(info$is_rework)) {
          new_inspeksi_id <- info$qc_id
          dbExecute(conn, "DELETE FROM qc_pengukuran_detail WHERE inspeksi_id = $1", params=list(new_inspeksi_id))
          update_inspeksi_query <- "UPDATE qc_inspeksi SET hasil = $1, nama_operator = $2, waktu_inspeksi = NOW() WHERE qc_id = $3"
          dbExecute(conn, update_inspeksi_query, params = list(data_to_save$final_status, user_info()$nama, new_inspeksi_id))
        } else {
          inspeksi_query <- "INSERT INTO qc_inspeksi (part_number, tahap, nama_operator, hasil, product_id) VALUES ($1, $2, $3, $4, $5) RETURNING qc_id"
          res <- dbGetQuery(conn, inspeksi_query, params = list(input$part_number, input$tahap, user_info()$nama, data_to_save$final_status, prod_id))
          new_inspeksi_id <- res$qc_id[1]
        }
        for (res in data_to_save$results) {
          query <- "INSERT INTO qc_pengukuran_detail (inspeksi_id, standart_id, urutan_pengukuran, derajat, posisi, poin_ukur, nilai, status) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
          dbExecute(conn, query, params=list(new_inspeksi_id, res$sid, res$c, res$d, res$p, res$pt, res$val, res$stat))
        }
        dbCommit(conn); showNotification("Data berhasil disimpan.", type="message")
        reset_all_inputs()
      }, error = function(e) {
        dbRollback(conn); showNotification(paste("Gagal menyimpan:", e$message), type="error")
      })
    })
    
    # Download handler untuk report historis
    output$download_historical_report <- downloadHandler(
      filename = function() {
        paste0("Report_Historis_", input$part_number, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        data_to_download <- historical_report_data()
        if (is.null(data_to_download) || nrow(data_to_download) == 0) {
          write.csv(data.frame(Pesan="Tidak ada data historis untuk part number ini."), file, row.names = FALSE)
        } else {
          write.csv(data_to_download, file, row.names = FALSE, na = "")
        }
      }
    )
    
    reset_all_inputs <- function() {
      updateTextInput(session, "part_number", value = "")
      output$measurement_ui <- renderUI(NULL); output$reference_image_ui <- renderUI(NULL); output$rework_status_ui <- renderUI(NULL)
      standards_data(NULL); recap_data(NULL); rework_info(NULL); historical_report_data(NULL)
    }
    
    observeEvent(input$refresh_btn, { reset_all_inputs() })
    
    shinyjs::onclick("ots_image_wrapper", {
      showModal(modalDialog(title = "Gambar Referensi OTS", img(src = "ots_drawing.jpg", width = "100%"), footer = modalButton("Tutup"), size = "l", easyClose = TRUE))
    })
    
    shinyjs::onclick("mts_image_wrapper", {
      showModal(modalDialog(title = "Gambar Referensi MTS", img(src = "mts_drawing.jpg", width = "100%"), footer = modalButton("Tutup"), size = "l", easyClose = TRUE))
    })
    
  })
}