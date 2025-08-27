# kelola_user.R

# Muat library yang dibutuhkan
library(DBI)
library(RPostgres)
library(sodium)
library(getPass) # Untuk input password yang aman

# --- KONFIGURASI DATABASE (Salin dari global.R) ---
db_config <- list(
  dbname = "produksi_qc_v1",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "admin123" 
)

# Fungsi untuk koneksi ke database
get_db_conn <- function() {
  tryCatch({
    dbConnect(RPostgres::Postgres(),
              dbname = db_config$dbname, host = db_config$host,
              port = db_config$port, user = db_config$user,
              password = db_config$password)
  }, error = function(e) {
    stop("Gagal terhubung ke database: ", e$message)
  })
}

# --- FUNGSI UTAMA ---

# Fungsi untuk menambah user baru
tambah_user_baru <- function() {
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  
  email <- readline("Masukkan email (username) baru: ")
  nama <- readline("Masukkan nama lengkap: ")
  role <- readline("Masukkan role (misal: operator): ")
  password <- getPass("Masukkan password baru: ")
  
  # Lakukan hashing pada password
  hashed_password <- password_store(password)
  
  query <- "INSERT INTO operator (email, nama, role, password) VALUES ($1, $2, $3, $4)"
  dbExecute(conn, query, params = list(email, nama, role, hashed_password))
  
  cat("User", nama, "berhasil ditambahkan. ✅\n")
}

# Fungsi untuk meng-update password user yang sudah ada
update_password_user <- function() {
  conn <- get_db_conn()
  on.exit(dbDisconnect(conn))
  
  email <- readline("Masukkan email (username) user yang akan di-update: ")
  password <- getPass("Masukkan password BARU untuk user ini: ")
  
  # Lakukan hashing pada password baru
  hashed_password <- password_store(password)
  
  query <- "UPDATE operator SET password = $1 WHERE email = $2"
  result <- dbExecute(conn, query, params = list(hashed_password, email))
  
  if (result > 0) {
    cat("Password untuk user", email, "berhasil di-update. ✅\n")
  } else {
    cat("User dengan email", email, "tidak ditemukan. ❌\n")
  }
}

# --- CARA PENGGUNAAN ---
# 1. Buka file ini di RStudio.
# 2. Jalankan seluruh skrip ini (Ctrl+A lalu Ctrl+Enter).
# 3. Di Console, panggil fungsinya:
#    - Untuk menambah user: tambah_user_baru()
#    - Untuk update password: update_password_user()
# 4. PENTING: Gunakan fungsi update_password_user() untuk semua user Anda yang ada saat ini
#    agar password mereka diubah dari plain text menjadi hash.