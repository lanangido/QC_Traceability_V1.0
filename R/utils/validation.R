# R/utils/validation.R
validate_measurement <- function(value, min_val, max_val) {
  # Mengembalikan "Pass" atau "Fail"
  if (is.na(value)) return("Fail")
  # Jika standar min/max tidak ada (NULL), anggap Pass
  if (is.na(min_val) || is.na(max_val)) return("Pass")
  
  if (value >= min_val && value <= max_val) {
    return("Pass")
  } else {
    return("Fail")
  }
}