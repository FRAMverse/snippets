## function to compare two vectors and print which entries are present in one and missing in the other.0
check_mismatch = function(x, y, both = TRUE){
  cat("Present only in first vector:\n   ", 
      paste0(unique(x)[!unique(x) %in% y], collapse = ", "),
      "\n")
  cat("Present only in second vector:\n   ", 
      paste0(unique(y)[!unique(y) %in% x], collapse = ", "),
      "\n")
  if(both){
    cat("Present in both vectors:\n   ", 
        paste0(intersect(x, y), collapse = ", "),
        "\n")
  }
}

check_mismatch(1:10, 2:11)
