# codes zeros to "-" and other to "+"
codeAsChar <- function(x) {
  ifelse(x == 0, "-", "   +")
}

# codes NAs to zeros and others to 1
naZero <- function(x) {
  ifelse(is.na(x), 0, (x > 0) * 1)
}
