#' Convert encoding of a \code{data.frame}.
#'
#' @param x Data.frame to convert
#' @param from_encoding Character string describing the current encoding.
#' @param to_encoding Character string describing the target encoding.
#' @return A data.frame in the target encoding.
#'
#' @examples
#' tab_cgl <- data.frame(a = c('encodings', 'nightmare'), b = c('ñ', 'o'))
#' convertEncoding(tab_cgl)
#'
#' @importFrom magrittr %>%
#' @export
convertEncoding <- function(x, from_encoding = "UTF-8", to_encoding = "ISO-8859-1") {
    my_names <- iconv(names(x), from_encoding, to_encoding)
    # if any column name is NA, leave the names otherwise replace them with new names
    if (any(is.na(my_names))) {
        names(x)
    } else {
        names(x) <- my_names
    }
    
    # get column classes
    x_char_columns <- sapply(x, class)
    # identify character columns
    x_cols <- names(x_char_columns[x_char_columns == "character"])
    
    # convert all string values in character columns to specified encoding
    x <- x %>% dplyr::mutate_each_(dplyr::funs(iconv(., from_encoding, to_encoding)), 
        x_cols)
    # return x
    return(x)
}
