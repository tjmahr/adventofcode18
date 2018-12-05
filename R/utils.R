
#' @export
read_text_lines <- function(x) {
  x %>%
    strsplit("\\n") %>%
    unlist() %>%
    stringr::str_trim() %>%
    keep_if(function(x) x != "")
}

keep_if <- function(data, predicate) {
  Filter(predicate, data)
}
