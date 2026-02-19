#' @title define_reconstruct_hyphenated_words_function
#'
#' @description 
#' Detects and repairs words that have been split by hyphens, typically occurring 
#' at the end of lines in PDF documents or OCR outputs (e.g., transforming 
#' "dic- tionnaire" into "dictionnaire").
#' 
#' The function handles two cases:
#' 1. Hyphens followed by spaces.
#' 2. Hyphens followed by line breaks (newlines).
#'
#' It specifically targets **lowercase letters** (including French accented characters) 
#' to avoid accidentally merging proper compound words or capitalized acronyms.
#'
#' @param text A character vector to be processed.
#'
#' @return A character vector with hyphenated words merged back together.
#'
#' @export

clean_hyphenation  <- function(text) {
  
  # Simple hyphenation on space
  text <- str_replace_all(text, "([a-zéèêëàâäôöùûüïîç])-\\s+([a-zéèêëàâäôöùûüïîç])", "\\1\\2")
  
   # Complex hyphenation over line breaks (newlines, spaces, tabs)
  text <- str_replace_all(text, "([a-zéèêëàâäôöùûüïîç])-\\s*\\n+(\\s*\\n+)*\\s*([a-zéèêëàâäôöùûüïîç])", "\\1\\3")
  
  return(text)
}