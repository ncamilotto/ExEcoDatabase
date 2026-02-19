#' @title Add Last Characters Snippet
#' 
#' @description
#' Iterates over a data list and extracts the last characters from the "text" 
#' field of each item, storing them in a new "last_chars_snippet" field. 
#' It skips items whose names end with a specific suffix and handles missing 
#' or empty text values.
#'
#' @param data_list A list where each element is expected to be a list containing 
#'  at least a "text" field.
#' @param n_chars An integer specifying the number of characters to extract 
#'  from the end of the text. Defaults to 100.
#' @param exclude_suffix A character string representing the suffix of names to exclude. 
#'  Items with names ending in this suffix will not be processed. Defaults to "_first_page".
#' 
#' @return A list containing the original items, updated with the "last_chars_snippet" 
#'  field for applicable elements (set to NA if the text is missing or the item was excluded).

add_last_chars_snippet <- function(data_list, n_chars = 100, exclude_suffix = "_first_page") {
  
  # Iterate over the list using imap to access both the item (.x) and its name (.y)
  imap(data_list, function(item, name) {
    
    # Check if the element name does NOT end with the excluded suffix
    if (!str_ends(name, exclude_suffix)) {
      current_text <- item$text
      
      # Ensure the text is neither NULL nor NA before processing
      if (!is.null(current_text) && !is.na(current_text)) {
        # Extract the last 'n_chars' from the text
        item[["last_chars_snippet"]] <- str_sub(current_text, -n_chars, -1)
      } else {
        # Assign NA if text is missing
        item[["last_chars_snippet"]] <- NA
      }
    }
    
    return(item)
  })
}

journal_filtered_extracted_pages <- add_last_chars_snippet(journal_filtered_extracted_pages)
