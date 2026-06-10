#' @title restore_legacy_pages_structure
#' 
#' @description
#' Acts as a backward-compatibility adapter. It takes the Tidy format where 
#' document pages are stored in a single "pages" array, and spreads them 
#' back into individual variables ("text_page_1", "text_page_2", etc.). 
#' This ensures downstream scripts that rely on the legacy structure 
#' continue to function without modification.
#'
#' @param data_list A list containing raw article data with a "pages" array.
#' 
#' @return A list with the legacy "text_page_X" structure restored and 
#'   the "pages" array removed.

restore_legacy_pages_structure <- function(data_list) {
  
  if (!is.list(data_list)) {
    return(data_list)
  }
  
  # Apply the transformation to each article in the list
  res <- lapply(data_list, function(article) {
    if (is.list(article)) {
      pages_array <- article$pages
      
      # If the "pages" array exists and has content
      if (!is.null(pages_array) && length(pages_array) > 0) {
        # Create dynamically text_page_1, text_page_2, etc.
        for (i in seq_along(pages_array)) {
          article[[paste0("text_page_", i)]] <- pages_array[[i]]
        }
      }
      
      # Remove the "pages" element to strictly match legacy format
      article$pages <- NULL
    }
    
    return(article)
  })
  
  return(res)
}

# Apply the adapter process to the raw data (overwriting it in memory)
raw_data_list <- restore_legacy_pages_structure(raw_data_list)