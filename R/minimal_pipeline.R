#' @title Common Minimal Pipeline
#' 
#' @description
#' Performs a simplified processing step by renaming the "cleaned_text" field 
#' to "cut_text" for every item in the list. This is useful when the full text 
#' is already clean and does not require extraction via markers.
#'
#' @param data_list A list containing data items (e.g., articles). Each item is 
#'  checked for the existence of a "cleaned_text" field.
#' 
#' @return The modified `data_list` where "cleaned_text" has been moved to 
#'  "cut_text", and the original "cleaned_text" field removed.
#' 

common_minimal_pipeline <- function(data_list) {
  
  # Loop through each article using a numeric index for tracking
  for (article in seq_along(data_list)) {
    
    # Check if cleaned_text exists
    if ("cleaned_text" %in% names(data_list[[article]])) {
      
      # Rename cleaned_text to cut_text
      data_list[[article]]$cut_text <- data_list[[article]]$cleaned_text
      data_list[[article]]$cleaned_text <- NULL
    }
  }
  
  return(data_list)
}

filtered_dataset <- common_minimal_pipeline(journal_filtered_datalist)