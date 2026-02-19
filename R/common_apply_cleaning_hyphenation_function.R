#' @title apply_cleaning_hyphenation_function
#'
#' @description 
#' Iterates through the article dataset and applies the `clean_hyphenation` 
#' function to the `cut_text` field of each article.
#' 
#' This is a wrapper function that ensures cleaning is only attempted on 
#' articles where `cut_text` is not NA.
#'
#' @param data_list A list of articles (e.g., `filtered_dataset`) where each 
#'                  element is a list containing a `cut_text` field.
#'
#' @return The modified `data_list` with repaired text.
#'

apply_hyphenation_cleaning <- function(data_list) {
  
  # Loop through each article using a numeric index for tracking
  for (i in seq_along(data_list)) {
    
    # --- Step 1: Check if 'cut_text' exists (is not NA) ---
    # We only process articles that were successfully cut in the previous step.
    if (!is.na(data_list[[i]][["cut_text"]])) {
      
      # --- Step 2: Apply the cleaning function ---
      # The `clean_hyphenation` function is applied to the existing text,
      # and the result overwrites the old 'cut_text'.
      data_list[[i]][["cut_text"]] <- clean_hyphenation(data_list[[i]][["cut_text"]])
      
    }
  }
  
  # The function returns the modified list
  return(data_list)
}

# -------------------------------------------------------------------------
# Execution: Run the Hyphenation Process
# -------------------------------------------------------------------------

filtered_dataset <- apply_hyphenation_cleaning(filtered_dataset)