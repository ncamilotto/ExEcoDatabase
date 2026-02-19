#' @title Finalize Dataset
#' 
#' @description
#' Filters and standardizes the article dataset for final output. It retains only 
#' a specific set of fields from each item in the input list and renames the extracted
#'  content field from "cut_text" to "text".
#'
#' @param data_list A list containing article data objects with various fields.
#' 
#' @return A named list containing the cleaned article objects, each restricted 
#'  to the defined fields and with the text field standardized.

finalize_dataset <- function(data_list) {
  
  # Define the exact list of fields to keep in the final output
  fields_to_keep <- c(
    "id",
    "document_title",
    "author",
    "publication_year",
    "standardized_journal_name",
    "original_journal_name",
    "volume",
    "issue",
    "start_page",
    "end_page",
    "cut_text"
  )
  
  # Initialize a list to store the results.
  finalized_list <- list() 
  
  # The rest of your loop is perfect and needs no changes.
  for (i in seq_along(data_list)) {
    article_id <- names(data_list)[i]
    current_article <- data_list[[i]]
    
    # --- Step 1: Select only the desired fields ---
    existing_fields <- intersect(fields_to_keep, names(current_article))
    new_article <- current_article[existing_fields]
    
    # --- Step 2: Rename 'cut_text' to 'text' ---
    if ("cut_text" %in% names(new_article)) {
      names(new_article)[names(new_article) == "cut_text"] <- "text"
    }
    
    # Add the cleaned-up article to our (now correctly initialized) list
    finalized_list[[article_id]] <- new_article
  }
  
  # Return the newly created, clean list
  return(finalized_list)
}


# Run the Process

# This will now work as expected
filtered_dataset <- finalize_dataset(filtered_dataset)