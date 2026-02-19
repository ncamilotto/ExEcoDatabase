#' @title Extract Clipping Markers
#' 
#' @description
#' Consolidates specific clipping markers ("first_chars_snippet" and "last_chars_snippet") 
#' from a list of page data. It groups items by their base name (removing "_first_page" 
#' or "_last_page" suffixes) and creates a merged list containing the found snippets 
#' for each unique entry.
#'
#' @param journal_filtered_extracted_pages A list of extracted page data, where 
#'  element names typically contain suffixes indicating page position.
#' 
#' @return A named list where keys are the unique base names and values are lists 
#'  containing the extracted snippet fields.

extract_clipping_markers <- function(journal_filtered_extracted_pages) {
  
  # Initialize the new list
  merged_list <- list()
  
  # Get all names from the list
  all_names <- names(journal_filtered_extracted_pages)
  
  # Extract the base names (without the suffixes _first_page or _last_page)
  base_names <- gsub("_(first|last)_page$", "", all_names)
  unique_base_names <- unique(base_names)
  
  # Define the fields to keep
  fields_to_keep <- c("first_chars_snippet", "last_chars_snippet")
  
  # Process each unique base name
  for (base_name in unique_base_names) {
    
    # Find all pages related to this base name
    related_indices <- which(base_names == base_name)
    related_names <- all_names[related_indices]
    
    # Initialize the new item that will be added to the merged list
    new_element <- list()
    
    # Browse the fields we want to keep
    for (field in fields_to_keep) {
      # Search for this field in all pages related to the article
      for (page_name in related_names) {
        if (field %in% names(journal_filtered_extracted_pages[[page_name]])) {
          new_element[[field]] <- journal_filtered_extracted_pages[[page_name]][[field]]
          break
        }
      }
    }
    
    # Add the newly created item to the final list.
    merged_list[[base_name]] <- new_element
  }
  
  return(merged_list)
}

# Apply the function to your list
clipping_markers <- extract_clipping_markers(journal_filtered_extracted_pages)