#' @title null_to_na
#' 
#' @description
#' Recursively traverses a list or vector and replaces all NULL values 
#' with NA, preserving the original structure.
#'
#' @param item A list, vector, or NULL to process.
#' 
#' @return An object of the same type and structure as `item`, with NULLs 
#'   replaced by NA (logical).
 
null_to_na <- function(item) {
  # If the item is a list, apply the function recursively to each element
  if (is.list(item)) {
    return(lapply(item, null_to_na))
  }
  
  # If the item is NULL, replace it with NA
  if (is.null(item)) {
    return(NA)
  }
  
  # Otherwise, return the value as is
  return(item)
}

#' @title data_sanitization
#' 
#' @description
#' Cleans the raw data list by filtering unwanted articles and normalizing values.
#'
#' 
#' @param data_list A list containing raw article data.
#' @return A sanitized list with specific articles removed and NULLs converted to NAs.

data_sanitization <- function(data_list) {
  
  if (is.list(data_list)) {
    
    # Filter out articles where the title is "Back Matter" or "Front Matter"
    data_list <- Filter(function(article) {
      !(
        is.list(article) &&
        !is.null(article$document_title) &&
        article$document_title %in% c("Back Matter", "Front Matter")
      )
    }, data_list)
    
    # Apply the null_to_na transformation to the remaining elements
    return(lapply(data_list, null_to_na))
  }
  
  # Edge case: If the input list itself is NULL, return NA
  if (is.null(data_list)) {
    return(NA)
  }
  
  # Otherwise, return the input data as is
  return(data_list)
}

# Apply the sanitization process to the raw data
cleaned_data_list <- data_sanitization(raw_data_list)
