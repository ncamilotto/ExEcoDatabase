#' @title extract_start_end_pages
#'
#' @description 
#' Iterates through the cleaned dataset and extracts specific pages based on 
#' the article length. 
#' 
#' - If an article has a single page: It extracts that page.
#' - If an article has multiple pages: It creates two separate entries in the 
#'   output list (one for the first page, one for the last page).
#'
#' Metadata is preserved and attached to each extracted text segment.
#'
#' @param data_list A list of cleaned articles (e.g., `cleaned_data_list`) 
#'                  containing `cleaned_text_page_X` fields.
#'
#' @return A new flattened list `extracted_pages_list` where keys are modified 
#'         to indicate if they are single pages or split into first/last pages.
#'
#' @export


extract_first_last_pages <- function(data_list) {
  
  # Create a new empty list to store the final result
  extracted_pages_list <- list()
  
  # Loop through each article using a numeric index for progress tracking
  for (i in seq_along(data_list)) {
    
    # Get the current article's ID/name and content
    article_id <- names(data_list)[i]
    article <- data_list[[i]]
    
    # --- Step 1: Identify Keys and Metadata ---
    
    # Identify keys for text pages (e.g., "cleaned_text_page_1")
    text_page_keys <- grep("^cleaned_text_page_", names(article), value = TRUE)
    
    # Identify metadata keys.
    # These are keys that are NOT raw text, NOT concatenated cleaned text, 
    # and NOT individual page text.
    meta_keys <- names(article)[
      !grepl("^text", names(article)) & 
      names(article) != "cleaned_text" &
      !grepl("^cleaned_text_page_", names(article))
    ]
    
    # Extract metadata to preserve it in the new structure
    metadata <- article[meta_keys]
    
    # Sort pages numerically to ensure we identify the true First and Last page
    page_nums <- as.numeric(gsub("cleaned_text_page_", "", text_page_keys))
    text_keys_sorted <- text_page_keys[order(page_nums)]
    
    
    # --- Step 2: Process based on Page Count ---
    
    # CASE 1: The article has only one single text page
    if (length(text_keys_sorted) == 1) {
      
      # Extract the content of the single page
      text_content <- article[text_keys_sorted]
      names(text_content) <- "text" # Rename key to generic "text"
      
      # Combine metadata and the renamed text
      new_article <- c(metadata, text_content)
      
      # Add to final list using the original ID
      extracted_pages_list[[article_id]] <- new_article
      
    # CASE 2: The article has multiple pages
    } else if (length(text_keys_sorted) > 1) {
      
      # 2a. Process the FIRST page
      # --------------------------
      first_page_key <- text_keys_sorted[1]
      first_page_content <- article[first_page_key]
      names(first_page_content) <- "text"
      
      # Combine metadata and first page text
      first_page_article <- c(metadata, first_page_content)
      
      # Add to list with suffix "_first_page"
      extracted_pages_list[[paste0(article_id, "_first_page")]] <- first_page_article
      
      # 2b. Process the LAST page
      # -------------------------
      last_page_key <- text_keys_sorted[length(text_keys_sorted)]
      last_page_content <- article[last_page_key]
      names(last_page_content) <- "text"
      
      # Combine metadata and last page text
      last_page_article <- c(metadata, last_page_content)
      
      # Add to list with suffix "_last_page"
      extracted_pages_list[[paste0(article_id, "_last_page")]] <- last_page_article
    }
  }
  
  # The function returns the new list containing extracted pages
  return(extracted_pages_list)
}


# -------------------------------------------------------------------------
# Execution: Run the Page Extraction Process
# -------------------------------------------------------------------------

extracted_pages_list <- extract_first_last_pages(cleaned_data_list)













