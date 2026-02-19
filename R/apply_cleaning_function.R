#' @title apply_cleaning_function
#'
#' @description 
#' Iterates through the raw article dataset, cleans markdown formatting from 
#' individual pages, and concatenates them into a single `cleaned_text` field.
#' 
#' This wrapper function ensures that pages are processed and sorted numerically 
#' before concatenation. It also includes real-time progress tracking.
#'
#' @param data_list A list of articles (e.g., `raw_data_list`) where each 
#'                  element is a list containing paginated text fields 
#'                  (e.g., `text_page_1`, `text_page_2`).
#'
#' @return The modified `data_list` with individual cleaned pages and 
#'         a unified `cleaned_text` field.
#'
#' @export

process_articles <- function(data_list) {
  
  # Get the total number of articles to process for the progress counter
  total_items <- length(data_list)
  cat(sprintf("🧽 Starting the text cleaning process for %d articles.\n", total_items))
  
  # We loop through each article using a numeric index for progress tracking
  for (i in seq_along(data_list)) {
    
    # Get the current article's ID/name using the index
    article_id <- names(data_list)[i]

    # --- Step 1: Clean each page individually ---
    # Find all page text elements (e.g., "text_page_1", "text_page_2")
    page_names <- names(data_list[[i]])[grep("^text_page_\\d+$", names(data_list[[i]]))]

    # Apply markdown cleaning to each page
    for (page in page_names) {
      cleaned_page_name <- sub("text_page", "cleaned_text_page", page)
      data_list[[i]][[cleaned_page_name]] <- clean_markdown(data_list[[i]][[page]])
    }

    # --- Step 2: Concatenate the cleaned pages in the correct order ---
    # Find all the newly created cleaned page elements
    cleaned_page_names <- names(data_list[[i]])[grep("^cleaned_text_page_\\d+$", names(data_list[[i]]))]
    
    # Extract page numbers to sort them correctly
    page_numbers <- as.numeric(gsub("cleaned_text_page_", "", cleaned_page_names))
    sorted_cleaned_page_names <- cleaned_page_names[order(page_numbers)]

    # Paste all cleaned pages together into a single string
    concatenated_content <- paste(
      sapply(sorted_cleaned_page_names, function(name) data_list[[i]][[name]]),
      collapse = " "
    )

    # --- Step 3: Create full concatenated cleaned_text ---
    data_list[[i]][["cleaned_text"]] <- concatenated_content

    # --- Step 4: Report progress ---
    # Display progress every 1000 articles or on the very last one.
    if (i %% 1000 == 0 || i == total_items) {
      cat(
        sprintf(
          "⏳ Processed article %d / %d (%.2f%%)\n",
          i,
          total_items,
          (i / total_items) * 100
        )
      )
    }
  }
  
  # The function returns the modified list
  return(data_list)
}

# Apply the function to your original data list
cleaned_data_list <- process_articles(cleaned_data_list)