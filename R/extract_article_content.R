#' @title Extract Article Content (Sequential Search)
#' 
#' @description
#' Extracts text by finding the start marker first, then looking for the end marker 
#' *after* the start position (allowing overlaps).
#'
#' @param data_list A list containing article data.
#' @param markers_list A list containing snippets.
#' 
#' @return The modified `data_list` with "cut_text" and "problem" fields.

extract_article_content <- function(data_list, markers_list) {
  
  require(stringr) # Ensure stringr is loaded
  
  for (article_id in names(data_list)) {
    
    # Init problem
    data_list[[article_id]]$problem <- NA
    
    if (!is.null(data_list[[article_id]]$cleaned_text)) {
      
      full_text <- data_list[[article_id]]$cleaned_text
      
      start_marker <- markers_list[[article_id]]$first_chars_snippet
      end_marker   <- markers_list[[article_id]]$last_chars_snippet
      
      # Check marker validity
      has_valid_start <- !is.null(start_marker) && !is.na(start_marker) && start_marker != ""
      has_valid_end   <- !is.null(end_marker)   && !is.na(end_marker)   && end_marker != ""
      
      # CASE 1: Missing markers -> Keep full text
      if (!has_valid_start || !has_valid_end) {
        data_list[[article_id]]$cut_text <- full_text
        
      } else {
        # CASE 2: Sequential search
        
        # 1. Find the START
        idx_start_matrix <- str_locate(full_text, fixed(start_marker))
        
        if (!is.na(idx_start_matrix[1])) {
          
          pos_start_abs <- idx_start_matrix[1, "start"]
          
          # 2. Define search zone for END
          # We take the text from 'pos_start_abs' to the end.
          # This allows the end marker to overlap with the start marker.
          search_zone <- str_sub(full_text, pos_start_abs, -1)
          
          # 3. Find the END in this restricted zone
          idx_end_rel_matrix <- str_locate(search_zone, fixed(end_marker))
          
          if (!is.na(idx_end_rel_matrix[1])) {
            
            # 4. Recalculate absolute end position
            # Absolute Pos = Absolute Start + Relative End - 1
            pos_end_rel <- idx_end_rel_matrix[1, "end"]
            pos_end_abs <- pos_start_abs + pos_end_rel - 1
            
            # Final extraction
            data_list[[article_id]]$cut_text <- str_sub(full_text, pos_start_abs, pos_end_abs)
            
          } else {
            # Start is found, but End is not found AFTER the start
            data_list[[article_id]]$cut_text <- NA
            data_list[[article_id]]$problem  <- "END marker not found after the start marker."
          }
          
        } else {
          # Start is not found at all
          data_list[[article_id]]$cut_text <- NA
          data_list[[article_id]]$problem  <- "START marker not found in text."
        }
      }
      
    } else {
      data_list[[article_id]]$cut_text <- NA
      data_list[[article_id]]$problem  <- "Missing source text."
    }
  }
  
  return(data_list)
}


# 1. Execute function
filtered_dataset <- extract_article_content(data_list = journal_filtered_datalist, 
                                            markers_list = clipping_markers)

# 2. Initialize error list
articles_extraction_failed <- list()

# 3. Verification loop
for (article_id in names(filtered_dataset)) {
  
  # If cut_text is NA, extraction failed (or no text existed)
  if (is.na(filtered_dataset[[article_id]]$cut_text)) {
    
    # Retrieve logged problem. 
    # If NA (rare case), set generic message.
    problem_msg <- filtered_dataset[[article_id]]$problem
    if (is.na(problem_msg)) problem_msg <- "Unknown error (cut_text is NA without reason)"
    
    # Store error message instead of TRUE
    articles_extraction_failed[[article_id]] <- problem_msg
  }
}

# 4. Display results
failure_count <- length(articles_extraction_failed)

if (failure_count > 0) {
  cat(sprintf("❌ There are %d articles that could not be extracted.\n", failure_count))
  
  # Show top 5 problems for example
  print(head(articles_extraction_failed, 5))
} else {
  cat("✅ All articles successfully processed.\n")
}