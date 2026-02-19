#' @title find_closest_overlap_position
#' 
#' @description
#' Scans page text to locate the positions of multiple potential overlapping 
#' titles using fuzzy matching (Levenshtein distance). It dynamically identifies 
#' all keys matching "overlap_short_title_X", determines the starting index 
#' of each title after a specific threshold (ignoring the header/start of text), 
#' and identifies which overlap occurs earliest.
#'
#' @details
#' The function skips pages suffixed with "_first_page". It employs a sliding 
#' window for fuzzy matching with a distance tolerance adapted to the length 
#' of the title. It relies on a global `CHAR_THRESHOLD` (defaulting to 100) to 
#' skip the beginning of the text during the search.
#'
#' @param articles_list A list of lists representing page data. Each element 
#'   must contain a `text` field and may contain multiple keys matching the 
#'   pattern `^overlap_short_title_\\d+$`.
#' 
#' @return A list with the same structure as `articles_list`. Modified elements 
#'   receive:
#'   \itemize{
#'     \item Specific position fields (e.g., `chars_before_overlap_1`) only if 
#'     a valid match is found.
#'     \item `closest_overlap_field`: The name of the overlap key found at the 
#'     minimum index.
#'     \item `closest_overlap_position`: The integer index of that closest match.
#'   }

find_closest_overlap_position <- function(articles_list) {
  
  # 1. Identify all "overlapping title" fields dynamically
  all_keys <- unique(unlist(lapply(articles_list, names)))
  overlap_keys <- grep("^overlap_short_title_\\d+$", all_keys, value = TRUE)
  
  # Setup simple counter variables
  total_items <- length(articles_list)
  
  for (i in seq_along(articles_list)) {
    
    # Custom Console Progress
    if (i %% 500 == 0 || i == total_items) {
      cat(sprintf("✂️  Processed %d / %d pages (%.2f%%)\n", i, total_items, (i / total_items) * 100))
      flush.console()
    }
    
    article_id <- names(articles_list)[i]
    
    # Logic: Skip start pages ("_first_page")
    if (grepl("_first_page$", article_id)) {
      next
    }
    
    current_article <- articles_list[[i]]
    page_text <- current_article$text
    
    # Dictionary to store positions for this specific article
    found_positions <- list()
    
    for (key in overlap_keys) {
      
      overlap_title <- current_article[[key]]
      
      suffix_id <- sub("overlap_short_title", "", key)
      result_col_name <- paste0("chars_before_overlap_", suffix_id)
      
      # Check for validity
      if (is.null(overlap_title) || is.na(overlap_title) || 
          is.null(page_text) || is.na(page_text) || 
          isTRUE(current_article$needs_truncation == 0)) {
        
        found_positions[[key]] <- NA
        next
      }
      
      # Define Max Distance Threshold based on title length
      title_len <- nchar(overlap_title)
      
      max_allowed_dist <- dplyr::case_when(
        title_len < 5  ~ 0,
        title_len <= 9 ~ 1,
        title_len <= 15 ~ 3,
        title_len <= 30 ~ 5,
        title_len <= 70 ~ 8,
        TRUE ~ 12
      )
      
      # Prepare strings
      overlap_title_lower <- tolower(overlap_title)
      page_text_lower     <- tolower(page_text)
      text_len            <- nchar(page_text_lower)
      
      min_dist_found    <- Inf
      title_start_index <- NA
      
      threshold_val <- if(exists("CHAR_THRESHOLD")) CHAR_THRESHOLD else 100 
      
      # Fuzzy Search Loop
      if (text_len >= title_len) {
        loop_range <- text_len - title_len + 1
        for (j in 1:loop_range) {
          if (j <= threshold_val) next 
          
          substring_candidate <- substr(page_text_lower, j, j + title_len - 1)
          dist <- stringdist::stringdist(overlap_title_lower, substring_candidate, method = "lv")
          
          if (dist < min_dist_found) {
            min_dist_found    <- dist
            title_start_index <- j
            if (dist == 0) break
          }
        }
      }
      
      # Validation and Conditional Assignment
      if (!is.na(title_start_index) && 
          min_dist_found <= max_allowed_dist && 
          title_start_index > threshold_val) {
        
        chars_before_overlap <- title_start_index - 1
        articles_list[[i]][[result_col_name]] <- chars_before_overlap
        found_positions[[key]] <- chars_before_overlap
        
      } else {

        found_positions[[key]] <- NA
      }
    }
    
    # Logic: Find the closest overlap (Minimum position)
    valid_positions <- Filter(Negate(is.na), found_positions)
    
    if (length(valid_positions) > 0) {
      closest_key <- names(valid_positions)[which.min(unlist(valid_positions))]
      articles_list[[i]]$closest_overlap_field    <- closest_key
      articles_list[[i]]$closest_overlap_position <- valid_positions[[closest_key]]
    } else {
      articles_list[[i]]$closest_overlap_field    <- NA
      articles_list[[i]]$closest_overlap_position <- NA
    }
  }
  
  return(articles_list)
}

# --- EXECUTION ---

journal_filtered_extracted_pages <- find_closest_overlap_position(journal_filtered_extracted_pages)


#' @title trim_text_after_overlap
#' 
#' @description
#' Truncates the text content of specific pages to remove overlapping content 
#' identified at the end. It uses the `closest_overlap_position` attribute 
#' to determine the cut-off point, retaining only the text preceding the overlap. 
#' Pages named with the suffix "_first_page" are explicitly skipped to preserve 
#' their content.
#'
#' @param articles_list A named list of lists, where each element represents 
#'   page data. Eligible elements must contain a `text` field and a valid 
#'   `closest_overlap_position` (integer).
#' 
#' @return A list with the same structure as `articles_list`. The `text` field 
#'   of processed elements is updated to include only the characters before 
#'   the overlap position (substring from 1 to `cut_pos - 1`).

trim_text_after_overlap <- function(articles_list) {
  
  for (i in seq_along(articles_list)) {
    
    article_id <- names(articles_list)[i]
    
    if (grepl("_first_page$", article_id)) {
      next
    }
    
    current_article <- articles_list[[i]]
    cut_pos <- current_article$closest_overlap_position
    
    if (is.null(cut_pos) || is.na(cut_pos) || 
        is.null(current_article$text) || is.na(current_article$text)) {
      next
    }
    
    if (cut_pos > 0) {
      new_text <- substr(current_article$text, 1, cut_pos - 1)
    } else {
      new_text <- ""
    }
    
    articles_list[[i]]$text <- new_text
  }
  
  return(articles_list)
}

# 2. Apply the function
journal_filtered_extracted_pages <- trim_text_after_overlap(journal_filtered_extracted_pages)


# Statistics

count_before <- sum(purrr::imap_lgl(journal_filtered_extracted_pages, function(x, name) {
  !str_ends(name, "_first_page") && isTRUE(x$needs_truncation == 1)
}))

journal_filtered_extracted_pages <- imap(journal_filtered_extracted_pages, function(x, name) {
  
  if (str_ends(name, "_first_page")) {
    return(x)
  }
  
  if (isTRUE(x$needs_truncation == 1)) {
    pos_overlap <- x$closest_overlap_position
    
    if (!is.null(pos_overlap) && !is.na(pos_overlap)) {
      x$needs_truncation <- 0
    } else {
      x$needs_truncation <- 1
    }
  }
  return(x)
})

count_after <- sum(purrr::imap_lgl(journal_filtered_extracted_pages, function(x, name) {
  !str_ends(name, "_first_page") && isTRUE(x$needs_truncation == 1)
}))

cat("📄 There are", count_after, "documents where the next title position could not be determined.\n")

undetected_next_title_articles <- journal_filtered_extracted_pages[
  imap_lgl(journal_filtered_extracted_pages, function(x, name) {
    !str_ends(name, "_first_page") && isTRUE(x$needs_truncation == 1)
  })
] 