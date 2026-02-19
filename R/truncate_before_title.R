#' @title find_title_position
#' 
#' @description
#' Locates the starting position of a specific title within a page's text using 
#' a fuzzy matching approach (Levenshtein distance). It normalizes the text 
#' (lowercasing, whitespace reduction) for the search, finds the best match 
#' using a sliding window, and maps the position back to the original text index.
#'
#' @param page_text A character string representing the content of the page.
#' @param title A character string representing the title to search for.
#' @param element_id A character string identifier for the current element. 
#'   Used to skip specific cases (e.g., "_last_page").
#' 
#' @return An integer representing the starting character index of the title 
#'   in the original `page_text`. Returns NA_integer_ if the title is not found 
#'   within the allowed distance threshold or if inputs are invalid.


find_title_position <- function(data_list) {
  
  core_find_pos <- function(page_text, title, element_id) {
    
    # 1. Skip conditions
    if (grepl("_last_page$", element_id) ||
        is.null(title) || is.na(title) ||
        is.null(page_text) || is.na(page_text)) {
      return(NA_integer_)
    }
    
    # 2. Dynamic distance threshold
    title_len <- nchar(title)
    
    max_allowed_dist <- if (title_len < 5) {
      0
    } else if (title_len <= 9) {
      1
    } else if (title_len <= 15) {
      3
    } else if (title_len <= 30) {
      5
    } else if (title_len <= 45) {
      7
    } else if (title_len <= 70) {
      9
    } else {
      12
    }
    
    # 3. Prepare normalized versions for fuzzy search
    title_lower <- tolower(title)
    page_text_lower <- tolower(page_text)
    
    title_normalized <- gsub("\\s+", " ", title_lower)
    page_text_normalized <- gsub("\\s+", " ", page_text_lower)
    
    text_len_normalized <- nchar(page_text_normalized)
    title_len_normalized <- nchar(title_normalized)

    if (title_len_normalized > text_len_normalized) {
      return(NA_integer_)
    }
    
    # 4. Sliding window search on normalized text
    min_dist_found <- Inf
    title_start_index <- NA_integer_
    loop_range <- text_len_normalized - title_len_normalized + 1
    
    for (j in seq_len(loop_range)) {
      
      substring_candidate <- substr(
        page_text_normalized,
        j,
        j + title_len_normalized - 1
      )
      
      dist <- stringdist::stringdist(
        title_normalized,
        substring_candidate,
        method = "lv"
      )
      
      if (dist < min_dist_found) {
        min_dist_found <- dist
        
        # Map position from normalized text back to original text
        chars_before_in_normalized <- j - 1
        char_count <- 0
        original_pos <- 0
        
        for (k in seq_len(nchar(page_text_lower))) {
          current_char <- substr(page_text_lower, k, k)
          
          # Count only characters that remain after normalization
          if (!grepl("\\s", current_char) || 
              (k > 1 && !grepl("\\s", substr(page_text_lower, k - 1, k - 1)))) {
            char_count <- char_count + 1
          }
          
          if (char_count > chars_before_in_normalized) {
            original_pos <- k
            break
          }
        }
        
        title_start_index <- original_pos
        
        # Early exit if perfect match found
        if (dist == 0) break
      }
    }
    
    # 5. Return position only if within allowed distance
    if (min_dist_found <= max_allowed_dist) {
      return(title_start_index)
    } else {
      return(NA_integer_)
    }
  }
  
  total_items <- length(data_list)
  item_names <- names(data_list)
  
  for (i in seq_along(data_list)) {
    
    # Progress reporting
    if (i %% 500 == 0 || i == total_items) {
      cat(
        sprintf(
          "✂️ Processed %d / %d pages (%.2f%%)\n",
          i,
          total_items,
          (i / total_items) * 100
        )
      )
    }
    
    element_id <- if(!is.null(item_names)) item_names[i] else paste0("item_", i)
    current_title <- data_list[[i]]$short_title
    page_text <- data_list[[i]]$text
    
    chars_before <- core_find_pos(page_text, current_title, element_id)
    
    data_list[[i]]$chars_before_title <- chars_before
  }
  
  return(data_list)
}

journal_filtered_extracted_pages <- find_title_position(journal_filtered_extracted_pages)


#' @title trim_text_before_title
#' 
#' @description
#' Iterates through a list of page data and trims the text content of each page 
#' based on a pre-calculated character count. If the cut point falls within a word,
#' the partial word fragment is removed to ensure clean text boundaries.
#'
#' @param data_list A list of lists, where each element represents page data. 
#'   Each inner list must contain a `text` field (character string) and a 
#'   `chars_before_title` field (numeric or integer indicating how many characters 
#'   to remove).
#' 
#' @return A list with the same structure and names as the input `data_list`. 
#'   The `text` field of each element is updated (substringed) if a valid 
#'   character count is found; otherwise, the element remains unchanged.

#' @title trim_text_before_title
#' 
#' @description
#' Iterates through a list of page data and trims the text content of each page 
#' based on a pre-calculated character count. If the cut point falls within a word,
#' the partial word fragment is removed to ensure clean text boundaries.
#'
#' @param data_list A list of lists, where each element represents page data. 
#'   Each inner list must contain a `text` field (character string) and a 
#'   `chars_before_title` field (numeric or integer indicating how many characters 
#'   to remove).
#' 
#' @return A list with the same structure and names as the input `data_list`. 
#'   The `text` field of each element is updated (substringed) if a valid 
#'   character count is found; otherwise, the element remains unchanged.

trim_text_before_title <- function(data_list) {
  
  cut_one_page <- function(page_data) {
    
    # 1. Get the cut parameter
    chars_count <- page_data$chars_before_title
    
    # 2. Check if valid - return unchanged if NA or NULL
    if (is.null(chars_count) || is.na(chars_count)) {
      if (!is.null(page_data$text) && !is.na(page_data$text)) {
        page_data$text <- trimws(page_data$text)
      }
      return(page_data)
    }
    
    # 3. Determine where the text should start
    new_start_index <- chars_count - 3 
    
    # 4. Perform the cut only if there are characters to remove
    if (new_start_index > 1) {
      
      current_text <- page_data$text
      
      # Safety check: ensure text is not NULL/NA before substring
      if (!is.null(current_text) && !is.na(current_text)) {
        
        # Extract from the calculated position
        trimmed_text <- substr(
          current_text, 
          new_start_index, 
          nchar(current_text)
        )
        
        # Check if we're starting in the middle of a word
        # A word fragment starts with an alphanumeric character but is preceded by one
        if (nchar(trimmed_text) > 0) {
          first_char <- substr(trimmed_text, 1, 1)
          
          # If first character is alphanumeric (we might be mid-word)
          if (grepl("[[:alnum:]]", first_char, perl = TRUE)) {
            
            # Find the next word boundary (space, punctuation, or start of text)
            word_end <- regexpr("^[[:alnum:]]+", trimmed_text, perl = TRUE)
            
            if (word_end > 0) {
              match_length <- attr(word_end, "match.length")
              
              # Skip past this partial word fragment
              if (match_length > 0 && match_length < nchar(trimmed_text)) {
                trimmed_text <- substr(trimmed_text, match_length + 1, nchar(trimmed_text))
                
                # Also skip any following spaces/punctuation to start clean
                leading_spaces <- regexpr("^[\\s\\p{P}]+", trimmed_text, perl = TRUE)
                if (leading_spaces > 0) {
                  space_length <- attr(leading_spaces, "match.length")
                  if (space_length > 0 && space_length < nchar(trimmed_text)) {
                    trimmed_text <- substr(trimmed_text, space_length + 1, nchar(trimmed_text))
                  }
                }
                
                # Remove any remaining leading whitespace after word fragment removal
                trimmed_text <- trimws(trimmed_text)
              }
            }
          } else {
            # If first character is NOT alphanumeric (space, punctuation, etc.)
            # Remove all leading whitespace and punctuation
            trimmed_text <- sub("^[\\s\\p{P}]+", "", trimmed_text, perl = TRUE)
          }
        }
        
        # Final cleanup: Remove any leading whitespace
        trimmed_text <- trimws(trimmed_text)
        
        # Overwrite text with trimmed version
        page_data$text <- trimmed_text
      }
    }
    
    return(page_data)
  }
  
  processed_list <- lapply(data_list, cut_one_page)
  
  names(processed_list) <- names(data_list)
  
  return(processed_list)
}

journal_filtered_extracted_pages <- trim_text_before_title(journal_filtered_extracted_pages)


#' @title extract_first_chars_snippet
#' 
#' @description
#' Generates a brief text preview (snippet) from the beginning of the content 
#' for each element in the list. This extraction is conditional: the snippet 
#' is only populated if the `chars_before_title` attribute is valid, implying 
#' that the page has been successfully processed for title positioning.
#'
#' @param data_list A list of lists, where each element represents page data. 
#'   Each inner list must contain a `text` field and is checked for the existence 
#'   of a `chars_before_title` field.
#' @param n_chars An integer specifying the maximum number of characters to 
#'   extract for the snippet. Defaults to 100.
#' 
#' @return A list with the same structure as `data_list`. Each element is updated 
#'   with a new field `first_chars_snippet`, which contains the extracted substring 
#'   or `NA` if the text is missing or the dependency check (`chars_before_title`) 
#'   fails.

extract_first_chars_snippet <- function(data_list, n_chars = 100) {
  
  # Fonction interne pour traiter une seule page
  process_one_page <- function(page_data) {
    
    # 1. Check dependency: 'chars_before_title'
    chars_before_param <- page_data$chars_before_title
    
    # Si la position n'a pas été trouvée précédemment, on ne remplit pas le snippet
    if (is.null(chars_before_param) || is.na(chars_before_param)) {
      page_data$first_chars_snippet <- NA
      return(page_data)
    }
    
    # 2. Case B: Prerequisite met -> Extract text
    current_text <- page_data$text
    
    # Safety check: Ensure text is not NULL/NA
    if (is.null(current_text) || is.na(current_text)) {
      page_data$first_chars_snippet <- NA
    } else {
      # Extract the first n characters
      # substr gère automatiquement les cas où text length < n_chars
      page_data$first_chars_snippet <- substr(current_text, 1, n_chars)
    }
    
    return(page_data)
  }

  processed_list <- lapply(data_list, process_one_page)
  
  return(processed_list)
}

journal_filtered_extracted_pages <- extract_first_chars_snippet(journal_filtered_extracted_pages)

undetected_title_articles <- journal_filtered_extracted_pages[sapply(journal_filtered_extracted_pages, function(x) is.na(x$first_chars_snippet)) & !endsWith(names(journal_filtered_extracted_pages), "_last_page")]

#' @title fill_missing_snippets
#' 
#' @description
#' Populates missing text previews for pages that do not already have a 
#' `first_chars_snippet`. It acts as a fallback mechanism by extracting the 
#' beginning of the raw `text` content. Elements identified by the suffix 
#' "_last_page" in their names are explicitly excluded from this operation.
#'
#' @param data_list A named list of lists, where each element represents page data.
#'   Used to check for existing snippets and the raw `text` source.
#' @param n_chars An integer specifying the number of characters to extract 
#'   when creating the fallback snippet. Defaults to 100.
#' 
#' @return A list with the same structure as `data_list`. The `first_chars_snippet` 
#'   field is updated with a substring of the raw text for eligible elements 
#'   where it was previously missing.

fill_missing_snippets <- function(data_list, n_chars = 100) {
  
  element_names <- names(data_list)
  
  for (i in seq_along(data_list)) {
    
    if (!grepl("_last_page$", element_names[i])) {
      
      current_snippet <- data_list[[i]]$first_chars_snippet
      
      if (is.null(current_snippet) || is.na(current_snippet)) {
        
        current_text <- data_list[[i]]$text
        
        if (!is.null(current_text) && !is.na(current_text)) {

          data_list[[i]]$first_chars_snippet <- substr(current_text, 1, n_chars)
        } else {
          
          data_list[[i]]$first_chars_snippet <- NA
        }
      }
    }
  }
  
  return(data_list)
}

journal_filtered_extracted_pages <- fill_missing_snippets(journal_filtered_extracted_pages)

num_missing <- length(undetected_title_articles)

cat("📄 There are", num_missing, "documents where the title position could not be determined.\n")

if (num_unique_articles > 0) {
  success_rate <- round((1 - num_missing / num_unique_articles) * 100, digits = 2)
  cat("ℹ️ The method successfully located the title in", success_rate, "% of cases.\n")
}