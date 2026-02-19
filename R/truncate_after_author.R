#' @title count_words
#' 
#' @description
#' Calculates the number of non-empty words in a character string. It splits 
#' the text based on whitespace patterns and filters out empty results. 
#' Returns 0 if the input is `NA` or an empty string.
#'
#' @param text A character string to be analyzed.
#' 
#' @return An integer representing the count of valid words found in `text`.
#' 

count_words <- function(text) {
  if (is.na(text) || text == "") return(0)
  
  words <- unlist(strsplit(text, "\\s+"))
  # Filter out empty strings
  non_empty_words <- words[nchar(words) > 0]
  
  return(length(non_empty_words))
}

#' @title compute_levenshtein_distance
#' 
#' @description
#' Computes the Levenshtein edit distance between two words using the 
#' `stringdist` package. It performs a case-insensitive comparison by 
#' converting inputs to lowercase. Returns `Inf` if either input is `NA` 
#' or empty, effectively treating missing data as maximally dissimilar.
#'
#' @param word1 A character string representing the first word.
#' @param word2 A character string representing the second word.
#' 
#' @return A numeric value representing the Levenshtein distance, or `Inf` 
#'   if inputs are invalid.

compute_levenshtein_distance <- function(word1, word2) {
  if (is.na(word1) || is.na(word2) || nchar(word1) == 0 || nchar(word2) == 0) {
    return(Inf)  # Return a large value if one of the words is empty or NA
  }
  
  return(
    stringdist::stringdist(
      tolower(word1),
      tolower(word2),
      method = "lv"
    )
  )
}


#' @title thresholds_reached
#' 
#' @description
#' Evaluates whether a substring of text, ending at a specific position, 
#' satisfies minimum length requirements. It extracts the text from the start 
#' up to `end_position` and checks if both the character count and word count 
#' strictly exceed the global constants `CHAR_THRESHOLD` and `WORD_THRESHOLD`.
#'
#' @param end_position An integer representing the end index of the substring 
#'   to be evaluated.
#' @param text A character string representing the source text.
#' 
#' @return A logical value. Returns `TRUE` if both the character length and 
#'   the word count of the substring are greater than their respective 
#'   thresholds; otherwise, returns `FALSE`.


thresholds_reached <- function(end_position, text) {
  text_up_to_position <- substr(text, 1, end_position)
  
  character_count <- nchar(text_up_to_position)
  word_count <- count_words(text_up_to_position)
  
  return(
    character_count > CHAR_THRESHOLD &&
    word_count > WORD_THRESHOLD
  )
}


#' @title max_allowed_distance
#' 
#' @description
#' Determines the maximum acceptable Levenshtein distance (error tolerance) 
#' for a surname based on its character length. It defines a dynamic threshold 
#' where longer names allow for more discrepancies, while shorter names 
#' require stricter or exact matching.
#'
#' @param surname A character string representing the surname to be evaluated.
#' 
#' @return An integer (0, 1, 2, or 3) representing the allowed distance 
#'   threshold. Returns `NA` if the input surname is missing.

max_allowed_distance <- function(surname) {
  if (is.na(surname)) return(NA)
  
  surname_length <- nchar(surname)
  
  if (surname_length < 5) {
    return(0)
  } else if (surname_length <= 6) {
    return(1)
  } else if (surname_length <= 10) {
    return(2)
  } else {
    return(3)
  }
}

# Variables for statistics
total_eligible_articles <- 0
total_processed_articles <- 0
total_ignored_articles <- 0 
total_names_found <- 0
total_initials_found <- 0
total_without_name_or_initial <- 0

# Lists to store unmatched articles
articles_without_name_found <- list()
articles_without_initials_found <- list()


#' @title detect_surname_in_text
#' 
#' @description
#' Scans a text string to locate a specific surname using a multi-pass 
#' fuzzy matching approach (Levenshtein distance). It accounts for OCR 
#' inconsistencies by attempting strict word matching, loose tokenization 
#' (ignoring punctuation), and specific handling for hyphenated names.
#'
#' @details
#' The function operates in three passes:
#' \enumerate{
#'   \item \strong{Strict Scan:} Checks words based on exact whitespace positioning.
#'   \item \strong{Loose Token Scan:} Splits text by punctuation and separators 
#'   to handle noisy OCR.
#'   \item \strong{Hyphenated Scan:} Specifically targets hyphenated tokens.
#' }
#' If a match is found, the function checks if the text preceding the match 
#' meets the global `CHAR_THRESHOLD` and `WORD_THRESHOLD` via the 
#' `thresholds_reached` function. If these thresholds are met, the text is 
#' truncated at the end of the surname; otherwise, the full text is returned 
#' even if the name was found.
#'
#' @param text A character string representing the content to be searched.
#' @param surname A character string representing the target surname.
#' 
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{text}: The character string (truncated if the surname was 
#'     found and thresholds were met, otherwise the original text).
#'     \item \code{found}: A logical value indicating whether the surname 
#'     was successfully identified in the text.
#'   }

detect_surname_in_text <- function(text, surname) {
  
  # ─────────────────────────────────────────────────────────────
  # Guard clauses
  # ─────────────────────────────────────────────────────────────
  
  if (is.na(surname) || surname == "") {
    return(list(text = text, found = FALSE))
  }
  
  surname_lower <- tolower(surname)
  max_distance <- max_allowed_distance(surname_lower)
  
  # ─────────────────────────────────────────────────────────────
  # Pass 1: position-aware word matching (strict scan)
  # OCR reasonably clean, preserves exact positions
  # ─────────────────────────────────────────────────────────────
  
  word_positions <- gregexpr("\\S+", text)[[1]]
  if (word_positions[1] == -1) {
    return(list(text = text, found = FALSE))
  }
  
  for (i in seq_along(word_positions)) {
    word_start <- word_positions[i]
    word_end <- word_start + attr(word_positions, "match.length")[i] - 1
    
    original_word <- substr(text, word_start, word_end)
    normalized_word <- tolower(original_word)
    
    candidate_variants <- c(
      normalized_word,
      gsub("[[:punct:]&&[^-]]", "", normalized_word),
      gsub("[[:punct:]]", "", normalized_word)
    )
    
    for (variant in candidate_variants) {
      if (compute_levenshtein_distance(variant, surname_lower) <= max_distance) {
        if (thresholds_reached(word_end, text)) {
          return(list(text = substr(text, 1, word_end), found = TRUE))
        } else {
          return(list(text = text, found = TRUE))
        }
      }
    }
    
    # Handle hyphenated words at position level
    hyphen_preserved_word <- gsub("[[:punct:]&&[^-]]", "", normalized_word)
    if (grepl("-", hyphen_preserved_word)) {
      hyphen_parts <- unlist(strsplit(hyphen_preserved_word, "-"))
      for (part in hyphen_parts) {
        if (nchar(part) > 0 &&
            compute_levenshtein_distance(part, surname_lower) <= max_distance) {
          if (thresholds_reached(word_end, text)) {
            return(list(text = substr(text, 1, word_end), found = TRUE))
          } else {
            return(list(text = text, found = TRUE))
          }
        }
      }
    }
  }
  
  # ─────────────────────────────────────────────────────────────
  # Pass 2: tolerant token-based matching
  # OCR-noisy text, punctuation and separators unreliable
  # ─────────────────────────────────────────────────────────────
  
  text_lower <- tolower(text)
  loose_tokens <- unlist(strsplit(
    text_lower,
    "[\\s\\.,;:!?()\\[\\]\\{\\}-]"
  ))
  
  for (token in loose_tokens) {
    if (is.na(token) || nchar(token) <= 2) next
    
    semi_clean_token <- gsub("[[:punct:]&&[^-]]", "", token)
    fully_clean_token <- gsub("[[:punct:]]", "", token)
    
    # Hyphen-aware matching
    if (grepl("-", semi_clean_token)) {
      token_parts <- strsplit(semi_clean_token, "-")[[1]]
      for (part in token_parts) {
        clean_part <- gsub("[[:punct:]]", "", part)
        if (nchar(clean_part) > 0 &&
            compute_levenshtein_distance(clean_part, surname_lower) <= max_distance) {
          
          match_pos <- regexpr(token, text_lower, fixed = TRUE)
          if (match_pos[1] > 0) {
            match_end <- match_pos[1] + attr(match_pos, "match.length")[1] - 1
            if (thresholds_reached(match_end, text)) {
              return(list(text = substr(text, 1, match_end), found = TRUE))
            } else {
              return(list(text = text, found = TRUE))
            }
          }
        }
      }
    }
    
    # Fully cleaned token matching
    if (nchar(fully_clean_token) > 2 &&
        compute_levenshtein_distance(fully_clean_token, surname_lower) <= max_distance) {
      
      match_pos <- regexpr(token, text_lower, fixed = TRUE)
      if (match_pos[1] > 0) {
        match_end <- match_pos[1] + attr(match_pos, "match.length")[1] - 1
        if (thresholds_reached(match_end, text)) {
          return(list(text = substr(text, 1, match_end), found = TRUE))
        } else {
          return(list(text = text, found = TRUE))
        }
      }
    }
  }
  
  # ─────────────────────────────────────────────────────────────
  # Pass 3: matching on hyphenated names
  # ─────────────────────────────────────────────────────────────
  
  hyphen_preserved_text <- gsub("[[:punct:]&&[^-]]", " ", text)
  hyphenated_tokens <- grep(
    "-",
    unlist(strsplit(hyphen_preserved_text, "\\s+")),
    value = TRUE
  )
  
  for (token in hyphenated_tokens) {
    token_parts <- unlist(strsplit(token, "-"))
    for (part in token_parts) {
      if (nchar(part) > 0 &&
          compute_levenshtein_distance(part, surname_lower) <= max_distance) {
        
        match_pos <- regexpr(token, text, fixed = TRUE)
        if (match_pos[1] > 0) {
          match_end <- match_pos[1] + attr(match_pos, "match.length")[1] - 1
          if (thresholds_reached(match_end, text)) {
            return(list(text = substr(text, 1, match_end), found = TRUE))
          } else {
            return(list(text = text, found = TRUE))
          }
        }
      }
    }
  }
  
  # ─────────────────────────────────────────────────────────────
  # No match found
  # ─────────────────────────────────────────────────────────────
  
  return(list(text = text, found = FALSE))
}

#' @title check_initials_in_text
#' 
#' @description
#' Searches a text string for a specific set of initials using regular 
#' expressions. It prioritizes a whole-word match (respecting word boundaries) 
#' before falling back to a raw substring search to account for varied 
#' formatting or scanning issues.
#'
#' @details
#' The function automatically escapes dots in the `initials` input to prevent 
#' regex errors. Upon finding a match, it verifies if the text preceding the 
#' match meets global size requirements via `thresholds_reached`. If valid, 
#' the text is truncated at the end of the initials; otherwise, the full text 
#' is returned despite the match.
#'
#' @param text A character string representing the content to be searched.
#' @param initials A character string representing the initials (e.g., "J.F.K.").
#' 
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{text}: The character string (truncated if initials were 
#'     found and thresholds were met, otherwise the original text).
#'     \item \code{found}: A logical value indicating whether the initials 
#'     were successfully identified in the text.
#'   }

check_initials_in_text <- function(text, initials) {
  
  if (is.na(initials) || initials == "") {
    return(list(text = text, found = FALSE))
  }
  
  # Escape dots in initials for regex
  initials_clean <- gsub("\\.", "\\\\.", initials)
  
  # ─────────────────────────────────────────────────────────────
  # Approach 1: whole word match
  # ─────────────────────────────────────────────────────────────
  
  match_pos <- regexpr(paste0("\\b", initials_clean), text, ignore.case = TRUE)
  if (match_pos > 0) {
    match_end <- match_pos + attr(match_pos, "match.length") - 1
    if (thresholds_reached(match_end, text)) {
      return(list(text = substr(text, 1, match_end), found = TRUE))
    } else {
      return(list(text = text, found = TRUE))
    }
  }
  
  # ─────────────────────────────────────────────────────────────
  # Approach 2: raw match anywhere
  # ─────────────────────────────────────────────────────────────
  
  match_pos <- regexpr(initials_clean, text, ignore.case = TRUE)
  if (match_pos > 0) {
    match_end <- match_pos + attr(match_pos, "match.length") - 1
    if (thresholds_reached(match_end, text)) {
      return(list(text = substr(text, 1, match_end), found = TRUE))
    } else {
      return(list(text = text, found = TRUE))
    }
  }
  
  # ─────────────────────────────────────────────────────────────
  # No match found
  # ─────────────────────────────────────────────────────────────
  
  return(list(text = text, found = FALSE))
}


#' @title check_both_initials_in_text
#' 
#' @description
#' Tries to find initials in a text by checking both orders (firstname first,
#' then surname first) and stops as soon as one matches.
#'
#' @param text A character string representing the content to be searched.
#' @param initials_firstname_first Initials in "M. B." format.
#' @param initials_surname_first Initials in "B. M." format.
#' 
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{text}: The character string (truncated if initials were 
#'     found and thresholds were met, otherwise the original text).
#'     \item \code{found}: A logical value indicating whether the initials 
#'     were successfully identified in the text.
#'   }

check_both_initials_in_text <- function(text, initials_firstname_first, initials_surname_first) {
  
  # Try firstname first order (e.g. "M. B.")
  result <- check_initials_in_text(text, initials_firstname_first)
  if (result$found) return(result)
  
  # Fallback: try surname first order (e.g. "B. M.")
  result <- check_initials_in_text(text, initials_surname_first)
  if (result$found) return(result)
  
  return(list(text = text, found = FALSE))
}


# Apply detection methods to articles list
journal_filtered_extracted_pages <- mapply(
  function(article, article_id) {

    # Skip first pages or non-segmentable articles
    if (str_ends(article_id, "_first_page") ||
        isTRUE(article$needs_truncation == 0)) {

      total_ignored_articles <<- total_ignored_articles + 1
      return(article)
    }
    
    surname                  <- article$surname
    initials_firstname_first <- article$initials_firstname_first
    initials_surname_first   <- article$initials_surname_first
    
    # Increment counters for eligible articles
    total_eligible_articles  <<- total_eligible_articles + 1
    total_processed_articles <<- total_processed_articles + 1
    
    # Step 1: check last name (only if surname is available)
    name_found <- FALSE
    if (!is.na(surname) && surname != "") {
      surname_result <- detect_surname_in_text(article$text, surname)
      article$text <- surname_result$text
      
      if (surname_result$found) {
        total_names_found <<- total_names_found + 1
        article$needs_truncation <- 0
        name_found <- TRUE
      }
    }
    
    # Step 2: check both initials orders if name was not found
    if (!name_found) {
      total_without_name_or_initial <<- total_without_name_or_initial + 1
      if (!is.na(surname) && surname != "") {
        articles_without_name_found[[article_id]] <<- article
      }
      
      initials_available <- (!is.na(initials_firstname_first) && initials_firstname_first != "") ||
                            (!is.na(initials_surname_first)   && initials_surname_first != "")
      
      if (initials_available) {
        initials_result <- check_both_initials_in_text(
          article$text,
          initials_firstname_first,
          initials_surname_first
        )
        article$text <- initials_result$text
        
        if (initials_result$found) {
          total_initials_found <<- total_initials_found + 1
          article$needs_truncation <- 0
        } else {
          articles_without_initials_found[[article_id]] <<- article
        }
      } else {
        # If no initials available, article remains undetected
        articles_without_initials_found[[article_id]] <<- article
      }
    }
    
    return(article)
    
  }, 
  journal_filtered_extracted_pages, 
  names(journal_filtered_extracted_pages), 
  SIMPLIFY = FALSE
)

# Statistics

percent_names_found <- ifelse(
  total_eligible_articles > 0, 
  (total_names_found / total_eligible_articles) * 100, 
  0
)


percent_initials_found <- ifelse(
  total_without_name_or_initial > 0,
  (total_initials_found / total_without_name_or_initial) * 100,
  0
)

percent_total_found <- ifelse(
  total_eligible_articles > 0,
  ((total_names_found + total_initials_found) / total_eligible_articles) * 100,
  0
)

cat(
  "📄 There are ", length(articles_without_initials_found)," documents where the author name was not found.\n"
)


# Filter articles where truncation is needed
undetected_author_articles <- Filter(
  function(x) x$needs_truncation == 1, 
  articles_without_initials_found
)

# Check if there are articles to process
if (length(undetected_author_articles) > 0) {
  
  # Create a data frame
  article_names <- unique(sub("_.*$", "", names(undetected_author_articles)))
  
  df_articles_without_end_match <- data.frame(
    article_name = article_names,
    Status = rep("", length(article_names)),
    stringsAsFactors = FALSE
  )
}