# ─────────────────────────────────────────────────────────────
# 1. Aggregate individual journal datasets into ExEco_Dataset
# ─────────────────────────────────────────────────────────────

# Identify all keys in the intermediate_objects list that correspond to final filtered datasets
dataset_keys <- grep("_filtered_dataset$", names(intermediate_objects), value = TRUE)

# Extract only these specific datasets
all_filtered_datasets <- intermediate_objects[dataset_keys]

# Concatenate all journal-specific lists of articles into a single, unified list.
# By applying unname() to the top-level list BEFORE concatenation, we drop the 
# journal prefixes (e.g., "journal_name_filtered_dataset") while perfectly 
# preserving the original article IDs as the names of the final list elements.
ExEco_Dataset <- do.call(c, unname(all_filtered_datasets))

# ─────────────────────────────────────────────────────────────
# 2. Format Author Variable
# ─────────────────────────────────────────────────────────────

# Iterate through the dataset to clean and format the "author" field.
# The JSON to R conversion often leaves authors in nested lists (e.g., `[[1]] list()`).
# We flatten these structures and concatenate multiple authors into a single string.
ExEco_Dataset <- lapply(ExEco_Dataset, function(article) {
  
  # Flatten the author list to remove nested structures
  flat_authors <- unlist(article$author)
  
  # Check if the resulting vector is empty (handles NULL and list()) or purely NAs
  if (length(flat_authors) == 0 || all(is.na(flat_authors))) {
    article$author <- NA_character_
  } else {
    # Filter out any potential NAs mixed with actual names
    valid_authors <- flat_authors[!is.na(flat_authors)]
    
    # Collapse multiple authors into a single string separated by " ; "
    article$author <- paste(valid_authors, collapse = " ; ")
  }
  
  return(article)
})

# Output a quick validation message to the console
message(sprintf("​🗃️​​ ExEco_Dataset successfully created. Total number of articles: %d", length(ExEco_Dataset)))

#' @title apply_french_title_case
#'
#' @description 
#' Iterates through a nested article dataset and applies a French-aware 
#' title case transformation to selected character fields.
#'
#' The function modifies the following fields when they exist and are not NA:
#' - `document_title`
#' - `standardized_journal_name`
#' - `original_journal_name`
#'
#' Transformation rules:
#' - Converts text to lowercase first.
#' - Capitalizes the first word of the title strictly.
#' - Capitalizes content words (nouns, verbs, adjectives).
#' - Keeps updated list of "small words" (articles, prepositions, conjunctions) in lowercase.
#' - Handles apostrophes intelligently:
#'    - "L'histoire" -> "L'Histoire" (First word capitalized)
#'    - "Cours d'histoire" -> "Cours d'Histoire" (Prefix lowercase inside title)
#'    - "D'un monde" -> "D'un Monde" (Suffix "un" stays lowercase if it's a small word)
#'
#' @param data_list A named list of articles where each element is a list
#'                  containing bibliographic metadata fields.
#'
#' @return The modified `data_list` with updated title-cased fields.
#'
#' @importFrom purrr map
#' @importFrom stringr str_to_lower str_to_title str_split str_detect

apply_french_title_case <- function(data_list) {

  # List of French small words (articles, prepositions, conjunctions, elided forms)
  small_words <- c(
    # Articles
    "le", "la", "les", "un", "une", "des", "du", "au", "aux",
    # Common prepositions (short to medium length)
    "de", "à", "en", "sur", "dans", "par", "pour", "sous", 
    "vers", "avec", "sans", "chez", "entre", "contre", "outre",
    # Conjunctions
    "et", "ou", "ni", "car", "mais",
    # Elided forms (before apostrophe) and pronouns
    "d", "l", "qu", "c", "j", "m", "n", "s", "t", "y"
  )

  french_title_case <- function(x) {

    if (is.null(x) || is.na(x)) return(x)

    x <- stringr::str_to_lower(x)

    words <- stringr::str_split(x, " ", simplify = TRUE)

    result <- apply(words, 1, function(row_words) {

      sapply(seq_along(row_words), function(i) {

        word <- row_words[i]
        is_first_word <- (i == 1)

        # Handle apostrophes (e.g., "d'histoire", "l'analyse", "d'un")
        if (stringr::str_detect(word, "'")) {

          parts <- stringr::str_split(word, "'", simplify = TRUE)
          
          # Only checking the first split (prefix) and second split (suffix)
          prefix <- parts[1]
          suffix <- parts[2]

          # LOGIC FOR PREFIX (before apostrophe)
          # If it is the first word of the title, capitalize it (e.g., "L'...")
          # Otherwise, keep it lowercase if it is in small_words (e.g., "... d'...")
          if (is_first_word) {
            prefix <- stringr::str_to_title(prefix)
          } else {
            prefix <- ifelse(prefix %in% small_words, prefix, stringr::str_to_title(prefix))
          }

          # LOGIC FOR SUFFIX (after apostrophe)
          # If the suffix is a small word (e.g., "un" in "d'un"), keep it lowercase.
          # Otherwise, capitalize it (e.g., "Histoire" in "d'Histoire").
          suffix <- ifelse(suffix %in% small_words, 
                           suffix, 
                           stringr::str_to_title(suffix))

          return(paste0(prefix, "'", suffix))
        }

        # --- Standard word handling (no apostrophe) ---

        # Always capitalize the very first word of the string
        if (is_first_word) {
          return(stringr::str_to_title(word))
        }

        # Keep small words lowercase
        if (word %in% small_words) {
          return(word)
        }

        # Default: capitalize content words
        stringr::str_to_title(word)

      })

    })

    paste(result, collapse = " ")
  }

  purrr::map(data_list, function(article) {

    if (!is.null(article$document_title) && !is.na(article$document_title)) {
      article$document_title <- french_title_case(article$document_title)
    }

    if (!is.null(article$standardized_journal_name) && !is.na(article$standardized_journal_name)) {
      article$standardized_journal_name <- french_title_case(article$standardized_journal_name)
    }

    if (!is.null(article$original_journal_name) && !is.na(article$original_journal_name)) {
      article$original_journal_name <- french_title_case(article$original_journal_name)
    }

    article
  })
}

ExEco_Dataset <- apply_french_title_case(ExEco_Dataset)