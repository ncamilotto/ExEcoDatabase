#' @title generate_initials
#' 
#' @description
#' Converts an author's full name (formatted as "Surname, Firstname") into 
#' initials. It handles specific cases such as surnames starting with the 
#' particle "de", compound first names, and ensures the output has at least
#' 2 letters.
#'
#' @param author_name A character string representing the author's name.
#' @param order A character string indicating the order of initials.
#'   \code{"firstname_first"} (default) returns "M. B." for "Bloch, Marc".
#'   \code{"surname_first"} returns "B. M." for "Bloch, Marc".
#' 
#' @return A character string containing the formatted initials,
#'  or NA if the input is NA, invalid, or results in fewer than 2 letters.

generate_initials <- function(author_name, order = c("firstname_first", "surname_first")) {
  order <- match.arg(order)
  
  if (is.na(author_name)) return(NA_character_)
  
  final_initials <- NA_character_
  
  if (!grepl(",", author_name)) {
    final_initials <- author_name
  } else {
    name_parts <- trimws(unlist(strsplit(author_name, ",")))
    
    if (length(name_parts) >= 2) {
      surname    <- name_parts[1]
      first_name <- name_parts[2]
      
      # Handle surnames starting with "de"
      if (startsWith(tolower(surname), "de ")) {
        surname_without_prefix <- sub("^de\\s+", "", surname, ignore.case = TRUE)
        surname_initial        <- paste0("de ", toupper(substr(surname_without_prefix, 1, 1)), ".")
      } else {
        # Handle compound surnames (hyphenated)
        surname_parts <- unlist(strsplit(surname, "-"))
        if (length(surname_parts) > 1) {
          surname_initials <- paste0(
            toupper(substr(trimws(surname_parts), 1, 1)),
            collapse = ".-"
          )
          surname_initial <- paste0(surname_initials, ".")
        } else {
          surname_initial <- paste0(toupper(substr(surname, 1, 1)), ".")
        }
      }
      
      # Handle compound first names (hyphenated)
      first_name_parts <- unlist(strsplit(first_name, "-"))
      if (length(first_name_parts) > 1) {
        first_name_initials <- paste0(
          toupper(substr(trimws(first_name_parts), 1, 1)),
          collapse = ".-"
        )
        first_name_initials <- paste0(first_name_initials, ".")
      } else {
        # Handle space-separated first names (e.g., "M H")
        first_name_parts <- unlist(strsplit(trimws(first_name), "\\s+"))
        first_name_initials <- paste(
          paste0(toupper(substr(trimws(first_name_parts), 1, 1)), "."),
          collapse = " "
        )
      }
      
      # Combine initials according to order
      final_initials <- if (order == "firstname_first") {
        paste(first_name_initials, surname_initial)
      } else {
        paste(surname_initial, first_name_initials)
      }
    }
  }
  
  if (is.na(final_initials)) return(NA_character_)
  
  nb_letters <- nchar(gsub("[^[:alpha:]]", "", final_initials))
  
  if (nb_letters < 2) return(NA_character_)
  
  return(final_initials)
}

#' @title extract_surname
#' 
#' @description
#' Parses an author's name string to extract the main surname. 
#' If the input format is "Surname, Firstname", it isolates the part before the comma.
#' If no comma is present, it considers the whole string as the potential surname.
#' For compound surnames, it applies a heuristic to select the longest part as the main surname.
#'
#' @param author_name A character string representing the author's name.
#' 
#' @return A character string containing the extracted main surname, or NA_character_
#'   if the input is invalid or the extracted surname is too short (< 3 chars).
#' @importFrom stringr str_split

extract_surname <- function(author_name) {
  if (is.na(author_name)) return(NA_character_)
  
  full_surname <- trimws(str_split(author_name, ",")[[1]][1])
  
  # Split by space or hyphen
  surname_parts <- str_split(full_surname, " |-")[[1]]
  surname_parts <- trimws(surname_parts)
  
  # Remove empty strings that might result from splitting (e.g. "Name - Name")
  surname_parts <- surname_parts[surname_parts != ""]
  
  if (length(surname_parts) > 1) {
    # Heuristic: keep the longest part as the main surname
    main_surname <- surname_parts[which.max(nchar(surname_parts))]
  } else if (length(surname_parts) == 1) {
    main_surname <- surname_parts[1]
  } else {
    return(NA_character_)
  }
  
  # Return NA if surname is too short (likely noise)
  if (nchar(main_surname) < 3) return(NA_character_)
  
  return(main_surname)
}

# Apply enrichment to the list
journal_filtered_extracted_pages <- lapply(journal_filtered_extracted_pages, function(article) {
  
  current_author <- article$author
  
  # 1. Extract surname
  extracted_surname <- extract_surname(current_author)
  
  # 2. Calculate initials (both orders)
  if (!is.na(current_author)) {
    author_initials_firstname_first <- generate_initials(current_author, order = "firstname_first")
    author_initials_surname_first   <- generate_initials(current_author, order = "surname_first")
  } else {
    author_initials_firstname_first <- NA_character_
    author_initials_surname_first   <- NA_character_
  }
  
  # 3. Add new fields
  article$surname                  <- extracted_surname
  article$initials_firstname_first <- author_initials_firstname_first
  article$initials_surname_first   <- author_initials_surname_first
  
  return(article)
})

#' @title remove_title_prefixes
#' 
#' @description
#' Cleans article titles by removing standard editorial prefixes, section 
#' headers (rubrics), and "review-like" phrases (e.g., "Review of", 
#' "À propos de"). It specifically targets French academic journal conventions.
#'
#' @param title A character string representing the raw article title.
#' 
#' @return A cleaned character string with prefixes removed and whitespace trimmed. 
#'   Returns NA_character_ if the input is NULL or NA.

remove_title_prefixes <- function(title) {
  
  if (is.null(title) || is.na(title)) {
    return(NA_character_)
  }

  pattern_step1 <- paste0(
    "^\\s*(",
    
      # 1) Review-like prefixes
      "(?:",
        "review\\s+of",
        "|revue\\s+de",
        "|a\\s+propos\\s+de",
        "|à\\s+propos\\s+de",
      ")",
      "\\s+",
      
      "(?:",
        # Case A: Elision (l') -> No space required immediately after
        # (e.g., handles "Review of l'education")
        "l['']",
        "|",
        # Case B: Whole articles -> Space (\\s+) MANDATORY after
        # (e.g., handles "Review of la langue" but avoids cutting "Review of language")
        "(?:la|le|les|un|une|du|des|d['']un|d['']une)\\s+",
      ")?",
      
      "\\s*:?\\s*",
      
      "|",
      
      # 2) Section titles / Rubrics
      "(?:",
        "la\\s+pensée\\s+engagée",
        "|points?\\s+de\\s+vue",
        "|la\\s+cité",
        "|les\\s+essais",
        "|le\\s+roman",
        "|les\\s+lettres",
        "|le\\s+cinéma",
        "|les\\s+événements\\s+et\\s+les\\s+hommes",
        "|l['']éducation",
        "|le\\s+théâtre",
        "|les\\s+livres",
        "|les\\s+arts",
        "|l['']\\s+esprit\\s+du\\s+mois",
      ")",
      "\\s*:\\s*",
    
    ")"
  )
  
  cleaned_title <- sub(pattern_step1, "", title, ignore.case = TRUE)
  cleaned_title <- trimws(cleaned_title)
  
  # Step 2: Handle "Sur" (On/About) prefixes
  pattern_step2 <- paste0(
    "^\\s*",
    "sur",
    "\\s+",
    "(?:",
      "(?:l['']|la|le|les|un|une|du|des|d['']un|d['']une)",
      "\\s+",
    ")?"
  )
  
  cleaned_title <- sub(pattern_step2, "", cleaned_title, ignore.case = TRUE)
  cleaned_title <- trimws(cleaned_title)
  
  return(cleaned_title)
}

#' @title shorten_title
#' 
#' @description
#' Truncates a title string at the first significant punctuation mark found 
#' after the 10th character. This is useful for removing subtitles or long 
#' descriptions while preserving the main title context.
#'
#' @param title A character string representing the full title.
#' 
#' @return A character string containing the shortened title, trimmed of 
#'   whitespace. Returns NA_character_ if the input is NULL or NA.

shorten_title <- function(title) {
  
  if (is.null(title) || is.na(title)) {
    return(NA_character_)
  }
  
  punctuation_regex <- "[\\.:;?!,()\\-«\\[]"
  
  # Truncate at the first punctuation mark after 10 characters
  trunc_pattern <- paste0("^(.{10,}?)", punctuation_regex, ".*$")
  
  if (grepl(trunc_pattern, title, perl = TRUE)) {
    short_version <- sub(trunc_pattern, "\\1", title, perl = TRUE)
  } else {
    short_version <- title
  }
  
  trimws(short_version)
}


#' @title generate_short_titles
#' 
#' @description
#' Iterates through a list of articles to create shortened versions of titles. 
#' It processes both the main `document_title` and any potential `overlap_title_X` 
#' fields. It relies on `remove_title_prefixes` and `shorten_title` to clean 
#' and truncate the text.
#'
#' @param articles_list A list of article objects.
#' 
#' @return A modified list where each article includes:
#'   \itemize{
#'     \item `short_title`: The cleaned and shortened version of the main title.
#'     \item `overlap_short_title_X`: Shortened versions of any overlapping titles found.
#'   }
#' 
generate_short_titles <- function(articles_list) {
  
  articles_list <- lapply(articles_list, function(article) {
    
    cleaned_title <- remove_title_prefixes(article$document_title)
    article$short_title <- shorten_title(cleaned_title)
    
    overlap_fields <- grep("^overlap_title_", names(article), value = TRUE)
    
    for (field in overlap_fields) {
      cleaned_overlap <- remove_title_prefixes(article[[field]])
      
      field_num <- sub("^overlap_title_", "", field)
      short_field_name <- paste0("overlap_short_title_", field_num)
      
      article[[short_field_name]] <- shorten_title(cleaned_overlap)
    }
    
    article
  })
  
  articles_list
}

journal_filtered_extracted_pages <- generate_short_titles(journal_filtered_extracted_pages)