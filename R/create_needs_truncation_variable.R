#' @title detect_page_overlaps
#' 
#' @description
#' Identifies articles that end on the same page where another article begins
#' within the same journal issue. It marks these articles for truncation and
#' records the titles of the overlapping articles.
#'
#' @param data_list A named list of article objects containing metadata 
#'   (publication_year, standardized_journal_name, volume, issue, start_page, 
#'   end_page, document_title).
#' 
#' @return A modified list where articles with overlaps have `needs_truncation` 
#'   set to "1" and contain additional fields (`overlap_title_n`) listing the 
#'   overlapping titles. Articles without overlaps have `needs_truncation` set to "0".
#' 

detect_page_overlaps <- function(data_list) {
  
  # --- Input validation ---
  if (!is.list(data_list) || length(data_list) == 0) {
    stop("data_list must be a non-empty list")
  }
  
  if (is.null(names(data_list))) {
    stop("data_list must be a named list")
  }
  
  # --- STEP 1: Create the search index ---
  
  index_df <- map_dfr(data_list, function(article) {
    tibble(
      publication_year = as.character(article$publication_year),
      journal = as.character(article$standardized_journal_name),
      volume = as.character(article$volume),
      issue = as.character(article$issue),
      start_page = as.character(article$start_page),
      end_page = as.character(article$end_page),
      title = as.character(article$document_title)
    )
  }, .id = "article_id")
  
  # Extract base article ID (without author suffix)
  index_df <- index_df %>%
    mutate(base_article_id = str_replace(article_id, "-\\d+$", ""))
  
  # Cleaning: remove articles without valid pages
  index_df <- index_df %>%
    filter(!is.na(start_page) & start_page != "", 
           !is.na(end_page) & end_page != "")
  
  # --- STEP 2: Detect overlaps ---
  
  overlaps <- index_df %>%
    inner_join(index_df, 
               by = c("publication_year", "journal", "volume", "issue"), 
               suffix = c("_current", "_next"),
               relationship = "many-to-many"
    ) %>%
    filter(
      article_id_current != article_id_next,
      base_article_id_current != base_article_id_next,  # Exclude same article with different author suffixes
      end_page_current == start_page_next
    ) %>%
    select(article_id_current, title_next, base_article_id_current) %>%
    distinct()
  
  # --- STEP 3: Update the list ---
  
  # Initialize needs_truncation to "0" everywhere
  data_list <- map(data_list, function(article) {
    article$needs_truncation <- "0"
    return(article)
  })
  
  # Mark articles with overlaps
  unique_ids_with_overlap <- unique(overlaps$article_id_current)
  
  for (id in unique_ids_with_overlap) {
    overlap_titles <- overlaps$title_next[overlaps$article_id_current == id]
    
    if (length(overlap_titles) > 0) {
      # Mark the need for truncation
      data_list[[id]]$needs_truncation <- "1"
      
      # Add overlapping titles
      for (j in seq_along(overlap_titles)) {
        var_name <- paste0("overlap_title_", j)
        data_list[[id]][[var_name]] <- overlap_titles[j]
      }
    }
  }
  
  return(data_list)
}

# Apply the function
cleaned_data_list <- detect_page_overlaps(cleaned_data_list)