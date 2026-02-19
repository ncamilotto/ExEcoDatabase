#common_filter_journal

# Filtering data for the selected journal
journal_filtered_datalist <- cleaned_data_list %>%
  keep(~ .x$standardized_journal_name == standardized_journal_name)

journal_filtered_extracted_pages <- extracted_pages_list %>%
  keep(~ .x$standardized_journal_name == standardized_journal_name)

# Count unique articles
element_names <- names(journal_filtered_extracted_pages)
article_ids <- sub("_.*$", "", element_names)
num_unique_articles <- length(unique(article_ids))

# Display message
message(sprintf(
  "📂 Working on %d %s articles",
  num_unique_articles,
  standardized_journal_name
))