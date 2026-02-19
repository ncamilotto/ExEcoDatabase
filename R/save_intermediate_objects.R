#save_intermediate_objects

if (exists("journal_filtered_extracted_pages")) {
  
  object_name <- paste0(journal_label, "_journal_filtered_extracted_pages")
  
  assign(object_name, journal_filtered_extracted_pages)

  intermediate_objects[[object_name]] <- journal_filtered_extracted_pages
  
  rm(journal_filtered_extracted_pages)
}

if (exists("undetected_title_articles")) {
  
  object_name <- paste0(journal_label, "_undetected_title_articles")
  
  assign(object_name, undetected_title_articles)
  
  intermediate_objects[[object_name]] <- undetected_title_articles

  rm(undetected_title_articles)
}

if (exists("undetected_next_title_articles")) {
  
  object_name <- paste0(journal_label, "_undetected_next_title_articles")
  
  assign(object_name, undetected_next_title_articles)
  
  intermediate_objects[[object_name]] <- undetected_next_title_articles

  rm(undetected_next_title_articles)
}

if (exists("clipping_markers")) {
  
  object_name <- paste0(journal_label, "_clipping_markers")
  
  assign(object_name, clipping_markers)
  
  intermediate_objects[[object_name]] <- clipping_markers

  rm(clipping_markers)
}

if (exists("filtered_dataset")) {
  
  object_name <- paste0(journal_label, "_filtered_dataset")
  
  assign(object_name, filtered_dataset)
  
  intermediate_objects[[object_name]] <- filtered_dataset

  rm(filtered_dataset)
}

if (exists("undetected_author_articles")) {
  
  object_name <- paste0(journal_label, "_undetected_author_articles")
  
  assign(object_name, undetected_author_articles)
  
  intermediate_objects[[object_name]] <- undetected_author_articles

  rm(undetected_author_articles)
}

if (exists("articles_extraction_failed")) {
  
  object_name <- paste0(journal_label, "_articles_extraction_failed")
  
  assign(object_name, articles_extraction_failed)
  
  intermediate_objects[[object_name]] <- articles_extraction_failed

  rm(articles_extraction_failed)
}