#create_execo_index

# Transform the list into a dataframe without the "text" column
ExEco_df <- bind_rows(lapply(ExEco_Dataset, function(x) {
  
  # Remove the "text" element from each entry
  x$text <- NULL
  
  # Replace NULL values with NA and extract the first element if vector length > 1
  x <- lapply(x, function(val) {
    if (is.null(val)) NA else val[1]
  })
  
  # Convert each entry to a single-row dataframe
  as.data.frame(x, stringsAsFactors = FALSE)
}))

# Reset row names to avoid keeping the original list names as row indices
rownames(ExEco_df) <- NULL