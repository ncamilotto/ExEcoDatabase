# Check if the 'clean_environment' variable exists and is set to TRUE
# The exists() check prevents an error if the variable was not defined.

if (exists("clean_environment") && clean_environment == TRUE) {
  
  # Create a character vector containing the names of the objects to keep.
  objects_to_keep <- c("intermediate_objects", "ExEco_Dataset", "clean_environment", "raw_data_list", "cleaned_data_list")
  
  # Use setdiff() to find all objects in the environment (ls())
  objects_to_remove <- setdiff(ls(), objects_to_keep)
  
  # Remove the list of objects identified for removal.
  rm(list = objects_to_remove)
  
}

if (exists("clean_environment") && clean_environment == TRUE) {
  
  rm(clean_environment, objects_to_remove)

}