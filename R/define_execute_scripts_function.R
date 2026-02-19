#' @title define_execute_scripts_function
#' 
#' @description 
#' Iterates through a list of script filenames and executes them sequentially 
#' using `source()`. This is useful for running a data pipeline step by step.
#' 
#' The function verifies that each file exists before attempting to run it. 
#' If a file is missing, execution stops immediately with an error message.
#'
#' @param script_list A character vector containing the names of the R scripts 
#'                    to execute.
#' @param base_path A character string specifying the directory path where 
#'                  the scripts are located.
#'
#' @return NULL (invisibly). This function is called for its side effects 
#'         (executing code from other files).


execute_scripts <- function(script_list, base_path) {
  for (script_name in script_list) {
    script_path <- file.path(base_path, script_name)
    if (!file.exists(script_path)) {
      stop("❌ ERROR: Script not found: ", script_path)
    }
    cat("▶️ Executing:", script_name, "\n")
    source(script_path, encoding = "UTF-8")
  }
}