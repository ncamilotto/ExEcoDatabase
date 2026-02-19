# ─────────────────────────────────────────────────────────────
# Library Import 
# ─────────────────────────────────────────────────────────────

library(jsonlite)
library(purrr)
library(stringr)
library(stringi)
library(progress)
library(stringdist)
library(dplyr)

# ─────────────────────────────────────────────────────────────
# Initialization & Data Loading
# ─────────────────────────────────────────────────────────────

# 1. File path definition
file_path <- Sys.getenv("PATH_ExEco_Raw_OCR_Output_1918_1960_v2.0.json")

# 2. Data loading
raw_data_list <- fromJSON(file_path, simplifyVector = FALSE)
raw_data_list <- raw_data_list[order(names(raw_data_list))]

# 3. Retrieving the base path from the .Renviron file
base_path <- Sys.getenv("PATH_SCRIPTS")

# 4. Retrieving the base path from the .Renviron file
script_path <- file.path(base_path, "define_execute_scripts_function.R")
if (!file.exists(script_path)) {
  stop("Script not found: ", script_path)
}
source(script_path, encoding = "UTF-8")

# 4. Retrieving the base path from the .Renviron file
# If FALSE: only key output objects are kept (non-essential intermediates are removed)
# If TRUE: all intermediate objects created during the pipeline are preserved
clean_environment = TRUE

# ─────────────────────────────────────────────────────────────
# Preprocessing
# ─────────────────────────────────────────────────────────────

# List of scripts to execute
preprocessing_scripts <- c(
  "data_sanitization.R",
  "define_cleaning_function.R",
  "define_reconstruct_hyphenated_words_function.R",
  "apply_cleaning_function.R",
  "create_needs_truncation_variable.R",
  "extract_start_end_pages.R"
)

execute_scripts(preprocessing_scripts, base_path)

# ─────────────────────────────────────────────────────────────
# Define base pipelines
# ─────────────────────────────────────────────────────────────

pipelines <- list(
  looking_for_title_first = c(
    "filter_journal.R", 
    "create_custom_metadata.R", 
    "truncate_before_title.R", 
    "truncate_after_next_title.R", 
    "truncate_after_author.R",
    "add_last_characters_snippet.R",
    "combine_article_pages.R",
    "extract_article_content.R",
    "apply_cleaning_hyphenation_function.R",
    "finalize_dataset.R",
    "save_intermediate_objects.R"
  ),
  
  looking_for_author_first = c(
    "filter_journal.R", 
    "create_custom_metadata.R", 
    "truncate_before_title.R", 
    "truncate_after_author.R",
    "truncate_after_next_title.R",
    "add_last_characters_snippet.R",
    "combine_article_pages.R",
    "extract_article_content.R",
    "apply_cleaning_hyphenation_function.R",
    "finalize_dataset.R",
    "save_intermediate_objects.R"
  ),

    only_overlap_title = c(
    "filter_journal.R", 
    "create_custom_metadata.R", 
    "truncate_before_title.R", 
    "truncate_after_next_title.R", 
    "add_last_characters_snippet.R",
    "combine_article_pages.R",
    "extract_article_content.R",
    "apply_cleaning_hyphenation_function.R",
    "finalize_dataset.R",
    "save_intermediate_objects.R"
  ),

    no_final_cut = c(
    "filter_journal.R", 
    "create_custom_metadata.R", 
    "truncate_before_title.R", 
    "add_last_characters_snippet.R",
    "combine_article_pages.R",
    "extract_article_content.R",
    "apply_cleaning_hyphenation_function.R",
    "finalize_dataset.R",
    "save_intermediate_objects.R"
  ),
  
  minimal = c(
    "filter_journal.R",
    "minimal_pipeline.R",
    "apply_cleaning_hyphenation_function.R", 
    "finalize_dataset.R",
    "save_intermediate_objects.R"
  )
)

# ─────────────────────────────────────────────────────────────
# Journal Configuration
# ─────────────────────────────────────────────────────────────

CHAR_THRESHOLD <- 50
WORD_THRESHOLD <- 10

journal_config <- list(
  list(journal_label = "annales_d_histoire_economique_et_sociale", standardized_journal_name = "Annales d'histoire économique et sociale", pipeline = "looking_for_author_first", run = TRUE),
  list(journal_label = "droit_social",                             standardized_journal_name = "Droit Social",                             pipeline = "only_overlap_title",       run = TRUE),
  list(journal_label = "economie_appliquee",                       standardized_journal_name = "Économie appliquée",                       pipeline = "minimal",                  run = TRUE),
  list(journal_label = "x_crise",                                  standardized_journal_name = "X-Crise",                                  pipeline = "no_final_cut",             run = TRUE),
  list(journal_label = "economie_et_humanisme",                    standardized_journal_name = "Economie et humanisme",                    pipeline = "looking_for_title_first",  run = TRUE),
  list(journal_label = "esprit",                                   standardized_journal_name = "Esprit",                                   pipeline = "looking_for_title_first",  run = TRUE),
  list(journal_label = "journal_des_economistes",                  standardized_journal_name = "Journal des économistes",                  pipeline = "minimal",                  run = TRUE),
  list(journal_label = "l_annee_sociologique",                     standardized_journal_name = "L'Année sociologique",                     pipeline = "looking_for_title_first",  run = TRUE),
  list(journal_label = "revue_d_economie_politique",               standardized_journal_name = "Revue d'économie politique",               pipeline = "looking_for_author_first", run = TRUE),
  list(journal_label = "revue_de_l_economie_contemporaine",        standardized_journal_name = "Revue de l'Économie Contemporaine",        pipeline = "no_final_cut",             run = TRUE),
  list(journal_label = "revue_economique",                         standardized_journal_name = "Revue économique",                         pipeline = "looking_for_title_first",  run = TRUE)
)

# ─────────────────────────────────────────────────────────────
# Main Execution Loop
# ─────────────────────────────────────────────────────────────

intermediate_objects <- list()

for (journal in journal_config) {
  if (journal$run) {
    journal_label <- journal$journal_label
    standardized_journal_name <- journal$standardized_journal_name
    
    execute_scripts(pipelines[[journal$pipeline]], base_path)
  }
}

# ─────────────────────────────────────────────────────────────
# Final Aggregation & Cleanup
# ─────────────────────────────────────────────────────────────

script_list <- c(
  "create_execo_dataset.R",
  "clean_environment.R",
  "create_execo_index.R"
)

execute_scripts(script_list, base_path)