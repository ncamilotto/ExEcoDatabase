# The ExEco Dataset: A Textual Dataset of French Economic and Social Thought (1918–1960)

![Language](https://img.shields.io/badge/Language-R-blue.svg)
![License](https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg)
![Data](https://img.shields.io/badge/Data-Nakala-orange.svg)
[![Dashboard](https://img.shields.io/badge/Dashboard-Interactive_Exploration-success.svg)](https://ncamilotto.github.io/ExEcoDatabase/)

This repository contains the source code, processing pipeline, and interactive dashboard used to generate and explore the **ExEco Dataset**, a comprehensive textual corpus dedicated to the history of French economic and social thought.

**Author:** *Nicolas Camilotto (Université Côte D’Azur, CNRS, GREDEG)*

## 📊 Interactive Data Explorer

An interactive **Quarto Dashboard** accompanies this dataset, allowing users to dynamically explore the metadata, visualize temporal distributions across journals, analyze page counts, and search for specific authors or titles before downloading the raw files.

👉 **[Explore the ExEco Dataset Dashboard here](https://ncamilotto.github.io/ExEcoDatabase/)**

## 📄 Related Publication

This project is the technical companion to the following data paper, which details the historical context, epistemological choices, and research potential of the corpus:

    [TBD - Insert Citation Here]

## 🎯 Abstract

The ExEco Dataset documents a pivotal transitional era in French intellectual history (1918–1960), moving from traditional political economy toward professionalized, technocratic economics. The dataset comprises over 26,000 documents extracted from eleven major periodicals covering economics, as well as neighboring disciplines (sociology, history, law) and generalist intellectual debate.

This repository focuses on the data engineering stage: transforming noisy, page-level OCR outputs into a structured, article-level dataset with high-quality metadata, strict adherence to Tidy data principles, and rigorously cleaned text ready for Natural Language Processing (NLP).

## ⚙️ Methodology

The pipeline transforms the **Raw OCR Dataset** (page-level text stored in a JSON array, preserving structural artifacts like LaTeX math tags) into the **Curated Dataset** (article-level text, fully cleaned) through the following steps:

### 1. Backward Compatibility & Tidy Format
The raw input uses a strictly Tidy JSON structure where pages are stored in an array ("pages": [...]) to avoid sparse matrices. The restore_legacy_pages_structure.R adapter safely unpacks this array in memory to feed the downstream text processing functions.

### 2. Text Cleaning & Formatting
The text undergoes a comprehensive preprocessing pipeline to optimize its usability for downstream NLP analyses while strictly preserving the internal structure (paragraph and line breaks):
- **LaTeX & Markup Translation:** Translates complex OCR-generated LaTeX mathematical expressions into readable plain text while removing HTML/Markdown artifacts.
- **Typography & OCR Repair:** Standardizes quotation marks and spacing, and corrects common OCR confusions specific to French text (e.g., lowercase 'l' vs uppercase 'I').
- **De-hyphenation:** Systematically reconstructs words split across line breaks.

### 3. Article Segmentation (The "Cutting" Process)
A major challenge in historical digitization is the absence of systematic page breaks between articles. This repository implements a robust semi-automated R procedure to reconstruct article boundaries:
- **Start Boundary:** The script scans the OCR text using fuzzy matching (Levenshtein distance) to locate the current article's Title on its first page.
- **End Boundary:** To identify where the article stops, the algorithm employs a dual detection strategy. It searches for the Author's Signature (surname or initials) and the Next Article's Title on the final pages.
- **Multi-Author Robustness:** The algorithm dynamically iterates through the list of co-authors, validating the cut as soon as a recognized signature is matched.

## 📂 Repository Structure

The project relies on renv to ensure full reproducibility of the computational environment.

    .
    ├── R/
    │   ├── apply_cleaning_function.R  
    │   ├── apply_cleaning_hyphenation_function.R  
    │   ├── clean_environment.R  
    │   ├── combine_article_pages.R
    │   ├── create_custom_metadata.R
    │   ├── create_execo_dataset.R
    │   ├── create_execo_index.R
    │   ├── create_needs_truncation_variable.R
    │   ├── data_sanitization.R
    │   ├── define_cleaning_function.R
    │   ├── define_execute_scripts_function.R
    │   ├── define_reconstruct_hyphenated_words_function.R
    │   ├── extract_article_content.R
    │   ├── extract_start_end_pages.R
    │   ├── filter_journal.R
    │   ├── finalize_dataset.R
    │   ├── minimal_pipeline.R
    │   ├── restore_legacy_pages_structure.R 
    │   ├── save_intermediate_objects.R
    │   ├── truncate_after_author.R
    │   ├── truncate_after_next_title.R
    │   └── truncate_before_title.R
    ├── renv/
    │   ├── .gitignore  
    │   └── activate.R  
    ├── index.qmd 
    ├── .Rprofile
    ├── .gitignore
    ├── README.md
    ├── main_exco_dataset.R
    └── renv.lock


## 📥 Data Availability

The code in this repository requires the Raw OCR Files to run. Both the Raw input and the final Curated output are permanently hosted on Nakala and adhere to FAIR principles:

> **Data Repository (Nakala):** https://doi.org/10.34847/NKL.0DEE15OW

## 🚀 How to Reproduce

This project is fully reproducible using renv for package management and .Renviron for path configuration.

### 1. Setup
Clone this repository to your local machine:
    git clone https://github.com/ncamilotto/ExEcoDatabase.git
    cd ExEcoDatabase


### 2. Install Dependencies
Open an R session in the project root. renv should automatically activate. Restore the environment to install the exact package versions used:

    renv::restore()


### 3. Data Preparation
Download the latest **Raw OCR Output JSON file** from the Nakala repository and save it to a secure location on your machine.

### 4. Configuration (.Renviron)
To link the code to your local files without modifying the scripts, you must create an .Renviron file in the project root.
Create a file named .Renviron (no extension) in the ExEcoDatabase/ folder. Paste the following content and adapt the paths to your local setup:

    # Path to the folder containing the R scripts in this repository
    PATH_SCRIPTS="C:/path/to/your/ExEcoDatabase/R"

    # Full path to the downloaded Raw JSON dataset
    PATH_ExEco_Raw_OCR_Output.json="C:/path/to/your/data/ExEco_Raw_OCR_Output_1918_1960_v2.0.json"


### 5. Run the Pipeline
Open the main_exco_dataset.R file located in the root of the repository. This script orchestrates the entire pipeline using the paths defined in your environment.

## ⚖️ License

* The **Code** in this repository is available under the MIT License.
* The curated **ExEco Dataset** is released under the Creative Commons Attribution-NonCommercial 4.0 International (CC BY-NC 4.0) license.