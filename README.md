# The ExEco Dataset: A Textual Dataset of French Economic and Social Thought (1918–1960)

![alt text](https://img.shields.io/badge/Language-R-blue.svg)
![alt text](https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg)
![alt text](https://img.shields.io/badge/Data-Nakala-orange.svg)

This repository contains the source code and processing pipeline used to generate the ExEco Dataset, a comprehensive textual dataset dedicated to the history of French economic and social thought.

**Title:** *The ExEco Dataset: A Textual Dataset of French Economic and Social Thought (1918–1960)*

**Author:** *Nicolas Camilotto (Université Côte D’Azur, CNRS, GREDEG)*

## Related Publication

This project is associated with the following data paper:

```text
tdb
```

## Abstract

The ExEco Corpus documents the emergence of economic expertise in France, covering the professionalization of the discipline and the continuity of economic thought across the Vichy regime. The dataset comprises 26 212 documents extracted from eleven major periodicals covering economics, sociology, and history.

This repository focuses on the data processing stage: transforming noisy, page-level OCR outputs into a structured, article-level dataset with high-quality metadata and cleaned text.

## Repository Structure

The project relies on renv to ensure reproducibility of the environment.

```text
.
├── R/
│   ├── add_last_characters_snippet.R  
│   └── apply_cleaning_function.R  
│   └── apply_cleaning_hyphenation_function.R  
│   └── clean_environment.R  
│   └── combine_article_pages.R
│   └── common_apply_cleaning_hyphenation_function.R
│   └── create_custom_metadata.R
│   └── create_execo_dataset.R
│   └── create_execo_index.R
│   └── create_needs_truncation_variable.R
│   └── data_sanitization.R
│   └── define_cleaning_function.R
│   └── define_execute_scripts_function.R
│   └── define_reconstruct_hyphenated_words_function.R
│   └── extract_article_content.R
│   └── extract_start_end_pages.R
│   └── filter_journal.R
│   └── finalize_dataset.R
│   └── minimal_pipeline.R
│   └── save_dataset.R
│   └── save_intermediate_objects.R
│   └── truncate_after_author.R
│   └── truncate_after_next_title.R
│   └── truncate_before_title.R
├── renv/
│   ├── .gitignore  
│   └── activate.R  
├── .Rprofile
├── .gitignore
├── README.md
├── main_exco_dataset.R
└── renv.lock
```

## Methodology

The pipeline transforms the Raw OCR Dataset (page-level text with artifacts) into the Curated Dataset (article-level text, cleaned) through the following steps:

### 1. Article Segmentation (The "Cutting" Process)

A major challenge in the raw files is the absence of systematic page breaks between articles. This repository implements a semi-automated R procedure to reconstruct article boundaries:
- Start Boundary: The script scans the OCR text using fuzzy matching to locate the current article's Title.
- End Boundary: To identify where the article stops, the algorithm employs a dual detection strategy. It searches for the Author's Signature (surname or initials) and the Next Article's Title.
- Truncation: Based on these detected markers, all text preceding the current title and following the signature (and/or the next title) is removed to strictly isolate the article content.

### 2. Text Cleaning

Once isolated, the text undergoes aggressive cleaning to optimize it for Natural Language Processing (NLP):
- De-hyphenation: Reconstructing words split across lines.
- Noise Removal: Stripping OCR artifacts, layout tags, and headers/footers.
- Typography: Normalizing quotes and spacing.

## Data Availability

The code in this repository requires the Raw OCR Files to run. Both the Raw input and the final Curated output are hosted on Nakala:

    Data Repository (Nakala): https://doi.org/10.34847/NKL.0DEE15OW

    Input: ExEco_Raw_v1.0.json (Raw OCR text)
    Output: ExEco_Curated_v1.0.json (Cleaned text)

## How to Reproduce

This project is fully reproducible using renv for package management and .Renviron for path configuration.

### 1. Setup

Clone this repository to your local machine:

```sh
git clone https://github.com/ncamilotto/ExEcoDatabase.git
cd ExEcoDatabase
```

### 2. Install Dependencies

Open an R session in the project root. renv should automatically activate. Restore the environment to install the exact package versions used:

```R
renv::restore()
```

### 3. Data Preparation

Download the latest Raw OCR Dataset from the Nakala repository (https://doi.org/10.34847/NKL.0DEE15OW) and save it to a location of your choice on your machine.

### 4. Configuration (.Renviron)

To link the code to your local files without modifying the scripts, you must create an .Renviron file in the project root.
Create a file named .Renviron (no extension) in the ExEcoDatabase/ folder.
Paste the following content and adapt the paths to your local setup:

```Bash
# Path to the folder containing the R scripts in this repository
PATH_SCRIPTS="path/to/your/ExEcoDatabase/R"

# Full path to the downloaded Raw JSON dataset
PATH_ExEco_Raw_OCR_Output.json="path/to/your/data/execo_raw_ocr_output.json"
```

### 5. Run the Pipeline

Open the main.R file located in the root of the repository. This script orchestrates the entire pipeline using the paths defined in your environment.

## License

The code in this repository is available under the MIT License.
The curated ExEco Dataset is released under the Creative Commons Attribution-NonCommercial 4.0 International (CC BY-NC 4.0) license.
Note: This license applies to the curated dataset and annotations. The underlying original documents published before 1956 are in the public domain.

## Citation

If you use this dataset or code in your research, please cite the following paper:

```text
tbd
```