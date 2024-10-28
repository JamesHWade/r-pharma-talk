library(elmer)
library(tidyverse)
library(pdftools)

read_pdf <- function(file_path) {
  pdf_text(file_path) |> read_lines() |> str_squish() |> str_c(collapse = " ")
}

study_info_spec <- type_object(
  "Information about a medical research study",
  # Basic study information
  title = type_string("The title of the study."),
  authors = type_array(
    "List of authors with names and affiliations.",
    items = type_object(
      name = type_string("Name of the author."),
      affiliation = type_string("Affiliation of the author.")
    )
  ),
  published_date = type_string("The date the study was published."),
  journal = type_string("The journal in which the study is published."),
  condition_addressed = type_string("The medical condition that is being addressed in the study."),
  study_objective = type_string("The objective of the study."),
  study_design = type_string("The design of the study."),
  
  # Participant information
  participants = type_object(
    "Details about study participants including criteria.",
    number = type_number("Number of participants in the study."),
    inclusion_criteria = type_string("Inclusion criteria for participants."),
    exclusion_criteria = type_string("Exclusion criteria for participants.")
  ),
  
  # Intervention details
  intervention = type_object(
    "Details about the interventions conducted in the study.",
    dosage_groups = type_array(
      "Dosage groups for the intervention.",
      items = type_number()
    ),
    duration = type_string("Duration of the study intervention.")
  ),
  
  # Outcome measures
  outcome_measures = type_object(
    "Details about the outcome measures used in the study.",
    primary_endpoint = type_string("Primary outcome measurement."),
    secondary_endpoints = type_string("Secondary outcome measurements."),
    exploratory_measures = type_string("Exploratory outcome measurements.")
  ),
  
  # Results
  results = type_object(
    "Results of the study.",
    primary_outcome = type_string("Primary outcome of the study."),
    secondary_outcomes = type_string("Secondary outcomes of the study."),
    safety_and_adverse_events = type_string("Overview of safety and adverse events.")
  ),
  
  # Discussion
  discussion = type_object(
    "Discussion and conclusions of the study.",
    efficacy_comparison = type_string("Comparison of efficacy with other treatments."),
    safety_profile = type_string("Safety profile comparison."),
    implications = type_string("Implications of the study findings.")
  )
)

# Function to flatten study data into a table format
studies_to_table <- function(studies_list) {
  # If a single study is provided, wrap it in a list
  if (!is.null(studies_list$title)) {
    studies_list <- list(studies_list)
  }
  
  # Function to format authors as a single string
  format_authors <- function(authors) {
    author_names <- sapply(authors, function(x) x$name)
    paste(author_names, collapse = "; ")
  }
  
  # Function to format author affiliations
  format_affiliations <- function(authors) {
    affiliations <- unique(sapply(authors, function(x) x$affiliation))
    paste(affiliations, collapse = "; ")
  }
  
  # Function to format dosage groups
  format_dosages <- function(dosage_groups) {
    paste(dosage_groups, collapse = ", ")
  }
  
  # Convert each study to a single row
  studies_df <- map_df(studies_list, function(study) {
    tibble(
      # Basic study information
      title = study$title,
      authors = format_authors(study$authors),
      affiliations = format_affiliations(study$authors),
      journal = study$journal,
      published_date = study$published_date,
      condition = study$condition_addressed,
      
      # Study characteristics
      study_objective = study$study_objective,
      study_design = study$study_design,
      
      # Participant information
      n_participants = study$participants$number,
      inclusion_criteria = study$participants$inclusion_criteria,
      exclusion_criteria = study$participants$exclusion_criteria,
      
      # Intervention details
      dosage_groups = format_dosages(study$intervention$dosage_groups),
      duration = study$intervention$duration,
      
      # Outcomes
      primary_endpoint = study$outcome_measures$primary_endpoint,
      secondary_endpoints = study$outcome_measures$secondary_endpoints,
      exploratory_measures = study$outcome_measures$exploratory_measures,
      
      # Results
      primary_outcome = study$results$primary_outcome,
      secondary_outcomes = study$results$secondary_outcomes,
      safety_events = study$results$safety_and_adverse_events,
      
      # Discussion
      efficacy_comparison = study$discussion$efficacy_comparison,
      safety_profile = study$discussion$safety_profile,
      implications = study$discussion$implications
    )
  })
  
  studies_df
}

# Example usage:
# chat <- chat_openai(model = "gpt-4o")
# text <- read_pdf(file_path = "NEJMoa2302392.pdf")
# data <- chat$extract_data(text, spec = study_info_spec)
# data_tbl <- studies_to_table(data)
# write_csv(data_tbl, "extracted_study_info.csv")

readr::read_csv("extracted_study_info.csv",
                col_types = cols(.default = "c")) |>
  pivot_longer(cols = everything()) |> 
  gt::gt() |> 
  gtExtras::gt_theme_pff()
