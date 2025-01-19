library(tidyverse)
library(readxl)


# Set file paths
data_dir_path = "datasets/"
mapping_file_path = "mapping_file.xlsx"


# Read the mapping file into a big table containing data from every Excel sheet
map = tibble()
for(sheet_name in excel_sheets(mapping_file_path)) {
  map = mapping_file_path %>%
    read_excel(sheet = sheet_name, col_types = "text") %>% 
    mutate(sheet = sheet_name, .before = dataset) %>% 
    bind_rows(map)
  rm(sheet_name)
}
map = rename(map, Dataset_ID = dataset, cleaned_orig_field_name = orig_field)


# Arrange all lists of NCIT terms in alphabetical order
map = map %>% 
  mutate(across(starts_with("NCIT"), function(vals) {
    vals %>% 
      str_split("\\|\\|") %>% 
      map(~ sort(.x) %>% paste(collapse = "||")) %>% 
      unlist()
  }))


# Create massive database:
# For each dataset...
database = tibble()
for (filename in list.files(data_dir_path)) {
  
  # Read in the database
  database = read_tsv(
    paste0(data_dir_path, filename),
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
    ) %>%
    
    # Pivot everything except Dataset_ID and Sample_ID
    pivot_longer(cols = -c(Dataset_ID, Sample_ID), names_to = "cleaned_orig_field_name", values_to = "orig_values") %>% 
    filter(!is.na(orig_values)) %>% 
    
    # Add it to the database
    bind_rows(database)
}
rm(filename)


# ******************
#  CREATE DATABASES
# ******************

categorical = map %>% 
  filter(sheet == "Categorical") %>% 
  separate_rows(orig_values, sep = "\\|\\|") %>% 
  select(Dataset_ID, cleaned_orig_field_name, orig_values, NCIT_field, NCIT_values, comments) %>% 
  unique() %>% 
  inner_join(database) %>% 
  select(Dataset_ID, Sample_ID, NCIT_field, NCIT_values, cleaned_orig_field_name, orig_values, comments)

numerical = map %>% 
  filter(sheet == "Numerical") %>% 
  select(Dataset_ID, cleaned_orig_field_name, NCIT_field, NCIT_values, comments) %>%
  unique() %>%
  inner_join(database) %>%
  select(Dataset_ID, Sample_ID, NCIT_field, NCIT_values, cleaned_orig_field_name, orig_values, comments)

ranged = map %>% 
  filter(sheet == "Ranged") %>% 
  select(Dataset_ID, cleaned_orig_field_name, orig_values, remapped_values, NCIT_field, NCIT_values, comments) %>% 
  unique() %>% 
  inner_join(database) %>%
  select(Dataset_ID, Sample_ID, NCIT_field, NCIT_values, cleaned_orig_field_name, remapped_values, orig_values, comments)

ids = map %>% 
  filter(sheet == "IDs") %>% 
  select(Dataset_ID, cleaned_orig_field_name, NCIT_field, comments) %>% 
  unique() %>%
  inner_join(database) %>% 
  select(Dataset_ID, Sample_ID, NCIT_field, cleaned_orig_field_name, orig_values, comments)

