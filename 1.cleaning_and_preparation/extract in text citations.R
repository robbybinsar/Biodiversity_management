library(tidyverse)
library(stringr)
library(officer)

pull_cites <- function (text) {
  str_extract_all(text, "(?<=\\()[A-Z][a-z][^()]* [12][0-9]{3}(?=\\))|[A-Z][a-z]+ \\([12][0-9]{3}[^()]*", simplify = T) %>% 
    gsub("\\(", "", .) %>% 
    str_split(., "; ") %>% 
    unlist()
}

read_doc_as_string <- function(doc_path) {
  # Read the Word document
  doc <- read_docx(doc_path)
  
  # Extract all the text from the document
  text_elements <- docx_summary(doc)$text
  
  # Combine all the text into a single string
  combined_text <- paste(text_elements, collapse = " ")
  
  return(combined_text)
}

# Path to your Word document
doc_path <- "C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Writting/12072024_Draft 2 Coconut Sustainability CLEANED.docx"

# Read the document and get the text as a single string
document_text <- read_doc_as_string(doc_path)

# Print the combined text
print(document_text)

test <- pull_cites(document_text)
