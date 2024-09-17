library(dplyr)
library(rgbif)
library(openxlsx)
library(stringdist)
library(inborutils)
library(tidyverse)
library(taxize)
library(rcites)
library(rredlist)
library(rebird)
library(xml2)

set_token("Fafh0Cn4SPZ5rI86BeH5fwtt")
Sys.setenv(ENTREZ_KEY = "f7041649502ffe57baeb36b4be7acd695808")
Sys.setenv(IUCN_REDLIST_KEY = "5abaac6464f1172201641c99a3357982c62423e69a08920925bc430d1753812b")
Sys.setenv(EBIRD_KEY = "6bd80i259l13")

# Assuming you have a vector of species codes
species_codes <- c(
  "indpit1", "whtkin2", "whcbab1", "grtdro1", "grecou1", "placuc3", "mowowl1",
  "eucdov", "cohcuc1", "whrsha", "ingori1", "placuc1", "whbwat1", "brnhao1",
  "babcuc2", "indnig1", "larnil1", "putbab1", "asidrc3", "grebit1", "asikoe2",
  "asikoe3", "pursun3", "phcdov1", "comchi1", "grpsni1", "sulcud1", "junowl1",
  "orbowl1", "y00839", "indcuc1"
)

# Replace with your actual species codes

# Fetch the list of species in North Sulawesi
list_SA <- ebirdregionspecies("ID-SL-SA")

# Check presence of each species code in the list and store results in a data frame
presence_df <- data.frame(
  speciescd = species_codes,
  North.Sulawesi = sapply(species_codes, function(speciescd) {
    species_present <- grepl(speciescd, list_SA$speciesCode)
    if (any(species_present)) {
      return("PRESENT")
    } else {
      return("NOT PRESENT")
    }
  })
)

# View the resulting data frame
print(presence_df)
