#-------------------------------------------------------------------------------
library(tidyverse)

#-------------------------------------------------------------------------------
# Load the data to clean, here it's the names of species from complexes dataset
#-------------------------------------------------------------------------------

library(amoebatech)

complexes <-
  complexes

data <-
  complexes

#-------------------------------------------------------------------------------
# Step 1 Formatting
#-------------------------------------------------------------------------------

species_list <-
  data %>% 
  select(species) %>% 
  
  # save a copy of the original names
  mutate(species_old = species, .before = species) %>% 
  
  # make it lower case
  mutate(species = str_to_lower(species)) %>%
  
  # remove stuff that is not letters ([] - one of in range; [^] - anything but; + - one or more)
  mutate(species = str_replace_all(species, "[^a-z]+", " ")) %>%
  
  # remove stuff between major-minor signs
  mutate(species = str_replace_all(species, "<.*>", " ")) %>%
  
  # eliminate useless spaces (if more than one or at the end) ($- at the end)
  mutate(species = str_remove_all(species, "^ +| +$")) %>%
  
  # underscore instead of spaces
  mutate(species = str_replace_all(species, " ", "_")) %>%
  distinct()

#-------------------------------------------------------------------------------
# Step 2 Load the correct reference dictionary for further spell checking
#-------------------------------------------------------------------------------  

# It's important that this dictionary is always updated and contains all the names,
# including genus level names (arcella sp.), pulled groups (corythion - trinema (how to make it gen level only?..)), 
# and similar, unwanted taxa will be cleaned out later

# We are using two dictionaries: first one is from verified database, second one is custom species list,
# containing all the species that are used and present in our datasets, but absent in the "veryfier base"

# First we load the verified list of species, and put it into the right format
# (we need to delete "f.", "var.", to further split names into three columns,
# to add "sp." to the genus level, so everything will be coherent)

dict_verified <-
  read.csv("https://raw.githubusercontent.com/eaermolaeva/spellcheck_ver1/main/verified_list.csv") %>%
  select(species = 1) %>%
  
  # make it lower case
  mutate(species = str_to_lower(species)) %>%
  
  mutate(species = str_replace_all(species, " f. ", " ")) %>%
  mutate(species = str_replace_all(species, " var. ", " ")) %>%
  
  mutate(to_sep = species) %>%
  separate(to_sep, sep = " ", into = c("gen", "sp", "sub_sp")) %>%
  
  #add "sp" for genus if species is na
  mutate(sp = case_when(
    is.na(sp) ~ "sp",
    T ~ sp
  ))

# This is the additional species list, it should be updated and checked manually

dict_additional <-
  read.csv("https://raw.githubusercontent.com/eaermolaeva/spellcheck_ver1/main/additional_list.csv") %>%
  select(additional_list = 1) %>%
  rename(species = additional_list) %>%
  #read.csv("dict_2_12_22.csv") %>%
  
  # make it lower case
  mutate(species = str_to_lower(species)) %>%
  
  mutate(species = str_replace_all(species, " f. ", " ")) %>%
  mutate(species = str_replace_all(species, " var. ", " ")) %>%
  
  mutate(to_sep = species) %>%
  separate(to_sep, sep = " ", into = c("gen", "sp", "sub_sp")) %>%
  
  #add "sp" for genus if species is na
  mutate(sp = case_when(
    is.na(sp) ~ "sp",
    T ~ sp
  ))

# Then we merge those two into the final reference dictionary

dict <- 
  dict_verified %>%
  bind_rows(., dict_additional) %>%
  rename(species_old = species) %>%
  unite("species", c(gen, sp, sub_sp), sep = "_", na.rm = T)
  
#-------------------------------------------------------------------------------
# Step 3 Proceed a spell check on full species names,
# in cases when there is an odd combination of genus - species,
# or something  that cannot be caught, 
# after we spotted it here, we should ether correct the typo manually, or
# add the new species name into our "additional dictionary"
#-------------------------------------------------------------------------------  

dict4_species <- 
  dict %>% 
  select(species) %>% 
  distinct() %>% 
  pull(species) %>% # an array is needed for spell check
  discard(.,is.na(.)) # drop na  

spellchecked4_species <-
  species_list %>%
  select(species) %>% 
  # create a row for each combination of name and dictionary entry
  mutate(dict = list(dict4_species)) %>% 
  unnest_longer(dict) %>% 
  # compute distance for each combination (row)
  rowwise() %>% 
  mutate(dist = adist(species, dict)[,1]) %>% 
  # extract minimum distance combination for each original name
  group_by(species) %>% 
  slice_min(dist)

#---CHECK THE RESULT -> ONLY IF EVERYTHING WORKS, REPLACE!!!
#---IF NOT, UPDATE THE "ADDITIONAL DICTIONARY", AND RUN ALL THE STEPS ONCE MORE

species_list <-
  species_list %>%
  left_join(., spellchecked4_species, by = "species") %>%
  rename(., species_not_clean = species) %>%
  rename(., species = "dict", species_dist = "dist") %>%
  distinct()

#-------------------------------------------------------------------------------
# Step 4 Replace synonyms
# (With a dictionary of synonyms)
#-------------------------------------------------------------------------------  

synoms <- 
  read.csv("https://raw.githubusercontent.com/eaermolaeva/spellcheck_ver1/main/synoms2.csv") %>%
  rename(species = species_outdated)

species_list <- 
  species_list %>%
  left_join(., synoms, by = "species") %>%
  mutate(species_right = case_when(
    is.na(species_current) ~ species,
    T ~ species_current
  )) 

species_list_final <-
  species_list %>%
  select(species_old, species = species_right)

#-------------------------------------------------------------------------------
# Step 5 Replace all in the original dataset
#-------------------------------------------------------------------------------  

data <- 
  data %>%
  rename(., "species_old" = "species")

data_cleaned <-
  data %>%
  left_join(., species_list_final, by = "species_old") %>%
  select(-species_old) %>%
  relocate(species)

#-------------------------------------------------------------------------------
# Done
#-------------------------------------------------------------------------------
