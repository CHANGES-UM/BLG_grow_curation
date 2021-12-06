### MATCHING LAKES FROM HISTORICAL DATA TO MDNR AUTHORITY FILE ### 
#written by Michael Lenard July 6, 2021 

#### l oad libraries #### 
library(tidyverse)
library(stringr)
library(purrr)

#### read in data tables #### 
#Read in Humphries table (MDNR authority file), the Zooniverse classification data, 
#and the Zooniverse subjects (image) data

lake_data <- read_csv('HUMPHRIES_COUNTY_LATLONG_IFR_LAKES.csv')

classification_data <- read_csv('text_reducer_collapsed.csv') %>% 
  pivot_wider(subject_id, names_from = task, values_from = data.consensus_text)

subjects_data <- read_csv('angling-for-data-on-michigan-fishes-subjects-17771.csv')

# 1) Join Zooniverse subjects data to classification data

# 2) Take T and R transcription data, take only the first value, 
# strip non-digits, truncate to 2 characters, and remove leading zeroes.
# Save split, stripped T & R data to new columns. 

# 3) Split S data into multiple columns on punctuation and whitespace

# 4) Extract lake name from filename. The regex finds the first string
# between two undercores (_text_) or between an underscore and the start of TRS
# (i.e. _text33N), capitalizes it, strips underscores, and removes whitespace, 
# punctuation, and the word 'LAKE' for purposes of matching. Save to new column

df <- subjects_data %>%
  inner_join(classification_data, by = "subject_id") %>%
  mutate(T = sub("[,-/;&].*", "", T8)) %>%
  mutate(T = gsub(pattern = "\\D+", replacement = '', x = `T`)) %>%
  mutate(T = str_sub(`T`, start = 0, end = 2)) %>%
  mutate(T = gsub(pattern = "0(?=\\d)", replacement = '', x = `T`, perl = TRUE)) %>%
  mutate(R = sub("[,-/;&].*", "", T9)) %>%
  mutate(R = gsub(pattern = "\\D+", replacement = '', x = R)) %>%
  mutate(R = gsub(pattern = "0(?=\\d)", replacement = '', x = R, perl = TRUE)) %>%
  separate(T10, c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10"), remove = FALSE) %>%
  mutate(lakename = str_extract(filename, pattern = "_.*?([_\\d]|T\\d)")) %>%
  mutate(lakename = toupper(lakename)) %>%
  mutate(lakename = gsub(pattern = "\\s|[\\(\\)',]|LAKE", replacement = '', x = lakename)) %>%
  mutate(lakename = str_sub(lakename, start = 2, end = -2)) %>%
  mutate(lakename = gsub(pattern = "^(WEST|EAST|NORTH|SOUTH|SOUTHEAST|SOUTHWEST|NORTHEAST|NORTHWEST|SMALL|LITTLE|BIG)?(.*)$", 
                         replacement = "\\2\\1", x = lakename, perl = TRUE))

# Take the previous data frame and use the stripped-down T & R columns
# to construct a partial TRS code for matching with the Humphries table TRS column.
# Searches original data for N, S, E, and W to insert appropriate cardinal
# characters into the code

zoon_data <- df %>%
  mutate(TR = case_when(
    str_detect(T8, "[nN]") & str_detect(T9, "[eE]") ~ sprintf("T%sNR%sE", `T`, R),
    str_detect(T8, "[nN]") & str_detect(T9, "[wW]") ~ sprintf("T%sNR%sW", `T`, R),
    str_detect(T8, "[sS]") & str_detect(T9, "[eE]") ~ sprintf("T%sSR%sE", `T`, R),
    str_detect(T8, "[sS]") & str_detect(T9, "[wW]") ~ sprintf("T%sSR%sW", `T`, R),
    TRUE                                            ~ ""
  ))

# 1) Truncate TRS column to just T & R for matching purposes. Remove leading zeroes.

# 2) Pull out the S part of TRS column and save to its own column & remove zeroes

# 3) Make new column in the Humphries table data frame to hold a capitalized, 
# stripped-down version of the lake name for matching purposes

lake_data <- lake_data %>%
  mutate(TR = str_extract(TRS, ".*(?=S\\d+)")) %>%
  mutate(TR = gsub(pattern = "0(?=\\d)", replacement = '', x = TR, perl = TRUE)) %>%
  mutate(S = str_extract(TRS, "(?<=S)\\d+")) %>%
  mutate(S = gsub(pattern = "0(?=\\d)", replacement = '', x = S, perl = TRUE)) %>%
  mutate(lakename = toupper(Lake_Name)) %>%
  mutate(lakename = gsub(pattern = "\\s|[\\(\\)',.]|LAKE|POND", replacement = '', x = lakename)) %>%
  mutate(lakename = gsub(pattern = "^(WEST|EAST|NORTH|SOUTH|SOUTHEAST|SOUTHWEST|NORTHEAST|NORTHWEST|SMALL|LITTLE|BIG)?(.*)$", 
                         replacement = "\\2\\1", x = lakename, perl = TRUE))

# 1) Attempt to join Zooniverse data to Humphries table on TR code and modified 
# lake name.

# 2) Flag rows where there was no match with "x" and rows that matched but
# where the S column in the lake data does not match any of S1-10 with "!"

# 3) Remove some columns for clarity & ease of use

joined <- zoon_data %>% left_join(lake_data, by = c("TR", "lakename")) %>%
  mutate(flag = case_when(
    is.na(S) ~ "x",
    !is.na(S) & (S == S1 | S == S2 | S == S3 | S == S4 | S == S5 | S == S6 | S == S7 | S == S8 | S == S9 | S == S10) ~ "",
    !is.na(S) & TRUE ~ "!")) %>%
  select(subject_id, workflow_id, metadata, filename, T8, T9, T10, `T`, R, S1, S2, 
         S3, S4, S5, S6, S7, S8, S9, S10, flag,
         lakename, TR, Humphry_Key, New_Key, COUNTY, Lake_No, Lake_Name, Acres_GIS,
         Acres, Town, Range, Section1, Section2, Section3, Section4, Section5,
         Section6, Section7, Section8, Section9, Section10, TRS, S, LONG_DD, LAT_DD)

write_csv(joined, "lake_keys_[date].csv", na = '')