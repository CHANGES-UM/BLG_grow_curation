### BLUEGILL GROW CARDS DATA CURATION###
## written by Katelyn King Aug 12, 2021 

#### load libraries #### 
library(dplyr)
library(tidyr)
library(stringr)
library(rqdatatable)

#### blgrow_text (length info) cleaning ####

#read in files with values to match (e.g task names)
grow_task_values<-read.csv("data/blg_grow_data/GROW_task_values.csv", header=TRUE)

#read in data tables 
blgrow_text<-read.csv("data/blg_grow_data/text_reducer_bluegill_texts.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views')%>% 
  left_join(grow_task_values) # join with the task values to get the actual names

#preliminary cleaning
#remove string after // 
blgrow_text$data<-str_remove(blgrow_text$data.consensus_text, "//.*")
#remove one weird answer '3 six year old samples' should say 'no six year old samples' 
blgrow_text$data<-ifelse(blgrow_text$data =='3 six year old samples', 0, blgrow_text$data) 

#separate numbers from text and standardize units 
#summary(as.factor(blgrow_text$unit)) #cards are in inches unless otherwise noted; check out all units, don't want anything in parenthesis which are weights. looked at several cards where weird things are happening   
blgrow_text<-blgrow_text %>%
  separate(data, c("data", "units1"), sep = " ") %>% #new column for units 
  separate(data,
           into = c("data", "text"), 
           sep= "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])") %>% #separate numbers from text
  mutate(units= case_when(
    units1 == 'in' ~ 'inches', # if ~ then 
    units1 == 'in.' ~ 'inches', # if ~ then 
    units1 == 'inches' ~ 'inches',
    units1 == 'mm' ~ 'mm',
    units1 == 'cm' ~ 'cm',
    text=="mm" ~ "mm"
  )) #else be NA if its not any of these conditions 

#*manual check consensus scores <=2 #### 
# what was viewed by only 1 person 
manual_length1<- filter(blgrow_text, data.number_views == 1) 
#what was viewed by >= 2 people but half didn't agree 
manual_length2<- filter(blgrow_text, (data.number_views >2 & data.consensus_score <= 2)| (data.number_views ==2 & data.consensus_score < 2) ) 

#good data where consensus is >50% 
blg_text_good<-filter(blgrow_text, (data.number_views >2 & data.consensus_score > 2)| (data.number_views ==2 & data.consensus_score >= 2) ) %>%
  select(subject_id, column_name, data)

#merging back in manually reviewed data 
manual_length1<-read.csv("data/blg_grow_data/blg_text_bad1_KK.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, data)
manual_length2<-read.csv("data/blg_grow_data/text_review_calla.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, data)

lengths_clean<-gtools::smartbind(blg_text_good, manual_length1, manual_length2) %>%
    pivot_wider( #rotate so that each of the tasks is a column 
      id_cols = c(subject_id),
      names_from = column_name, 
      values_from = c(data), 
      values_fill = list(data = NA) )

#### blgrow_dropdown cleaning (age data) #### 
#read in files
blgrow_dropdown<-read.csv("data/blg_grow_data/dropdown_reducer_bluegill_texts_dropdowns.csv", na.strings = c("", "NA")) %>%
  select(subject_id, task, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') 
grow_age_values<-read.csv("data/blg_grow_data/GROW_age_values.csv", header=TRUE) %>% 
  mutate(code = as.factor(code))
grow_task_values<-read.csv("data/blg_grow_data/GROW_task_values.csv", header=TRUE)

#preliminary cleaning 
blgrow_dropdown$sep <- gsub("[[:punct:]]", "", blgrow_dropdown$data_value)  #remove punctuation 
blgrow_dropdown$sep<-gsub("\\s", "_", blgrow_dropdown$sep) #replace space with _

#separate into four columns, one for each of the reviews if there are different answers
blgrow_dropdown<-separate(data=blgrow_dropdown, col=sep, c("age_hash", "review", "age_hash2", "review2", "age_hash3", "review3", "age_hash4", "review4" ), sep = "_")
blgrow_dropdown$age_hash<-as.factor(blgrow_dropdown$age_hash)
blgrow_dropdown$age_hash2<-as.factor(blgrow_dropdown$age_hash2)
blgrow_dropdown$age_hash3<-as.factor(blgrow_dropdown$age_hash3)
blgrow_dropdown$age_hash4<-as.factor(blgrow_dropdown$age_hash4)

# join 
blgrow_dropdown <-left_join(blgrow_dropdown, grow_task_values) %>% #get actual column name 
  left_join(grow_age_values, by=c('age_hash' = 'code')) %>% #get age values for the first review
  left_join(grow_age_values, by=c('age_hash2' = 'code'))  %>% #get age values for the 2nd review
  left_join(grow_age_values, by=c('age_hash3' = 'code')) %>% #get age values for the 3rd review
  left_join(grow_age_values, by=c('age_hash4' = 'code')) %>% #get age values for the 4th review
  rename(age1=age_group.x, age2=age_group.y, age3=age_group.x.x, age4=age_group.y.y)

#for some reason this hash code is not matching 9f71969064233 (note there aren't any review4s)
blgrow_dropdown$age1<-ifelse(blgrow_dropdown$age_hash =='9f71969064233', 5, blgrow_dropdown$age1) 
blgrow_dropdown$age2<-ifelse(blgrow_dropdown$age_hash2 =='9f71969064233', 5, blgrow_dropdown$age2) 
blgrow_dropdown$age3<-ifelse(blgrow_dropdown$age_hash3 =='9f71969064233', 5, blgrow_dropdown$age3) 

# keep >= 3s OR any that were 2 but don't have disagreements (NAs) 
age_good<-filter(blgrow_dropdown, review >= 3 | review2 >= 3 | (review == 2 & is.na(review2)) ) %>%
  mutate(age= case_when(
    is.na(review2) ~ age1, # if ~ then (total agreement)
    review2 > review ~ age2, # if ~ then (majority selected)
    review > review2 ~ age1 # if ~ then (majority selected)
  )) %>%
  select(subject_id, column_name, age)

#*observations for manual review ####
age_manual<-filter(blgrow_dropdown, review < 3  & review2 < 3 | (review == 1 & is.na(review2))) %>%
  select(-c(task, data_value, age_hash, age_hash2, age_hash3, age_hash4))

#join manually reviewed and good datasets 
age_manual<-read.csv( "data/blg_grow_data/ages_manual_Calla.csv") %>% #after manual review 
  select(subject_id, column_name, age)

age_data<-rbind(age_good, age_manual)

#pivot table so tasks are colums 
age_format<-tidyr::pivot_wider(data= age_data, 
                               id_cols = c(subject_id),
                               names_from = column_name, 
                               values_from = c(age), 
                               values_fill = list(age = NA) )


#*join age data: dropdown ages and length text  ####
blg_age<-left_join(age_format, lengths_clean)
#re-order columns 
blg_age <- blg_age[, c(1, 2,16,17,18, 3, 19,20,21, 4, 22,23,24, 5, 25,26,27, 6, 28,29,30, 7, 31,32,33, 8, 34,35,36, 9, 37,38,39, 10, 40,41,42, 11, 43,45,44, 12, 46,47,48, 13, 49,50,51, 14, 52,53,54, 15, 55,56,57)]

#because each task was based on rows of data on the cards, which could start at any age, we need to pull out each set of columns and then stack them 
first<-blg_age[,c(1:5)] 
colnames(first)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
first<- first %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
second<-blg_age[,c(1, 6:9)]
colnames(second)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
second<- second %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
third<-blg_age[,c(1, 10:13)]
colnames(third)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
third<- third %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
fourth<-blg_age[,c(1, 14:17)]
colnames(fourth)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
fourth<- fourth %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
fifth<-blg_age[,c(1, 18:21)]
colnames(fifth)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
fifth<- fifth %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
sixth<-blg_age[,c(1, 22:25)]
colnames(sixth)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
sixth<- sixth %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
seventh<-blg_age[,c(1, 26:29)]
colnames(seventh)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
seventh<- seventh %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
eigth<-blg_age[,c(1, 30:33)]
colnames(eigth)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
eigth<- eigth %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
nine<-blg_age[,c(1, 34:37)]
colnames(nine)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
nine<- nine %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
ten<-blg_age[,c(1, 38:41)]
colnames(ten)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
ten<- ten %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
eleven<-blg_age[,c(1, 42:45)]
colnames(eleven)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
eleven<- eleven %>% 
  filter_at(vars("age_group", "fish_count", "length_range", "length_mean"),any_vars(!is.na(.))) # drop if all 4 are NA 
twelve<-blg_age[,c(1, 46:49)]
colnames(twelve)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
twelve<- twelve %>% 
  filter_at(vars("age_group", "fish_count", "length_range"),any_vars(!is.na(.))) # drop if all 4 are NA 
thirteen<-blg_age[,c(1, 50:53)]
colnames(thirteen)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
thirteen<- thirteen %>% 
  filter_at(vars("age_group", "fish_count", "length_range"),any_vars(!is.na(.))) # drop if all 4 are NA 
fourteen<-blg_age[,c(1, 54:57)]
colnames(fourteen)<-c("subject_id", "age_group", "fish_count", "length_range", "length_mean")
fourteen<- fourteen %>% 
  filter_at(vars("age_group", "fish_count", "length_range"),any_vars(!is.na(.))) # drop if all 4 are NA 
#bind them all
all_grow<-rbind(first, second, third, fourth, fifth, sixth, seventh, eigth, nine, ten, eleven, twelve, thirteen, fourteen)

# split out ranges 
all_grow<-all_grow %>%
  separate(length_range, c("length_min", "length_max"), sep = "-")
#classifying as numeric
all_grow$fish_count <- as.numeric(as.character(all_grow$fish_count))
all_grow$length_min <- as.numeric(as.character(all_grow$length_min))
all_grow$length_max <- as.numeric(as.character(all_grow$length_max))
all_grow$length_mean <- as.numeric(as.character(all_grow$length_mean))


#### clean up tags and comments #### 
#read in comments and tags from volunteers 
blgrow_tags<-read.csv("data/blg_grow_data/BLG-tags_2021-07-15.csv") %>%
  select(subject_id, 'X_...name') %>%
  rename(tag = 'X_...name')
blgrow_comments<-read.csv("data/blg_grow_data/BLG-comments_2021-07-15.csv") %>%
  select(subject_id, "X_...comment_body") %>%
  rename(comment = "X_...comment_body")

#combine tags for each card so that they are in one row
tags <- aggregate(blgrow_tags[2], blgrow_tags[-2], 
                  FUN = function(X) paste(unique(X), collapse=", ")) 
tags$tag <- ifelse(tags$subject_id =='58755099', 'millimetres', tags$tag) #58755099 checked this card and it is mm not cm 
#combine comments for each card so that they are in one row
comments <- aggregate(blgrow_comments[2], blgrow_comments[-2], 
                      FUN = function(X) paste(unique(X), collapse=", "))

#### join all data #### 
#lake match data 
lake_match <-read.csv("data/blg_grow_data/lake_match_13Sep2021.csv", header=TRUE, na.strings = c("", "NA")) %>%
  select(subject_id, county, lakename, 'new.key') %>%
  rename(new_key = 'new.key') %>%
  drop_na('new_key')

#join grow data, basic dates, lake matching info, tags and comments 
#and correct units based on tags from volunteers 
BLG_GROW<-left_join(all_grow, dates_clean_plus_manual) %>%
  left_join(lake_match) %>%
  left_join(tags) %>%
  left_join(comments) %>%
  left_join(urls) %>%
  drop_na('url_front')%>% #drop cards without a url, these were removed from workflows 
  mutate(units= case_when(
    (tag == 'centimeters' | tag == 'centimetres' | tag == 'centimetres, centimeters' | tag == 'oops, centimeters' | tag == 'inches, centimeters') ~  "centimeters", # if ~ then 
    (tag == 'length, millimetres' | tag == 'millimetres') ~ 'millimeters', 
    length_mean > 25 | length_min > 25  ~ 'millimeters', 
    TRUE ~ 'inches'   )) #else inches because this was the specified unit on the cards  

#add some comments for cards with multiple bluegill records
BLG_GROW$comment<-ifelse(BLG_GROW$subject_id == "58642823", ' two gear types', BLG_GROW$comment) #insert comment two gear types 58642823
BLG_GROW$comment<-ifelse(BLG_GROW$subject_id == "58642824", ' two gear types', BLG_GROW$comment)#insert comment two gear types 58642824

#if the fish count is 0 and not NA then should not be length values. these were all manually checked
BLG_GROW$length_min <- ifelse(BLG_GROW$fish_count == 0 & !is.na(BLG_GROW$fish_count), NA, BLG_GROW$length_min) 
BLG_GROW$length_max <- ifelse(BLG_GROW$fish_count == 0 & !is.na(BLG_GROW$fish_count), NA, BLG_GROW$length_max) 
BLG_GROW$length_mean <- ifelse(BLG_GROW$fish_count == 0 & !is.na(BLG_GROW$fish_count), NA, BLG_GROW$length_mean) 

#### QA/QC of units #### 
#investigate cards that are in the 10-26 range to see if they are cm or in
#mid_range<-filter(BLG_GROW, (length_mean > 10 & length_mean < 26)  | length_min > 10 & length_min < 26) %>%
#      select(subject_id, fish_count, length_min, length_max, length_mean, units, comment, URL_front, URL_back)

#several cards are in cm and were not tagged by volunteers: 
#58746371 ; 58753899 ; 58754016 ; 59964512; 58746501; 58754235
BLG_GROW$units <- ifelse(BLG_GROW$subject_id =='58746371', 'centimeters', BLG_GROW$units) 
BLG_GROW$units <- ifelse(BLG_GROW$subject_id =='58753899', 'centimeters', BLG_GROW$units) 
BLG_GROW$units <- ifelse(BLG_GROW$subject_id =='58754016', 'centimeters', BLG_GROW$units) 
BLG_GROW$units <- ifelse(BLG_GROW$subject_id =='59964512', 'centimeters', BLG_GROW$units) 
BLG_GROW$units <- ifelse(BLG_GROW$subject_id =='58746501', 'centimeters', BLG_GROW$units) 
BLG_GROW$units <- ifelse(BLG_GROW$subject_id =='58754235', 'centimeters', BLG_GROW$units) 

#one outlier that looks like a typo based on max and mean values 
BLG_GROW$length_min <- ifelse(BLG_GROW$subject_id =='58642811' & BLG_GROW$age_group == 7, 7.1, BLG_GROW$length_min) 

#convert everything to millimeters 
BLG_GROW<- BLG_GROW %>%
  mutate(length_min_mm = case_when(
    units == 'inches' ~ length_min*25.4, 
    units == 'centimeters' ~ length_min*10, 
    TRUE ~ length_min # else keep as mm 
  )) %>%
  mutate(length_max_mm = case_when(
    units == 'inches' ~ length_max*25.4, 
    units == 'centimeters' ~ length_max*10, 
    TRUE ~ length_max # else keep as mm 
  ))  %>%
  mutate(length_mean_mm = case_when(
    units == 'inches' ~ length_mean*25.4, 
    units == 'centimeters' ~ length_mean*10, 
    TRUE ~ length_mean # else keep as mm 
  )) %>%
  select(-c(length_min, length_max, length_mean, units))

#re-order columns for more intuitive data structure for final data set 
BLG_GROW <- BLG_GROW[, c(1, 12, 11, 10, 2, 3, 17, 18,19, 4,5,6, 7, 8,9, 13, 14, 15, 16)]

