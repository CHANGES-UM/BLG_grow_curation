### BLUEGILL GROW CARDS DATA CURATION###
## written by Katelyn King Aug 12, 2021 

#### load libraries #### 
library(dplyr)
library(tidyr)
library(stringr)
library(rqdatatable)

#### read in lake match data #### 
lake_match <-read.csv("lake_match_13Sep2021.csv", header=TRUE, na.strings = c("", "NA")) %>%
  select(subject_id, county, lakename, 'new.key') %>%
  rename(new_key = 'new.key') %>%
  drop_na('new_key')

#### basic_dropdown (dates) cleaning #### 
#read in file and remove unwanted columns and NAs
basic_dropdown<-read.csv("BLG_GROW/BLG_data/dropdown_reducer_basic_GROW_dropdowns.csv", na.strings = c("", "NA")) %>%
  select(subject_id, task, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') 
#read in files with values to match (e.g. the date hash and task names)
basic_task_values<-read.csv("BLG_GROW/BLG_data/Basic_task_value.csv", header=TRUE) %>%
  select(task, column_name)
basic_date_values<-read.csv("BLG_GROW/BLG_data/Basic_date_values.csv", header=TRUE)

#separate reviews into four columns, one for each potentially different review 
basic_dropdown$sep <- gsub("[[:punct:]]", "", basic_dropdown$data_value)  #remove punctuation 
basic_dropdown$sep<-gsub("\\s", "_", basic_dropdown$sep) #replace space with _
basic_dropdown<- separate(data=basic_dropdown, col=sep, c("date_hash", "review", "date_hash2", "review2", "date_hash3", "review3", "date_hash4", "review4" ), sep = "_")
basic_dropdown <-left_join(basic_dropdown, basic_task_values) %>% #get actual column name 
  left_join(basic_date_values, by=c('date_hash' = 'code')) %>% #get date values for the first review
  left_join(basic_date_values, by=c('date_hash2' = 'code'))  %>% #get date values for the 2nd review
  left_join(basic_date_values, by=c('date_hash3' = 'code')) %>% #get date values for the 3rd review
  left_join(basic_date_values, by=c('date_hash4' = 'code')) %>% #get date values for the 4th review
  rename(month1=month.x, month2=month.y, month3=month.x.x, month4=month.y.y)
#if month is NA then replace with the day/year, else keep month 
basic_dropdown$month1<-ifelse(is.na(basic_dropdown$month1), basic_dropdown$date_hash, basic_dropdown$month1)  
basic_dropdown$month2<-ifelse(is.na(basic_dropdown$month2), basic_dropdown$date_hash2, basic_dropdown$month2) 
basic_dropdown$month3<-ifelse(is.na(basic_dropdown$month3), basic_dropdown$date_hash3, basic_dropdown$month3) 
basic_dropdown$month4<-ifelse(is.na(basic_dropdown$month4), basic_dropdown$date_hash4, basic_dropdown$month4)  

# keep 4s and 3s OR any that were 3,2,1 but don't have disagreements (NAs) 
dates_good<-filter(basic_dropdown, review >= 3 | is.na(review2) | review2 >= 3) %>%
  mutate(date= case_when(
    is.na(review2) ~ month1, # if ~ then 
    review2 > review ~ month2, # if ~ then 
    review > review2 ~ month1 # if ~ then 
  )) %>%
  select(subject_id, column_name, date)

#pivot table so that each task is now a column 
basic_dates_format<-tidyr::pivot_wider(data= dates_good, 
                                       id_cols = c(subject_id),
                                       names_from = column_name, 
                                       values_from = c(date), 
                                       values_fill = list(date = NA) )

#filter out observations where the review is split 50:50 or less
dates_bad<-filter(basic_dropdown, review < 3 & !is.na(review2) & review2 < 3) %>%
  select(-c(task, data_value, date_hash, date_hash2, date_hash3, date_hash4))

#if reviews are split because choosing 'same as', use the actual date  
dates_new<-dates_bad %>%
  mutate(date= case_when( 
    (month2 == 'Same as Begin Date (Month)' | month2 == 'Same as Begin Date (Day)' | month2 == 'Same as Begin Date (Year)') & is.na(month3)   ~ month1, # if ~ then 
    (month1 == 'Same as Begin Date (Month)' | month1 == 'Same as Begin Date (Day)' | month1 == 'Same as Begin Date (Year)') & is.na(month3)   ~ month2
    #else be NA if its not either of these conditions 
  ))

#pivot table so that each task is now a column 
basic_dates_format2<-tidyr::pivot_wider(data= dates_new, 
                                        id_cols = c(subject_id),
                                        names_from = column_name, 
                                        values_from = c(date), 
                                        values_fill = list(date = NA) )

#join both of the good tables and replace NAs from one table with data from another (rqdatatable package)
dates_clean<-natural_join(basic_dates_format, basic_dates_format2, by  = 'subject_id', jointype = "FULL")

#* observations for manual review####
#dates_for_review<-filter(dates_new, is.na(date)) 
#write.csv(dates_for_review, "/Users/Desktop/dates_for_review.csv", row.names = FALSE)

#merging back in manually reviewed data 
manual_dates<-read.csv("dates_for_review.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, date)

#pivot table 
manual_dates_format<-tidyr::pivot_wider(data= manual_dates, 
                                        id_cols = c(subject_id),
                                        names_from = column_name, 
                                        values_from = c(date), 
                                        values_fill = list(date = NA) )
#join tables 
dates_clean_plus_manual<-natural_join(dates_clean, manual_dates_format, by  = 'subject_id', jointype = "FULL")

#change "same as..." to the actual date 
dates_clean_plus_manual$end_date_day<-ifelse(dates_clean_plus_manual$end_date_day =='Same as Begin Date (Day)', dates_clean_plus_manual$begin_date_day, dates_clean_plus_manual$end_date_day) 
dates_clean_plus_manual$end_date_month<-ifelse(dates_clean_plus_manual$end_date_month =='Same as Begin Date (Month)', dates_clean_plus_manual$begin_date_month, dates_clean_plus_manual$end_date_month) 
dates_clean_plus_manual$end_date_year<-ifelse(dates_clean_plus_manual$end_date_year =='Same as Begin Date (Year)', dates_clean_plus_manual$begin_date_year, dates_clean_plus_manual$end_date_year) 

#### blgrow_text (length info) cleaning ####
#read in data tables 
blgrow_text<-read.csv("BLG_GROW/BLG_data/text_reducer_bluegill_texts.csv") %>%
  select(subject_id, task, 'data.aligned_text', 'data.number_views', 'data.consensus_score','data.consensus_text') %>%
  drop_na('data.number_views')
grow_task_values<-read.csv("BLG_GROW/BLG_data/GROW_task_values.csv", header=TRUE)
blgrow_text<-left_join(blgrow_text, grow_task_values) # join with the task values to get the actual names

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
blg_text_bad1<- filter(blgrow_text, data.number_views == 1) 
#what was viewed by >= 2 people but half didn't agree 
blg_text_bad2<- filter(blgrow_text, (data.number_views >2 & data.consensus_score <= 2)| (data.number_views ==2 & data.consensus_score < 2) ) 

#good data where consensus is >50% 
blg_text_good<-filter(blgrow_text, (data.number_views >2 & data.consensus_score > 2)| (data.number_views ==2 & data.consensus_score >= 2) ) %>%
  select(subject_id, column_name, data)

#manual review 
#write.csv(blg_text_bad1, "/Users/Desktop/blg_text_bad1.csv", row.names = FALSE)
#write.csv(blg_text_bad2, "/Users/Desktop/blg_text_bad2", row.names = FALSE)

#merging back in manually reviewed data 
manual_length1<-read.csv("blg_text_bad1.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, data)
manual_length2<-read.csv("blg_text_bad2.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, data)
lengths_clean<-gtools::smartbind(blg_text_good, manual_length1, manual_length2) 

#rotate so that each of the tasks is a column 
length_format<-tidyr::pivot_wider(data= lengths_clean, 
                                  id_cols = c(subject_id),
                                  names_from = column_name, 
                                  values_from = c(data), 
                                  values_fill = list(data = NA) )

#### blgrow_dropdown cleaning (age) #### 
#read in files
blgrow_dropdown<-read.csv("BLG_GROW/BLG_data/dropdown_reducer_bluegill_texts_dropdowns.csv", na.strings = c("", "NA")) %>%
  select(subject_id, task, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') 
grow_age_values<-read.csv("BLG_GROW/BLG_data/GROW_age_values.csv", header=TRUE)
grow_age_values$code<-as.factor(grow_age_values$code)
grow_task_values<-read.csv("BLG_GROW/BLG_data/GROW_task_values.csv", header=TRUE)

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
age_bad<-filter(blgrow_dropdown, review < 3  & review2 < 3 | (review == 1 & is.na(review2))) %>%
  select(-c(task, data_value, age_hash, age_hash2, age_hash3, age_hash4))
#write.csv(age_bad, "/Users//Desktop/ages_for_review.csv", row.names = FALSE)

#join manually reviewed and good datasets 
age_manual<-read.csv( "age_bad.csv") %>% #after manual review 
  select(subject_id, column_name, age)

age_data<-rbind(age_good, age_manual)

#pivot table so tasks are colums 
age_format<-tidyr::pivot_wider(data= age_data, 
                               id_cols = c(subject_id),
                               names_from = column_name, 
                               values_from = c(age), 
                               values_fill = list(age = NA) )


#*join age data: dropdown ages and length text  ####
blg_age<-left_join(age_format, length_format)
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
blgrow_tags<-read.csv("BLG_GROW/BLG_data/BLG-tags_2021-07-15.csv") %>%
  select(subject_id, 'X_...name') %>%
  rename(tag = 'X_...name')
blgrow_comments<-read.csv("BLG_GROW/BLG_data/BLG-comments_2021-07-15.csv") %>%
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
#join grow data, basic dates, lake matching info, tags and comments 
#and correct units based on tags from volunteers 
BLG_GROW<-left_join(all_grow, dates_clean_plus_manual) %>%
  left_join(lake_match) %>%
  left_join(tags) %>%
  left_join(comments) %>%
  left_join(urls) %>%
  drop_na('URL_front')%>% #drop cards without a url, these were removed from workflows 
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

#re-order columns for more intuitive data structure 
BLG_GROW <- BLG_GROW[, c(1, 12, 11, 10, 2, 3, 17, 18,19, 4,5,6, 7, 8,9, 13, 14, 15, 16)]

#final data set 
#write.csv(BLG_GROW, "BLG_GROW/blg_grow_qa_qc.csv", row.names = FALSE)
