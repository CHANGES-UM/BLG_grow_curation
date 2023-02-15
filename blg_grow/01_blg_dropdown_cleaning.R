### BLUEGILL GROW CARDS BASIC CLEANING###
## written by Katelyn King Aug 12, 2021 

#### load libraries #### 
library(dplyr)
library(tidyr)
library(stringr)
library(rqdatatable)


#### load data #### 

#read in urls 
urls<-read.csv("data/blg_grow_data/basic_GROW_URLs.csv", na.strings = c("", "NA"))  %>% 
  distinct(subject_id, .keep_all = TRUE) %>%
  rename(url_front = URL_front, url_back = URL_back) 

#read in the aggregated file from zooniverse for a dropdown task
#remove unwanted columns and NAs
basic_dropdown<-read.csv("data/blg_grow_data/dropdown_reducer_basic_GROW_dropdowns.csv", na.strings = c("", "NA")) %>%
  select(subject_id, task, 'data.value') %>%
  drop_na('data.value') %>% 
  rename(data_value = 'data.value') 

#read in files with values to match (e.g. the date hash and task names)
basic_task_values<-read.csv("data/blg_grow_data/Basic_task_value.csv", header=TRUE) %>%
  select(task, column_name)
basic_date_values<-read.csv("data/blg_grow_data/Basic_date_values.csv", header=TRUE)

#### data cleaning ####

#remove punctuation and replace spaces with _
#separate the number of reviews from the answers 
basic_dropdown$sep <- gsub("[[:punct:]]", "", basic_dropdown$data_value)  #remove punctuation 
basic_dropdown$sep<-gsub("\\s", "_", basic_dropdown$sep) #replace space with _
basic_dropdown<- separate(data=basic_dropdown, col=sep, c("date_hash", "review", "date_hash2", "review2", "date_hash3", "review3", "date_hash4", "review4" ), sep = "_") %>%
  left_join(basic_task_values) %>% #get actual column name 
  left_join(basic_date_values, by=c('date_hash' = 'code')) %>% #get date values for the first review
  left_join(basic_date_values, by=c('date_hash2' = 'code'))  %>% #get date values for the 2nd review
  left_join(basic_date_values, by=c('date_hash3' = 'code')) %>% #get date values for the 3rd review
  left_join(basic_date_values, by=c('date_hash4' = 'code')) %>% #get date values for the 4th review
  rename(month1=month.x, month2=month.y, month3=month.x.x, month4=month.y.y) %>%
  mutate(month1 = ifelse(is.na(month1), date_hash, month1), #if month is NA then replace with the day/year, else keep month 
         month2 = ifelse(is.na(month2), date_hash2, month2),
         month3 = ifelse(is.na(month3), date_hash3, month3),
         month4 = ifelse(is.na(month4), date_hash4, month4)
  )


#filter out the ones where the review is split 50:50 or less for manual review 
dates_bad<-filter(basic_dropdown, review < 3 & !is.na(review2) & review2 < 3) %>%
  select(-c(task, data_value, date_hash, date_hash2, date_hash3, date_hash4)) %>%
  mutate(date= case_when(          #but if there is a split because choosing 'same as' then don't need to review these 
    (month2 == 'Same as Begin Date (Month)' | month2 == 'Same as Begin Date (Day)' | month2 == 'Same as Begin Date (Year)') & is.na(month3)   ~ month1, # if ~ then 
    (month1 == 'Same as Begin Date (Month)' | month1 == 'Same as Begin Date (Day)' | month1 == 'Same as Begin Date (Year)') & is.na(month3)   ~ month2)
    #else be NA if its not either of these conditions 
  )


#keep dates with consensus >=3 OR any that were 3,2,1 but don't have disagreements (NAs) 
dates_good<-filter(basic_dropdown, review >= 3 | is.na(review2) | review2 >= 3) %>%
  mutate(date= case_when(
    is.na(review2) ~ month1, # if ~ then - chose the entry with the most reviews 
    review2 > review ~ month2, 
    review > review2 ~ month1 
  )) %>%
  select(subject_id, column_name, date) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(date), 
              values_fill = list(date = NA) ) %>%
  mutate(end_date_day = ifelse(end_date_day =='Same as Begin Date (Day)', begin_date_day, end_date_day), #change "same as..." to the actual date 
         end_date_month =ifelse(end_date_month =='Same as Begin Date (Month)', begin_date_month, end_date_month),
         end_date_year = ifelse(end_date_year =='Same as Begin Date (Year)', begin_date_year, end_date_year) 
  )

#merging back in manually reviewed data 
manual_dates<-read.csv("data/blg_grow_data/dates_manual_Kartik.csv", na.strings = c("", "NA")) %>%
  select(subject_id, column_name, date) %>% 
  pivot_wider(id_cols = c(subject_id),
              names_from = column_name, 
              values_from = c(date), 
              values_fill = list(date = NA) )

dates_clean_plus_manual<-natural_join(dates_good, manual_dates, by  = 'subject_id', jointype = "FULL")


##### QA/QC ####

#if begin and end years don't match then review these manually
dates_clean_plus_manual$review<-ifelse(dates_clean_plus_manual$begin_date_year != dates_clean_plus_manual$end_date_year, "TRUE", "FALSE") 

grow_dates_review<-read.csv("data/blg_grow_data/grow_dates_review_cb.csv")%>% 
  select(-c("review")) %>%
  rename(url_front = URL_front, url_back = URL_back) 

dates_final<-filter(dates_clean_plus_manual, review == 'FALSE' | is.na(review)) %>% 
  left_join(urls) %>% 
  drop_na('url_front') %>% 
  select(-c("review")) %>%
  mutate (comment = NA)%>%
  rbind(grow_dates_review) #add on the manually reviewed dates 
 