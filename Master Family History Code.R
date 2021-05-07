#Hello! Welcome to the Master Family History Code. This script was written by Lisa and Kate.
#If you run the entire script, you will get an excel file that has each person's initials and the summary family history variables.
#Right now, all output excel sheets are saved in Lisa's folder on the server, but that may change after she leaves. 
#If you need more specific information than the family history variables, feel free to modify! 



#Step 1: Set up your R environment. 

rm(list = ls()) # command to clear all variables from R environment
setwd("W:/string-mbd/RA Instruction Manuals/Lisa Gorham/R Files/")

library(readxl)
library(writexl)
library(tidyverse)
library(utils)
library(splitstackshape)



#Step 2: Load the clinical and behavioral databases from the server. 

clinical_database <- read_xlsx("W:/string-mbd/Database/Master Psychometric Database/MASTER_DATABASE_CLINICAL.xlsx")
behavioral_database <- read_xlsx("W:/string-mbd/Database/Master Psychometric Database/MASTER_DATABASE_BEHAVIOURAL.xlsx")



#Step 3: Get your family history data.
  #Pull relevant columns, filter out rows without family history data, group by participant, select most recent row for each participant, 
  #filter out ineligible participants, remove unnecessary columns
  #Note: this may need to be modified if more patients refuse to complete the interview; right now there are only 3 people (see below)

family_interview_database <- clinical_database %>% 
  select(Initials, SDAN, PLUSID, SEX, DOB, Participant_Type2, Eligible, Protocol, matches("c_family_interview_")) %>%
  filter(!is.na(c_family_interview_date), !is.na(SDAN))%>%
  group_by(SDAN)%>%
  slice(n()) %>%
  filter(!(Eligible == '5' | Eligible == '6'), str_detect(Protocol, '0037')) %>% select(-Eligible, -Protocol) %>%
  filter(!(Initials == 'SNMY'), !(Initials == 'HHAR'), !(Initials == 'JNSG'))



#Step 4: Fix the patients who have more family members than there were spaces in the family tree.
  #Select the "other" column and split-up its contents into multiple columns, rename the new columns with correct family member ID numbers and headings, 
  #re-join to family history dataframe

other <- family_interview_database %>%
  select(SDAN, c_family_interview_fam_other)%>%
  cSplit("c_family_interview_fam_other", ", ")

for(colName in names(other)){
  if(str_detect(colName, "c_family_interview_fam_other")){
    colNum <- as.numeric(substring(colName, 30))
    familyMemberNumber <- (colNum-1)%/%8+26
    colBase = case_when(
      colNum %% 8 == 1 ~ "_name",
      colNum %% 8 == 2 ~ "_relationship",
      colNum %% 8 == 3 ~ "_sex",
      colNum %% 8 == 4 ~ "_gender",
      colNum %% 8 == 5 ~ "_bio",
      colNum %% 8 == 6 ~ "_side",
      colNum %% 8 == 7 ~ "_living",
      colNum %% 8 == 0 ~ "_notes"
    )
    names(other)[names(other) == colName] <- paste(familyMemberNumber,colBase, sep = "")
  }
}

family_interview_database <- family_interview_database %>%
  select(-c_family_interview_fam_other)%>%
  full_join(other)



#Step 5: Fix the formatting. 
  #Select family member columns, reshape data from wide to long format, remove blank rows

select_family <- family_interview_database %>% 
  select(-c_family_interview_p1id:-c_family_interview_TDiff) %>%
  mutate_all(as.character)

long <- select_family %>% 
  gather(c_family_interview_fam1_name:ncol(select_family), key = "key", value = "value") %>%
  filter(!str_detect(key, "_name")) %>%
  mutate(key = gsub("c_family_interview_fam", "", key)) %>%
  separate(key, c("number","type"),"_") %>%
  spread(key = "type", value = "value") %>%
  filter(!is.na(bio)) %>%
  arrange(SDAN)



#Step 6: Select the family history data relevant for Lisa's analyses. 
  #Note: This can be changed if you are trying to look at different disorders. 
  #Select depression and suicide columns, reshape data from wide to long format, remove blank rows

depression <- family_interview_database %>%
  select(Initials:Participant_Type2, c_family_interview_dep1_fam_num:c_family_interview_dep20_age)%>%
  gather(c_family_interview_dep1_fam_num:c_family_interview_dep20_age, key = "key", value = "value")%>%
  mutate(key = gsub("c_family_interview_dep", "", key))%>%
  separate(key, c("dep_num","type"), "_", extra = "merge")%>%
  spread(key = "type", value = "value")%>%
  filter(!is.na(fam_num))%>%
  mutate_all(as.character)%>%
  arrange(SDAN)

suicide <- family_interview_database %>%
  select(Initials:Participant_Type2, c_family_interview_suicide1_fam_num:c_family_interview_suicide8_death)%>%
  gather(c_family_interview_suicide1_fam_num:c_family_interview_suicide8_death, key = "key", value = "value")%>%
  mutate(key = gsub("c_family_interview_suicide", "", key))%>%
  separate(key, c("suic_num", "type"), "_", extra = "merge")%>%
  spread(key = "type", value = "value")%>%
  filter(!is.na(fam_num))%>%
  mutate_all(as.character)%>%
  arrange(SDAN)



#Step 7: Match the disorder data with the family tree info. 
  #Match family member data with depression and suicide data, clean up data (accounting for misspelling, differences in capitalization, etc.)

join <- full_join(long, depression, by = c("Initials", "SDAN", "PLUSID", "SEX", "DOB", "Participant_Type2", "number" = "fam_num"))%>%
  full_join(suicide, by = c("Initials", "SDAN", "PLUSID", "SEX", "DOB", "Participant_Type2", "number" = "fam_num"))

join_clean <- join %>% rename("dep_age" = "age.x", "suic_age" = "age.y")%>%
  mutate(bio = ifelse(str_detect(bio, '[Ff][Uu][Ll][Ll]'), "bio-full", bio))%>%
  mutate(bio = ifelse(str_detect(bio, '[Hh][Aa][Ll][Ff]'), "bio-half", bio)) %>% 
  mutate(bio = ifelse(str_detect(bio, '[Aa][Dd][Oo][Pp][Tt]'), "adopted", bio)) %>% 
  mutate(bio = ifelse(str_detect(bio, '[Ss][Tt][Ee][Pp]'), "step", bio))%>%
  mutate(bio = ifelse(str_detect(bio, '[Uu][Nn][Kk][Nn][Oo][Ww][Nn]'), "unknown", bio)) %>% 
  mutate(relationship = ifelse(str_detect(relationship, '[Ss][Ii][Bb][Ll][Ii][Nn][Gg]'), "sibling", relationship)) %>% 
  mutate(relationship = ifelse(str_detect(relationship, '[Aa][Uu][Nn][Tt]')|str_detect(relationship, '[Uu][Nn][Cc][Ll][Ee]'), "aunt/uncle", relationship)) %>% 
  mutate(relationship = ifelse(str_detect(relationship, '[Gg][Rr][Aa][Nn][Dd]'), "grandparent", relationship)) %>% 
  mutate(relationship = ifelse(str_detect(relationship, '[Nn][Ii][Ee][Cc][Ee]')|str_detect(relationship, '[Nn][Ee][Pp][Hh][Ee][Ww]'), "niece/nephew", relationship))%>%
  mutate(relationship = ifelse(str_detect(relationship, '[Ccb][Oo][Uu][Ss][Ii][Nn]'), "cousin", relationship))%>%
  mutate(relationship = ifelse(str_detect(relationship, '1'), "parent1", relationship)) %>% 
  mutate(relationship = ifelse(str_detect(relationship, '2'), "parent2", relationship)) %>% 
  mutate(relationship = ifelse(str_detect(relationship, '3'), "parent3", relationship)) %>% 
  mutate(relationship = ifelse(str_detect(relationship, '4'), "parent4", relationship))%>%
  filter(bio != '999' | relationship != '999')%>%
  mutate(gender = ifelse(str_detect(gender, '[Nn][Oo][Nn].?[Cc][Oo][Nn][Ff][Oo][Rr][Mm]')|str_detect(gender, '[Qq][Uu][Ee][Ee][Rr]')|str_detect(gender, '[Tt][Rr][Aa][Nn][Ss]'), "non-binary", gender)) %>% 
  mutate(gender = ifelse(str_detect(gender, '^[Ff]$')|str_detect(gender, 'FALSE'), "F", gender)) %>% 
  mutate(gender = ifelse(str_detect(gender, '^[Mm]$'), "M", gender))




#Step 8: Create a variable that indicates the shared genetic variance of the family member and our patient. 
  #Add genetics column
genetics <- join_clean %>% 
  mutate(genetics = ifelse((bio == "bio-full" & str_detect(relationship, '^parent'))| (bio == "bio-full" & relationship == "sibling"), .5, NA)) %>% 
  mutate(genetics = ifelse((bio == "bio-full" & relationship == "grandparent")|(bio == "bio-full" & relationship == "aunt/uncle")|(bio == "bio-full" & relationship == "niece/nephew")|(bio == "bio-half" & relationship == "sibling"), .25, genetics)) %>% 
  mutate(genetics = ifelse((bio == "adopted" | bio == "step"), 0, genetics)) %>% 
  mutate(genetics = ifelse((bio == "bio-full" & relationship == "cousin")|(bio == "bio-half" & relationship == "aunt/uncle")|(bio == "bio-half" & relationship == "niece/nephew"), .125, genetics)) %>% 
  mutate(genetics = ifelse((bio == "bio-half" & relationship == "cousin")|(bio == "bio-half" & relationship == "niece/nephew"), .0625, genetics)) %>% 
  mutate(dep_num = as.numeric(dep_num), suic_num = as.numeric(suic_num))


#Step 9: Export new dataset into an excel file for use if someone wants to see the complete family tree for each person. 
write_xlsx(genetics, "W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/CompleteFamTreeData.xlsx")


#------------------------------------------------------------------------------------------------------------------------------------------------------
#NOW WE HAVE A COMPLETE DATASET AND CAN START ACTUALLY CREATING VARIABLES OF INTEREST FOR ANALYSIS 

#variables we need:
#how many family members (extended or immediate) have depression?
#does the person have a biological parent with depression? Yes/no
#Does the person have an immediate family member with depression? yes/no
#does the person have a family member who has attempted suicide? yes/no
#does the person have a family member who has completed suicide? yes/no

#Calculate number of family members with depression, whether or not parents have depression, and whether or not an immediate biological family member has depression
num_dep_fam <- genetics %>%
  summarize(num_dep_fam = max(dep_num, na.rm = TRUE))%>%
  mutate(num_dep_fam = ifelse(num_dep_fam == "-Inf", 0, num_dep_fam))

num_dep_parent <- genetics %>%
  filter(str_detect(relationship, '^[Pp][Aa][Rr][Ee][Nn][Tt]'))%>%
  filter(str_detect(bio, '^[Bb][Ii][Oo][-][Ff][Uu][Ll][Ll]'))%>%
  summarize(dep_parent = max(dep_num, na.rm = TRUE))%>%
  mutate(dep_parent = ifelse(dep_parent == "-Inf", 0, 1))

num_dep_immed <- genetics %>% 
  filter(genetics == .5) %>% 
  summarize(dep_immed = max(dep_num, na.rm = TRUE)) %>% 
  mutate(dep_immed = ifelse(dep_immed == "-Inf", 0, 1))

#does the person have a family member who has attempted suicide
suicideattempt <- genetics %>%
  summarize(suicideattempt = max(suic_num, na.rm = TRUE)) %>%
  mutate(suicideattempt = ifelse(suicideattempt == "-Inf", 0, suicideattempt)) %>%
  mutate(suicideattempt = ifelse(suicideattempt == 0, 0, 1))


#does the person have a family member who has completed suicide
suicidecomplete <- genetics %>%
  summarize(suicidecomplete = max(death, na.rm = TRUE)) %>%
  mutate(suicidecomplete = ifelse(is.na(suicidecomplete), 0, suicidecomplete))%>%
  mutate(suicidecomplete = ifelse(suicidecomplete == 1, 1, 0))


#checking to see if anything is missing 
info <- genetics %>%
  select(Initials, SDAN, PLUSID)%>%
  distinct()
famhxinfo <- full_join(info, num_dep_fam)%>%
  full_join(num_dep_parent)%>%
  full_join(num_dep_immed)%>%
  full_join(suicideattempt) %>%
  full_join(suicidecomplete)

#famhxinfo gives us a dataset with the five variables of interest 

#Export new dataset: Note that this is the simplified dataset you can use in analyses. 
write_xlsx(famhxinfo, "W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/AggregateFamHxData.xlsx")


#adding in other data
participants <- genetics$SDAN


select_clinical_data <- clinical_database %>% 
  filter(str_detect(SDAN, paste(participants, collapse = "|"))) %>% 
  select(Initials, SDAN, PLUSID, SEX, Participant_Type2, DOB, Clinical_Visit_Date, Clinical_Visit_Type, Clinical_Visit_Code, Age_at_visit, p_mfq_date, p_mfq_tot, s_mfq_date, s_mfq_tot, s_case_date, s_case__neg_tot, p_case_date, p_case__neg_tot, c_ksadsdx_date, c_ksadsdx_clin_name, c_ksadsdx_visit_type, c_ksadsdx_epset_annual_weeks_mdd)



info <- genetics %>%
  select(Initials, SDAN)%>%
  distinct()

aggregate <- full_join(info, famhxinfo) %>% full_join(select_clinical_data) 


#this dataset matches the family history data to the clinical database; useful for doing analyses. 
write_xlsx(aggregate, "W:/string-mbd/RA Instruction Manuals/Lisa Gorham/Projects/Family History Interview/CompleteClinicalFamHx.xlsx")


#Calculate number of family members who have attempted suicide, whether or not a parent has attempted suicide, and whether or not an immediate biological family member has attempted suicide
#num_suic_fam <- genetics %>% 
# summarize(num_suic_fam = max(suic_num, na.rm = TRUE)) %>% 
#mutate(num_suic_fam = ifelse(num_suic_fam == "-Inf", 0, num_suic_fam))

#num_suic_parent <- genetics %>% 
# filter(str_detect(relationship, '^[Pp][Aa][Rr][Ee][Nn][Tt]')) %>% 
#summarize(suic_parent = max(suic_num, na.rm = TRUE)) %>% 
# mutate(suic_parent = ifelse(suic_parent == "-Inf", 0, 1))

#num_suic_immed <- genetics %>% 
# filter(genetics == .5) %>% 
#summarize(suic_immed = max(suic_num, na.rm = TRUE)) %>% 
#mutate(suic_immed = ifelse(suic_immed == "-Inf", 0, 1))
