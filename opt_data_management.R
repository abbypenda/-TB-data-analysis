library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(tidyverse)
library(lubridate)
library(REDCapTidieR)

#redcap_tz_interv <- "https://redcap.nimr.or.tz/redcap_v14.5.0/API/project_api.php?pid=72"
#super_token <- "EDE3FFDAF724D5E7BFD66DAB782B5E79"
#my_redcap_data <- read_redcap(redcap_tz_interv, super_token)%>%
#bind_tibbles()

#redcap_tz_contr <- "https://redcap.nimr.or.tz/redcap_v14.5.0/index.php?pid=72"
#token <- "EDE3FFDAF724D5E7BFD66DAB782B5E79"
#my_redcap_data <- read_redcap(redcap_tz_contr, token)

#redcap_uganda_inter <- "https://redcap.nimr.or.tz/redcap_v14.5.0/API/project_api.php?pid=86"
#token <- "564DFCBE7AC68B0A426DA37369EC134D"
#my_redcap_interv <- read_redcap(redcap_uganda_inter, token)

#redcap_uganda_contr <- "https://redcap.nimr.or.tz/redcap_v14.5.0/API/project_api.php?pid=85"
#token <- "75B5223E6130173E64A612F0B3AB3F4C"
#my_redcap_contr <- read_redcap(redcap_uganda_contr, token)

opt_treat_tz <- read.csv("Data/original/OPTICTB_DATA_2025-09-22_2201.csv")%>%
  
#none of these children was assessed for TB symptoms or signs in their facility
filter(!record_id%in%c("189-30","190-26","192-2","193-49","194-48","194-52",
                         "195-2","195-68","195-69","198-33","200-17","200-29",
                         "200-31","200-35","200-36","200-67","200-77","203-17"))%>%
  
#RENAMING HEALTH FACILITIES BELONGING TO INTERVENTION ARM
mutate(redcap_data_access_group = if_else(redcap_data_access_group == "kigamboni_health_c","Kigamboni Health Center",
                           if_else(redcap_data_access_group == "mnazi_mmoja_distri","Mnazi Mmoja District Hospital",
                           if_else(redcap_data_access_group == "amana_regional_ref","Amana Regional Referral Hospital",
                           if_else(redcap_data_access_group == "vingunguti_dispens","Vingunguti Dispensary",
                           if_else(redcap_data_access_group == "chanika_hc","Chanika Health Centre",
                           if_else(redcap_data_access_group == "mwananyamala_regio","Mwananyamala Regional Referral Hospital",
                           if_else(redcap_data_access_group == "magomeni_health_ce","Magomeni Health Center",
                           if_else(redcap_data_access_group == "tandale_health_cen","Tandale Health Centre",
                           if_else(redcap_data_access_group == "mzinga_dispensary","Mzinga Dispensary",
                           if_else(redcap_data_access_group == "temeke_regional_re","Temeke Regional Referral Hospital",
                           if_else(redcap_data_access_group == "tambukareli_dispen","Tambukareli Dispensary",
                           if_else(redcap_data_access_group == "kigamboni_mc_distr","Kigamboni Mc District Hospital",
                           if_else(redcap_data_access_group == "mkoani_health_cent","Mkoani Health Center",
                           if_else(redcap_data_access_group == "mkuranga_district","Mkuranga District Hospital",
                           if_else(redcap_data_access_group == "irene_kilimahewa_h","Irene kilimahewa health center",
                           if_else(redcap_data_access_group == "tabata_a_dispensar","Tabata A Dispensary",
                           if_else(redcap_data_access_group == "tumaini_mission_he","Tumaini Mission Health center",
                           if_else(redcap_data_access_group == "tumbi_regional_ref" ,"Tumbi Regional Referral Hospital",
                           if_else(redcap_data_access_group == "kerege_health_cent", "Kerege health center",
                           if_else(redcap_data_access_group == "chalinze_health_ce", "Chalinze Health Center",
                           if_else(redcap_data_access_group == "drc","DRC",redcap_data_access_group))))))))))))))))))))))%>%
  filter(!redcap_data_access_group=="drc")%>%  # this health facility is not known
  filter(!record_id%in%c("192-3","193-1","193-2","201-1"))%>% # consists of missing entries across all columns
  mutate(Date_fi = as.Date(date_fi, format= "%Y-%m-%d"))
 

#RENAMING HEALTH FACILITIES BELONGING TO CONTROL ARM
opt_control_tz <- read.csv("Data/original/OPTICTBControl_DATA_2025-09-22_2202.csv")%>%
   
#none of these children was assessed for TB symptoms or signs in their facility
filter(!record_id%in%c("253-19", "256-9", "260-29", "262-2","262-4",
                        "262-7", "262-8", "262-9", "262-12", "262-13",
                        "262-14", "262-25", "262-37"))%>%
  
#exclude demo entries
filter(!record_id%in%c("253-19", "256-9", "260-29", "262-2","262-7",
                         "262-8", "262-9", "262-9", "262-12", "262-13",
                         "262-14", "262-25"))%>%
   
#RENAMING HEALTH FACILITIES BELONGING TO CONTROL ARM  
mutate(redcap_data_access_group = if_else(redcap_data_access_group == "buguruni_health_ce","Buguruni Health Center",
                         if_else(redcap_data_access_group == "bunju_health_cente","Bunju Health Center",
                         if_else(redcap_data_access_group == "mbagala_rangi_tatu","Mbagala Rangi Tatu District Hospital",
                         if_else(redcap_data_access_group == "charambe_dispensar","Charambe dispensary",
                         if_else(redcap_data_access_group == "mbezi_health_cente","Mbezi Health Center",
                         if_else(redcap_data_access_group == "ubungo_district_ho","Ubungo District Hospital",
                         if_else(redcap_data_access_group == "sinza_palestina","Sinza palestina",
                         if_else(redcap_data_access_group == "kimbiji_health_cen","Kimbiji Health Center",
                         if_else(redcap_data_access_group == "mlandizi_health_ce","Mlandizi Health Center",
                         if_else(redcap_data_access_group == "bagamoyo_district","Bagamoyo District Hospital",
                         if_else(redcap_data_access_group == "st_vincent_health","St. Vincent Health Center",
                         if_else(redcap_data_access_group == "msoga_district_hos","Msoga District Hospital",
                         if_else(redcap_data_access_group == "kivule_district_ho", "Kivule District Hospital",
                         if_else(redcap_data_access_group == "kigogo_health_cent", "Kigogo Health center",
                         if_else(redcap_data_access_group == "mbande", "Mbande Health Centre",
                         if_else(redcap_data_access_group == "kairuki_memorial_h","Kairuki memorial hospital",
                         if_else(redcap_data_access_group == "kibiti_district_ho","Kibiti District Hospital",
                         if_else(redcap_data_access_group == "kisarawe_district","Kisarawe District Hospital",redcap_data_access_group)))))))))))))))))))%>%
  mutate(Date_fi = as.Date(date_fi, format= "%Y-%m-%d"))
 

#=======================================================================================
##Data from Uganda 
#=======================================================================================
opt_treat_ug <- read.csv("Data/original/OPTICTBUGANDAInterve_DATA_2025-09-20_0455.csv")%>%

#RENAMING HEALTH FACILITIES BELONGING TO INTERVENTION ARM
mutate(redcap_data_access_group = if_else(redcap_data_access_group == "mityana_general_ho","Mityana General Hospital",
                                    if_else(redcap_data_access_group == "","Mityana General Hospital",
                                    if_else(redcap_data_access_group == "kyenjojo_gh","Kyenjojo General Hospital",
                                    if_else(redcap_data_access_group == "bwera_gh","Bwera General Hospital",
                                    if_else(redcap_data_access_group == "bundibyugyo_gh","Bundibugy General Hospital",
                                    if_else(redcap_data_access_group == "butunduzi_hc_iii","Butunduzi Health Center III",
                                    if_else(redcap_data_access_group == "rukunyu_gh","Rukunyu General Hospital",
                                    if_else(redcap_data_access_group == "bumanya_health__ce","Bumanya Health Centre IV",
                                    if_else(redcap_data_access_group == "iganga_ismamic_mc","Iganga Islamic Medical Health Centre III",
                                    if_else(redcap_data_access_group == "mubende_rrh","Mubende Regional Referral Hospital",
                                    if_else(redcap_data_access_group == "bukuya_hc4","Bukuya Health Centre IV",
                                    if_else(redcap_data_access_group == "iganga_generalhosp","Iganga General Hospital",
                                    if_else(redcap_data_access_group == "iganga_municipal_c","Iganga Town Council Health Centre III",
                                    if_else(redcap_data_access_group == "kidera_hciv","Kidera Health Centre IV",
                                    if_else(redcap_data_access_group == "nyahuka_hc_iv", "Nyahuka Health Centre IV",
                                    if_else(redcap_data_access_group == "kibiito_hc_iv", "Kibiito Health Center IV",
                                    if_else(redcap_data_access_group == "rwamwanja_hc_iv", "Rwamwanja Health Center IV",
                                    if_else(redcap_data_access_group == "kyempango_hc_iii", "Kyempango Health Center III",
                                    if_else(redcap_data_access_group == "goma_hc_iv","Goma Health Centre III",
                                    if_else(redcap_data_access_group == "mukono_general_hos","Mukono General Hospital",
                                            redcap_data_access_group)))))))))))))))))))))%>%
 mutate(Date_fi = as.Date(date_fi, format= "%Y-%m-%d"))
 
 
opt_control_ug <- read.csv("Data/original/OPTICTBUGANDAControl_DATA_2025-09-20_0455.csv")%>%
#RENAMING HEALTH FACILITIES BELONGING TO INTERVENTION ARM
mutate(redcap_data_access_group = if_else(redcap_data_access_group == "","Kagando Hospital",
                                  if_else(redcap_data_access_group == "kagando_hospital","Kagando Hospital",
                                  if_else(redcap_data_access_group == "st_benedicts_healt","St. Benedict's Health Centre III",
                                  if_else(redcap_data_access_group == "walukuba_health_ce","Walukuba Health Centre IV",
                                  if_else(redcap_data_access_group == "kawolo_general_hos","Kawolo General Hospital",
                                  if_else(redcap_data_access_group == "nawaikoke_health_c","Nawaikoke Health Centre III",
                                  if_else(redcap_data_access_group == "kiyunga_health_cen","Kiyunga Health Centre IV",
                                  if_else(redcap_data_access_group == "kamuli_hospital","Kamuli General Hospital",
                                  if_else(redcap_data_access_group == "kayunga_regional_r","Kayunga Regional Referral Hospital",
                                  if_else(redcap_data_access_group == "kassanda_hc_iv","Kassanda Health Centre IV",
                                  if_else(redcap_data_access_group == "busaru_hc_iv","Busaru Health Center IV",
                                  if_else(redcap_data_access_group == "rukoki_hc_iv","Rukoki Health Centre IV",
                                  if_else(redcap_data_access_group == "kyarushozi_hc_iv","Kyarusozi Health Center IV",
                                  if_else(redcap_data_access_group == "virika_hospital","Virika Hospital",
                                  if_else(redcap_data_access_group == "st_paul_hospital_k","St. Paul Kasese Hospital",
                                  if_else(redcap_data_access_group == "katooke_hc_iii","Katooke Health Centre III",
                                  if_else(redcap_data_access_group == "kiboga_hospital","kiboga_hospital",
                                          redcap_data_access_group))))))))))))))))))%>%
mutate(Date_fi = as.Date(date_fi, format= "%Y-%m-%d"))




#=============================================================
#************************************************************
#GENERATING QUERIES FROM DIFFERENT FROMS (CRFs)---TANZANIA
#************************************************************
#=============================================================

#**************************
#*ELIGIBILITY FORM (CRF)
#**************************

#extracting missing health facility name
queried_HF_NAME <- opt_treat_tz %>%
#exclude children who came for the next visit following the first visit
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(redcap_data_access_group == "")%>%
  select(record_id, redcap_event_name, date_fi)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "redcap_data_access_group")%>%
  mutate(issue = "health facility name is missing")
write_xlsx(queried_HF_NAME, "queries/Doreen/HF NAME.xlsx")

#extracting missing visit date
queried_visit_date <- opt_treat_tz %>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(date_fi == "")%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "Visit Date")%>%
  mutate(issue = "visit date is missing")
write_xlsx(queried_visit_date, "queries/Doreen/visit_date.xlsx")

#extracting missing study ID
queried_study_id <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(study_id == "") %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "Study ID")%>%
  mutate(issue = "study id is not generated")
write_xlsx(queried_study_id, "queries/Doreen/part_ID.xlsx")

#extracting missing participant sex
queried_sex <- opt_treat_tz %>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(is.na(sex)) %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "sex")%>%
  mutate(issue = "child sex is missing")
write_xlsx(queried_sex, "queries/Doreen/part_sex.xlsx")

#extracting missing participant age
queried_age <- opt_treat_tz %>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(is.na(child_age)) %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "Is the child younger than 10 years old today?")%>%
  mutate(issue = "child age is missing")
write_xlsx(queried_age, "queries/Doreen/part_age.xlsx")

#extracting missing caretaker phone number
queried_phone <- opt_treat_tz %>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(is.na(phone)) %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "Phone number")%>%
  mutate(issue = "phone number is missing")
write_xlsx(queried_phone, "queries/Doreen/phone number.xlsx")

#extracting missing screen date
queried_sdate <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(sdate_ic == "") %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "screening date")%>%
  mutate(issue = "screening date is missing")
write_xlsx(queried_sdate, "queries/Doreen/part_ID.xlsx")

#extracting missing presumptive TB signs and symptoms
queried_TB <- opt_treat_tz %>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(is.na(presumptive_tb___1)) %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Nmae = "Eligibility")%>%
  mutate(Data_field = "TB features which have lasted more than 2 weeks?")%>%
  mutate(issue = "none of the TB features was checked")
write_xlsx(queried_TB, "queries/Doreen/presumptive features.xlsx")

#extracting missing TB diagnosis
queried_tb_treatment <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(presumptive_tb___1 == 0)%>%  #child with at least one features of presumptive TB 
  filter(is.na(tb_treatment))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "child diagnosed with TB disease last six months")%>%
  mutate(issue = "tb treatment is missing for child with at least one feaatures of pressumptive TB")
write_xlsx(queried_tb_treatment, "queries/Doreen/tb diagnosis.xlsx")

#extracting missing consent form
queried_informed_consent <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(tb_treatment == 0)%>%   #child who did not receive TB diagnosis within the last 6 months
  filter(is.na(informed_consent))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "signed the Informed Consent Forms")%>%
  mutate(issue = "Informed consent was not checked for parents/guardian consented")
write_xlsx(queried_informed_consent, "queries/Doreen/informed_consent.xlsx")

#extracting missing assent form
queried_informed_assent <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(informed_consent == 1 & age>=7)%>%   #consented parents/guardians
  filter(is.na(assent))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "signed assent Forms")%>%
  mutate(informed_consent = "signed assent form was not checked for children aged 7 or less years")
write_xlsx(queried_informed_assent, "queries/Doreen/assent.xlsx")


#*************************
#*ASSESSMENT FORM (CRF)
#*************************

#extracting missing stabilization assessment
queried_stabilized <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(danger_signs == 1)%>%   #consented parents/guardians
  filter(is.na(stabilized))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment")%>%
  mutate(Data_field = "Has the child returned after stabilization?")%>%
  mutate(issue = "stabilization was not checked")
write_xlsx(queried_stabilized, "queries/Doreen/stabilized.xlsx")

#extracting missing referral
queried_referred <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(stabilized == 1)%>%   #consented parents/guardians
  filter(is.na(need_referal))%>%
  select(record_id, redcap_event_name,stabilized,redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment")%>%
  mutate(Data_field = "Does the child need to be transferred?")%>%
  mutate(issue = "referal was not checked")
write_xlsx(queried_referred, "queries/Doreen/stabilized.xlsx")

#extracting missing reasons for visit assessment
queried_visit_reasons <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(need_referal==0|danger_signs==0)%>%  #child not referred or has no danger signs
  filter(is.na(reason_for_visit))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment")%>%
  mutate(Data_field = "Reason for Visit")%>%
  mutate(issue = "reason for visit was not checked")
write_xlsx(queried_referred, "queries/Doreen/visit reasons.xlsx")

#extracting missing Assessment Algorithm
queried_algorithm <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(need_referal == 0 | danger_signs == 0)%>%   #consented parents/guardians
  filter(is.na(asmt_algorithm))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Nmae = "Assessment")%>%
  mutate(Data_field = "assessment algorithm")%>%
  mutate(issue = "assessment algorithm was not checked")
write_xlsx(queried_referred, "queries/assessment algorithm.xlsx")

#extracting missing x ray ordered
queried_xray <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(need_referal==0|danger_signs==0)%>%   #consented parents/guardians
  filter(is.na(xray_orederd))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment")%>%
  mutate(Data_field = "Was X-ray Ordered?")%>%
  mutate(issue = "x ray ordered was missing")
write_xlsx(queried_xray, "queries/Doreen/xrayordered.xlsx")

#extracting missing cough records
queried_cough <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_orederd==1)%>%   #for children who x-ray was ordered
  filter(is.na(cough))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment")%>%
  mutate(Data_field = "Cough longer than 2 weeks?")%>%
  mutate(issue = "missing entries for Cough longer than 2 weeks")
write_xlsx(queried_cough, "queries/Doreen/cough.xlsx")

#*******************************
#*RISK ASSESSMENT FORM (CRF)
#*******************************

#extracting missing risk assessment date
queried_riskdate <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(age<2 | age_month<12)%>%
  filter(date_riskassmnt == "")%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "risk assessment")%>%
  mutate(Data_field = "Date")%>%
  mutate(issue = "missing entries for risk assessment date")
write_xlsx(queried_riskdate, "queries/Doreen/risk_date.xlsx")

#extracting missing children under 2 years
queried_under_2yrs <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(age<2 | age_month<12)%>%
  filter(is.na(under_2))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "risk assessment")%>%
  mutate(Data_field = "Is the child under 2 years old?")%>%
  mutate(issue = "missing entries for child under 2 years old")
write_xlsx(queried_under_2yrs, "queries/Doreen/child_2yrs.xlsx")

#extracting missing children living with HIV
queried_hiv_status <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(need_referal==0|danger_signs==0)%>%   #consented parents/guardians
  filter(is.na(live_with_hiv))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "risk assessment")%>%
  mutate(Data_field = "Does the child live with HIV?")%>%
  mutate(issue = "missing entries for child living with HIV")
write_xlsx(queried_hiv_status, "queries/Doreen/child_HIV.xlsx")

#extracting missing Severe acute Malnutrition
queried_severe_maln <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(need_referal==0|danger_signs==0)%>%   #consented parents/guardians
  filter(is.na(severe_malnutrition))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "risk assessment")%>%
  mutate(Data_field = "Does the child have severe acute malnutrition?")%>%
  mutate(issue = "missing entries for Severe acute Malnutrtion")
write_xlsx(queried_severe_maln, "queries/Doreen/child_severe_maln.xlsx")

#extracting missing empirical treatment
queried_imp_treat <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(need_referal==0|danger_signs==0)%>%   #consented parents/guardians
  filter(is.na(imperical_treatment))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "risk assessment")%>%
  mutate(Data_field = "Did the child receive empirical treatment?")%>%
  mutate(issue = "missing entries for impirical treatment")
write_xlsx(queried_imp_treat, "queries/Doreen/imp_treat.xlsx")

#*****************************************************
#*After Imperical Treatment(after_imperical_treatment)
#*****************************************************

#extracting missing date after empirical treatment
queried_imp_date <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(imperical_treatment == 1)%>%   #consented parents/guardians
  filter(date_after_imperical =="")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "After Imperical Treatment(after_imperical_treatment)")%>%
  mutate(Data_field = "Date Returned after Imperical Treatment")%>%
  mutate(issue = "missing entries for Date Returned after Imperical Treatment")
write_xlsx(queried_imp_date, "queries/Doreen/imp_date.xlsx")


#extracting missing status of children after empirical treatment
queried_imp_status <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(imperical_treatment == 1)%>%   #consented parents/guardians
  filter(is.na(status_after_imperical))%>% #children with missing assessment after empirical treatment
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "After Imperical Treatment(after_imperical_treatment)")%>%
  mutate(Data_field = "Child Status")%>%
  mutate(issue = "missing entries for Child Status")
write_xlsx(queried_imp_status, "queries/Doreen/imp_status.xlsx")

#*************************************
#*AMicrobiological evaluation form
#*************************************

#extracting missing for microbiology test
queried_micr_test <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(danger_signs==0 | need_referal==0)%>%   #consented parents/guardians
  filter(is.na(spec_not_colledted))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Microbiological evaluation form")%>%
  mutate(Data_field = "Specimens collected for Mycobacterial test testing?")%>%
  mutate(issue = "missing entries for microbiology test")
write_xlsx(queried_micr_test, "queries/Doreen/microb_test.xlsx")

#extracting missing for Specimen Collection Date
queried_spec_coldate <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(spec_not_colledted == 1)%>%   #consented parents/guardians
  filter(micro_specimen == "")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Microbiological evaluation form")%>%
  mutate(Data_field = "Specimen Collection Date")%>%
  mutate(issue = "missing entries for Specimen Collection Date")
write_xlsx(queried_spec_coldate, "queries/Doreen/specimen_date.xlsx")

#extracting missing for sample submission?  
queried_sample_subm <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(spec_not_colledted == 1)%>%   #consented parents/guardians
  filter(micro_specimen == "")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Microbiological evaluation form")%>%
  mutate(Data_field = "Was the sample submitted?")%>%
  mutate(issue = "missing entries for sample submitted")
write_xlsx(queried_sample_subm, "queries/Doreen/sumple_submission.xlsx")

#extracting missing for Is the child a close or household TB contact?
queried_household_TB <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(danger_signs==0 | need_referal==0)%>%   #consented parents/guardians
  filter(is.na(household_tb_contact))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Microbiological evaluation form")%>%
  mutate(Data_field = "Is the child a close or household TB contact?")%>%
  mutate(issue = "missing entries for household TB contact")
write_xlsx(queried_household_TB, "queries/Doreen/household_TB_contact.xlsx")

#extracting missing for Is there a household member who smokes?
queried_household_smoke <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(danger_signs==0 | need_referal==0)%>%   #consented parents/guardians
  filter(is.na(household_smoke))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Microbiological evaluation form")%>%
  mutate(Data_field = "Is there a household member who smokes?")%>%
  mutate(issue = "missing entries for household member who smokes")
write_xlsx(queried_household_smoke, "queries/Doreen/household_smoke.xlsx")

#************************
#*X-ray evaluation form
#************************

#extracting missing for x ray performed in children
queried_xray_performed <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_orederd==1)%>%   #for children xray ordered
  filter(is.na(xray_performed))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "X-ray evaluation form")%>%
  mutate(Data_field = "If there was X-ray performed or not, please check here")%>%
  mutate(issue = "missing entries for xray peformed")
write_xlsx(queried_xray_performed, "queries/Doreen/xray performed.xlsx")

#extracting missing for x ray request date
queried_xray_rdate <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_performed == 1)%>%   #consented parents/guardians
  filter(xrayrequest_date =="")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "X-ray evaluation form")%>%
  mutate(Data_field = "X-ray request Date")%>%
  mutate(issue = "missing entries for xray request date")
write_xlsx(queried_xray_rdate, "queries/Doreen/xray request_date.xlsx")

#extracting missing for total symptomatic score with x ray
queried_xray_syscore <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_orederd == 1)%>%   #consented parents/guardians
  filter(is.na(xxray_score))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "X-ray evaluation form")%>%
  mutate(Data_field = "Total Symptomatic Score with X-ray")%>%
  mutate(issue = "missing entries for total symptomatic score with x ray")
write_xlsx(queried_xray_syscore, "queries/Doreen/xray sympscore.xlsx")

#extracting missing for total symptomatic score with x ray
queried_xray_wsyscore <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_orederd == 0)%>%   #consented parents/guardians
  filter(is.na(nxray_score))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "X-ray evaluation form")%>%
  mutate(Data_field = "Total Symptomatic Score without X-ray")%>%
  mutate(issue = "missing entries for total symptomatic score without x ray")
write_xlsx(queried_xray_wsyscore, "queries/Doreen/xray wsympscore.xlsx")

#extracting missing for total x ray score
queried_total_xray_score <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_performed == 1)%>%   #consented parents/guardians
  filter(xrayrequest_date =="")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "X-ray evaluation form")%>%
  mutate(Data_field = "Total X-ray Score")%>%
  mutate(issue = "missing entries for total x ray score")
write_xlsx(queried_total_xray_score, "queries/Doreen/total_xrayscore.xlsx")

#extracting missing for scoring date
queried_scoring_date <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_performed == 1)%>%   #consented parents/guardians
  filter(scoring_date =="")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "X-ray evaluation form")%>%
  mutate(Data_field = "Scoring date")%>%
  mutate(issue = "missing entries for total x ray score")
write_xlsx(queried_scoring_date, "queries/Doreen/xscoring date.xlsx")

#**********************
#*Assessment Outcomes
#**********************

#extracting missing for Decision date
queried_decision_date <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(danger_signs==0 | need_referal==0)%>%   #consented parents/guardians
  filter(decision_date =="")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment Outcomes")%>%
  mutate(Data_field = "Decision date")%>%
  mutate(issue = "missing entries for Decision date")
write_xlsx(queried_decision_date, "queries/Doreen/decision date.xlsx")

#extracting missing for Decision date
queried_treat_decision <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(danger_signs==0 | need_referal==0)%>%   #consented parents/guardians
  filter(is.na(treatment_decision))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment Outcomes")%>%
  mutate(Data_field = "Treatment Decision")%>%
  mutate(issue = "missing entries for Treatment Decision")
write_xlsx(queried_treat_decision, "queries/Doreen/decision_treat.xlsx")

#extracting missing for empirical treatment
queried_treat_emp <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(treatment_decision==1)%>%   #consented parents/guardians
  filter(is.na(treatment_decision))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment Outcomes")%>%
  mutate(Data_field = "Was the child given empirical treatment?")%>%
  mutate(issue = "missing entries for empirical treatment")
write_xlsx(queried_treat_emp, "queries/Doreen/empirical_treat.xlsx")

#extracting missing for next 2 months visits
queried_next_visit <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(treatment_decision==1)%>%   #consented parents/guardians
  filter(mont2visit_date =="")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment Outcomes")%>%
  mutate(Data_field = "Next Visit")%>%
  mutate(issue = "missing entries for next visit date")
write_xlsx(queried_next_visit, "queries/Doreen/next_visit.xlsx")

#***********************
#*Month follow up form
#***********************

#extracting missing for scheduled visits
queried_sched_visit <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(treatment_decision==1)%>%   #consented parents/guardians
  filter(is.na(schedulled))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Month follow up form")%>%
  mutate(Data_field = "Type of Scheduled Visit")%>%
  mutate(issue = "missing entries for scheduled visits")
write_xlsx(queried_sched_visit, "queries/Doreen/scheduled_visit.xlsx")

#extracting missing for Next Visit is
queried_next_visit <- opt_treat_tz%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(schedulled==1)%>%   #consented parents/guardians
  filter(nextvism6 == "")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Month follow up form")%>%
  mutate(Data_field = "Next Visit is")%>%
  mutate(issue = "missing entries for next visits")
write_xlsx(queried_next_visit, "queries/Doreen/next_visit.xlsx")

#***************************************************************
#*MONTHLY FOLLOW-UP AFTER TB TREATMENT
#***************************************************************

two_months_follow_up<-subset(opt_treat_tz)[, c(1,2,5,169:177)]%>%
  filter(redcap_event_name == "screening_arm_1" & treatment_decision ==1)%>%
  #mutate(num_days = as.numeric(difftime(as.Date(mont2visit_date), as.Date(decision_date)),units = "days"))
  mutate(num_days = as.numeric(difftime(Sys.Date(), as.Date(decision_date)),units = "days"))%>%
  mutate(due_date = if_else(num_days>60,1,0))

six_months_follow_up<-subset(opt_treat_tz)[, c(1,2,5,178:244)]%>%
  filter(redcap_event_name == "2_month_visit_arm_1")%>%
  mutate(num_days = as.numeric(difftime(Sys.Date(), as.Date(vdate)),units = "days"))%>%
  mutate(due_date = if_else(num_days>180,1,0))

#=============================================================
#*************************************************************
#GENERATING QUERIES FROM DIFFERENT FROMS (CRFs)---UGANDA
#*************************************************************
#=============================================================


#***********************
#*ELIGIBILITY FORM (CRF)
#***********************

#extracting missing health facility name
queried_HF_NAME <- opt_treat_ug %>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(redcap_data_access_group == "")%>%
  select(record_id, redcap_event_name, date_fi)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "redcap_data_access_group")%>%
  mutate(issue = "health facility name is missing")
write_xlsx(queried_HF_NAME, "queries/Uganda/HF NAME.xlsx")

#extracting missing visit date
queried_visit_date <- opt_treat_ug %>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(date_fi == "")%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "Visit Date")%>%
  mutate(issue = "visit date is missing")
write_xlsx(queried_visit_date, "queries/Uganda/visit_date.xlsx")

#extracting missing study ID
queried_study_id <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(study_id == "") %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "Study ID")%>%
  mutate(issue = "study id is not generated")
write_xlsx(queried_study_id, "queries/Uganda/part_ID.xlsx")

#extracting missing participant sex
queried_sex <- opt_treat_ug %>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(is.na(sex)) %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "sex")%>%
  mutate(issue = "child sex is missing")
write_xlsx(queried_sex, "queries/Uganda/part_sex.xlsx")

#extracting missing participant age
queried_age <- opt_treat_ug %>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(is.na(child_age)) %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "Is the child younger than 10 years old today?")%>%
  mutate(issue = "child age is missing")
write_xlsx(queried_age, "queries/Uganda/part_age.xlsx")

#extracting missing caretaker phone number
queried_phone <- opt_treat_ug %>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(is.na(phone)) %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "Phone number")%>%
  mutate(issue = "phone number is missing")
write_xlsx(queried_phone, "queries/Uganda/phone number.xlsx")

#extracting missing screen date
queried_sdate <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(sdate_ic == "") %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "screening date")%>%
  mutate(issue = "screening date is missing")
write_xlsx(queried_sdate, "queries/Uganda/part_ID.xlsx")

#extracting missing presumptive TB signs and symptoms
queried_TB <- opt_treat_ug %>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(is.na(presumptive_tb___1)) %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Nmae = "Eligibility")%>%
  mutate(Data_field = "TB features which have lasted more than 2 weeks?")%>%
  mutate(issue = "none of the TB features was checked")
write_xlsx(queried_TB, "queries/Uganda/presumptive features.xlsx")

#extracting missing TB diagnosis
queried_tb_treatment <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(presumptive_tb___1 == 0)%>%  #child with at least one features of presumptive TB 
  filter(is.na(tb_treatment))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "child diagnosed with TB disease last six months")%>%
  mutate(issue = "tb treatment is missing for child with at least one feaatures of pressumptive TB")
write_xlsx(queried_tb_treatment, "queries/Uganda/tb diagnosis.xlsx")

#extracting missing consent form
queried_informed_consent <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(tb_treatment == 0)%>%   #child who did not receive TB diagnosis within the last 6 months
  filter(is.na(informed_consent))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "signed the Informed Consent Forms")%>%
  mutate(issue = "Informed consent was not checked for parents/guardian consented")
write_xlsx(queried_informed_consent, "queries/Uganda/informed_consent.xlsx")

#extracting missing assent form
queried_informed_assent <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(informed_consent == 1 & age>=7)%>%   #consented parents/guardians
  filter(is.na(assent))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "signed assent Forms")%>%
  mutate(informed_consent = "signed assent form was not checked for children aged 7 or less years")
write_xlsx(queried_informed_assent, "queries/Doreen/assent.xlsx")


#*************************
#*ASSESSMENT FORM (CRF)
#*************************

#extracting missing danger signs assessment
queried_danger_signs <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(tb_treatment==0 & informed_consent == 1)%>%   #consented parents/guardians
  filter(is.na(danger_signs))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment")%>%
  mutate(Data_field = "Are any Danger Signs present?")%>%
  mutate(issue = "presence of any danger signs was not checked")
write_xlsx(queried_danger_signs, "queries/Uganda/danger signs.xlsx")

#extracting missing stabilization assessment
queried_stabilized <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(danger_signs == 1)%>%   #consented parents/guardians
  filter(is.na(stabilized))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment")%>%
  mutate(Data_field = "Has the child returned after stabilization?")%>%
  mutate(issue = "stabilization was not checked")
write_xlsx(queried_stabilized, "queries/Uganda/stabilized.xlsx")

#extracting missing referral
queried_referred <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(stabilized == 1)%>%   #consented parents/guardians
  filter(is.na(need_referal))%>%
  select(record_id, redcap_event_name,stabilized,redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment")%>%
  mutate(Data_field = "Does the child need to be transferred?")%>%
  mutate(issue = "referal was not checked")
write_xlsx(queried_referred, "queries/Uganda/stabilized.xlsx")

#extracting missing reasons for visit assessment
queried_visit_reasons <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(need_referal==0|danger_signs==0)%>%  #child not referred or has no danger signs
  filter(is.na(reason_for_visit))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment")%>%
  mutate(Data_field = "Reason for Visit")%>%
  mutate(issue = "reason for visit was not checked")
write_xlsx(queried_referred, "queries/Uganda/visit reasons.xlsx")

#extracting missing x ray ordered
queried_xray <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(need_referal==0|danger_signs==0)%>%   #consented parents/guardians
  filter(is.na(xray_orederd))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment")%>%
  mutate(Data_field = "Was X-ray Ordered?")%>%
  mutate(issue = "x ray ordered was missing")
write_xlsx(queried_xray, "queries/Uganda/xrayordered.xlsx")

#extracting missing cough records
queried_cough <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_orederd==1)%>%   #for children who x-ray was ordered
  filter(is.na(cough))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment")%>%
  mutate(Data_field = "Cough longer than 2 weeks?")%>%
  mutate(issue = "missing entries for Cough longer than 2 weeks")
write_xlsx(queried_cough, "queries/Uganda/cough.xlsx")

#*********************************
#*RISK ASSESSMENT FORM (CRF)
#*********************************

#extracting missing risk assessment date
queried_riskdate <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(age<2 | age_month<12)%>%
  filter(date_riskassmnt == "")%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "risk assessment")%>%
  mutate(Data_field = "Date")%>%
  mutate(issue = "missing entries for risk assessment date")
write_xlsx(queried_riskdate, "queries/Uganda/risk_date.xlsx")

#extracting missing children under 2 years
queried_under_2yrs <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(age<2 | age_month<12)%>%
  #filter(need_referal==0|danger_signs==0)%>%   #no danger signs and not transferred
  filter(is.na(under_2))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "risk assessment")%>%
  mutate(Data_field = "Is the child under 2 years old?")%>%
  mutate(issue = "missing entries for child under 2 years old")
write_xlsx(queried_under_2yrs, "queries/Uganda/child_2yrs.xlsx")

#extracting missing children living with HIV
queried_hiv_status <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(need_referal==0|danger_signs==0)%>%   #consented parents/guardians
  filter(is.na(live_with_hiv))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "risk assessment")%>%
  mutate(Data_field = "Does the child live with HIV?")%>%
  mutate(issue = "missing entries for child living with HIV")
write_xlsx(queried_hiv_status, "queries/Uganda/child_HIV.xlsx")

#extracting missing Severe acute Malnutrition
queried_severe_maln <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(need_referal==0|danger_signs==0)%>%   #consented parents/guardians
  filter(is.na(severe_malnutrition))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "risk assessment")%>%
  mutate(Data_field = "Does the child have severe acute malnutrition?")%>%
  mutate(issue = "missing entries for Severe acute Malnutrtion")
write_xlsx(queried_severe_maln, "queries/Uganda/child_severe_maln.xlsx")

#extracting missing empirical treatment
queried_imp_treat <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(need_referal==0|danger_signs==0)%>%   #consented parents/guardians
  filter(is.na(imperical_treatment))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "risk assessment")%>%
  mutate(Data_field = "Did the child receive empirical treatment?")%>%
  mutate(issue = "missing entries for impirical treatment")
write_xlsx(queried_imp_treat, "queries/Uganda/imp_treat.xlsx")

#********************************************************
#*After Imperical Treatment(after_imperical_treatment)
#********************************************************

#extracting missing date after empirical treatment
queried_imp_date <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(imperical_treatment == 1)%>%   #consented parents/guardians
  filter(date_after_imperical =="")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "After Imperical Treatment(after_imperical_treatment)")%>%
  mutate(Data_field = "Date Returned after Imperical Treatment")%>%
  mutate(issue = "missing entries for Date Returned after Imperical Treatment")
write_xlsx(queried_imp_date, "queries/Uganda/imp_date.xlsx")


#extracting missing status of children after empirical treatment
queried_imp_status <- opt_treat_ug%>%
  filter(!!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(imperical_treatment == 1)%>%   #consented parents/guardians
  filter(is.na(status_after_imperical))%>% #children with missing assessment after empirical treatment
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "After Imperical Treatment(after_imperical_treatment)")%>%
  mutate(Data_field = "Child Status")%>%
  mutate(issue = "missing entries for Child Status")
write_xlsx(queried_imp_status, "queries/Uganda/imp_status.xlsx")

#*************************************
#*AMicrobiological evaluation form
#*************************************

#extracting missing for microbiology test
queried_micr_test <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(danger_signs==0 | need_referal==0)%>%   #consented parents/guardians
  filter(is.na(spec_not_colledted))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Microbiological evaluation form")%>%
  mutate(Data_field = "Specimens collected for Mycobacterial test testing?")%>%
  mutate(issue = "missing entries for microbiology test")
write_xlsx(queried_micr_test, "queries/Uganda/microb_test.xlsx")

#extracting missing for Specimen Collection Date
queried_spec_coldate <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(spec_not_colledted == 1)%>%   #consented parents/guardians
  filter(micro_specimen == "")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Microbiological evaluation form")%>%
  mutate(Data_field = "Specimen Collection Date")%>%
  mutate(issue = "missing entries for Specimen Collection Date")
write_xlsx(queried_spec_coldate, "queries/Uganda/specimen_date.xlsx")

#extracting missing for sample submission?  
queried_sample_subm <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(spec_not_colledted == 1)%>%   #consented parents/guardians
  filter(micro_specimen == "")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Microbiological evaluation form")%>%
  mutate(Data_field = "Was the sample submitted?")%>%
  mutate(issue = "missing entries for sample submitted")
write_xlsx(queried_sample_subm, "queries/Uganda/sumple_submission.xlsx")

#extracting missing for Is the child a close or household TB contact?
queried_household_TB <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(danger_signs==0 | need_referal==0)%>%   #consented parents/guardians
  filter(is.na(household_tb_contact))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Microbiological evaluation form")%>%
  mutate(Data_field = "Is the child a close or household TB contact?")%>%
  mutate(issue = "missing entries for household TB contact")
write_xlsx(queried_household_TB, "queries/Uganda/household_TB_contact.xlsx")

#extracting missing for Is there a household member who smokes?
queried_household_smoke <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(danger_signs==0 | need_referal==0)%>%   #consented parents/guardians
  filter(is.na(household_smoke))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Microbiological evaluation form")%>%
  mutate(Data_field = "Is there a household member who smokes?")%>%
  mutate(issue = "missing entries for household member who smokes")
write_xlsx(queried_household_smoke, "queries/Uganda/household_smoke.xlsx")

#*****************************
#*X-ray evaluation form
#*****************************

#extracting missing for x ray performed in children
queried_xray_performed <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_orederd==1)%>%   #for children xray ordered
  filter(is.na(xray_performed))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "X-ray evaluation form")%>%
  mutate(Data_field = "If there was X-ray performed or not, please check here")%>%
  mutate(issue = "missing entries for xray peformed")
write_xlsx(queried_xray_performed, "queries/Uganda/xray performed.xlsx")

#extracting missing for x ray request date
queried_xray_rdate <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_performed == 1)%>%   #consented parents/guardians
  filter(xrayrequest_date =="")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "X-ray evaluation form")%>%
  mutate(Data_field = "X-ray request Date")%>%
  mutate(issue = "missing entries for xray request date")
write_xlsx(queried_xray_rdate, "queries/Uganda/xray request_date.xlsx")

#extracting missing for total symptomatic score with x ray
queried_xray_syscore <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_orederd == 1)%>%   #consented parents/guardians
  filter(is.na(xxray_score))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "X-ray evaluation form")%>%
  mutate(Data_field = "Total Symptomatic Score with X-ray")%>%
  mutate(issue = "missing entries for total symptomatic score with x ray")
write_xlsx(queried_xray_syscore, "queries/Uganda/xray sympscore.xlsx")

#extracting missing for total symptomatic score with x ray
queried_xray_wsyscore <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_orederd == 0)%>%   #consented parents/guardians
  filter(is.na(nxray_score))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "X-ray evaluation form")%>%
  mutate(Data_field = "Total Symptomatic Score without X-ray")%>%
  mutate(issue = "missing entries for total symptomatic score without x ray")
write_xlsx(queried_xray_wsyscore, "queries/Uganda/xray wsympscore.xlsx")

#extracting missing for total x ray score
queried_total_xray_score <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_performed == 1)%>%   #consented parents/guardians
  filter(xrayrequest_date =="")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "X-ray evaluation form")%>%
  mutate(Data_field = "Total X-ray Score")%>%
  mutate(issue = "missing entries for total x ray score")
write_xlsx(queried_total_xray_score, "queries/Uganda/total_xrayscore.xlsx")

#extracting missing for scoring date
queried_scoring_date <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(xray_performed == 1)%>%   #consented parents/guardians
  filter(scoring_date =="")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "X-ray evaluation form")%>%
  mutate(Data_field = "Scoring date")%>%
  mutate(issue = "missing entries for total x ray score")
write_xlsx(queried_scoring_date, "queries/Uganda/xscoring date.xlsx")

#****************************
#*Assessment Outcomes
#****************************

#extracting missing for Decision date
queried_decision_date <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(danger_signs==0 | need_referal==0)%>%   #consented parents/guardians
  filter(decision_date =="")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment Outcomes")%>%
  mutate(Data_field = "Decision date")%>%
  mutate(issue = "missing entries for Decision date")
write_xlsx(queried_decision_date, "queries/Uganda/decision date.xlsx")

#extracting missing for Decision date
queried_treat_decision <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(danger_signs==0 | need_referal==0)%>%   #consented parents/guardians
  filter(is.na(treatment_decision))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment Outcomes")%>%
  mutate(Data_field = "Treatment Decision")%>%
  mutate(issue = "missing entries for Treatment Decision")
write_xlsx(queried_treat_decision, "queries/Uganda/decision_treat.xlsx")

#extracting missing for empirical treatment
queried_treat_emp <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(treatment_decision==1)%>%   #consented parents/guardians
  filter(is.na(treatment_decision))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment Outcomes")%>%
  mutate(Data_field = "Was the child given empirical treatment?")%>%
  mutate(issue = "missing entries for empirical treatment")
write_xlsx(queried_treat_emp, "queries/Uganda/empirical_treat.xlsx")

#extracting missing for next 2 months visits
queried_next_visit <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(treatment_decision==1)%>%   #consented parents/guardians
  filter(mont2visit_date =="")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Assessment Outcomes")%>%
  mutate(Data_field = "Next Visit")%>%
  mutate(issue = "missing entries for next visit date")
write_xlsx(queried_next_visit, "queries/Uganda/next_visit.xlsx")

#****************************
#*Month follow up form
#****************************

#extracting missing for scheduled visits
queried_sched_visit <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(treatment_decision==1)%>%   #consented parents/guardians
  filter(is.na(schedulled))%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Month follow up form")%>%
  mutate(Data_field = "Type of Scheduled Visit")%>%
  mutate(issue = "missing entries for scheduled visits")
write_xlsx(queried_sched_visit, "queries/Uganda/scheduled_visit.xlsx")

#extracting missing for Next Visit is
queried_next_visit <- opt_treat_ug%>%
  filter(!redcap_event_name%in%c("2_month_visit_arm_1","6_month_visit_arm_1"))%>%
  filter(schedulled==1)%>%   #consented parents/guardians
  filter(nextvism6 == "")%>%
  select(record_id, redcap_event_name, redcap_data_access_group)%>%
  mutate(CRF_Name = "Month follow up form")%>%
  mutate(Data_field = "Next Visit is")%>%
  mutate(issue = "missing entries for next visits")
write_xlsx(queried_next_visit, "queries/Uganda/next_visit.xlsx")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#=============================================================
#*************************************************************
#GENERATING QUERIES FROM DIFFERENT FROMS (CRFs)---DRC
#*************************************************************
#=============================================================

#***********************
#*ELIGIBILITY FORM (CRF)
#***********************

#extracting missing health facility name
queried_HF_NAME <- opt_treat_ug %>%
  filter(redcap_data_access_group == "")%>%
  select(record_id, redcap_event_name, date_fi)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "redcap_data_access_group")%>%
  mutate(issue = "health facility name is missing")
write_xlsx(queried_HF_NAME, "queries/Uganda/HF NAME.xlsx")

#extracting missing visit date
queried_visit_date <- opt_treat_ug %>%
  filter(date_fi == "")%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "Visit Date")%>%
  mutate(issue = "visit date is missing")
write_xlsx(queried_visit_date, "queries/Uganda/visit_date.xlsx")

#extracting missing study ID
queried_study_id <- opt_treat_ug%>%
  filter(study_id == "") %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "Study ID")%>%
  mutate(issue = "study id is not generated")
write_xlsx(queried_study_id, "queries/Uganda/part_ID.xlsx")

#extracting missing participant sex
queried_sex <- opt_treat_ug %>%
  filter(is.na(sex)) %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "sex")%>%
  mutate(issue = "child sex is missing")
write_xlsx(queried_sex, "queries/Uganda/part_sex.xlsx")

#extracting missing participant age
queried_age <- opt_treat_ug %>%
  filter(is.na(child_age)) %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "Is the child younger than 10 years old today?")%>%
  mutate(issue = "child age is missing")
write_xlsx(queried_age, "queries/Uganda/part_age.xlsx")

#extracting missing caretaker phone number
queried_phone <- opt_treat_ug %>%
  filter(is.na(phone)) %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "Phone number")%>%
  mutate(issue = "phone number is missing")
write_xlsx(queried_phone, "queries/Uganda/phone number.xlsx")

#extracting missing screen date
queried_sdate <- opt_treat_ug%>%
  filter(sdate_ic == "") %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "screening date")%>%
  mutate(issue = "screening date is missing")
write_xlsx(queried_sdate, "queries/Uganda/part_ID.xlsx")

#extracting missing presumptive TB signs and symptoms
queried_TB <- opt_treat_ug %>%
  filter(is.na(presumptive_tb___1)) %>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Nmae = "Eligibility")%>%
  mutate(Data_field = "TB features which have lasted more than 2 weeks?")%>%
  mutate(issue = "none of the TB features was checked")
write_xlsx(queried_TB, "queries/Uganda/presumptive features.xlsx")

#extracting missing TB diagnosis
queried_tb_treatment <- opt_treat_ug%>%
  filter(presumptive_tb___1 == 0)%>%  #child with at least one features of presumptive TB 
  filter(is.na(tb_treatment))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "child diagnosed with TB disease last six months")%>%
  mutate(issue = "tb treatment is missing for child with at least one feaatures of pressumptive TB")
write_xlsx(queried_tb_treatment, "queries/Uganda/tb diagnosis.xlsx")

#extracting missing consent form
queried_informed_consent <- opt_treat_ug%>%
  filter(tb_treatment == 0)%>%   #child who did not receive TB diagnosis within the last 6 months
  filter(is.na(informed_consent))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "signed the Informed Consent Forms")%>%
  mutate(issue = "Informed consent was not checked for parents/guardian consented")
write_xlsx(queried_informed_consent, "queries/Uganda/informed_consent.xlsx")

#extracting missing assent form
queried_informed_assent <- opt_treat_ug%>%
  filter(informed_consent == 1 & age>=7)%>%   #consented parents/guardians
  filter(is.na(assent))%>%
  select(record_id, redcap_event_name,redcap_data_access_group)%>%
  mutate(CRF_Name = "Eligibility")%>%
  mutate(Data_field = "signed assent Forms")%>%
  mutate(informed_consent = "signed assent form was not checked for children aged 7 or less years")
write_xlsx(queried_informed_assent, "queries/Doreen/assent.xlsx")






#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#DATA MONITORING FOR TANZANIA
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#=================================================================
#*****************************************************************
# MONTHLY DATA MONITORING FOR INTERVENTION ARM
#*****************************************************************
#=================================================================
interv_arm <- opt_treat_tz%>%

#Eligible children with at least one presumptive TB symptoms
mutate(presumpt_tb = if_else(presumptive_tb___2 == 1 |
                                       presumptive_tb___3 == 1 |
                                       presumptive_tb___4 == 1 |
                                       presumptive_tb___5 == 1 |
                                       presumptive_tb___6 == 1 |
                                       presumptive_tb___7 == 1, 1,0))%>%
rowwise() %>%
mutate(TDA_score = sum(xxray_score, nxray_score, xray_score, na.rm = TRUE))%>%
mutate(bin_tda_score = if_else(TDA_score>10,1,0))

#=================================================================
#Presumptive cases
#=================================================================

#children with at least one feature of presumptive TB
April_presumpt <- interv_arm%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

May_presumpt <- interv_arm%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

June_presumpt <- interv_arm%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

July_presumpt <- interv_arm%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

August_presumpt <- interv_arm%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

September_presumpt <- interv_arm%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

#==============================================================
# Microbiological evaluation form ------ intervention arm
#==============================================================

#Positive results 
MEF_interv <-interv_arm%>%
 mutate(binscore = if_else(micro_lab_result%in%c(1,3,4) | 
                             lab_res_microscopy%in%c(3,4,5) |
                             lab_res_culture%in%c(3,4,5), 1,0))

#children with positive microbiology test
 April_posresults <- MEF_interv%>%
   filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
   group_by(binscore)%>%
   summarise(count = n())
 
 May_posresults <- MEF_interv%>%
   filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
   group_by(binscore)%>%
   summarise(count = n())
 
 June_posresults <- MEF_interv%>%
   filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
   group_by(binscore)%>%
   summarise(count = n())
 
 July_posresults <- MEF_interv%>%
   filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
   group_by(binscore)%>%
   summarise(count = n())
 
 August_posresults <- MEF_interv%>%
   filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
   group_by(binscore)%>%
   summarise(count = n())
 
 september_posresults <- MEF_interv%>%
   filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
   group_by(binscore)%>%
   summarise(count = n())
 
#==============================================================
# Household TB contact form ------ intervention arm
#==============================================================
 
#TB contact tracing
TB_contact <- interv_arm%>%
  mutate(binscore = if_else(household_tb_contact == 1, 1,0)) 

#children with at least one feature of presumptive TB
April_contact <- TB_contact%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

May_contact <- TB_contact%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

June_contact <- TB_contact%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

July_contact <- TB_contact%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

August_contact <- TB_contact%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

september_contact <- TB_contact%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

#***********************************************************
#  X-ray evaluation form(xray_evaluation_form)
#***********************************************************

XRE_interv <-interv_arm%>%
  rowwise() %>%
  mutate(TDA_score = sum(xxray_score, nxray_score, xray_score, na.rm = TRUE))%>%
  mutate(bin_tda_score = if_else(TDA_score>10,1,0))

#children with at least one feature of presumptive TB
April_tda_score <- XRE_interv%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(bin_tda_score)%>%
  summarise(count = n())

May_tda_score <- XRE_interv%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(bin_tda_score)%>%
  summarise(count = n())

June_tda_score <- XRE_interv%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(bin_tda_score)%>%
  summarise(count = n())

July_tda_score <- XRE_interv%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(bin_tda_score)%>%
  summarise(count = n())

August_tda_score <- XRE_interv%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(bin_tda_score)%>%
  summarise(count = n())

september_tda_score <- XRE_interv%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(bin_tda_score)%>%
  summarise(count = n())

#=============================================================
#Assessment Outcomes(assessment_outcomes)
#=============================================================
TRM_interv <-interv_arm%>%
  mutate(treat_decision = if_else(treatment_decision==1,1,
                                  if_else(treatment_decision==0,0,NA)))

#children with at least one feature of presumptive TB
April_trm_deci <- TRM_interv%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

May_trm_deci <- TRM_interv%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

June_trm_deci <- TRM_interv%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

July_trm_deci <- TRM_interv%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

August_trm_deci <- TRM_interv%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

september_trm_deci <- TRM_interv%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

#=================================================================
#*****************************************************************
# MONTHLY DATA MONITORING FOR CONTROL ARM
#*****************************************************************
#=================================================================

ContrOl_arm <- opt_control_tz%>%
  
  #Eligible children with at least one presumptive TB symptoms
  mutate(presumpt_tb = if_else(presumptive_tb___2 == 1 |
                                 presumptive_tb___3 == 1 |
                                 presumptive_tb___4 == 1 |
                                 presumptive_tb___5 == 1 |
                                 presumptive_tb___6 == 1 |
                                 presumptive_tb___7 == 1 |
                                 presumptive_tb___8 == 1, 1,0))  

#children with at least one feature of presumptive TB
April_presumtb <- ContrOl_arm%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

May_presumtb <- ContrOl_arm%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

June_presumtb <- ContrOl_arm%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

July_presumtb <- ContrOl_arm%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

August_presumtb <- ContrOl_arm%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

september_presumtb <- ContrOl_arm%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

#==============================================================
# Microbiological evaluation form ------ intervention arm
#==============================================================

#Positive results 
ContrOl_results <-opt_control_tz%>%
  mutate(binscore = if_else(micro_lab_result%in%c(1,3,4) | 
                              lab_res_microscopy%in%c(3,4,5) |
                              lab_res_culture%in%c(3,4,5), 1,0))

#children with positive microbiology test
April_posresults <- ContrOl_results%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

May_posresults <- ContrOl_results%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

June_posresults <- ContrOl_results%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

July_posresults <- ContrOl_results%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

August_posresults <- ContrOl_results%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

september_posresults <- ContrOl_results%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

#==============================================================
# Household TB contact form ------ intervention arm
#==============================================================

#TB contact tracing
ContrOl_contact <- opt_control_tz%>%
  mutate(binscore = if_else(household_tb_contact == 1, 1,0)) 

#children with at least one feature of presumptive TB
April_contact <- ContrOl_contact%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

May_contact <- ContrOl_contact%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

June_contact <- ContrOl_contact%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

July_contact <- ContrOl_contact%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

August_contact <- ContrOl_contact%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

september_contact <- ContrOl_contact%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

#=============================================================
#Assessment Outcomes(assessment_outcomes)
#=============================================================
Control_outcm <-opt_control_tz%>%
  mutate(treat_decision = if_else(treatment_decision==1,1,
                                  if_else(treatment_decision==0,0,NA)))

#children with at least one feature of presumptive TB
April_trm_deci <- Control_outcm%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

May_trm_deci <- Control_outcm%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

June_trm_deci <- Control_outcm%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

July_trm_deci <- Control_outcm%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

August_trm_deci <- Control_outcm%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

september_trm_deci <- Control_outcm%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#DATA MONITORING FOR UGANDA
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#=================================================================
#*****************************************************************
# MONTHLY DATA MONITORING FOR INTERVENTION ARM
#*****************************************************************
#=================================================================
interv_arm <- opt_treat_ug%>%
  
  #Eligible children with at least one presumptive TB symptoms
  mutate(presumpt_tb = if_else(presumptive_tb___2 == 1 |
                                 presumptive_tb___3 == 1 |
                                 presumptive_tb___4 == 1 |
                                 presumptive_tb___5 == 1 |
                                 presumptive_tb___6 == 1 |
                                 presumptive_tb___7 == 1, 1,0))%>%
  rowwise() %>%
  mutate(TDA_score = sum(xxray_score, nxray_score, xray_score, na.rm = TRUE))%>%
  mutate(bin_tda_score = if_else(TDA_score>10,1,0))

#=================================================================
#Presumptive cases
#=================================================================

#children with at least one feature of presumptive TB
April_presumpt <- interv_arm%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

May_presumpt <- interv_arm%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

June_presumpt <- interv_arm%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

July_presumpt <- interv_arm%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

August_presumpt <- interv_arm%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

September_presumpt <- interv_arm%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

#==============================================================
# Microbiological evaluation form ------ intervention arm
#==============================================================

#Positive results 
MEF_interv <-opt_treat_ug%>%
  mutate(binscore = if_else(micro_lab_result%in%c(1,3,4) | 
                              lab_res_microscopy%in%c(3,4,5) |
                              lab_res_culture%in%c(3,4,5), 1,0))

#children with positive microbiology test
April_posresults <- MEF_interv%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

May_posresults <- MEF_interv%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

June_posresults <- MEF_interv%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

July_posresults <- MEF_interv%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

August_posresults <- MEF_interv%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

september_posresults <- MEF_interv%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

#==============================================================
# Household TB contact form ------ intervention arm
#==============================================================

#TB contact tracing
TB_contact <- opt_treat_ug%>%
  mutate(binscore = if_else(household_tb_contact == 1, 1,0)) 

#children with at least one feature of presumptive TB
April_contact <- TB_contact%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

May_contact <- TB_contact%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

June_contact <- TB_contact%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

July_contact <- TB_contact%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

August_contact <- TB_contact%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

september_contact <- TB_contact%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

#***********************************************************
#  X-ray evaluation form(xray_evaluation_form)
#***********************************************************

XRE_interv <-opt_treat_ug%>%
  rowwise() %>%
  mutate(TDA_score = sum(xxray_score, nxray_score, xray_score, na.rm = TRUE))%>%
  mutate(bin_tda_score = if_else(TDA_score>10,1,0))

#children with at least one feature of presumptive TB
April_tda_score <- XRE_interv%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(bin_tda_score)%>%
  summarise(count = n())

May_tda_score <- XRE_interv%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(bin_tda_score)%>%
  summarise(count = n())

June_tda_score <- XRE_interv%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(bin_tda_score)%>%
  summarise(count = n())

July_tda_score <- XRE_interv%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(bin_tda_score)%>%
  summarise(count = n())

August_tda_score <- XRE_interv%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(bin_tda_score)%>%
  summarise(count = n())

september_tda_score <- XRE_interv%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(bin_tda_score)%>%
  summarise(count = n())

#=============================================================
#Assessment Outcomes(assessment_outcomes)
#=============================================================
TRM_interv <-opt_treat_ug%>%
  mutate(treat_decision = if_else(treatment_decision==1,1,
                                  if_else(treatment_decision==0,0,NA)))

#children with at least one feature of presumptive TB
April_trm_deci <- TRM_interv%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

May_trm_deci <- TRM_interv%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

June_trm_deci <- TRM_interv%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

July_trm_deci <- TRM_interv%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

August_trm_deci <- TRM_interv%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

september_trm_deci <- TRM_interv%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

#=================================================================
#*****************************************************************
# MONTHLY DATA MONITORING FOR CONTROL ARM
#*****************************************************************
#=================================================================

ContrOl_arm <- opt_control_ug%>%
  
  #Eligible children with at least one presumptive TB symptoms
  mutate(presumpt_tb = if_else(presumptive_tb___2 == 1 |
                                 presumptive_tb___3 == 1 |
                                 presumptive_tb___4 == 1 |
                                 presumptive_tb___5 == 1 |
                                 presumptive_tb___6 == 1 |
                                 presumptive_tb___7 == 1 |
                                 presumptive_tb___8 == 1, 1,0))  

#children with at least one feature of presumptive TB
April_presumtb <- ContrOl_arm%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

May_presumtb <- ContrOl_arm%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

June_presumtb <- ContrOl_arm%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

July_presumtb <- ContrOl_arm%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

August_presumtb <- ContrOl_arm%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

september_presumtb <- ContrOl_arm%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(presumpt_tb)%>%
  summarise(count = n())

#==============================================================
# Microbiological evaluation form ------ intervention arm
#==============================================================

#Positive results 
ContrOl_results <-opt_control_ug%>%
  mutate(binscore = if_else(micro_lab_result%in%c(1,3,4) | 
                              lab_res_microscopy%in%c(3,4,5) |
                              lab_res_culture%in%c(3,4,5), 1,0))

#children with positive microbiology test
April_posresults <- ContrOl_results%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

May_posresults <- ContrOl_results%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

June_posresults <- ContrOl_results%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

July_posresults <- ContrOl_results%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

August_posresults <- ContrOl_results%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

september_posresults <- ContrOl_results%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

#==============================================================
# Household TB contact form ------ intervention arm
#==============================================================

#TB contact tracing
ContrOl_contact <- opt_control_ug%>%
  mutate(binscore = if_else(household_tb_contact == 1, 1,0)) 

#children with at least one feature of presumptive TB
April_contact <- ContrOl_contact%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

May_contact <- ContrOl_contact%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

June_contact <- ContrOl_contact%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

July_contact <- ContrOl_contact%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

August_contact <- ContrOl_contact%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(binscore)%>%
  summarise(count = n())

september_contact <- ContrOl_contact%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(binscore)%>%
  summarise(count = n())

#=============================================================
#Assessment Outcomes(assessment_outcomes)
#=============================================================
Control_outcm <-opt_control_ug%>%
  mutate(treat_decision = if_else(treatment_decision==1,1,
                                  if_else(treatment_decision==0,0,NA)))

#children with at least one feature of presumptive TB
April_trm_deci <- Control_outcm%>%
  filter(between(Date_fi, as.Date('2025-04-01'), as.Date('2025-04-30')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

May_trm_deci <- Control_outcm%>%
  filter(between(Date_fi, as.Date('2025-05-01'), as.Date('2025-05-31')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

June_trm_deci <- Control_outcm%>%
  filter(between(Date_fi, as.Date('2025-06-01'), as.Date('2025-06-30')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

July_trm_deci <- Control_outcm%>%
  filter(between(Date_fi, as.Date('2025-07-01'), as.Date('2025-07-31')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

August_trm_deci <- Control_outcm%>%
  filter(between(Date_fi, as.Date('2025-08-01'), as.Date('2025-08-31')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

september_trm_deci <- Control_outcm%>%
  filter(between(Date_fi, as.Date('2025-09-01'), as.Date('2025-09-30')))%>%
  group_by(treat_decision)%>%
  summarise(count = n())

#==============================================================
#Quality of life / Ajibu mtoto (7 - 10 Years)(quality_of_life_ajibu_mtoto_7_10_years)
#==============================================================
QLF_7_10_interv <-subset(opt_treat[,c(1,5,6,21,157:165)])
QLF_7_10_contr <-subset(opt_control[,c(1,5,6,21,118:126)])

#=============================================================
#Quality Of Life / Ajibu Mzazi au Mlezi ( 3 - 6 Years)(quality_of_life_ajibu_mzazi_au_mlezi_3_6_years)
#=============================================================
QLF7_10_interv <-subset(opt_treat[,c(1,5,6,21,166:173)])
QLF7_10_contr <-subset(opt_control[,c(1,5,6,21,127:134)])

#============================================================= 
#Part 1: Social Economic - Gharama za Matibabu(social_economic_gharama_za_matibabu)
#=============================================================
SES_interv  <-subset(opt_treat[,c(1,5,6,21,174:297)])
SES_contr <-subset(opt_control[,c(1,5,6,21,135:259)])
  
#================================================================
#Household members(household_members)
#================================================================
HLIST_interv <-subset(opt_treat[,c(1,5,6,21,298:321)])
HLIST_contr <-subset(opt_control[,c(1,5,6,21,260:322)])

#===============================================================
#TB Medication Adherence(tb_medication_adherence)
#===============================================================
Cosnt_interv <-subset(opt_treat[,c(1,5,6,21,322:360)])
Cosnt_contr <-subset(opt_control[,c(11,5,6,21,323:330)])

#===============================================================
#Month follow up form(month_follow_up_form)
#===============================================================
TBR_interv <-subset(opt_treat[,c(1,5,6,21,361:368)])
TBR_contr <-subset(opt_control[,c(1,5,6,21,331:335)])

#===============================================================
#Mortality Information(mortality_information)
#===============================================================
MORT_interv <-subset(opt_treat[,c(1,5,6,21,369:373)])
MORT_contr <-subset(opt_control[,c(1,5,6,21,336:365)])

#===============================================================
#TB Treatment(tb_treatment)
#===============================================================
TBT_interv <-subset(opt_treat[,c(1,5,6,21,374:403)])
TBT_contr <-subset(opt_control[,c(1,5,6,21,366:374)])

#===============================================================
#Reference Standard Assessment(reference_standard_assessment) 
#===============================================================  
RSA_interv <-subset(opt_treat[,c(1,5,6,21,404:412)])
RSA_contr <-subset(opt_control[,c(1,5,6,21,375:381)])

