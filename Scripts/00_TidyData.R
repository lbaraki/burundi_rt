# PROJECT: Burundi Recency Testing Analysis 
# PURPOSE: Munge and Analysis of Receny Testing Data 
# AUTHOR: Lemlem Baraki | SI
# REF ID:   eba4e412
# LICENSE: MIT
# DATE: 2023-06-23
# NOTES: Lemlem Baraki | SI

# LOCALS & SETUP ============================================================================

  # Libraries
library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)
library(table1)
    
    
  # SI specific paths/functions  
    load_secrets()
    data_folder <- "Data/"
    #merdata <- file.path(glamr::si_path("path_msd"))
    #file_path <- return_latest(folderpath = merdata,
     # pattern = "Site_IM_FY20-22_20220318_v2_1_Zambia")
      
  # Grab metadata
   #msd_source <- source_info(file_path)
  
  
  # REF ID for plots
    ref_id <- "eba4e412"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  msd
    #Recency data
    df <- data_folder %>% 
    return_latest("Final ANSS Recency Data.xlsx") %>%
      read_excel()%>%
      janitor::clean_names()
      

# MUNGE ============================================================================
  
  #pt level data
    #Want to have demographic variables (5): age_band, sex, marriage, residence_district, education
      #sex: (recode 1 for male, 0 for female)
      #age_band: 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+ (align with MER)
      #married: single, married, divorced, separate, widower, free-union
      #residence_district: centre, nord, sud, bururi, fota, isale
      #education: no school, fundamental, post-fundamental, university 
        #use forcats::fct_relevel to set the order
      #occupation: 15 options -> reduce to top 5 (farmer, unemployed, sex worker, student, other)
    df_clean <- df %>%
      filter(sidainfo_site_id != 5013,
             sidainfo_site_id != 5015) %>% #exclude duplicates 
      mutate(sex_binary = ifelse(sex == "Male", "1", "0")) %>%
      mutate(ever_tested = case_when(ever_tested == "Never tested before" ~ "0",
                                     ever_tested == "Tested less than 6 months ago" ~ "0-6 months",
                                     ever_tested == "Between 6 and 12 months" ~ "6-12 months", 
                                     ever_tested == "Between 13 and 24 months" ~ "13-24 months", 
                                     ever_tested == "Over 2 years ago" ~ "24+ months"
                                      ))%>% 
      mutate(educ_fct = str_remove_all(education, "-")%>% factor())%>%
      mutate(educ_fct = fct_relevel(educ_fct, "Fundamental", after = 2))%>%
      mutate(occup_cat = case_when(
        occupation %in% c("Bike taxi", "Truck drivers", "Employee", "Good", "Hairdressers", 
                          "Masons", "Mechanic", "Photographer", "Private", "Seasonal workers",
                          "Small business", "Tailor") ~ "Other", 
        occupation %in% c("Student or student") ~ "Student", 
        TRUE ~ occupation))%>%
      select(age_band, sex_binary, married, residence_province, education, educ_fct, ever_tested, 
             test_entry, test_reason, population_group, occupation, occup_cat, rita_result, 
             rtri_result, vl_result)
    
    
    #Want to have characteristic variables (4): ever_tested, test_entry, test_reason, population_group
      #ever_tested: 0, < 6 months, 6-12 mon, 13-24 mon, 24+ mon (recoded above)
      #test_entry: Community Relay Initiative, CPN prenatal consultation service, Indexation, Voluntary screening service
      #test_reason: Targeted voluntary screening, Self test, Screening intiated by the service provider, Index testing, CPN
      #population_group: general, MSM, Sex Worker
    

# ANALYSIS ============================================================================
    
#outcome variable: rita_result

tally(~rita_result, data = df_clean, format = "count")
    
#exposure variables:occup_cat, educ_fct
    
tally(~occup_cat, data = df_clean, format = "count")

tally(~educ_fct, data = df_clean, format = "count")
  

# VIZ ============================================================================

  #Make tables
    
    #Compute p-values wit this function:
    pvalue <- function(x, ...) {
      # Construct vectors of data y, and groups (strata) g
      y <- unlist(x)
      g <- factor(rep(1:length(x), times=sapply(x, length)))
      if (is.numeric(y)) {
        # For numeric variables, perform a standard 2-sample t-test
        p <- t.test(y ~ g)$p.value
      } else {
        # For categorical variables, perform a chi-squared test of independence
        p <- chisq.test(table(y, g))$p.value
      }
      # Format the p-value, using an HTML entity for the less-than sign.
      # The initial empty string places the output on the line below the variable label.
      c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
    }
    
    
   #Table 1: outcome by rita_result
    df_clean$rita_result <-factor(df_clean$rita_result,
                                  labels = c("Long term", "Recent"))
    
    df_clean$sex_binary <- factor(df_clean$sex_binary, 
                                  labels = c("Females", "Males"))
    
      #Add labels 
      label(df_clean$sex_binary) <- "Sex"
      label(df_clean$age_band) <- "Age"
      label(df_clean$married) <- "Marital Status"
      label(df_clean$residence_province) <- "Province"
      label(df_clean$educ_fct) <- "Education"
      label(df_clean$ever_tested) <- "Testing Frequency"
      label(df_clean$population_group) <- "Key Population Group"
      label(df_clean$occup_cat) <- "Occupation"
    
    table1::table1(~sex_binary + age_band  + population_group + residence_province +
                     educ_fct + occup_cat + married | rita_result, 
                   data = df_clean,
                   #overall = F, #T/F for overall column
                   extra.col = list(`P-value`=pvalue), #add in p-values as last row
                   topclass = "Rtable1-grid Rtable1-shade Rtable1-times", #table style
                   #footnote = "***", 
                   caption = "Table 1: Baseline RITA Cohort Demographics")
    
    si_save("table1.png", path = "Images")
  
    #Table 2: outcome by rtri_result
    df_clean$rtri_result <-factor(df_clean$rtri_result,
                                  labels = c("Long term", "Recent"))
    
    table1::table1(~sex_binary + age_band + married + residence_district + 
                     education + population_group + ever_tested| rtri_result, 
                   data = df_clean,
                   #overall = F, 
                   extra.col = list(`P-value`=pvalue), #substitute in p-values as last row
                   topclass = "Rtable1-grid Rtable1-shade Rtable1-times", #table style
                   #footnote = "***", 
                   caption = "Table 2: Baseline RTRI Cohort Demographics")
    
    si_save("table2.png", path = "Images")

# SPINDOWN ============================================================================

      today <- lubridate::today()#includes day/time
      
      #write_csv(df_final, glue("Dataout/newdata-burundi-rt-data-cleaned-{today}.csv"))
      