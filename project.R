# Final Project. Machine Learning
# Link to data set: https://www.kaggle.com/miker400/washington-state-home-mortgage-hdma2016
# Description of dataset: https://cfpb.github.io/api/hmda/fields.html
# Authors: Alexander Parunov, Mathieu Chiavassa, Wang Yang

if(!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

# Read data frame
hdma_df <- read.csv("Washington_State_HDMA-2016.csv")
dim(hdma_df)

remove_columns <- function(hdma_df, column_names) {
  for(cn in column_names) {
    colnumber <- which(colnames(hdma_df) == cn)
    if(colnumber > 0) hdma_df <- hdma_df[,-colnumber]
  }
  return(hdma_df)
}

# Columns to remove: 
# We don't need state abbreviation, it's always Washington (WA)
# We don't care about ids and sequence number and year (2016)
# We don't need agency name, we can use just abbreviation
# We also don't need (co)applicant race names after 1.
# Census tract number says nothing
# application_date_indicator is always after 1/1/2004, so this variable is meaningless
# hoepa_status_name is mostly Not HOEPA loan, so this variable is meaningless

columns_to_remove = c("state_abbr","state_name", "respondent_id", "sequence_number", "as_of_year", "agency_name",
                      "msamd_name", "applicant_race_name_2","applicant_race_name_3","applicant_race_name_4","applicant_race_name_5",
                      "co_applicant_race_name_2", "co_applicant_race_name_3","co_applicant_race_name_4","co_applicant_race_name_5",
                      "census_tract_number","application_date_indicator","hoepa_status_name")

hdma_df <- remove_columns(hdma_df, columns_to_remove)

dim(hdma_df) #18 columns removed

summary(hdma_df)

# Let's put better names to categorical variables
levels(hdma_df$applicant_ethnicity_name) <- c("Hispanic/Latino","NA","NA","Not Hispanic/Latino")
levels(hdma_df$applicant_race_name_1) <- c("American Indian/Alaska Native","Asian","Black/African American","NA","Hawaiian","NA","White")
levels(hdma_df$applicant_sex_name) <- c("female","NA","male","NA")

levels(hdma_df$co_applicant_ethnicity_name) <- c("Hispanic/Latino","NA","No co-applicant","NA","Not Hispanic/Latino")
levels(hdma_df$co_applicant_race_name_1) <- c("American Indian/Alaska Native","Asian","Black/African American","NA","Hawaiian","No co-applicant","NA","White")
levels(hdma_df$co_applicant_sex_name) <- c("female","NA","male","No co-applicant","NA")

hdma_df$application_date_indicator <- as.factor(hdma_df$application_date_indicator)
levels(hdma_df$application_date_indicator) <- c("After 1/1/2004","NA")

levels(hdma_df$edit_status_name) <- c("Other","Quality edit failure only")
levels(hdma_df$lien_status_name) <- c("NA","Not secured","Secured by first lien","Secured by subordinate lien")
levels(hdma_df$owner_occupancy_name) <- c("NA","Not owner-occupied","Owner-occupied")
levels(hdma_df$preapproval_name) <- c("NA","Not Requested", "Requested")
levels(hdma_df$property_type_name) <- c("Manufactured","Multifamily","1-4 family")

# NA rate_spread can be regarded as 1, since 1*n = n. No rate means it doesn't affect the product
hdma_df$rate_spread[is.na(hdma_df$rate_spread)] <- 1

# Perform data cleansing. Marking outlier numbers as NAs and then perform imputation to insert those values

hdma_df$loan_amount_000s[hdma_df$loan_amount_000s == 99999.0] <- NA
hdma_df$applicant_income_000s[hdma_df$applicant_income_000s == 9999] <- NA

# Save pre-processed data frame for future uses, so we can skip above given lines
processed_file_name <- "hdma_processed.Rdata"
save(hdma_df, file = processed_file_name)

# Load preprocessed data frame to skip above written lines of code to process raw dataframe

load(processed_file_name)

# This dataframe has columns where there exist at least 1 NA value. We need to perform imputation, but we should be
# careful since data is quite big and calling library mice(hdma_df_just_nas, m =1) is not good. I also tried to run
# this in parallel on 7 cores, still slow. We might consider to impute column by column
hdma_df_just_nas <- hdma_df[,c(1:9,which(colnames(hdma_df) == "census_tract_number"))]
  