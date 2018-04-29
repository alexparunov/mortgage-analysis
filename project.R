# Final Project. Machine Learning
# Link to data set: https://www.kaggle.com/miker400/washington-state-home-mortgage-hdma2016
# Authors: Alexander Parunov, Mathieu Chiavassa, Wang Yang

if(!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

# Read data frame
hdma_df <- read.csv("Washington_State_HDMA-2016.csv")
dim(hdma_df)

#We don't need state abbreviation, it's always Washington (WA)
colnumber <- which(colnames(hdma_df) == "state_abbr")
if(colnumber > 0) hdma_df <- hdma_df[,-colnumber]
colnumber <- which(colnames(hdma_df) == "state_name")
if(colnumber > 0) hdma_df <- hdma_df[,-colnumber]
colnumber <- which(colnames(hdma_df) == "respondent_id")

# We don't care about ids and sequence number and year (2016)
if(colnumber > 0) hdma_df <- hdma_df[,-colnumber]
colnumber <- which(colnames(hdma_df) == "sequence_number")
if(colnumber > 0) hdma_df <- hdma_df[,-colnumber]
colnumber <- which(colnames(hdma_df) == "as_of_year")
if(colnumber > 0) hdma_df <- hdma_df[,-colnumber]

dim(hdma_df)

summary(hdma_df)
# NA rate_spread can be regarded as 1, since 1*n = n. No rate means it doesn't affect the product
hdma_df$rate_spread[is.na(hdma_df$rate_spread)] <- 1

# Perform data cleansing. Marking outlier numbers as NAs and then perform imputation to insert those values

hdma_df$loan_amount_000s[hdma_df$loan_amount_000s == 99999.0] <- NA
hdma_df$applicant_income_000s[hdma_df$applicant_income_000s == 9999] <- NA

# This dataframe has columns where there exist at least 1 NA value. We need to perform imputation, but we should be
# careful since data is quite big and calling library mice(hdma_df_just_nas, m =1) is not good. I also tried to run
# this in parallel on 7 cores, still slow. We might consider to impute column by column
hdma_df_just_nas <- hdma_df[,c(1:9,which(colnames(hdma_df) == "census_tract_number"))]
  