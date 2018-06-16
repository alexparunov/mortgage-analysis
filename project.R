# Final Project. Machine Learning
# Link to data set: https://www.kaggle.com/miker400/washington-state-home-mortgage-hdma2016
# Description of dataset: https://cfpb.github.io/api/hmda/fields.html
# Authors: Alexander Parunov, Mathieu Chiavassa, Wang Yang Ye

if(!require(rstudioapi)) {
  install.packages("rstudioapi")
  require(rstudioapi)
}
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
# in preapproval_name too many NA's
# denial resons can't predict the outcome

columns_to_remove = c("state_abbr","state_name", "respondent_id", "sequence_number", "as_of_year", "agency_name",
                      "msamd_name", "applicant_race_name_2","applicant_race_name_3","applicant_race_name_4","applicant_race_name_5",
                      "co_applicant_race_name_2", "co_applicant_race_name_3","co_applicant_race_name_4","co_applicant_race_name_5",
                      "census_tract_number","application_date_indicator","hoepa_status_name", "preapproval_name","denial_reason_name_1","denial_reason_name_2",
                      "denial_reason_name_3")

hdma_df <- remove_columns(hdma_df, columns_to_remove)

dim(hdma_df) #22 columns removed

summary(hdma_df)

# Let's put better names to categorical variables
levels(hdma_df$applicant_ethnicity_name) <- c("Hispanic/Latino",NA,NA,"Not Hispanic/Latino")
levels(hdma_df$applicant_race_name_1) <- c("American Indian/Alaska Native","Asian","Black/African American",NA,"Hawaiian",NA,"White")
levels(hdma_df$applicant_sex_name) <- c("female",NA,"male",NA)

levels(hdma_df$co_applicant_ethnicity_name) <- c("Hispanic/Latino",NA,"No co-applicant",NA,"Not Hispanic/Latino")
levels(hdma_df$co_applicant_race_name_1) <- c("American Indian/Alaska Native","Asian","Black/African American",NA,"Hawaiian","No co-applicant",NA,"White")
levels(hdma_df$co_applicant_sex_name) <- c("female",NA,"male","No co-applicant",NA)

levels(hdma_df$edit_status_name) <- c("Other","Quality edit failure only")
levels(hdma_df$lien_status_name) <- c(NA,"Not secured","Secured by first lien","Secured by subordinate lien")
levels(hdma_df$owner_occupancy_name) <- c(NA,"Not owner-occupied","Owner-occupied")
levels(hdma_df$preapproval_name) <- c(NA,"Not Requested", "Requested")
levels(hdma_df$property_type_name) <- c("Manufactured","Multifamily","1-4 family")

# NA rate_spread can be regarded as 1, since 1*n = n. No rate means it doesn't affect the product
hdma_df$rate_spread[is.na(hdma_df$rate_spread)] <- 1

# Perform data cleansing. Marking outlier numbers as NAs and then perform imputation to insert those values
hdma_df$loan_amount_000s[hdma_df$loan_amount_000s == 99999.0] <- NA
hdma_df$applicant_income_000s[hdma_df$applicant_income_000s == 9999] <- NA
hdma_df$owner_occupancy_name[is.na(hdma_df$owner_occupancy_name)] <- NA

# Save pre-processed data frame for future uses, so we can skip above given lines
save(hdma_df, file = "hdma_processed.Rdata")

if(!require(rstudioapi)) {
  install.packages("rstudioapi")
  require(rstudioapi)
}

setwd(dirname(getActiveDocumentContext()$path))

# Load data frame which contains no NA's. The imputation was done on Google Virtual Machine since it took around 17hours
# The below given dataframe is ready to be worked on.

load(file = "hdma_cleaned.Rdata")

hdma_df <- hdma_df_c

# Let's do PCA of continuous variables and see if there are any variables correlated so we can remove them or substitude with another
library(FactoMineR)
library(factoextra)
pca.hmda <- PCA(hdma_df[,1:9])
fviz_pca_var(pca.hmda)

# From PCA we observed that applicant_income and loan amount are highly correlated so we can remove them and substitude with another variable

# Pretty much number of years it would take a payer to pay a loan fully assuming that he pays 20% of his gross anual income on loan
hdma_df$payable_period <- hdma_df$loan_amount_000s/(0.20*hdma_df$applicant_income_000s)

require(dplyr)
#swap columns
hdma_df$loan_amount_000s <- hdma_df$payable_period
hdma_df <- rename(hdma_df, "payable_period" = "loan_amount_000s")

# remove 2 columns which we used to calculate payable_period
hdma_df <- subset(hdma_df, select = -c(applicant_income_000s))
hdma_df <- hdma_df[,-ncol(hdma_df)]

summary(hdma_df)

# save file for future usage
save(hdma_df, file = "hdma_processed.Rdata")

# Load file to continue work
load(file = "hdma_processed.Rdata")

# Transform levels of active varible for easy interpretation. Initial classes are following:
# 1) Application approved but not accepted
# 2) Application denied by financial institution
# 3) Application withdrawn by applicant
# 4) File closed for incompleteness
# 5) Loan originated
# 6) Loan purchased by the institution
# 7) Preapproval request approved but not accepted
# 8) Preapproval request denied by financial institution
levels(hdma_df$action_taken_name) <- c("approved","denied","withdrawn","closed","originated","purchased","preapproved","predenied")

# Let's construct dataset which has almost equal amount of all classes, but reduced size
hdma_subset <- hdma_df[hdma_df$action_taken_name == "predenied",]
hdma_subset <- rbind(hdma_df[hdma_df$action_taken_name == "preapproved",], hdma_subset)

# Take 10% of each subsets
prop <- 0.10
approved_subset <- hdma_df[hdma_df$action_taken_name == "approved",]
approved_subset <- approved_subset[sample(nrow(approved_subset), size = prop*nrow(approved_subset)),]
hdma_subset <- rbind(approved_subset, hdma_subset)

closed_subset <- hdma_df[hdma_df$action_taken_name == "closed",]
# Take 10% of this dataset
closed_subset <- closed_subset[sample(nrow(closed_subset), floor(prop*nrow(closed_subset))),]
hdma_subset <- rbind(closed_subset, hdma_subset)

# Gettin random 10k individuals from each classes given below
set.seed(953)
denied_subset <- hdma_df[hdma_df$action_taken_name == "denied",]

denied_subset <- denied_subset[sample(nrow(denied_subset),floor(prop*nrow(denied_subset))),]

withdrawn_subset <- hdma_df[hdma_df$action_taken_name == "withdrawn",]
withdrawn_subset <- withdrawn_subset[sample(nrow(withdrawn_subset),floor(prop*nrow(withdrawn_subset))),]

purchased_subset <- hdma_df[hdma_df$action_taken_name == "purchased",]
purchased_subset <- purchased_subset[sample(nrow(purchased_subset), floor(prop*nrow(purchased_subset))),]

hdma_subset <- rbind(denied_subset, hdma_subset)
hdma_subset <- rbind(withdrawn_subset, hdma_subset)
hdma_subset <- rbind(purchased_subset, hdma_subset)

# rest are originated class individuals to fill 30k subset
originated_subset <- hdma_df[hdma_df$action_taken_name == "originated",]
originated_subset <- originated_subset[sample(nrow(originated_subset), 30000-nrow(hdma_subset)),]

hdma_subset <- rbind(originated_subset, hdma_subset)

# Save subset
save(hdma_subset, file = "hdma_subset.Rdata")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load subset for training/testing
load("hdma_subset.Rdata")
hdma_subset <- subset(hdma_subset, select=-county_name)

hdma_subset[1:8] <- scale(hdma_subset[1:8])

# It will do k modality -> k binary vabirables transformation
library(onehot)
encoder <- onehot(hdma_subset[,-ncol(hdma_subset)])
encoded_m1 <- predict(encoder, hdma_subset[,-ncol(hdma_subset)])

# This matrix has just numerical values, i.e. we used 1-versus-K, i.e. onehot encoding to encode out dataframe
# We can use encoded_m for future purposes (PCA/Clustering/Training models/etc.)
encoded_m <- data.frame(encoded_m1, hdma_subset$action_taken_name)

library(FactoMineR)
pca <- PCA(encoded_m, quali.sup = ncol(encoded_m), graph = FALSE)

Psi <- pca$ind$coord

library(cluster)
dist.matr <- dist(Psi, method = "euclidean")

hc.matr <- hclust(dist.matr, method = "ward.D2")
n <- length(hc.matr$height)
#barplot(hc.matr$height[(n-40):n], ylim = c(0, max(round(hc.matr$height+2))), 
 #       main = "Aggregated distance at each iteration")

# From barplot we see that aggregated distance started increasing rapidly at 200, so
# number of classes we select will be 200. But we know that we have 8 in original one, so let's stick to it.
nclasses <- length(levels(encoded_m$hdma_subset.action_taken_name))
ct <- cutree(hc.matr, k = nclasses)
cdg <- aggregate(as.data.frame(Psi), list(ct), mean)[,-1]

k8 <- kmeans(x = Psi, centers = cdg)

# Plot clusters
plot(Psi[,1], Psi[,2], col = as.factor(k8$cluster), xlab = "Dim1", ylab = "Dim2",
     pch=20, main = "Clusters of Individuals")

n_total <- round(nrow(encoded_m))
n_train <- floor(n_total * 0.8)
n_test <- n_total - n_train

set.seed(739)
train_indexes <- sample(seq(from = 1, to = n_total), size = n_train)

train_set <- encoded_m[train_indexes,]
test_set <- encoded_m[-train_indexes,]


# Random Forest classifier
library(randomForest)





# SVM
library(e1071)
library(rpart)

svm.model <- svm(hdma_subset.action_taken_name ~ ., data = train_set, scale = FALSE, 
                 kernel = "radial", gamma = 0.1, cost = 20)

svm.pred <- predict(svm.model, test_set[,-ncol(encoded_m)])

pred.table <- table(pred = svm.pred, true = test_set[,ncol(encoded_m)])
sum(diag(pred.table))/sum(pred.table)
