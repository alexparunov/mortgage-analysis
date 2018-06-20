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

#------ Preprocessing part ------

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

# Load data frame which contains no NA's. The imputation was done on Google Virtual Machine since it took around 17hours
# The below given dataframe is ready to be worked on.

load(file = "hdma_cleaned.Rdata")

hdma_df <- hdma_df_c

# Find outliers for loan amount column
loan_outliers <- boxplot.stats(hdma_df$loan_amount_000s)$out
# We select rows where loan amount is less than minimal value of loan outliers
hdma_df <- hdma_df[hdma_df$loan_amount_000s < min(loan_outliers),]

#We are doing same with income outliers
income_outliers <- boxplot.stats(hdma_df$applicant_income_000s)$out
hdma_df <- hdma_df[hdma_df$applicant_income_000s < min(income_outliers),]

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

# Let's construct dataset which has almost same proportion as the original data, but with reduced size (30000)
# This show us the proportion of each class of response variable
t <- table(hdma_df$action_taken_name)/nrow(hdma_df)
t_n <- as.numeric(t)

hdma_subset <- hdma_df[hdma_df$action_taken_name == "predenied",]
hdma_subset <- rbind(hdma_df[hdma_df$action_taken_name == "preapproved",], hdma_subset)


approved_subset <- hdma_df[hdma_df$action_taken_name == "approved",]
set.seed(953)
approved_subset <- approved_subset[sample(nrow(approved_subset), floor(t_n[1]*30000)),]
hdma_subset <- rbind(approved_subset, hdma_subset)

closed_subset <- hdma_df[hdma_df$action_taken_name == "closed",]
set.seed(953)
closed_subset <- closed_subset[sample(nrow(closed_subset), floor(t_n[4]*30000)),]
hdma_subset <- rbind(closed_subset, hdma_subset)

denied_subset <- hdma_df[hdma_df$action_taken_name == "denied",]
set.seed(953)
denied_subset <- denied_subset[sample(nrow(denied_subset), floor(t_n[2]*30000)),]

withdrawn_subset <- hdma_df[hdma_df$action_taken_name == "withdrawn",]
set.seed(953)
withdrawn_subset <- withdrawn_subset[sample(nrow(withdrawn_subset), floor(t_n[3]*30000)),]

purchased_subset <- hdma_df[hdma_df$action_taken_name == "purchased",]
set.seed(953)
purchased_subset <- purchased_subset[sample(nrow(purchased_subset), floor(t_n[6]*30000)),]

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

library(e1071)
library(rpart)

# Load subset for training/testing
load("hdma_subset.Rdata")
hdma_subset <- subset(hdma_subset, select=-c(county_name, rate_spread, co_applicant_sex_name,
                                             co_applicant_race_name_1, co_applicant_ethnicity_name))

loan_range <- seq(from = 0, to = max(hdma_subset$loan_amount_000s)+50-max(hdma_subset$loan_amount_000s)%%50, by = 50)
hdma_subset$loan_amount_range <- cut(hdma_subset$loan_amount_000s, loan_range)

income_range <- seq(from = 0, to = max(hdma_subset$applicant_income_000s)+50-max(hdma_subset$applicant_income_000s)%%50, by = 50)
hdma_subset$income_range <- cut(hdma_subset$applicant_income_000s, income_range)

# Transform continous variables
# Some variables are skewed so we use log-tranformation to correct it
par(mfrow=c(2,4))
for(i in 1:8){
  plot(density(hdma_subset[, i]), main=colnames(hdma_subset)[i])
  print(skewness(hdma_subset[, i]))
}

hdma_subset$minority_population <- log(hdma_subset$minority_population)
for(i in 1:8) {
  hdma_subset[,i] <- (hdma_subset[,i] - min(hdma_subset[,i]))/(max(hdma_subset[,i]) - min(hdma_subset[,i]))
}

# Restore plot style
par(mfrow=c(1,1))


# ------ Detecting possible outliers ------
library(chemometrics)
subset_outliers <- Moutlier(hdma_subset[, 1:8],quantile = 0.975, plot = TRUE)
max(subset_outliers$rd)
min(subset_outliers$rd)
# From the robust mahalanobis distances we haven't detected more outliers after in the subset data


#save(hdma_subset, file = "hdma_subset_norm.Rdata")

# This data frame "hdma_subset" can be used for classifications algorithms
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file = "hdma_subset_norm.Rdata")

# The column index of response/class variable
resp_variable <- which(colnames(hdma_subset) == "action_taken_name")

# It will do k modality -> k binary vabirables transformation
library(onehot)
encoder <- onehot(hdma_subset[,-resp_variable], max_levels = 15)
encoded_m1 <- predict(encoder, hdma_subset[,-resp_variable])

# This matrix has just numerical values, i.e. we used 1-versus-K, i.e. onehot encoding to encode out dataframe
# We can use encoded_m for future purposes (PCA/Clustering/Training models/etc.)
encoded_m <- data.frame(encoded_m1, hdma_subset$action_taken_name)

# We can continue from here
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file = "encoded_m.Rdata")
library(e1071)
library(rpart)

# Split data into 80/20 train/test data and do classification.
n_total <- round(nrow(encoded_m))
n_train <- floor(n_total * 0.8)
n_test <- n_total - n_train

set.seed(739)
train_indexes <- sample(seq(from = 1, to = n_total), size = n_train)

train_set <- encoded_m[train_indexes,]
test_set <- encoded_m[-train_indexes,]


# ------ PCA + HC + KMEANS ------
# Free memory
gc()
load(file = "encoded_m.Rdata")
pca = PCA(encoded_m, quali.sup = c(which(colnames(encoded_m) == "hdma_subset.action_taken_name")), ncp=ncol(encoded_m))
#pca = PCA(wine_quality, quali.sup = c(12), ncp=12)
pca$eig
plot(pca$eig[,1])
lines(pca$eig[,1])
# Sum up to 80% of total inertia
nd = 34
Psi <- pca$ind$coord[,c(1:nd)]
d <- dist(Psi, method = "euclidean")
hc <- hclust(d, method = "ward.D2")

# Determining the greatest height jump and number of clusters
height.diff <- c(hc$height,0) - c(0,hc$height)
before.maxi <- which.max(height.diff[c(1:(length(height.diff)-2))]) - 1
nc = length(hc$height) - before.maxi + 1
height.line <- (hc$height[before.maxi + 1] + hc$height[before.maxi ])/2

# Print the cluster dendrogram and last 100 jumps
par(mfrow = c(1, 1))
plot(hc)
abline(a=height.line,b=0,col="red",)
barplot(hc$height[(length(hc$height) - 100):length(hc$height)], xlab = "Height histogram")
abline(a=height.line,b=0,col="red",)

c3 <- cutree(hc,nc)
gc()
# computation of the centroids of clusters
cdg <- aggregate(Psi,list(c3),mean)[,2:(nd+1)]
Bss <- sum(rowSums(cdg^2)*as.numeric(table(c3)))
Tss <- sum(rowSums(Psi^2))
(optimization.criterion.before <- 100*Bss/Tss)

iden <- rownames(Psi)
par(mfrow=c(1,1))
plot(Psi[,1],Psi[,2],type="p",main="Clustering with HC", col=c3, xlab = "Dim. 1", ylab = "Dim. 2")
#text(Psi[,1],Psi[,2],col=c3,labels=iden,cex = 0.6)
abline(h=0,v=0,col="gray")
legend("topleft",c("c1","c2","c3","c4","c5","c6","c7"),pch=20,col=c(1:7))
points(cdg, pc=21, col = "gold", bg="gold", cex = 1.5)
text(cdg - rep(0.3,length(cdg)),labels=c("G1","G2","G3","G4","G5","G6","G7"),col="red3", cex=1.2)

library(fpc)
# K-means computation
k_def <- kmeans(Psi,centers=cdg, iter.max = 10)
Bss <- sum(rowSums(k_def$centers^2)*as.numeric(table(k_def$cluster)))
Wss <- sum(k_def$withinss)
(optimization.criterion.kmean <- 100*(Bss/(Bss+Wss)))

# printing final clusters
par(mfrow=c(1,1))
plot(Psi,type="p",col=k_def$cluster+15, main="K-means Clustering")
#text(Psi,labels=iden,col=k_def$cluster)
abline(h=0,v=0,col="gray")
legend("bottomright",c("C1","C2","C3","C4","C5", "C6", "C7"),pch=20,col=c(16:22))
points(k_def$centers, pc=21, col = "gold", bg="gold", cex = 1.5)
text(k_def$centers - rep(0.45,length(k_def$centers)),labels=c("G1","G2","G3","G4","G5","G6","G7"),col="red3", cex=1.2)

# Quality of the Kmeans clustering
Bss = k_def$betweenss
Wss = k_def$tot.withinss
(optimization.criterion.after <- 100*Bss/(Bss+Wss))


# ------ Classification models part ------

# Naive Bayes classifier
model.nb <- naiveBayes(action_taken_name ~ ., data = hdma_subset[train_indexes,])
# Compute now the test error
pred <- predict(model.nb, hdma_subset[-train_indexes, -which(colnames(hdma_subset) == "action_taken_name")])

# Form and display confusion matrix & overall error
(ct <- table(Truth=hdma_subset[-train_indexes,]$action_taken_name, Preds=pred) )

# Accuracy and error rate
(sum(diag(ct))/sum(ct))
1 - sum(diag(ct))/sum(ct)

# Random Forest classifier
library(randomForest)

set.seed(788)
model.rf <- randomForest(action_taken_name~ ., data=hdma_subset[train_indexes,], ntree=100, proximity=FALSE)

pred.rf <- predict (model.rf, hdma_subset[-train_indexes,], type="class")
ct <- table(Truth=hdma_subset[-train_indexes,]$action_taken_name, Pred=pred.rf)
(sum(diag(ct))/sum(ct))

# Stratify the sampling in the boostrap resamples, upsample the less represented class
set.seed(788)
model.rf2 <- randomForest(action_taken_name ~ ., data = hdma_subset[train_indexes,], ntree=100, 
                          proximity=FALSE, sampsize=c(approved=550, denied=3000, withdrawn=3000, closed=900, originated= 8000, purchased=2200, preapproved=10, predenied=25), strata=hdma_subset[train_indexes,]$action_taken_name)

pred.rf2 <- predict (model.rf2, hdma_subset[-train_indexes,], type="class")
ct <- table(Truth=hdma_subset[-train_indexes,]$action_taken_name, Pred=pred.rf2)
(sum(diag(ct))/sum(ct))
# It seems like that upsample method is not helping

## Now we can try to optimize the number of trees, guided by OOB:
(ntrees <- round(10^seq(1,3.7,by=0.2)))
# prepare the structure to store the partial results
rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0

ii <- 1
for (nt in ntrees)
{ 
  print(nt)
  model.rf <- randomForest(action_taken_name~ ., data=hdma_subset[train_indexes,], ntree=nt, proximity=FALSE)
  
  # get the OOB
  rf.results[ii,"OOB"] <- model.rf$err.rate[nt,1]
  # Free memory
  gc()
  ii <- ii+1
}
#save(rf.results, file = "rf_result.Rdata")
load(file = "rf_result.Rdata")

rf.results
(ntrees.best <- 398)

model.rf3 <- randomForest(action_taken_name~ ., data=hdma_subset[train_indexes,], ntree=ntree.best, proximity=FALSE)
pred.rf3 <- predict (model.rf3, hdma_subset[-train_indexes, -which(colnames(hdma_subset) == "action_taken_name")], type="class")

(ct <- table(Truth=hdma_subset[-train_indexes,]$action_taken_name, Pred=pred.rf3))
#model.rf
# Variable's importance, if we eliminate the least important variablem, the error rate increases.
varImpPlot(pred.rf3)

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)


# Support Vector Classifier
gammas <- 2^seq(-3,4)
svm.models.gammas <- list()

# Following code is used to tune parameters (gammas and costs) of SVC
# Train for various gammas for RBF kernel SVC, keepig cost = 10 and gammas varying.
i <- 1
for(g in gammas) {
  svm.model <- svm(hdma_subset.action_taken_name ~ ., data = train_set, scale = FALSE, 
                   kernel = "radial", gamma = g, cost = 10)
  svm.models.gammas[[i]] <- svm.model
  i <- i+1
}

#save(svm.models.gammas, file = "svm_models_gammas.Rdata")

svm.predictions.gammas <- list()
i <- 1
for(svm.model in svm.models.gammas) {
  svm.pred <- predict(svm.model, test_set[,-ncol(encoded_m)])
  svm.predictions.gammas[[i]] <- svm.pred
  i <- i+1
}

i <- 1
errors.gammas <- vector(length = length(svm.predictions.gammas))
for(i in 1:length(svm.predictions.gammas)) {
  pred.table <- table(pred = svm.predictions.gammas[[i]], true = test_set[,ncol(encoded_m)])
  errors.gammas[i] <- 1 - sum(diag(pred.table))/sum(pred.table)
}

costs <- 10^seq(0,3, by = 0.5)
svm.models.costs <- list()

# Train for various costs for RBF kernel SVC, keeping gamma = 0.05 and costs varying.
i <- 1
for(c in costs) {
  svm.model <- svm(hdma_subset.action_taken_name ~ ., data = train_set, scale = FALSE, 
                   kernel = "radial", gamma = 0.05, cost = c)
  svm.models.costs[[i]] <- svm.model
  i <- i+1
}

save(svm.models.costs, file = "svm_models_costs.Rdata")

svm.predictions.costs <- list()
i <- 1
for(svm.model in svm.models.costs) {
  svm.pred <- predict(svm.model, test_set[,-ncol(encoded_m)])
  svm.predictions.costs[[i]] <- svm.pred
  i <- i+1
}

i <- 1
errors.costs <- vector(length = length(svm.predictions.costs))
for(i in 1:length(svm.predictions.costs)) {
  pred.table <- table(pred = svm.predictions.costs[[i]], true = test_set[,ncol(encoded_m)])
  errors.costs[i] <- 1 - sum(diag(pred.table))/sum(pred.table)
}

# After training and testing with various costs and gammas, error is suboptimal with gamma = 0.05 and cost = 10
# However increase of cost doesn't affect error as much as increase of gamma. Keeping gamma 0.05 is good choice,
# because we can't make it too small, it will take more time to train.

svm.optimal <- svm(hdma_subset.action_taken_name ~ ., data = train_set, scale = FALSE, kernel = "radial", gamma = 0.05, cost = 10)
svm.optimal.preds <- predict(svm.optimal, test_set[,-ncol(encoded_m)])
svm.pred.table <- table(pred = svm.optimal.preds, true = test_set[,ncol(encoded_m)])

# Best accuracy so ar is 67.2% :)
sum(diag(svm.pred.table))/sum(svm.pred.table)


# Cross-validation part
library(TunePareto)
library(randomForest)
library(e1071)

# Function which does average CROSS-VALIDATION and returns results of CROSS-VALIDATION
model.CV <- function (k, method) {
  if(method == "SVM") {
    CV.folds <- generateCVRuns(encoded_m[train_indexes,]$hdma_subset.action_taken_name, ntimes = 1, nfold = k, 
                               stratified = TRUE)
    train_data <- encoded_m[train_indexes,]
  } else {
    CV.folds <- generateCVRuns(hdma_subset[train_indexes,]$action_taken_name, ntimes=1, nfold=k, stratified=TRUE)
    train_data <- hdma_subset[train_indexes,]
  }
  
  cv.results <- matrix (rep(0,4*k),nrow=k)
  colnames (cv.results) <- c("k","fold","TR error","VA error")
  
  all.cv.results <- list()
  cv.results[,"TR error"] <- 0
  cv.results[,"VA error"] <- 0
  
  for (j in 1:k) {
    print(j)
    # get VA data
    va <- unlist(CV.folds[[1]][[j]])
    #print(length(va))
    # train on TR data
    if (method == "SVM") {
      my.model.TR <- svm(hdma_subset.action_taken_name ~ ., data = train_data[-va,], 
                      scale = FALSE, kernel = "radial", gamma = 0.05, cost = 10, CV=FALSE) 
    }
    else if (method == "RandomForest") {
      my.model.TR <- randomForest(action_taken_name ~ ., data=train_data[-va,], ntree=398, proximity=FALSE) 
    }
    else if (method == "NaiveBayes") { 
      my.model.TR <-  naiveBayes(action_taken_name ~ ., data = train_data[-va,]) 
    }
    else stop("Wrong method")
    
    # predict TR data
    if (method == "SVM") {
      pred.va <- predict(my.model.TR, train_data[-va, -which(colnames(train_data) == "hdma_subset.action_taken_name")], type="class")
      tab <- table(Truth = train_data[-va,]$hdma_subset.action_taken_name, Pred = pred.va)
    } 
    else if (method == "RandomForest") {
      pred.va <- my.model.TR$predicted
      #print(length(pred.va))
      #print(length(train_data[-va,]$action_taken_name))
      tab <- table(Truth = train_data[-va,]$action_taken_name, Pred = pred.va)
    }
    else {
      pred.va <- predict(my.model.TR, train_data[-va, -which(colnames(train_data) == "action_taken_name")], type="class")
      tab <- table(train_data[-va,]$action_taken_name, pred.va)
    }
    
    cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    # predict VA data
    if(method == "SVM") {
      td <- train_data[va,]$hdma_subset.action_taken_name
      pred.va <- predict (my.model.TR, train_data[va, -which(colnames(train_data) == "hdma_subset.action_taken_name")], type="class")
      tab <- table(Truth = train_data[va,]$hdma_subset.action_taken_name, Pred = pred.va)
    } else {
      pred.va <- predict (my.model.TR, train_data[va, -which(colnames(train_data) == "action_taken_name")], type="class")
      tab <- table(Truth = train_data[va,]$action_taken_name, Pred = pred.va)
    }
    
    cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    all.cv.results[[j]] <- cv.results
  }
    
  return(all.cv.results)
  
}


# 10 fold CROSS-VALIDATION for NaiveBayes
k <- 10
nb.cv <- model.CV(k, method = "NaiveBayes")
nb.cv.df <- as.data.frame(nb.cv[10]) 
# plot result for mean VA error for each fold [1-10]
nb.mean.cv <- vector(mode = "numeric",length = k)
for(j in 1:k){
  nb.mean.cv[j] <- mean(nb.cv[[j]][,"VA error"])
}
plot(nb.mean.cv,type="b",xlab="Value of k",ylab="Average CV error", xaxt="n")
axis(1, at=1:20,labels=1:20, las=2)
grid()

# 95% CI for CV error
pe.hat <- 0.37
dev <- sqrt(pe.hat*(1-pe.hat)/floor(30000*0.8))*1.967

sprintf("(%f,%f)", pe.hat-dev,pe.hat+dev)

# 10 fold CROSS-VALIDATION for Random Forest
k <- 10
rf.cv <- model.CV(k, method = "RandomForest")
rf.cv.df <- as.data.frame(rf.cv[10])
# plot result for mean VA error for each fold [1-10]
rf.mean.cv <- vector(mode = "numeric",length = k)
for(j in 1:k){
  rf.mean.cv[j] <- mean(rf.cv[[j]][,"VA error"])
}
plot(rf.mean.cv,type="b",xlab="Value of k",ylab="Average CV error", xaxt="n")
axis(1, at=1:20,labels=1:20, las=2)
grid()

# 95% CI for CV error
pe.hat <- 0.33
dev <- sqrt(pe.hat*(1-pe.hat)/floor(30000*0.8))*1.967

sprintf("(%f,%f)", pe.hat-dev,pe.hat+dev)


# 10 fold CROSS-VALIDATION for SVM
k <- 10
svm.cv <- model.CV(k, method = "SVM")
svm.cv.df <- as.data.frame(svm.cv[10])
# plot result for mean VA error for each fold [1-10]
svm.mean.cv <- vector(mode = "numeric",length = k)
for(j in 1:k){
  svm.mean.cv[j] <- mean(rb.cv[[j]][,"VA error"])
}
plot(svm.mean.cv,type="b",xlab="Value of k",ylab="Average CV error", xaxt="n")
axis(1, at=1:20,labels=1:20, las=2)
grid()

# 95% CI for CV error
pe.hat <- 0.32
dev <- sqrt(pe.hat*(1-pe.hat)/floor(30000*0.8))*1.967

sprintf("(%f,%f)", pe.hat-dev,pe.hat+dev)

# Load datasets to train/test final models
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file = "hdma_subset_norm.Rdata.Rdata")
load(file = "encoded_m.Rdata")
library(e1071)
library(rpart)

# Split data into 80/20 train/test data and do classification.
n_total <- round(nrow(encoded_m))
n_train <- floor(n_total * 0.8)
n_test <- n_total - n_train

set.seed(739)
train_indexes <- sample(seq(from = 1, to = n_total), size = n_train)

train_set <- encoded_m[train_indexes,]
test_set <- encoded_m[-train_indexes,]

# Random Forest Classifier
set.seed(788)
model.rf <- randomForest(action_taken_name ~ ., data = hdma_subset[train_indexes,], ntree=400, proximity=FALSE)

pred.rf <- predict (model.rf, hdma_subset[-train_indexes,], type="class")
ct <- table(Truth=hdma_subset[-train_indexes,]$action_taken_name, Pred=pred.rf)

# Best accuracy is 66%
(sum(diag(ct))/sum(ct))

# SV Classifier

svm.optimal <- svm(hdma_subset.action_taken_name ~ ., data = train_set, scale = FALSE, 
                   kernel = "radial", gamma = 0.05, cost = 10)
svm.optimal.preds <- predict(svm.optimal, test_set[,-ncol(encoded_m)])
svm.pred.table <- table(pred = svm.optimal.preds, true = test_set[,ncol(encoded_m)])

# Best accuracy is 67%
(sum(diag(svm.pred.table))/sum(svm.pred.table))


# Train the big data set using Random Forest algorithm
load(file = "hdma_processed.Rdata")
t_hdma <- hdma_df
t_hdma <- subset(t_hdma, select=-c(county_name, rate_spread, co_applicant_sex_name, co_applicant_race_name_1, co_applicant_ethnicity_name))

loan_range <- seq(from = 0, to = max(t_hdma$loan_amount_000s)+50-max(t_hdma$loan_amount_000s)%%50, by = 50)
t_hdma$loan_amount_range <- cut(t_hdma$loan_amount_000s, loan_range)

income_range <- seq(from = 0, to = max(t_hdma$applicant_income_000s)+50-max(t_hdma$applicant_income_000s)%%50, by = 50)
t_hdma$income_range <- cut(t_hdma$applicant_income_000s, income_range)

# Split data into 80/20 train/test data and do classification.
n_total <- round(nrow(t_hdma))
n_train <- floor(n_total * 0.8)
n_test <- n_total - n_train

set.seed(739)
train_indexes <- sample(seq(from = 1, to = n_total), size = n_train)

train_set <- t_hdma[train_indexes, ]
test_set <- t_hdma[-train_indexes, ]

library(randomForest)
model.big.rf <- randomForest(action_taken_name ~ ., data = train_set, ntree = 398, proximit = FALSE)

pred.big.rf <- predict (model.big.rf, test_set, type="class")
ct <- table(Truth=test_set$action_taken_name, Pred=pred.big.rf)

# Best accuracy is 68% !!!
(sum(diag(ct))/sum(ct))
