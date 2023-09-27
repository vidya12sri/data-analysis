# data-analysis

To predict the 199 unknown species with the known 89 species using Logistic regression (GLM) in R

# reading the given data and dividing the given data into training which is known data and testing which is need to be predicted data
trainingdata1 <- read.csv("C:\\Users\\svcha\\Desktop\\Desktop\\multi.csv")
trainingdata1 <- na.omit(trainingdata1)
trainingdata2 <- read.csv("C:\\Users\\svcha\\Desktop\\Desktop\\subterraneus.csv")
trainingdata2 <- na.omit(trainingdata2)
testingdata1 <- read.csv("C:\\Users\\svcha\\Downloads\\unknown1.csv")
colnames(trainingdata2) <- colnames(trainingdata1)
trainingdata <- rbind(trainingdata1, trainingdata2)
# renaming the columns of the data to get the process easy
colnames(trainingdata)[1] <- "I"
colnames(trainingdata)[2] <- "ID"
colnames(trainingdata)[3] <- "L"
colnames(trainingdata)[4] <- "H"
colnames(trainingdata)[5] <- "W"

colnames(testingdata1)[1] <- "I"
colnames(testingdata1)[2] <- "ID"
colnames(testingdata1)[3] <- "L"
colnames(testingdata1)[4] <- "H"
colnames(testingdata1)[5] <- "W"
# Finding outliers
boxplot(trainingdata[,c('L', "H","W")],
horizontal=TRUE,
 names = c("Length", "Height","Width"))
 # function to remove outliers in the data.
outliers <- function(x) {

  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1

 upper_limit = Q3 + (iqr*1.5)
 lower_limit = Q1 - (iqr*1.5)

 x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}
# removing outliers of data
trainingdata <- remove_outliers(trainingdata, c('L', 'H', 'W'))

# boxplot after removing the outliers
boxplot(trainingdata[,c('L', "H","W")],
horizontal=TRUE,
 names = c("Length", "Height","Width"))
 # Making one variable binary for prediction
trainingdata$ID_binary = ifelse(trainingdata$ID== "subterraneus",1,0)
# prediction using glm model
model <- glm(ID_binary ~ L + H + W ,data = trainingdata, family= 'binomial')
summary(model)
# testing the model with trainingdata
trainingdata$pred_values <-predict(model, type = "response")
trainingdata$final_pred_values <- ifelse(trainingdata$pred_values < 0.5,
"Multiplex", "Subterraneus")
# Creating a confusion matrix
model_pred <-predict(model, type = "response")
pred_confusion_matrix <- ifelse(model_pred <= 0.55, "Multiplex",
"Subterraneus")
predicted_values <- data.frame(Predicted = trainingdata$ID, true= pred_confusion_matrix )
confusion_matrix<-table(predicted_values)
confusion_matrix
# Checking the accuracy of the model
model_accuracy<-
sum(confusion_matrix[1,2]+confusion_matrix[2,1])/sum(confusion_matrix)
model_accuracy <-1-model_accuracy
model_accuracy
# Predicting the unknown species using the original model
testingdata1$pred_values <- predict(model, testingdata1 ,
type="response")
testingdata1$predicted_species <- ifelse(testingdata1$pred_values < 0.5,
"Multiplex", "Subterraneus")
head(testingdata1)
table(testingdata1["predicted_species"])
