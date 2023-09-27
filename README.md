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

