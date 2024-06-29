#installing required packages
install.packages("tidyverse")
install.packages("caret")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("GGally")
install.packages("knitr")
install.packages("readr")
install.packages("knitr")
install.packages("dplyr")
install.packages("mlbench")
install.packages("e1071")
install.packages("plotly")
install.packages("ggplot2")
install.packages("class")
install.packages("tidyr")

#Reloading required libraries
library(tidyverse)
library(caret)
library(corrplot)
library(gridExtra)
library(GGally)
library(knitr)
library(readr)
library(knitr)
library(dplyr)
library(mlbench)
library(e1071)
library(plotly)
library(ggplot2)
library(class)
library(tidyr)

#Importing the dataset
cancer_patient_data_ <- read.csv("cancer patient data .csv", stringsAsFactors = TRUE, header = TRUE)

#checking the structure of the dataset
str(cancer_patient_data_)

#Data Cleaning

#checking for null values
anyNA(cancer_patient_data_)

# Filtering rows where any column contains the value 0
zeros <- cancer_patient_data_ %>%
  select(-index) %>%
  filter_all(any_vars(. == 0))

zeros %>%
  summarise(total_rows_with_zeros = sum(count=n())) %>%
  ggplot(aes(x = "", y = total_rows_with_zeros, fill = "Total Rows with Zeros")) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "", y = "Total Rows with Zeros", title = "Total Rows with Zeros in Dataframe") +
  theme_minimal()



#selecting the required predictor variables
cancer_patient_data.subset <- select(cancer_patient_data_  , Level, Age, Gender, Air.Pollution, Genetic.Risk, Balanced.Diet, Obesity, OccuPational.Hazards, Smoking, Chest.Pain, Smoking, Weight.Loss, Shortness.of.Breath, Wheezing, Clubbing.of.Finger.Nails, Alcohol.use, Dust.Allergy,Dry.Cough, Snoring, Swallowing.Difficulty)

#Creating dataframe for correlation plot
cancer_patient_data.corr <- select(cancer_patient_data.subset, -Level)


#plotting correlation plot
corr1=cor(cancer_patient_data.corr, method = "kendall")
corrplot(corr1,method = "circle",
         type = "lower",outline = T,
         addgrid.col = "darkgray",order="hclust",
         mar = c(0,0,0,4),addrect = 4,
         rect.col = "black", rect.lwd = 5,
         cl.pos = "b", tl.col = "red",
         tl.cex =0.5, cl.cex = 0.5)

#checking for highly correlated variables
highlyCorrelated <- findCorrelation(corr1,
                                    cutoff = .6,
                                    verbose = TRUE,
                                    names = TRUE)
highlyCorrelated <- data.frame(highlyCorrelated)
highlyCorrelated


#Removing highly correlated variables
cancer_patient_data.new <- select(cancer_patient_data.subset,-OccuPational.Hazards, -Chest.Pain, -Alcohol.use, -Dust.Allergy, Genetic.Risk)

#checking the structure of the dataset again
str(cancer_patient_data.subset)

#creating dataframe for EDA
cancer_patient_data.plot <- cancer_patient_data.new


#EXPLORATORY DATA ANALYSIS
#Plotting the bar chart with ggplot2
ggplot(data = cancer_patient_data.plot, aes(x = Level, fill = Level)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("red", "blue")) +  # Set colors for bars
  labs(title = "Bar Plot of Patient's Symptoms Level's", x = "Levels", y = "Count") +
  theme_minimal()

# Create a histogram of ages
ggplot(cancer_patient_data.plot, aes(x = Age)) +
  geom_histogram(bins = 10, color = "black", fill = "skyblue", alpha = 0.5) +
  labs(title = "Distribution of Ages of Patients", x = "Age", y = "Frequency") +
  theme_minimal()

ggplot(cancer_patient_data_, aes(x = Air.Pollution, fill = Level)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Air Pollution by Level", x = "Air Pollution", y = "Density") +
  theme_minimal()



ggplot(cancer_patient_data_, aes(x = Age, color = Level, fill = Level)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Ages by Symptom Level", x = "Age", y = "Density") +
  scale_fill_manual(values = c("red", "blue")) +  
  scale_color_manual(values = c("red", "blue")) + 
  theme_minimal()

#plotting the density plot of air pollution by level
ggplot(cancer_patient_data_, aes(x = Air.Pollution, fill = Level)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Air Pollution by Level", x = "Air Pollution", y = "Density") +
  theme_minimal()
#plotting box plot

ggplot(data = cancer_patient_data.plot, mapping = aes(x = Level, y = Dry.Cough, fill = "blue")) +
  geom_boxplot(show.legend = FALSE, outlier.shape = 1, outlier.color = "red") +
  scale_fill_manual(values = "blue") +
  coord_flip()

 #converting the level column to numeric
cancer_patient_data.new <- cancer_patient_data.new %>%
  mutate(
    Level = as.numeric(Level)
  )


#normalizing dataset
head(cancer_patient_data.new)

normalize <- function(x) { + return ((x - min(x)) / (max(x) - min(x))) }
cancer_patient_data.new.n <- as.data.frame(lapply(cancer_patient_data.subset[,2:10], normalize))

#normalized dataset
head(cancer_patient_data.new.n)

#Building data model
set.seed(123)
dat.d <- sample(1:nrow(cancer_patient_data.new.n),size=nrow(cancer_patient_data.new.n)*0.7,replace = FALSE)
train.cancer<- cancer_patient_data.new[dat.d,]
test.cancer <- cancer_patient_data.new[-dat.d,]

train.cancer_labels <- cancer_patient_data.new[dat.d,1]
test.cancer_labels <- cancer_patient_data.new[-dat.d,1]

NROW(train.cancer_labels)




# Using k=26 for KNN
knn.26 <- knn(train = train.cancer, test = test.cancer, cl = train.cancer_labels, k = 26)
# Using k=27 for KNN
knn.27 <- knn(train = train.cancer, test = test.cancer, cl = train.cancer_labels, k = 27)

#Calculate the proportion of correct classification for k = 26
ACC.26 <- 100 * sum(test.cancer_labels == knn.26)/NROW(test.cancer_labels)

#Calculate the proportion of correct classification for k = 27
ACC.27 <- 100 * sum(test.cancer_labels == knn.27)/NROW(test.cancer_labels)

# Check prediction against actual value in tabular form for k=26
table(knn.26 ,test.cancer_labels)
knn.26

# Check prediction against actual value in tabular form for k=27
table(knn.27 ,test.cancer_labels)
knn.27

#confusion matrix to calculate accuracy
confusionMatrix(table(knn.27  ,test.cancer_labels))
confusionMatrix(table(knn.26  ,test.cancer_labels))


#model optimization

k.optm <- 1
K_values<-for (i in 1:30) {
  knn.mod <- knn(train = train.cancer,
                 test = test.cancer,
                 cl = train.cancer_labels,
                 k = i)
  k.optm[i] <- 100 * sum(test.cancer_labels == knn.mod) / NROW(test.cancer_labels)
  cat("k=", i, ": ", k.optm[i], "\n")
}

plot(k.optm, type = "b", xlab = "K-Value", ylab = "Accuracy level")


#this computes all the computational overhead prior to training
trctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5)

cancer_patient_data.caret <- data.frame(cancer_patient_data.subset)
cancer_patient_data.caret$Level <- as.factor(cancer_patient_data.caret$Level)
str(cancer_patient_data.caret)

#create a data partition to split data into 70% for training and 30% for testing
set.seed(77)
intrain2 <- createDataPartition(y=cancer_patient_data.caret$Level, p=0.7, list = FALSE)

#split the data
training.cancer <- cancer_patient_data.caret[intrain2, ]
testing.cancer <- cancer_patient_data.caret[-intrain2, ]

#train the KNN model
set.seed(7)
fit.knn <- train(Level ~., data=training.cancer,
                 method="knn",
                 preProcess = c("center", "scale"),
                 trControl=trainControl())

fit.knn

test_knn<-predict(fit.knn, newdata = testing.cancer)
test_knn
confusionMatrix(table(test_knn,
                      testing.cancer$Level))



grid.knn <- expand.grid(k = 1:30)
fit.knn_Grid <- train(Level ~., data=training.cancer,
                      method="knn",
                      preProcess = c("center", "scale"),
                      trControl = trctrl,
                      tuneGrid = grid.knn,tuneLength = 10)
fit.knn_Grid

#plot the SVM
plot(fit.knn_Grid)

#To test the accuracy of the model with the testing data.
test_knn_Grid <-predict(fit.knn_Grid, newdata = testing.cancer)
test_knn_Grid


confusionMatrix(table(test_knn_Grid,testing.cancer$Level))

caret_accuracy <- sum(test_knn_Grid == testing.cancer$Level) / nrow(test.cancer)
class_accuracy <- sum(knn.27 == test.cancer_labels) / nrow(test.cancer)

# Print accuracy results
print(paste("Accuracy (caret model):", caret_accuracy))
print(paste("Accuracy (class model):", class_accuracy))



