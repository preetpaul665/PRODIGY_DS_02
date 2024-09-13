# TITANIC SURVIVAL PREDICTION

Titanic <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\Codsoft\\Titanic-Dataset.csv")
View(Titanic)
attach(Titanic)

# Here we are omitting the variables 'Name' and 'Ticket' and 'Cabin' and 'PassengerId' considering that 
# these four variables doesn't have any association with our objective i.e. "Survived"

data <- Titanic[-c(1,4,9,11)]	# Extracting the useful data
View(data)
attach(data)
age.mean <- mean(Age, na.rm = TRUE)	# mean value of the variable 'Age' except the missing values
age.mean
data$Age <- replace(Age, is.na(Age)==1, age.mean) # Replacing the missing values of 'Age' by its mean value
data$Sex <- as.factor(data$Sex)	# Converting the variables into factors
data$Embarked <- as.factor(Embarked)
data$Pclass <- as.factor(Pclass)

index <- which(Embarked=="")
data <- data[-index,]	# Removing the rows which contains empty cells of 'Embarked'
View(data)

# Creating the prediction model using glm()

model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked, family = "binomial", data = data) 
model
summary(model)
anova(model, test = "Chisq")	# Performing Chi square goodness of fit test
model2 = step(model, direction = "backward")

# Using the library 'ResourceSelection' and performing Hosmer and Lemeshow goodness of fit test

library("ResourceSelection")
hoslem.test(data$Survived,fitted(model2))

#Plotting the Normal Q-Q plot

plot(model2, which = 2)

# Using the library 'hnp'

library("hnp")
hnp(model2)		# Residuals vs. Theoretical Quantiles graph

# Determining the influencial points in the data

influence <- influence.measures(model2)

# using the libraries 'caret' and 'ROCR' to obtain the confusion matrix and ROC curve

library("caret")
library("ROCR")

pre <- ifelse(fitted(model2)>0.5, 1, 0)
confusionMatrix(as.factor(pre), as.factor(data$Survived))	# Obtaining the confusion matrix

pred <- predict(model2,type="response", newdata = data)
pred1 <- prediction(pred, data$Survived)
perform <- performance(pred1, "tpr", "fpr")

# Plotting the ROC curve

plot(perform, print.cutoffs.at = seq(0, 1, 0.1), colorize = TRUE)	 










