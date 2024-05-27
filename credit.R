credit_card <- read.csv("D:/R files/creditcard.csv")
str(credit_card)
View(credit_card)
credit_card$Class <- factor(credit_card$Class, levels = c(0,1))
summary(credit_card)
sum(is.na(credit_card))


table(credit_card$Class)

prop.table(table(credit_card$Class))

labels <- c("legit", "fraud")
labels <- paste(labels, round(100*prop.table(table(credit_card$Class)), 2))
labels <- paste0(labels, "%")
pie(table(credit_card$Class), labels, col = c("orange", "red"),
    main = "Pie chart of credit card transaction")

predictions <- rep.int(0, nrow(credit_card))                
predictions <- factor(predictions, levels = c(0,1))
predictions
#install.packages("caret")
library(caret)
confusionMatrix(data = predictions, reference = credit_card$Class)
#install.packages("dplyr")
library(dplyr)
set.seed(1)
credit_card <- credit_card %>% sample_frac(0.1)
table(credit_card$Class)
library(ggplot2)
ggplot(data = credit_card, aes(x = V1, y = V2, col = Class)) + 
  geom_point() +
  theme_bw() +
  scale_color_manual(values = c('dodgerblue2', 'red'))
library(caTools)
set.seed(123)
data_sample = sample.split(credit_card$Class, SplitRatio = 0.80)
train_data = subset(credit_card, data_sample == TRUE)
test_data = subset(credit_card, data_sample == FALSE)
dim(train_data)
dim(test_data)
table(train_data$Class)
n_legit <- 22750
new_frac_legit <- 0.50
new_n_total <- n_legit/new_frac_legit
install.packages('ROSE')
library(ROSE)
oversampling_result <- ovun.sample(Class ~ ., 
                                   data = train_data,
                                   method = "over",
                                   N = new_n_total,
                                   seed = 2019)
oversampling_credit <- oversampling_result$data
table(oversampling_credit$Class)
library(ggplot2)
ggplot(data = oversampling_credit, aes(x = V1, y = V2, col = Class)) +
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw() + 
  scale_color_manual(values = c("dodgerblue2", 'red'))
table(train_data$Class)
n_fraud <- 35
new_frac_fraud <- 0.50
new_n_total1 <- n_fraud/new_frac_fraud
undersampling_result <- ovun.sample(Class ~ ., 
                                   data = train_data,
                                   method = "under",
                                   N = new_n_total1,
                                   seed = 2019)
undersampling_credit <- undersampling_result$data
table(undersampling_credit$Class)
ggplot(data = undersampling_credit, aes(x = V1, y = V2, col = Class)) +
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw() + 
  scale_color_manual(values = c("dodgerblue2", 'red'))
n_new <- nrow(train_data)
fraction_fraud_new <- 0.50
sampling_result <- ovun.sample(Class ~ ., 
                                    data = train_data,
                                    method = "both",
                                    N = n_new,
                               p = fraction_fraud_new,
                                    seed = 2019)
sampling_credit <- sampling_result$data
table(sampling_credit$Class)
ggplot(data = sampling_credit, aes(x = V1, y = V2, col = Class)) +
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw() + 
  scale_color_manual(values = c("dodgerblue2", 'red'))
install.packages("smotefamily")
library(smotefamily)
n0 <- 22750
n1 <- 35
r0 <- 0.6
ntimes <- ((1 - r0) / r0) * (n0 / n1) - 1
smote_output <- SMOTE(X = train_data[ , -c(1, 31)],
                      target = train_data$Class,
                      K = 5,
                      dup_size = ntimes)
credit_smote <- smote_output$data
colnames(credit_smote)[30] <- "Class"
prop.table(table(credit_smote$Class))
ggplot(train_data, aes(x = V1, y = V2, col = Class)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue2", 'red'))
ggplot(credit_smote, aes(x = V1, y = V2, col = Class)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue2", 'red'))
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
CART_model <- rpart(Class ~ . , credit_smote)
rpart.plot(CART_model, extra = 0, type = 5, tweak = 1.2)
predicted_val <- predict(CART_model, test_data, type = 'class')
library(caret)
confusionMatrix(predicted_val, test_data$Class)
predicted_val1 <- predict(CART_model, credit_card[-1], type = 'class')
confusionMatrix(predicted_val1, credit_card$Class)
