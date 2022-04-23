library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
churn <- read.csv('TelcoChurn.csv')
str(churn)

#Data cleaning and preparation#
#Check if missing values in dataset
sapply(churn, function(x) sum(is.na(x)))

#11 rows has missing values in TotalCharges, hence removing
churn <- churn[complete.cases(churn), ]

#Changing values of "No internet service" to "No" in 6 columns
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

#Changing "No phone service" to "No"
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

#Changing tenure values in different groups
min(churn$tenure)
max(churn$tenure)

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)

#Changing values of "SeniorCitizen" to Yes/No
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

#Corelation between numeric dataset
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

#Removing tenure and TotalCharges
churn$tenure <- NULL
churn$TotalCharges <- NULL

##EDA##

install.packages("magrittr") 
install.packages("dplyr")    
library(magrittr) 
library(dplyr)    
churn %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')
churn %>%
  dplyr::select(-Churn) %>% 
  keep(is.factor) %>%
  gather() %>%
  group_by(key, value) %>% 
  summarize(n = n()) %>% 
  ggplot() +
  geom_bar(mapping=aes(x = value, y = n, fill=key), color="black", stat='identity') + 
  coord_flip() +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')

t1 <- ggplot(churn, aes(x= Churn, group=ï..gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~ï..gender) + ggtitle("Gender vs Churn") +
  scale_y_continuous(labels = scales::percent)

t2 <- ggplot(churn, aes(x= Churn, group=SeniorCitizen)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~SeniorCitizen) + ggtitle("SeniorCitizen vs Churn") +
  scale_y_continuous(labels = scales::percent)

t3 <- ggplot(churn, aes(x= Churn, group=Partner)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~Partner) + ggtitle("Partner vs Churn") +
  scale_y_continuous(labels = scales::percent)

t4 <- ggplot(churn, aes(x= Churn, group=Dependents)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~Dependents) + ggtitle("Dependents vs Churn") +
  scale_y_continuous(labels = scales::percent)

t5 <- ggplot(churn, aes(x= Churn, group=Dependents)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~Dependents) + ggtitle("Dependents vs Churn") +
  scale_y_continuous(labels = scales::percent)

t6 <- ggplot(churn, aes(x= Churn, group=PhoneService)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~PhoneService) + ggtitle("Phoneservice vs Churn") +
  scale_y_continuous(labels = scales::percent)

t7 <- ggplot(churn, aes(x= Churn, group=MultipleLines)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~MultipleLines) + ggtitle("MultipleLines vs Churn") +
  scale_y_continuous(labels = scales::percent)

t8 <- ggplot(churn, aes(x= Churn, group=InternetService)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~InternetService) + ggtitle("InternetService vs Churn") +
  scale_y_continuous(labels = scales::percent)

t9 <- ggplot(churn, aes(x= Churn, group=OnlineSecurity)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~OnlineSecurity) + ggtitle("OnlineSecurity vs Churn") +
  scale_y_continuous(labels = scales::percent)

t10 <- ggplot(churn, aes(x= Churn, group=OnlineBackup)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~OnlineBackup) + ggtitle("OnlineBackup vs Churn") +
  scale_y_continuous(labels = scales::percent)

t11 <- ggplot(churn, aes(x= Churn, group=DeviceProtection)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~DeviceProtection) + ggtitle("DeviceProtection vs Churn") +
  scale_y_continuous(labels = scales::percent)

t12 <- ggplot(churn, aes(x= Churn, group=TechSupport)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~TechSupport) + ggtitle("Techsupport vs Churn") +
  scale_y_continuous(labels = scales::percent)

t13 <- ggplot(churn, aes(x= Churn, group=StreamingTV)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~StreamingTV) + ggtitle("StreamingTV vs Churn") +
  scale_y_continuous(labels = scales::percent)

t14 <- ggplot(churn, aes(x= Churn, group=i)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~i) + ggtitle("i vs Churn") +
  scale_y_continuous(labels = scales::percent)

t15 <- ggplot(churn, aes(x= Churn, group=Contract)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~Contract) + ggtitle("Contract vs Churn") +
  scale_y_continuous(labels = scales::percent)

t16 <- ggplot(churn, aes(x= Churn, group=PaperlessBilling)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~PaperlessBilling) + ggtitle("PaperlessBilling vs Churn") +
  scale_y_continuous(labels = scales::percent)

t17 <- ggplot(churn, aes(x= Churn, group=PaymentMethod)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~PaymentMethod) + ggtitle("PaymentMethod vs Churn") +
  scale_y_continuous(labels = scales::percent)

t18 <- ggplot(churn, aes(x= Churn, group=tenure_group)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill= "Status") +
  facet_grid(~tenure_group) + ggtitle("tenure vs Churn") +
  scale_y_continuous(labels = scales::percent)
grid.arrange(t1,t2,t3,t4,t5,t6, ncol=3)
grid.arrange(t7,t8,t9,t10,t11,t12, ncol=3)
grid.arrange(t13,t14,t15,t16,t17,t18, ncol=3)

#ML Model

#Logistics
#Splitting data in test-train set in ratio 70:30
intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn[intrain,]
testing<- churn[-intrain,]

#Fitting LR model
LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
summary(LogModel)

#Top features are contract, tenuregroup, paperlessBilling

anova(LogModel, test="Chisq")

#Predict
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

#Confusion Matrix
print("Confusion Matrix for Logistic Regression")
cm_lr = table(testing$Churn, fitted.results > 0.5)
cm_lr

library("ROCR")
pred_lr <- prediction(fitted.results, testing$Churn)    
perf_lr <- performance(pred_lr, measure = "tpr", x.measure = "fpr")     
plot(perf_lr, ,col=4, lwd = 2, add=TRUE)    
abline(0, 1)
per_lr = performance(pred_lr, "auc")@y.values
print(paste("Performance for LR:",per_lr))

#####SVM####
library(e1071)
classifier_svm = svm(formula = Churn ~ .,
                 data = training,
                 type = 'nu-classification',
                 kernel = 'sigmoid', cost=4, gamma= 0.01)

# Predicting the Test set results
y_pred_svm = predict(classifier_svm, newdata = testing[-18])

# Making the Confusion Matrix
print("Confusion matrix for Sigmoid SVM")
cm_svm = table(testing[, 18], y_pred_svm)
cm_svm

accuracy_svm <- (cm_svm[2, 2] + cm_svm[1, 1]) / sum(cm_svm)
accuracy_svm

p1<- predict(classifier_svm,testing, type="decision")
pr<-prediction(as.numeric(p1), as.numeric(testing$Churn))
prf<- performance(pr, measure="tpr",x.measure="fpr")
plot(prf,col=2, lwd = 2, add=TRUE)
lines(x = c(0,1), y = c(0,1),col="blue")
per_svm = performance(pr, "auc")@y.values
per_svm
#Random Forest
library(randomForest)
set.seed(123)
classifier_rf = randomForest(x = training[-18],
                          y = training$Churn,
                          ntree = 500)

# Predicting the Test set results
y_pred_rf = predict(classifier_rf, newdata = testing[-18])

# Making the Confusion Matrix
cm_rf = table(testing[, 18], y_pred_rf)
cm_rf
accuracy_rf <- (cm_rf[2, 2] + cm_rf[1, 1]) / sum(cm_rf)
print(paste("Accuracy for Random Forest with 500 trees:",accuracy_rf))

library(ROCR)
pred_test_rf <- predict(classifier_rf, newdata = testing, type="vote")
p_test_rf <- prediction(pred_test_rf[,2], testing$Churn)
perf_rf <- performance(p_test_rf, "tpr", "fpr")
plot(perf_rf, ,col=3, lwd = 2, add=TRUE)
perf_auc_rf_500 = performance(p_test_rf, "auc")@y.values
print(paste("AUC for 500 tress Randon Forest:",perf_auc_rf_500 ))

classifier_rf_300 = randomForest(x = training[-18],
                             y = training$Churn,
                             ntree = 300)

# Predicting the Test set results
y_pred_rf_300 = predict(classifier_rf_300, newdata = testing[-18])

# Making the Confusion Matrix
cm_rf_300 = table(testing[, 18], y_pred_rf_300)
cm_rf_300
accuracy_rf_300 <- (cm_rf_300[2, 2] + cm_rf_300[1, 1]) / sum(cm_rf_300)
print(paste("Accuracy for Random Forest with 500 trees:",accuracy_rf_300))

library(ROCR)
pred_test_rf_300 <- predict(classifier_rf_300, newdata = testing, type="vote")
p_test_rf_300 <- prediction(pred_test_rf_300[,2], testing$Churn)
perf_rf_300 <- performance(p_test_rf_300, "tpr", "fpr")
plot(perf_rf_300, colorize=T)
perf_auc_rf_300 = performance(p_test_rf_300, "auc")@y.values
print(paste("AUC for 300 tress Randon Forest:",perf_auc_rf_300 ))

##NB
classifier_nb = naiveBayes(x = training[-18],
                        y = training$Churn, laplace = 0.5)

# Predicting the Test set results
y_pred_nb = predict(classifier_nb, newdata = testing[-18])

# Making the Confusion Matrix
cm_nb = table(testing[, 18], y_pred_nb)
cm_nb 

accuracy_nb <- (cm_nb[2, 2] + cm_nb[1, 1]) / sum(cm_nb)
accuracy_nb

library(ROCR)
pred_test_naive <- predict(classifier_nb, newdata = testing, type="raw")
p_test_naive <- prediction(pred_test_naive[,2], testing$Churn)  ## taking the probability for "1" only
perf_naive <- performance(p_test_naive, "tpr", "fpr")
plot(perf_naive,col=5, lwd = 2, add=TRUE)
per_nb <- performance(p_test_naive, "auc")@y.values
per_nb

#Summary
final <- matrix(c(1-misClasificError,accuracy_nb,accuracy_rf,accuracy_svm,per_lr,per_nb,perf_auc_rf_500,per_svm),ncol=4,byrow=TRUE)
colnames(final) <- c("LR","Naive Bayes","Random Forest","SVM")
rownames(final) <- c("Accuracy", "AUC")
final

legend(0.6, 0.6, c("SVM","Random Forest","Logistic Regression","Naive Bayes" ), 2:5)

#Feature Selection
vip::vip(classifier_rf)