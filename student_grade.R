install.packages("MLmetrics")
library(MLmetrics)
library(caret)
library(base)
install.packages("psych")
library(psych)
grade <- read.csv("student_grades.csv", stringsAsFactors = TRUE)
View(grade)
str(grade)
names(grade)
dim(grade)
names(grade)
summary(grade)

#Checking missing values
colSums(is.na(grade))

#Checking for duplicated data.
sum(duplicated(grade))

#Outliers
boxplot(grade$absences,
        ylab = "Absences",
        main = "Boxplot of student's absences"
)
boxplot.stats(grade$absences)$out


#data transformation
grades_corr$school <- as.numeric(as.factor(grade$school))
grades_corr$sex <- as.numeric(as.factor(grade$sex))
grades_corr$address <- as.numeric(as.factor(grade$address))
grades_corr$famsize <- as.numeric(as.factor(grade$famsize))
grades_corr$Pstatus <- as.numeric(as.factor(grade$Pstatus))
grades_corr$Medu <- as.numeric(as.ordered(grade$Medu))
grades_corr$Fedu <- as.numeric(as.ordered(grade$Fedu))
grades_corr$Mjob <- as.numeric(as.factor(grade$Mjob))
grades_corr$Fjob <- as.numeric(as.factor(grade$Fjob))
grades_corr$reason <- as.numeric(as.factor(grade$reason))
grades_corr$guardian <- as.numeric(as.factor(grade$guardian))
grades_corr$traveltime <- as.numeric(as.ordered(grade$traveltime))
grades_corr$studytime <- as.numeric(as.ordered(grade$studytime))
grades_corr$schoolsup <- as.numeric( grade$schoolsup) - 1
'grades_corr$famsup <- as.numeric( grade$famsup) - 1'

grades_corr$paid <- as.numeric( grade$paid) - 1
grades_corr$activities <- as.numeric( grade$activities) - 1
grades_corr$nursery <- as.numeric( grade$nursery) - 1
grades_corr$higher <- as.numeric( grade$higher) - 1
grades_corr$internet <- as.numeric( grade$internet) - 1
grades_corr$romantic <- as.numeric( grade$romantic) - 1
grades_corr$famrel <- as.numeric(as.ordered(grade$famrel))
grades_corr$freetime <- as.numeric(as.ordered(grade$freetime))
grades_corr$goout <- as.numeric(as.ordered(grade$goout))
grades_corr$Dalc <- as.numeric(as.ordered(grade$Dalc))
grades_corr$Walc <- as.numeric(as.ordered(grade$Walc))
grades_corr$health <- as.numeric(as.ordered(grade$health))
grades_corr$Medu <- grade$Medu
grades_corr$Fedu <- grade$Fedu
grades_corr$traveltime <- grade$traveltime
grades_corr$failures <- grade$failures
grades_corr$freetime <- grade$freetime
grades_corr$goout <- grade$goout
grades_corr$Dalc <- grade$Dalc
grades_corr$Walc <- grade$Walc
grades_corr$health <- grade$health
grades_corr$absences <- grade$absences
view(grades_corr)
str(grades_corr)

#minmax normalization
rs_function <- function(x){(x-min(x))/(max(x)-min(x))}
grades_corr$school <- rs_function(grades_corr$school)
grades_corr$sex <- rs_function(grades_corr$sex)
grades_corr$age <- rs_function(grades_corr$age)
grades_corr$address <- rs_function(grades_corr$address)
grades_corr$famsize <- rs_function(grades_corr$famsize)
grades_corr$Pstatus <- rs_function(grades_corr$Pstatus)
grades_corr$Medu <- rs_function(grades_corr$Medu)
grades_corr$Fedu <- rs_function(grades_corr$Fedu)
grades_corr$Mjob <- rs_function(grades_corr$Mjob)
grades_corr$Fjob <- rs_function(grades_corr$Fjob)
grades_corr$reason <- rs_function(grades_corr$reason)
grades_corr$guardian <- rs_function(grades_corr$guardian)
grades_corr$traveltime <- rs_function(grades_corr$traveltime)
grades_corr$studytime <- rs_function(grades_corr$studytime)
grades_corr$failures <- rs_function(grades_corr$failures)
grades_corr$famrel <- rs_function(grades_corr$famrel)
grades_corr$freetime <- rs_function(grades_corr$freetime)
grades_corr$goout <- rs_function(grades_corr$goout)
grades_corr$Dalc <- rs_function(grades_corr$Dalc)
grades_corr$Walc <- rs_function(grades_corr$Walc)
grades_corr$health <- rs_function(grades_corr$health)
grades_corr$absences <- rs_function(grades_corr$absences)
View(grades_corr)


#checking for target data distribution
plot(as.factor(grade$Pass),col = "black",xlab="Result (Pass)",ylab="Quantity")
table(grade$Pass)



#Data visualization 
#Univalent analysis of all numeric attributes
u1=ggplot(data = grade) + geom_bar(mapping = aes(x = freetime), binwidth = 0.9, fill='black')
m1 <- ggplot(aes(x = freetime), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Free time after school")
u2=ggplot(data = grade) + geom_histogram(mapping = aes(x = traveltime), binwidth = 0.5, fill='black')
m2 <- ggplot(aes(x = traveltime), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Time taken to travel to school")
u3=ggplot(data = grade) + geom_histogram(mapping = aes(x = studytime), binwidth = 0.5, fill='black')
m3 <- ggplot(aes(x = studytime), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Weekly study time")
u4= ggplot(data = grade) + geom_histogram(mapping = aes(x = failures), binwidth = 0.5, fill='black')
m4 <- ggplot(aes(x = failures), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Number of previous assessment failures")
u5=ggplot(data = grade) + geom_histogram(mapping = aes(x = famrel), binwidth = 0.5, fill='black')
m5 <- ggplot(aes(x = famrel), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Quality of Family relationships")
u6=ggplot(data = grade) + geom_histogram(mapping = aes(x = goout), binwidth = 0.5, fill='black')
m6 <- ggplot(aes(x = goout), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Going out")
u7=ggplot(data = grade) + geom_histogram(mapping = aes(x = Dalc), binwidth = 0.5, fill='black')
m7 <- ggplot(aes(x = Dalc), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Per Day alcohol consumption")
u8=ggplot(data = grade) + geom_histogram(mapping = aes(x = Walc), binwidth = 0.5, fill='black')
m8 <- ggplot(aes(x = Walc), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Weekend alcohol consumption")
u9=ggplot(data = grade) + geom_histogram(mapping = aes(x = health), binwidth = 0.5, fill='black')
m9 <- ggplot(aes(x = freetime), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Current overall health status")
u10=ggplot(data = grade) + geom_histogram(mapping = aes(x = absences), binwidth = 0.5, fill='black')
m10 <- ggplot(aes(x = absences), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Absences")
u11=ggplot(data = grade) + geom_histogram(mapping = aes(x = age), binwidth = 0.5, fill='black')
m11 <- ggplot(aes(x = age), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Students Age")
grid.arrange(u1, u2, u3, u4, u5, u6)
grid.arrange(m1, m2, m3, m4, m5, m6)
grid.arrange(m7, m8, m9, m10, m11)
grid.arrange(u7, u8, u9, u10, u11)



u12=ggplot(data = grade) + geom_bar(mapping = aes(x = school), binwidth = 0.5, fill='black')
m12 <- ggplot(aes(x = school), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Students school")

u13=ggplot(data = grade) + geom_bar(mapping = aes(x = address), binwidth = 0.5, fill='black')
m13 <- ggplot(aes(x = address), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Students addresss")
u14=ggplot(data = grade) + geom_bar(mapping = aes(x = famsize), binwidth = 0.5, fill='black')
m14 <- ggplot(aes(x = famsize), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Students family size")
u15=ggplot(data = grade) + geom_bar(mapping = aes(x = Pstatus), binwidth = 0.5, fill='black')
m15 <- ggplot(aes(x = Pstatus), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Students parental status")
u16=ggplot(data = grade) + geom_bar(mapping = aes(x = Medu), binwidth = 0.5, fill='black')
m16 <- ggplot(aes(x = Medu), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Students mother education")
u17=ggplot(data = grade) + geom_bar(mapping = aes(x = Fedu), binwidth = 0.5, fill='black')
m17 <- ggplot(aes(x = Fedu), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Students father education")
u18=ggplot(data = grade) + geom_bar(mapping = aes(x = Mjob), binwidth = 0.5, fill='black')
m18 <- ggplot(aes(x = Mjob), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Students mother job")
u19=ggplot(data = grade) + geom_bar(mapping = aes(x = Fjob), binwidth = 0.5, fill='black')
m19 <- ggplot(aes(x = Fjob), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Students father job")
u20=ggplot(data = grade) + geom_bar(mapping = aes(x = reason), binwidth = 0.5, fill='black')
m20 <- ggplot(aes(x = reason), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Reason for choosing the school")
u21=ggplot(data = grade) + geom_bar(mapping = aes(x = guardian), binwidth = 0.5, fill='black')
m21 <- ggplot(aes(x = guardian), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Student's guardian")
u22=ggplot(data = grade) + geom_bar(mapping = aes(x = schoolsup), binwidth = 0.5, fill='black')
m22 <- ggplot(aes(x = schoolsup), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Student's school support")
u23=ggplot(data = grade) + geom_bar(mapping = aes(x = famsup), binwidth = 0.5, fill='black')
m23 <- ggplot(aes(x = famsup), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Student's family support")
u24=ggplot(data = grade) + geom_bar(mapping = aes(x = paid), binwidth = 0.5, fill='black')
m24 <- ggplot(aes(x = paid), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Student's extra paid for classes")
u25=ggplot(data = activities) + geom_bar(mapping = aes(x = activities), binwidth = 0.5, fill='black')
m25 <- ggplot(aes(x = paid), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Student's engage in extra curricular activities")
u26=ggplot(data = grade) + geom_bar(mapping = aes(x = nursery), binwidth = 0.5, fill='black')
m26 <- ggplot(aes(x = nursery), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Student's Attended nursery school")
u27=ggplot(data = grade) + geom_bar(mapping = aes(x = higher), binwidth = 0.5, fill='black')
m27 <- ggplot(aes(x = higher), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Student's who want higher education")
u28=ggplot(data = grade) + geom_bar(mapping = aes(x = internet), binwidth = 0.5, fill='black')
m28 <- ggplot(aes(x = internet), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Student's have internet accessibility")
u29=ggplot(data = grade) + geom_bar(mapping = aes(x = romantic), binwidth = 0.5, fill='black')
m29 <- ggplot(aes(x = romantic), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Is involved in a romantic relationship")

grid.arrange(m12, m13, m14, m15, m16, m17)
grid.arrange(m18, m19, m20, m22, m23, m24)
grid.arrange(m25, m26, m27, m28, m29)
grid.arrange(u12, u13, u14, u15, u16, u17)
grid.arrange(u18, u19, u20, u21, u22, u23)
grid.arrange(m19)

#Remove Multidisciplinary using correlation matrix
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(grade1, histogram = TRUE)
str(grades_corr)
#Chi square
library(MASS)
tbl1 <- table(grades_corr$Pass, grades_corr$school)
tbl2 <- table(grades_corr$Pass, grades_corr$sex)
tbl3 <- table(grades_corr$Pass, grades_corr$age)
tbl4 <- table(grades_corr$Pass, grades_corr$address)
tbl5 <- table(grades_corr$Pass, grades_corr$famsize)
tbl6 <- table(grades_corr$Pass, grades_corr$Pstatus)
tbl7 <- table(grades_corr$Pass, grades_corr$Medu)
tbl8 <- table(grades_corr$Pass, grades_corr$Fedu)
tbl9 <- table(grades_corr$Pass, grades_corr$Mjob)
tbl10 <- table(grades_corr$Pass, grades_corr$Fjob)
tbl11 <- table(grades_corr$Pass, grades_corr$reason)
tbl12 <- table(grades_corr$Pass, grades_corr$guardian)
tbl13 <- table(grades_corr$Pass, grades_corr$traveltime)
tbl14 <- table(grades_corr$Pass, grades_corr$studytime)
tbl15 <- table(grades_corr$Pass, grades_corr$failures)
tbl16 <- table(grades_corr$Pass, grades_corr$schoolsup)
tbl17 <- table(grades_corr$Pass, grades_corr$famsup)
tbl18 <- table(grades_corr$Pass, grades_corr$paid)
tbl19 <- table(grades_corr$Pass, grades_corr$activities)
tbl20 <- table(grades_corr$Pass, grades_corr$nursery)
tbl21 <- table(grades_corr$Pass, grades_corr$higher)
tbl22 <- table(grades_corr$Pass, grades_corr$internet)
tbl23 <- table(grades_corr$Pass, grades_corr$romantic)
tbl24 <- table(grades_corr$Pass, grades_corr$famrel)
tbl25 <- table(grades_corr$Pass, grades_corr$freetime)
tbl26 <- table(grades_corr$Pass, grades_corr$goout)
tbl27 <- table(grades_corr$Pass, grades_corr$Dalc)
tbl28 <- table(grades_corr$Pass, grades_corr$Walc)
tbl29 <- table(grades_corr$Pass, grades_corr$health)
tbl30 <- table(grades_corr$Pass, grades_corr$absences)
chisq.test(tbl1)
chisq.test(tbl2)
chisq.test(tbl3)
chisq.test(tbl4)
chisq.test(tbl5)
chisq.test(tbl6)
chisq.test(tbl7)
chisq.test(tbl8)
chisq.test(tbl9)
chisq.test(tbl10)
chisq.test(tbl11)
chisq.test(tbl12)
chisq.test(tbl13)
chisq.test(tbl14)
chisq.test(tbl15)
chisq.test(tbl16)
chisq.test(tbl17)
chisq.test(tbl18)
chisq.test(tbl19)
chisq.test(tbl20)
chisq.test(tbl21)
chisq.test(tbl22)
chisq.test(tbl23)
chisq.test(tbl24)
chisq.test(tbl25)
chisq.test(tbl26)
chisq.test(tbl27)
chisq.test(tbl28)
chisq.test(tbl29)
chisq.test(tbl30)



#multivariate EDA for categorical variables
ggplot(data = grade1) + geom_count(mapping = aes(x = failures, y = absences))
ggplot(data = grade1) + geom_count(mapping = aes(x = age, y = traveltime))
ggplot(data = grade1) + geom_count(mapping = aes(x = higher, y = traveltime))
ggplot(data = grade) + geom_count(mapping = aes(x = schoolsup, y = famsup))


ggplot(data = mushroom) + geom_count(mapping = aes(x = cap_surface, y = cap_color)) 
ggplot(mushroom, aes(x = cap_surface, y = cap_color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))



m1 <- ggplot(aes(x = Pass ), data = grade) +
  geom_bar(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Pass")
grid.arrange(m1, ncol = 2)

ggplot(data = grade,aes(x=Pass))+
  geom_bar(aes(fill=romantic),alpha=.9)+
  labs(y="Amount",x="Final Score (Pass)")

ggplot(data = grade,aes(x=Pass,y=failures))+
  geom_jitter(aes(ncol=2))+
  labs(x="Final Score (Pass)",y="Failures")


ggplot(data = grade,aes(x = schoolsup,y=Pass,fill=schoolsup))+
  geom_boxplot(show.legend = F)+
  labs(x="Extra Educational Support",y="Final Score (Pass)")

m2 <- ggplot(aes(x = schoolsup ), data = grade) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pass) +
  xlab("Extra Educational Support")
grid.arrange(m2, ncol = 2)

ggplot(data = grade,aes(x=Pass))+
  geom_density(aes(fill=internet),alpha=.9)+
  labs(y="Frequency",x="Final Score (Pass)")

ggplot(data = grade,aes(x=Pass))+
  geom_density(aes(fill=higher),alpha=.9)+
  labs(y="Frequency",x="Final Score (Pass)")

 rs_function <- function(x){(x-min(x))/(max(x)-min(x))}
grade$absences <- rs_function(grade$absences)
grades_corr <- grade


#Splitting data on train and test set
#create train and test set
set.seed(42)
grade[,"train"] <- ifelse(runif(nrow(grades_corr))<0.8, 1, 0)

trainset <- grades_corr[grades_corr$train == "1",]
testset <- grades_corr[grades_corr$train == "0",]

trainset <- trainset[-32]
testset <- testset[-32]

#Decision tree
grade_tree <- rpart(Pass~., data = trainset, method = 'class')
rpart.plot(grade_tree, extra = 106)
test_data <- testset[-31]
tree_pred <- predict(grade_tree, newdata = test_data, type = 'class')
table(predicted = tree_pred, actual = testset$Pass)
mean(tree_pred==testset$Pass)

#Random Forest
forest_grade <- cforest(Pass~., data = trainset, control = cforest_unbiased(mtry = 10, ntree = 50))
rf_prob <- predict(forest_grade, newdata = test_data, type = "response") 
rf_pred <- ifelse(rf_prob>0.5, 1, 0)
table(predicted = rf_pred, actual = testset$Pass)
mean(rf_pred==testset$Pass)
ForestVarImp <- varimp(forest_grade)
barplot(ForestVarImp)

#SVM
svm_trainset <- trainset
svm_trainset$Pass <- as.factor(svm_trainset$Pass) 
svm_grade <- svm(Pass~., data = svm_trainset)
summary(svm_grade)
svm_pred <- predict(svm_grade, newdata = test_data, type =
                      "response") 
table(predicted = svm_pred, actual = testset$Pass)
mean(svm_pred==testset$Pass) 


grade1$age<-grades_corr$age
grade1$absences<-grades_corr$absences
grade1$Medu<-grades_corr$Medu
grade1$Fedu<-grades_corr$Fedu
grade1$Mjob<-grades_corr$Mjob
grade1$traveltime<-grades_corr$traveltime
grade1$freetime<-grades_corr$freetime
grade1$Dalc<-grades_corr$Dalc
grade1$Walc<-grades_corr$Walc
grade1$higher<-grades_corr$higher
grade1$Pass<-grades_corr$Pass

drop <- c("freetime","famrel", "health")
grade1 = grade1[,!(names(grade1) %in% drop)]
str(grade1)
multi.hist(grade1, freq=F, dcol = "red", dlty=c("dotted", "solid"))

#Splitting data on train and test set
#create train and test set
str(grade1)
set.seed(42)
grade1 <- grade1 %>% relocate(Pass, .after = higher)
str(grade1)
grade1[,"train"] <- ifelse(runif(nrow(grade1))<0.8, 1, 0)

trainset1 <- grade1[grade1$train == "1",]
testset1 <- grade1[grade1$train == "0",]

trainset1 <- trainset1[-14]
testset1 <- testset1[-14]
str(trainset1)
#Decision tree
grade_tree <- rpart(Pass~., data = trainset1, method = 'class')
rpart.plot(grade_tree, extra = 106)
test_data <- testset1[-13]
tree_pred <- predict(grade_tree, newdata = test_data, type = 'class')
table(predicted = tree_pred, actual = testset1$Pass)
mean(tree_pred==testset1$Pass)


#random forest
forest_grade <- cforest(Pass~., data = trainset1, control = cforest_unbiased(mtry = 10, ntree = 50))
rf_prob <- predict(forest_grade, newdata = test_data, type = "response") 
rf_pred <- ifelse(rf_prob>0.5, 1, 0)
table(predicted = rf_pred, actual = testset1$Pass)
mean(rf_pred==testset1$Pass)
ForestVarImp <- varimp(forest_grade)
barplot(ForestVarImp)

#SVM
svm_trainset <- trainset1
str(svm_trainset)
svm_trainset$Pass <- as.factor(svm_trainset$Pass) 
svm_grade <- svm(Pass~., data = svm_trainset)
svm_pred <- predict(svm_grade, newdata = test_data, type =
                      "response")
table(predicted = svm_pred, actual = testset1$Pass)
mean(svm_pred==testset$Pass) 

#Decision Tree
TP <- 28
TN <- 19
FP <- 16
FN <- 16
Acc <- (TP+TN) / (TP+TN+FP+FN)
Acc



#Precision & Recall for Decision Tree
precision <- TP / (TP+FP)
recall <- TP / (TP+FN)
precision
recall

#F1 Score Decision Tree
F1 <- 2 * ((precision * recall) / (precision + recall))
F1

# MCC for Decision Tree
mcc <- ((TP * TN) - (FP * FN)) / (((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN)) ^ 0.5)
mcc



##Random Forest
TP <- 35
TN <- 18
FP <- 17
FN <- 9
Acc <- (TP+TN) / (TP+TN+FP+FN)
Acc



#Precision & Recall for Random Forest
precision <- TP / (TP+FP)
recall <- TP / (TP+FN)
precision
recall



#F1 Score Random Forest
F1 <- 2 * ((precision * recall) / (precision + recall))
F1



# MCC for Random Forest
mcc <- ((TP * TN) - (FP * FN)) / (((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN)) ^ 0.5)
mcc

#SVM
TP <- 37
TN <- 14
FP <- 7
FN <- 21
Acc <- (TP+TN) / (TP+TN+FP+FN)
Acc



#Precision & Recall for SVM
precision <- TP / (TP+FP)
recall <- TP / (TP+FN)
precision
recall



#F1 Score SVM
F1 <- 2 * ((precision * recall) / (precision + recall))
F1



# MCC for SVM
mcc <- ((TP * TN) - (FP * FN)) / (((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN)) ^ 0.5)
mcc