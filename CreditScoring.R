# This library enables SQL queries in R-Dataset
library(sqldf)

#Read Data
CreditTrain <- read.table("C:\\Users\\vishal sethia\\Documents\\#UTD Spring 2015\\Credit Scoring\\cs-training.csv", header=TRUE,sep=",");

# Data Cleaning Starts
CreditTrainFinal =sqldf('select * from CreditTrain where age > 0 and age < 104');

ageIncome=sqldf('select age,avg(MonthlyIncome) as AvgIncome from CreditTrainFinal group by age order by age');

ageCount=sqldf('select age,count(age) from CreditTrainFinal group by age order by age');

CreditTrainFinal =sqldf("select a.*,
                        case when a.MonthlyIncome!='NA' and a.MonthlyIncome!=0 and a.MonthlyIncome!=1 then a.MonthlyIncome else b.AvgIncome  end as Income
                       from CreditTrainFinal a left join ageIncome b 
                       on a.age=b.age")

ageDep=sqldf('select age,median(NumberofDependents) as MedDep from CreditTrainFinal group by age order by age');

CreditTrainFinal =sqldf("select a.*,                        
                        case when a.NumberofDependents!='NA' then a.NumberofDependents else b.MedDep end as Dependent
                       from CreditTrainFinal a left join ageDep b 
                       on a.age=b.age")

summary(CreditTrainFinal);

# Data Cleaning Ends

# Data Modelling Starts

# Logistic Regression model

CreditTrainFinal$SeriousDlqin2yrs <- factor(CreditTrainFinal$SeriousDlqin2yrs)
Creditlogit <- glm(SeriousDlqin2yrs ~
                   #RevolvingUtilizationOfUnsecuredLines +
                   age +
                   NumberOfTime30.59DaysPastDueNotWorse +
                   DebtRatio +                   
                   NumberOfOpenCreditLinesAndLoans +
                   NumberOfTimes90DaysLate +
                   NumberRealEstateLoansOrLines +
                   NumberOfTime60.89DaysPastDueNotWorse +
                   Dependent 
                  # Income   
                   , data = CreditTrainFinal, family = "binomial")

summary(Creditlogit);

exp(cbind(OR = coef(Creditlogit), confint(Creditlogit)))

CreditTrainFinal$rankP<- predict(Creditlogit, newdata = CreditTrainFinal, type = "response")
CreditTrainFinal$rankP<-round(CreditTrainFinal$rankP, digits = 3)

CreditTrainFinal =sqldf("select *,                        
                        case when rankP >.5 then 1 else 0 end as Score2
                       from CreditTrainFinal")


ConfusionMatrix <- table(CreditTrainFinal$SeriousDlqin2yrs,CreditTrainFinal$Score2)
ConfusionMatrix
       0      1
0 139651    318
1   9607    419

#Kaggle Rank/Score: 845/0.692021

# Decision Tree model Starts

library(party);
CreditTree <- ctree(SeriousDlqin2yrs ~
                RevolvingUtilizationOfUnsecuredLines +
                age +
                NumberOfTime30.59DaysPastDueNotWorse +
                DebtRatio +                   
                NumberOfOpenCreditLinesAndLoans +
                NumberOfTimes90DaysLate +
                NumberRealEstateLoansOrLines +
                NumberOfTime60.89DaysPastDueNotWorse +
                Dependent +
                Income,
              data=CreditTrainFinal);

CreditTrainFinal$tree <- predict(CreditTree, CreditTrainFinal)


ConfusionMatrix <- table(CreditTrainFinal$SeriousDlqin2yrs,CreditTrainFinal$tree)
ConfusionMatrix

       0      1
0 139068    901
1   8692   1334

#Kaggle Rank/Score: 899/0.558717

#Decision Tree Model Ends

# Random Forest Model Starts

library(randomForest)

fit <- randomForest(SeriousDlqin2yrs ~
                      RevolvingUtilizationOfUnsecuredLines +
                      age +
                      NumberOfTime30.59DaysPastDueNotWorse +
                      DebtRatio +                   
                      NumberOfOpenCreditLinesAndLoans +
                      NumberOfTimes90DaysLate +
                      NumberRealEstateLoansOrLines +
                      NumberOfTime60.89DaysPastDueNotWorse +
                      Dependent +
                      Income,
                    data=CreditTrainFinal, importance=TRUE,ntree=200)

varImpPlot(fit)
fit$confusion

#Kaggle Rank/Score: 655/0.849988

       0    1   class.error
0 138429 1540 0.01100243625
1   8140 1886 0.81188908837

# Random Forest Model Ends

# Neural Network Starts

library("neuralnet")
CreditTrainFinal$tree <- as.numeric(as.character(CreditTrainFinal$tree))
creditnet <- neuralnet(SeriousDlqin2yrs ~
                         RevolvingUtilizationOfUnsecuredLines +
                         age +
                         NumberOfTime30.59DaysPastDueNotWorse +
                         DebtRatio +                   
                         NumberOfOpenCreditLinesAndLoans +
                         NumberOfTimes90DaysLate +
                         NumberRealEstateLoansOrLines +
                         NumberOfTime60.89DaysPastDueNotWorse +
                         Dependent,
                       data=CreditTrainFinal,  lifesign = "minimal", 
                       linear.output = FALSE, threshold = 0.1)
plot(creditnet, rep = "best")

temp_test <- subset( CreditTrainFinal, select = c("RevolvingUtilizationOfUnsecuredLines",
                                          "age",
                                          "NumberOfTime30.59DaysPastDueNotWorse",
                                          "DebtRatio",                  
                                          "NumberOfOpenCreditLinesAndLoans",
                                          "NumberOfTimes90DaysLate",
                                          "NumberRealEstateLoansOrLines",
                                          "NumberOfTime60.89DaysPastDueNotWorse",
                                          "Dependent"))
creditnet.results <- compute(creditnet,temp_test)

results <- data.frame(actual = CreditTrainFinal$SeriousDlqin2yrs, prediction = creditnet.results$net.result)

results =sqldf("select *,                        
                        case when prediction >.5 then 1 else 0 end as Score
                       from results")

ConfusionMatrix <- table(results$actual,results$Score)
ConfusionMatrix
       0      1
0 138525   1444
1   8265   1761

#Kaggle Rank/Score: 583/0.857392

# Neural Network Ends
