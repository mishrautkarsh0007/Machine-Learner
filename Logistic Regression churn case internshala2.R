
install.packages("caret") 

install.packages("MASS")  
install.packages("mlogit")
install.packages("sqldf") 
install.packages("Hmisc") 
install.packages('dplyr') 
install.packages('HH')  
install.packages('gmodels')
install.packages('rms') 
install.packages('pROC')
library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(dplyr)
library(HH)
library(gmodels)
library(rms)


setwd("C:/Users/utkarsh/Documents/Data analytics/Documents")

data <- read.csv(file.choose(),header = TRUE)
View(data)
head(data)

dim(data)
str(data)
summary(data)


all_columns <- colnames(data)
cont_columns <- c("tenure","MonthlyCharges","TotalCharges" )
categ_columns <- c("gender","SeniorCitizen", "Partner", "Dependents", "PhoneService" ,
                   "MultipleLines" , "InternetService" ,"OnlineSecurity","OnlineBackup",
                   "DeviceProtection", "TechSupport","StreamingTV" ,"StreamingMovies" ,
                   "Contract","PaperlessBilling","PaymentMethod","Churn" )


sapply(data, function(x) sum(is.na(x)))
apply(data,2, function(x) sum(is.na(x))) #you can also use apply


nrow(data)
data<- na.omit(data)
data<- data[complete.cases(data),] 
nrow(data)


sapply(data, function(x) sum(is.na(x)))




uniAnalysisCateg<-function(var)
{
  Freq_tbl<-data.frame(table(data[,var]))
  Freq_tbl$var_name <-var
  colnames(Freq_tbl)<-c("values","freq","variable")
  return(Freq_tbl)
}
uniDataCateg <- data.frame(values = character(),freq = numeric(),variable = character())
for(i in 1 : length(categ_columns)){
  uniDataCateg<-rbind(uniDataCateg,uniAnalysisCateg(categ_columns[i]))
}

tot_custs<-nrow(data)
uniDataCateg$perc<-round(100*uniDataCateg$freq/tot_custs,0)
ggplot(uniDataCateg, aes(x = values, y = perc)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")
write.csv(uniDataCateg, "uniDataCateg.csv", row.names=FALSE)


uniAnalysisCont<-function(var)
{
  Pctl_tbl<-as.vector(quantile(data[,var], probs=c(.01, .10, .20, .50, .80, .90, .99, 1.0)))
  Pctl_tbl<-data.frame(c("P001","P010","P020","P050","P080","P090","P099","P100"),Pctl_tbl)
  Pctl_tbl<-data.frame(c(var,var,var,var),Pctl_tbl)
  colnames(Pctl_tbl)<-c("variable","quantiles","values")
  return(Pctl_tbl)
}
uniDataCont <- data.frame(variable = character(), quantiles = character(), values = numeric())
for(i in 1 : length(cont_columns)){
  uniDataCont<-rbind(uniDataCont,uniAnalysisCont(cont_columns[i]))
}
ggplot(uniDataCont, aes(x = quantiles, y = values)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")
write.csv(uniDataCont, "uniDataCont.csv", row.names=FALSE)



var1 <- data$gender
bivarfn_categ<-function(var1,var2)
{
  BiVarDataCateg <- data.frame(CrossTable(var1,data$Churn) )
  BiVarDataCateg <- BiVarDataCateg[BiVarDataCateg$prop.row.y==1,c("t.x","prop.row.Freq")]
  BiVarDataCateg$Variable <- var2
  colnames(BiVarDataCateg)<-c("value","response_frequency","variable")
  return(BiVarDataCateg)
}
biData <- data.frame(value = character(), response_frequency = numeric(), variable = character())
for(i in 1 : length(categ_columns)){
  biData<-rbind(biData,bivarfn_categ(data[,categ_columns[i]],categ_columns[i]))
}

ggplot(biData, aes(x = value, y = response_frequency)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")
write.csv(biData, "biAnalysisCateg.csv", row.names=FALSE)


BiVarDataCont = group_by(data, Churn)
biDataCont <- data.frame(Churn= numeric(), avg_value = numeric(), variable = character())
biDataCont<-rbind(biDataCont,data.frame(cbind(summarise(BiVarDataCont, avg_value = mean(tenure)),variable=c("tenure","tenure"))))
biDataCont<-rbind(biDataCont,data.frame(cbind(summarise(BiVarDataCont, avg_value = mean(MonthlyCharges)),variable=c("MonthlyCharges","MonthlyCharges"))))
biDataCont<-rbind(biDataCont,data.frame(cbind(summarise(BiVarDataCont, avg_value = mean(TotalCharges)),variable=c("TotalCharges","TotalCharges"))))

ggplot(biDataCont, aes(x = Churn, y = avg_value)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")
write.csv(biDataCont, "biAnalysisCont.csv", row.names=FALSE)

sort(categ_columns)
table(data$PaperlessBilling)

data$D_Contract_monthly <- ifelse(data$Contract=="Month-to-month" ,1,0)
data$D_Contract_biannual <- ifelse(data$Contract=="Two year" ,1,0)
data$D_Dependents_No <- ifelse(data$Dependents=="No" ,1,0)
data$D_DeviceProtection_No <- ifelse(data$DeviceProtection=="No" ,1,0)
data$D_DeviceProtection_NoServ <- ifelse(data$DeviceProtection=="No internet service" ,1,0)
data$D_InternetService_FO <- ifelse(data$InternetService=="Fiber optic" ,1,0)
data$D_InternetService_No <- ifelse(data$InternetService=="No" ,1,0)
data$D_OnlineBackup_No <- ifelse(data$OnlineBackup=="No" ,1,0)
data$D_OnlineSecurity <- ifelse(data$OnlineSecurity=="No" ,1,0)
data$D_PaperlessBilling <- ifelse(data$PaperlessBilling=="Yes" ,1,0)
data$D_Partner <- ifelse(data$Partner=="No" ,1,0)
data$D_PaymentMethod <- ifelse(data$PaymentMethod=="Electronic check" ,1,0)
data$D_PaymentMethod_CCA <- ifelse(data$PaymentMethod=="Credit card (automatic)" ,1,0)
data$D_StreamingMovies_No <- ifelse(data$StreamingMovies=="No" ,1,0)
data$D_StreamingTV_No <- ifelse(data$StreamingTV=="No" ,1,0)
data$D_TechSupport_No <- ifelse(data$TechSupport=="No" ,1,0)


colnames(data)
data2 <- data[, c(2,5,18:36)]
cust_cor <- data.frame(cor(data2,data2))
write.csv(cust_cor, "cust_cor.csv", row.names=FALSE)


colnames(data2)
pcaData <- data2[, -c(5)]
ncol(pcaData)
fit <- princomp(pcaData, cor=TRUE)
summary(fit)
loadings(fit)
plot(fit,type="lines")
data2$Factor9 <-  (0.588*data2$D_OnlineSecurity)+(-0.616*D_PaymentMethod)


fit <- lm(Churn ~., data= data2)
vif(fit)
colnames(data2)
fit <- lm(Churn ~SeniorCitizen+
            tenure+
            TotalCharges+
            Churn+
            D_InternetService_FO+
            D_InternetService_No+
            D_OnlineBackup_No+
            D_OnlineSecurity+
            D_PaperlessBilling+
            D_Partner+
            D_PaymentMethod+
            D_PaymentMethod_CCA+
            D_StreamingMovies_No+
            D_StreamingTV_No+
            D_TechSupport_No
            , data= data2)
vif(fit)


colnames(data2)
num <- data2[,c(2:5)]
cat <- data2[,-c(2:4)]
head(cat)
head(num)


str(num)

IVCalNum <- function(variable, target,data,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)  
  tableOutput <-sqldf(sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data 
                              group by rank",target,target))
  tableOutput <- sqldf("select *,
                       (n - good) bad
                       from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))
  tableOutput$IV <- tableOutput$WOE*(tableOutput$good_rate-tableOutput$bad_rate)
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

a1<- IVCalNum("tenure","Churn",num,groups=10)
a2<- IVCalNum("MonthlyCharges","Churn",num,groups=10)
a3<- IVCalNum("TotalCharges","Churn",num,groups=10)

IV_num<- data.frame(rbind(a1,a2,a3))
IV_num


IVCalCat <- function(target, variable, data) {
  tableOutput<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  tableOutput<- sqldf("select *, (n-good) bad from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))
  tableOutput$IV <- tableOutput$WOE*(tableOutput$good_rate-tableOutput$bad_rate)
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

IV_cat <- IVCalCat("Churn",colnames(cat)[1],cat)

for(i in 2:length(colnames(cat))){
  if(colnames(cat)[i] != "Churn"){
    IV_cat <- rbind(IV_cat,IVCalCat("Churn",colnames(cat)[i],cat))
  }
}

Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV

write.csv(Final_IV,"Final_IV.csv")



set.seed(113)
test =  sample(1:nrow(data2),nrow(data2)*0.2)
train = -test
data.train  = data2[train,]
data.test = data2[test,]
nrow(data.train)
nrow(data.test)



model <- glm(Churn~., data=data.train, family=binomial())
summary(model)

model <- glm(Churn~
SeniorCitizen+
  tenure+
  TotalCharges+
  D_InternetService_FO+
  D_OnlineSecurity+
  D_PaperlessBilling+
  D_PaymentMethod+
  D_StreamingMovies_No+
  D_TechSupport_No, data=data.train, family=binomial())
summary(model)


modelChi <- model$null.deviance - model$deviance
modelChi  

LM <- model$deviance
L0 <- model$null.deviance
R2.MCF<- 1- LM/L0
R2.MCF

R.cs <- 1 - exp ((LM - L0) /nrow(data.train))
R.cs

R.n <- R.cs /(1-(exp(-(L0/(nrow(data.train))))))
R.n


1-pchisq(deviance(model), df.residual(model)) #pchisq is the prob to be same i.e. how similar your null model is with the model with parameters
View(data.train)


Concordance = function(GLM.binomial) {
  outcome_and_fitted_col = cbind(GLM.binomial$y, GLM.binomial$fitted.values)
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
  else {zeros = zeros[1:length(ones[,1]),]}
  ones_and_zeros = data.frame(ones, zeros)
  conc = rep(NA, length(ones_and_zeros[,1]))
  disc = rep(NA, length(ones_and_zeros[,1]))
  ties = rep(NA, length(ones_and_zeros[,1]))
  for (i in 1:length(ones_and_zeros[,1])) {
    if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
    {conc[i] = 1
    disc[i] = 0
    ties[i] = 0}
    else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 0
      ties[i] = 1
    }
    else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 1
      ties[i] = 0
    }
  }
  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  return(list(concordance=conc_rate))
} 

Concordance(model)


xtabs(hp~gear, data = mtcars)
y <- data.train$Churn
yhat <- data.train$Score
g <- 10
hosmerlem <- function (y, yhat, g = 10) 
{  
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0,1, 1/g)), include.lowest = T)
  obs <- xtabs(cbind(1-y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2/expect)
  P <- 1 - pchisq(chisq, g - 2)   
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}


data.train$Score <- predict(model,data.train,type="response")
View(data.train[,c("Churn","Score")])

data.test$Score <- predict(model,data.test,type="response")
hosmerlem(data.train$Churn, data.train$Score)


library(pROC)
rocCurve   <- roc(response = data.train$Churn, predictor = data.train$Score, levels = c(0,1))

plot(rocCurve)


coords(rocCurve,"best")


predclass <-ifelse(data.train$Score>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.train$Churn)
Confusion


AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
AccuracyRate


auc(rocCurve)


Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric


Pctl_tbl<-as.vector(quantile(data.train[,"Score"], probs=c(.10,.20, .30,.40,.50, .60,.70, .80,.90)))
Pctl_tbl<-data.frame(c("P10","P20","P30","P40","P50","P60","P70","P80","P90"),Pctl_tbl)
colnames(Pctl_tbl)<-c("quantiles","Score")

data.train$Segment <-ifelse(data.train[,"Score"] > Pctl_tbl[Pctl_tbl$quantiles=="P90",2] , 10, 
                            ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P80",2], 9, 
                                   ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P70",2], 8,
                                          ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P60",2],7,
                                                 ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P50",2],6,
                                                        ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P40",2],5,
                                                               ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P30",2],4,
                                                                      ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P20",2],3,
                                                                             ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P10",2],2, 1)))))))))

LiftChartData<-data.frame(CrossTable(data.train$Churn,data.train$Segment) )
LiftChartData <- LiftChartData[LiftChartData$prop.col.x==1,c("t.Freq","prop.col.y")]
write.csv(LiftChartData, "LiftChartData.csv", row.names=FALSE)



Pctl_tbl<-as.vector(quantile(test_data[,"Score"], probs=c(.10,.20, .30,.40,.50, .60,.70, .80,.90)))
Pctl_tbl<-data.frame(c("P10","P20","P30","P40","P50","P60","P70","P80","P90"),Pctl_tbl)
colnames(Pctl_tbl)<-c("quantiles","Score")

test_data$Segment <-ifelse(test_data[,"Score"] > Pctl_tbl[Pctl_tbl$quantiles=="P90",2] , 10, 
                           ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P80",2], 9, 
                                  ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P70",2], 8,
                                         ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P60",2],7,
                                                ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P50",2],6,
                                                       ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P40",2],5,
                                                              ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P30",2],4,
                                                                     ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P20",2],3,ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P10",2],                                                  2, 1)))))))))

LiftChartData<-data.frame(CrossTable(test_data$Responder,test_data$Segment) )
LiftChartData <- LiftChartData[LiftChartData$prop.col.x==1,c("t.Freq","prop.col.y")]
write.csv(LiftChartData, "LiftChartData.csv", row.names=FALSE)


model$coefficients 
exp(model$coefficients)  

