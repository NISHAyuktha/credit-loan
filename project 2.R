setwd("C:\\Users\\Adesh\\Desktop\\IMRTICUS\\R\\r")

train=read.csv('creditrisk_train.csv')
test=read.csv('creditrisk_test.csv')
test$Loan_Status=NA

x_all=rbind(train,test)

table(is.na(x_all))
summary(x_all)

#gender
table(x_all$Gender)
levels (x_all$Gender)[levels(x_all$Gender)==""]="Male"

#dependents
table(x_all$Dependents)
levels(x_all$Dependents)[levels(x_all$Dependents)==""]="0"

#education
table(x_all$Education)

#self_employed
table(x_all$Self_Employed)
levels(x_all$Self_Employed)[levels(x_all$Self_Employed)==""]="0"

#applicant_income
table(is.na(x_all$ApplicantIncome))
boxplot(x_all$ApplicantIncome)
summary(x_all$ApplicantIncome)
x=(5516+(1.5*(x_all$ApplicantIncome)))
x_all$ApplicantIncome[x_all$ApplicantIncome>=x]=mean(x_all$ApplicantIncome)

#coapplicant income
table(is.na(x_all$CoapplicantIncome))
boxplot(x_all$CoapplicantIncome)
summary(x_all$CoapplicantIncome)
x=(2365+(1.5*IQR(x_all$ApplicantIncome)))
x_all$COapplicantIncome[x_all$CoapplicantIncome>=x]=mean(x_all$CoapplicantIncome)

#loan amount
table(is.na(x_all$LoanAmount))
summary(x_all$LoanAmount)
x_all$LoanAmount[is.na(x_all$LoanAmount)]=median(x_all$LoanAmount,na.rm=T)

#credit history
summary(x_all$Credit_History)
table(x_all$Credit_History)
x_all$Credit_History[is.na(x_all$Credit_History)]=1
x_all$Credit_History=as.factor(x_all$Credit_History)

#loan amount term
summary(x_all$Loan_Amount_Term)
x_all$Loan_Amount_Term[is.na(x_all$Loan_Amount_Term)]=360
table(x_all$Loan_Amount_Term)
x_all$Loan_Amount_Term[x_all$Loan_Amount_Term%in%c(6,12,36,60)]="1-5yrs"
x_all$Loan_Amount_Term[x_all$Loan_Amount_Term%in%c("300","350","360")]="360"
x_all$Loan_Amount_Term=as.factor(x_all$Loan_Amount_Term)

#Property Area
table(x_all$PropertyArea)

###CREATING DATA
x_train=x_all[1:nrow(train),]
x_test=x_all[-(1:nrow(train)),]
x_train$Loan_ID=NULL
x_test$Loan=NULL
x_test$Loan_Status=NULL

#logistic regression
model=glm(Loan_Status~.data="x_train",family="binomial")
summary(model)

#prediction
pred=predict(model,x_train[-12],type="response")
pred(1:10)
range(pred)
out=ifelse(pred>0.5,"Y","N")
table(out)
table(x_train$Loan_Status)

library(caret)
confusionMatrix(out,x_train$Loan_Status)
pred1=predict(model,x_test,type="response")
out1=ifelse(pred>0.5,"Y","N")
df=data.frame("Loan_Status"=out1)
write.csv(df"Logistic_Nisha.csv"),row.names=F)

