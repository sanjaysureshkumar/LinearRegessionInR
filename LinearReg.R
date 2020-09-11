
#Downloading data from URL

URL<-"https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv"
download.file(URL,"./Score11.csv",method="curl")


#Storing Data in a variable

Student_Score<-read.csv("Score11.csv",header=TRUE,stringsAsFactors=FALSE)
head(Student_Score)


#Shuffling the rows of the dataset

set.seed(2)
Shuffle<-sample(nrow(Student_Score))
Student_Score<-Student_Score[Shuffle,]
head(Student_Score)


#Finding Correlation between dependent and independent variable

cor(Student_Score$Hours,Student_Score$Scores)


#Top 80% rows are taken as training data

train_data<-head(Student_Score,n=0.8*nrow(Student_Score))
print(train_data)


#Bottom 20% rows are taken as Cross Validation data

Cross_Val_data<-tail(Student_Score,n=0.2*nrow(Student_Score))
print(Cross_Val_data)


#Plotting the data

with(train_data,
plot(Hours,Scores,xlab="Hours",col="red",ylab="Scores",pch=16,main="Student Scores Vs Study Time",ylim=c(0,100)),xlim=c(0,10))
with(Cross_Val_data,points(Hours,Scores,col="blue",pch=16))
legend("topleft",legend=c("Training","Cross_Validation"),col=c("red","blue"),pch=16)


#Running the linear regression modelling function

model<-lm(formula=Scores~Hours,data=Student_Score)
print(model)
summary(model)


#obtaining coefficients As : y=(beta1)x+beta2

list1<-coef(model)
Intercept<-list1[[1]]
print(Intercept)
Hour<-list1[[2]]
print(Hour)


#Plotting the regression line

abline(Intercept,Hour,lwd=2.5,lty=2,col="green")


#Copying the plot to a seperate .png file

dev.copy(png,"Intern.png")
dev.off()


#Predicting result to calculate metrics

Train_predicted <- predict(model, train_data)
print(Train_predicted)
Cross_Val_predicted <- predict(model, Cross_Val_data)
print(Cross_Val_predicted)
Train_ActVsPred<-data.frame(cbind(train_data$Scores,Train_predicted))
print(Train_ActVsPred)
CV_ActVsPred<-data.frame(cbind(Cross_Val_data$Scores,Cross_Val_predicted))
print(CV_ActVsPred)


#Calculating Metrics for training and Cross Validation data

Mean_Squared_Error<-0
Mean_Absolute_Error<-0
for(i in seq(1,length(Train_predicted)))
{
dif<-Train_predicted[[i]]-train_data[[i]]
dif<-dif**2
dif1<-abs(Train_predicted[[i]]-train_data[[i]])
Mean_Absolute_Error<-Mean_Absolute_Error+dif1
Mean_Squared_Error<-Mean_Squared_Error+dif
}
Mean_Squared_Error<-Mean_Squared_Error/length(Train_predicted)
print(Mean_Squared_Error)
Root_Mean_Squared_Error<-Mean_Squared_Error**(0.5)
print(Root_Mean_Squared_Error)
Mean_Absolute_Error<-Mean_Absolute_Error/length(Train_predicted)
print(Mean_Absolute_Error)


#R^2 value

summary(model)$r.squared


#Adjusted R^2 value

summary(model)$adj.r.squared


#P value

summary(model)$coefficients[,4]


#Metrics for Cross Validation data

CVMean_Squared_Error<-0
CVMean_Absolute_Error<-0
for(i in seq(1,length(Cross_Val_predicted)))
{
dif<-Cross_Val_predicted[[i]]-Cross_Val_data$Scores[[i]]
dif<-dif**2
dif1<-abs(Cross_Val_predicted[[i]]-Cross_Val_data$Scores[[i]])
CVMean_Absolute_Error<-CVMean_Absolute_Error+dif1
CVMean_Squared_Error<-CVMean_Squared_Error+dif
}
CVMean_Squared_Error<-CVMean_Squared_Error/length(Cross_Val_predicted)
print(CVMean_Squared_Error)
CVRoot_Mean_Squared_Error<-CVMean_Squared_Error**(0.5)
print(CVRoot_Mean_Squared_Error)
CVMean_Absolute_Error<-CVMean_Absolute_Error/length(Cross_Val_predicted)
print(CVMean_Absolute_Error)


#Predicting results for test data

test<-data.frame(Hours=9.25)
Pred_result<-predict(model,test)
print(Pred_result)
