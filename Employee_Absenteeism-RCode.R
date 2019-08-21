rm(list = ls())

#setting working directory
setwd("/Users/rishi/Desktop/All/edWisor/Project2")
getwd()
################################Package Installation####################################
#installing required packages
install.packages("readr", "readxl", "MLmetrics","plyr","dplyr","ggplot2","rpart","DMwR","randomForest","usdm","corrgram","DataCombine","xlsx")
l <- c("readr", "readxl", "MLmetrics","plyr","dplyr","ggplot2","rpart","DMwR","randomForest","usdm","corrgram","DataCombine","xlsx")
libraries(l)

#Importing the Dataset
emp_abs <- read_excel("Absenteeism_at_work_Project.xls")
emp_abs_dummy <- read_excel("Absenteeism_at_work_Project.xls")
View(emp_abs)
str(emp_abs)

#Checking for unique values count in each variable
unique_counts <- data.frame(apply(emp_abs, 2, function(x){length(unique(x))}))
View(unique_counts)

#Feature Engineering (converting categorical variables)
colnames(emp_abs)
emp_abs$ID <- as.factor(emp_abs$ID)

emp_abs$`Reason for absence`[emp_abs$`Reason for absence` == 0] <- 20
emp_abs$`Reason for absence` <- as.factor(emp_abs$`Reason for absence`)
levels(emp_abs$`Reason for absence`) 

emp_abs$`Month of absence`[emp_abs$`Month of absence` == 0] <- NA
emp_abs$`Month of absence` <- as.factor(emp_abs$`Month of absence`)

emp_abs$`Day of the week` <- as.factor(emp_abs$`Day of the week`)
emp_abs$Seasons <- as.factor(emp_abs$Seasons)
emp_abs$`Disciplinary failure` <- as.factor(emp_abs$`Disciplinary failure`)
emp_abs$Education <- as.factor(emp_abs$Education)
emp_abs$Son <- as.factor(emp_abs$Son)
emp_abs$`Social drinker` <- as.factor(emp_abs$`Social drinker`)
emp_abs$`Social smoker` <- as.factor(emp_abs$`Social smoker`)
emp_abs$Pet <- as.factor(emp_abs$Pet)

str(emp_abs)
#making a copy of dataset
df_emp_abs <- emp_abs


############################## Missing Value Analysis #####################################
#Getting number of missing values in each variables
missingval <- data.frame(apply(df_emp_abs, 2, function(x){sum(is.na(x))}))
missingval
missingval$Variables <- row.names(missingval)
names(missingval)[1] <- "Missing Values"
missingval$Percent <- (missingval$`Missing Values`/nrow(df_emp_abs)) *100 #getting missing value percentage
missingval <- missingval[order(-missingval$Percent),]  #sorting as per percentage
missingval

#plotting missing values graph
ggplot(data = missingval[1:11,], aes(x=reorder(Variables, -Percent),y = Percent))+
  geom_bar(stat = "identity",fill = "turquoise")+xlab("Parameter")+
  ggtitle("Missing data percentage") + theme_bw()


#Best method to compute Missing Values
#creating Missing Value
View(df_emp_abs$`Body mass index`)
df_emp_abs$`Body mass index`[10] <- NA #Actual Value = 29

df_emp_abs$`Body mass index`[10] = mean(df_emp_abs$`Body mass index`, na.rm = T)
df_emp_abs$`Body mass index`[10] #By Mean Method = 26.680

df_emp_abs$`Body mass index`[10] = median(df_emp_abs$`Body mass index`, na.rm = T)
df_emp_abs$`Body mass index`[10] #By Median Method = 25

df_emp_abs <- knnImputation(df_emp_abs, k = 5) #By KNNImputation = 29


##################################### OUTLIER ANALYSIS ######################################
#Getting only numeric variables
numeric_var <- sapply(df_emp_abs, is.numeric)
numeric_data <- df_emp_abs[,numeric_var]

#Getting only categorical variables
category_data <- df_emp_abs[,!numeric_var]

#Check for outliers using boxplots
for(i in 1:ncol(numeric_data)) {
  assign(paste0("box",i), ggplot(data = df_emp_abs, aes_string(y = numeric_data[,i])) +
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour = "red", fill = "grey", outlier.size = 1) +
           labs(y = colnames(numeric_data[i])) +
           ggtitle(paste("Boxplot: ",colnames(numeric_data[i]))))
}

gridExtra::grid.arrange(box1, box2, box3, box4, ncol(4))
gridExtra::grid.arrange(box5,box6,box7,box8, ncol(4))
gridExtra::grid.arrange(box9,box10,ncol(2))

#Replacing Outliers
for(i in numeric_columns){
  val = df_emp_abs[,i][df_emp_abs[,i] %in% boxplot.stats(df[,i])$out]
  print(paste(i,length(val)))
  df_emp_abs[,i][df_emp_abs[,i] %in% val] = NA
}

sapply(df_emp_abs,function(x){sum(is.na(x))})

#Missing values i.e. NA after replacing outliers
out_missingval <- data.frame(sapply(df_emp_abs,function(x){sum(is.na(x))}))
out_missingval$Columns <- row.names(out_missingval)
row.names(out_missingval) <- NULL
names(out_missingval)[1] <- "miss_percentage"
out_missingval$miss_percentage <- ((out_missingval$miss_percentage/nrow(emp_absent)) *100)
out_missingval <- out_missingval[order(-out_missingval$miss_percentage),]
out_missingval

#KNN imputation on all NA values
df_emp_abs <- knnImputation(df_emp_abs, k = 5)
sum(is.na(df))

############################ Graphical Representation #################################
#Distribution of categorical variables
#changing variables names by removing spaces
names(df_emp_abs)[2] <- "Reason_for_absence"
names(df_emp_abs)[3] <- "Month_of_absence"
names(df_emp_abs)[4] <- "Day_of_the_week"
names(df_emp_abs)[6] <- "Transportation_expense"
names(df_emp_abs)[7] <- "Distance_from_Residence_to_Work"
names(df_emp_abs)[8] <- "Service_time"
names(df_emp_abs)[10] <- "Work_load_Average_perday"
names(df_emp_abs)[11] <- "Hit_target"
names(df_emp_abs)[12] <- "Disciplinary_failure"
names(df_emp_abs)[15] <- "Social_drinker"
names(df_emp_abs)[16] <- "Social_smoker"
names(df_emp_abs)[20] <- "Body_mass_index"
names(df_emp_abs)[21] <- "Absenteeism_time_in_hours"

#setting the levels of the variables for plotting
levels(df_emp_abs$Month_of_absence) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
levels(df_emp_abs$Disciplinary_failure) <- c("No", "Yes")
levels(df_emp_abs$Education) <- c("High Scool", "Graduate", "PG", "Masters and Doctors")
levels(df_emp_abs$Social_drinker) <- c("No", "Yes")
levels(df_emp_abs$Social_smoker) <- c("No", "Yes")
levels(df_emp_abs$Day_of_the_week) <- c("Mon", "Tues", "Wed", "Thurs", "Fri")
levels(df_emp_abs$Seasons) <- c("summer", "autumn", "winter", "spring")

#Creating graphs
bar1 <- ggplot(data = df_emp_abs, aes(x = ID)) + geom_bar() + ggtitle("Count of ID") + theme_classic()
bar2 <- ggplot(data = df_emp_abs, aes(x = Reason_for_absence)) + geom_bar() + 
  ggtitle("Count of Reason for absence") + theme_classic()
bar3 <- ggplot(data = df_emp_abs, aes(x = Month_of_absence)) + geom_bar() + ggtitle("Count of Month") + theme_classic()
bar4 <- ggplot(data = df_emp_abs, aes(x = Disciplinary_failure)) + geom_bar() + 
  ggtitle("Count of Disciplinary failure") + theme_classic()
bar5 <- ggplot(data = df_emp_abs, aes(x = Education)) + geom_bar() + ggtitle("Education Qualification") + theme_classic()
bar6 <- ggplot(data = df_emp_abs, aes(x = Son)) + geom_bar() + ggtitle("Count of Son") + theme_classic()
bar7 <- ggplot(data = df_emp_abs, aes(x = Social_smoker)) + geom_bar() + 
  ggtitle("Count of Social smoker") + theme_classic()
bar8 <- ggplot(data = df_emp_abs, aes(x = Pet)) + geom_bar() + ggtitle("Count of Pets") + theme_classic()
bar9 <- ggplot(data = df_emp_abs, aes(x = Seasons)) + geom_bar() + ggtitle("Seasons") + theme_classic()
bar10 <- ggplot(data = df_emp_abs, aes(x = Day_of_the_week)) + geom_bar() + ggtitle("Day of the Week") +theme_classic()

#Arranging and plotting graphs
gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)
gridExtra::grid.arrange(bar5,bar6,bar7,bar8,ncol=2)
gridExtra::grid.arrange(bar9,bar10,ncol=2)

#Check the distribution of numerical data using histogram
hist1 <- ggplot(data = numeric_data, aes(x = Transportation_expense)) + 
  ggtitle("Transportation expense") + geom_histogram(bins = 25)
hist2 <- ggplot(data = numeric_data, aes(x =Height)) + 
  ggtitle("Distribution of Height") + geom_histogram(bins = 25)
hist3 <- ggplot(data = numeric_data, aes(x = Body_mass_index)) + 
  ggtitle("Body mass index") + geom_histogram(bins = 25)
hist4 <- ggplot(data = numeric_data, aes(x =Absenteeism_time_in_hours)) + 
  ggtitle("Absenteeism time in hours") + geom_histogram(bins = 25)
hist5 <- ggplot(data = numeric_data, aes(x =Distance_from_Residence_to_Work)) + 
  ggtitle("Distance from Residence to Work") + geom_histogram(bins = 25)
hist6 <- ggplot(data = numeric_data, aes(x = Service_time)) + 
  ggtitle("Service time") + geom_histogram(bins = 25)
hist7 <- ggplot(data = numeric_data, aes(x = Age)) + 
  ggtitle("Age Distribution") + geom_histogram(bins = 25)
hist8 <- ggplot(data = numeric_data, aes(x = Work_load_Average_perday)) + 
  ggtitle("WorkLoad (Avg/day)") + geom_histogram(bins = 25)
hist9 <- ggplot(data = numeric_data, aes(x = Hit_target)) + 
  ggtitle("Hit Target Distribution") + geom_histogram(bins = 25)
hist10 <- ggplot(data = numeric_data, aes(x = Weight)) + 
  ggtitle("Weight Distribution") + geom_histogram(bins = 25)


gridExtra::grid.arrange(hist1,hist2,hist3,hist4,ncol=2)
gridExtra::grid.arrange(hist5,hist6,hist7,hist9,ncol=2)
gridExtra::grid.arrange(hist8,hist10,ncol=2)

##################################### Feature Selection ########################################
#Check for multicollinearity using VIF
vifcor(numeric_data)

#Plotting Correlation Plot
corrgram(numeric_data, order = T, upper.panel=panel.pie, 
         text.panel=panel.txt, main = "Correlation Plot")

#Removing 'Body_mass_index' variable
df_emp_abs <- subset.data.frame(df_emp_abs, select = -c(Body_mass_index))

#copy of pre-processed data
processed_data <- df_emp_abs
write_csv(processed_data, "processed_data.csv")

##################################### Feature Scaling ########################################
hist(processed_data$Absenteeism_time_in_hours)

#Removing dependent variable
numeric_var <- sapply(processed_data, is.numeric)
numeric_data <- processed_data[,numeric_var]
numeric_cols <- names(numeric_data)
numeric_cols <- numeric_cols[-9] #i.e. 'Absenteeism time in hours' variable

#Normalizing continuous variables
for(i in numeric_cols){
  print(i)
  processed_data[,i] = (processed_data[,i] - min(processed_data[,i]))/
    (max(processed_data[,i]) - min(processed_data[,i]))
}

#Getting names of caegorical variables
category_cols <- names(category_data)


##################################### PCA for Dimensionality reduction ########################################
#splitting the data into Training and Test datasets
set.seed(123)
train_index = sample(1:nrow(processed_data), 0.8*nrow(processed_data))
train = processed_data[train_index,]
test = processed_data[-train_index,]

#Principal component analysis
prin_comp = prcomp(numeric_data)
pr_stdev = prin_comp$sdev #std deviation calculation
pr_var = pr_stdev^2 #variance calculation

#Variance Proportion 
prop_var = pr_var/sum(pr_var)

#Cumulative scree plot
plot(cumsum(prop_var), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#Adding training set with principal components
train.data = data.frame(Absenteeism_time_in_hours = train$Absenteeism_time_in_hours, prin_comp$x)

#selecting 45 components
train.data =train.data[,1:45]

#Transforming test into PCA
test.data = predict(prin_comp, newdata = test)
test.data = as.data.frame(test.data)
test.data=test.data[,1:45] #for 45 components only


##################################### DECISION TREE ########################################

#RMSE: 0.442
#MAE: 0.301
#R squared: 0.978

#Implementing Decision Tree Model
dt_model = rpart(Absenteeism_time_in_hours ~., data = train.data, method = "anova")

#Predicting on test cases
dt_predictions = predict(dt_model,test.data)

#DF for actual and predicted values
df_pred = data.frame("actual"=test[,115], "dt_pred"=dt_predictions)
head(df_pred)

#Calcuating Error 
print(postResample(pred = dt_predictions, obs = test$Absenteeism_time_in_hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism_time_in_hours,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")


##################################### RANDOM FOREST ########################################

#RMSE: 0.480
#MAE: 0.264
#R squared: 0.978

#Implementing Random Forest Model
rf_model = randomForest(Absenteeism_time_in_hours~., data = train.data, ntrees = 500)

#Predicting on test cases
rf_predict = predict(rf_model,test.data)

#DF for actual and predicted values
df_pred = cbind(df_pred,rf_predict)
head(df_pred)

#Calcuating Error 
print(postResample(pred = rf_predict, obs = test$Absenteeism_time_in_hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism_time_in_hours,type="l",lty=2,col="green")
lines(rf_predict,col="blue")


##################################### LINEAR REGRESSION ########################################

#RMSE: 0.003
#R squared: 0.999
#MAE: 0.002


#Implementing Linear Regression Model
lr_model = lm(Absenteeism_time_in_hours ~ ., data = train.data)
summary(lr_model)

#Predicting on test cases
lr_predictions = predict(lr_model,test.data)

#DF for actual and predicted values
df_pred = cbind(df_pred,lr_predictions)
head(df_pred)

#Calcuating Error 
print(postResample(pred = lr_predictions, obs =test$Absenteeism_time_in_hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism_time_in_hours,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

