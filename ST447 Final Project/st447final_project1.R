#We first set the working directory to the folder used for this project, containing the data files
setwd("~/Desktop/LSE_Courses/ST447 /ST447 Final Project")

# Enter Student ID to get XYZ's profile
ID = 201316007

#Load the XYZ function to generate XYZ's profile
source("XYZProfile.r")
XYZprofile(201316007)

# The profile of XYZ:
#- Age:  22
#- Gender:  Female
#- Home address:  Bury (Manchester)

# Throughtout this entire project, I am assuming that the name of my friend XYZ is Sarah

#We then Read in Files from years 2007-2015
#Data in year 2014/15
sheet_1_201415 <- read.csv("sheet_1.csv", stringsAsFactors = FALSE,header = T )

#Data in year 2013/14
sheet_2_201314 <- read.csv("sheet_2.csv", stringsAsFactors = FALSE,header = T )

#Data in year 2012/13
sheet_3_201213 <- read.csv("sheet_3.csv", stringsAsFactors = FALSE,header = T )

#2011/12 Data not available for Bury(Manchester)
sheet_4_201112 <- read.csv("sheet_4.csv", stringsAsFactors = FALSE, header = T)

# Data in year 2010/11
sheet_5_201011 <-read.csv("sheet_5.csv", stringsAsFactors = FALSE,header = T )

# Data in year 2009/10
sheet_6_200910 <-read.csv("sheet_6.csv", stringsAsFactors = FALSE,header = T )

# Data in year 2008/09
sheet_7_200809 <-read.csv("sheet_7.csv", stringsAsFactors = FALSE,header = T )

# Data in year 2007/08
sheet_8_200708 <-read.csv("sheet_8.csv", stringsAsFactors = FALSE,header = T )


#Rename columns
colnames(sheet_1_201415) <- c("Test_Centre","Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", "Female_Passes", "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")
colnames(sheet_2_201314) <- c("Test_Centre","Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", "Female_Passes", "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")
colnames(sheet_3_201213) <- c("Test_Centre","Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", "Female_Passes", "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")
colnames(sheet_4_201112) <- c("Test_Centre","Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", "Female_Passes", "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")
colnames(sheet_5_201011) <- c("Test_Centre","Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", "Female_Passes", "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")
colnames(sheet_6_200910) <- c("Test_Centre","Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", "Female_Passes", "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")
colnames(sheet_7_200809) <- c("Test_Centre","Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", "Female_Passes", "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")
colnames(sheet_8_200708) <- c("Test_Centre","Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", "Female_Passes", "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")

######## Part 1) and 2) ~ Finding the mean Pass Rates at each Centre given that Sarah is a 22 year old Female #####



# Extract Data for Tests conducted in Bury (Manchester) and Wood Green (London) from years 2007 -2015:
#2014/15
bury.2014 = sheet_1_201415[sheet_1_201415$`Test_Centre`=="Bury (Manchester)" , ] 
wgreen.2014 = sheet_1_201415[sheet_1_201415$`Test_Centre`=="Wood Green (London)" ,] 

#2013/14
bury.2013 = sheet_2_201314[sheet_2_201314$`Test_Centre`=="Bury (Manchester)",] 
wgreen.2013 = sheet_2_201314[sheet_2_201314$`Test_Centre`=="Wood Green (London)" ,] 

#2012/13
bury.2012 = sheet_3_201213[sheet_3_201213$`Test_Centre`=="Bury (Manchester)" ,] 
wgreen.2012 = sheet_3_201213[sheet_3_201213$`Test_Centre`=="Wood Green (London)" ,] 

#2011/12 Data not available for Bury(Manchester) but it is available for Wood Green:
wgreen.2011 = sheet_4_201112[sheet_4_201112$`Test_Centre`=="Wood Green (London)" ,] 

#2010/11
bury.2010 = sheet_5_201011[sheet_5_201011$`Test_Centre`=="Bury (Manchester)",] 
wgreen.2010 = sheet_5_201011[sheet_5_201011$`Test_Centre`=="Wood Green (London)",] 
#This call does not seem to include the Age =25 row so we can manually add it using rbind(). Note that the values 
# have been typed in from the excel sheet.
df<- data.frame( "Wood Green (London)",25, 163, 
                 75, 46.0122699,195,  64,
                 32.8205128,  358,  139,  38.8268156)
names(df)<-  c("Test_Centre","Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", "Female_Passes", "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")
wgreen.2010 = rbind(wgreen.2010,df)

#2009/10
bury.2009 = sheet_6_200910[sheet_6_200910$`Test_Centre`=="Bury (Manchester)" ,] 
wgreen.2009 = sheet_6_200910[sheet_6_200910$`Test_Centre`=="Wood Green (London)" ,] 

#2008/09
bury.2008 = sheet_7_200809[sheet_7_200809$`Test_Centre`=="Bury (Manchester)",] 
wgreen.2008 = sheet_7_200809[sheet_7_200809$`Test_Centre`=="Wood Green (London)",] 

#2007/08
bury.2007 = sheet_8_200708[sheet_8_200708$`Test_Centre`=="Bury (Manchester)",] 
wgreen.2007 = sheet_8_200708[sheet_8_200708$`Test_Centre`=="Wood Green (London)",] 

#I then merge all these data frames together to create one single data frame for each Area:
df_bury<- rbind(bury.2007, bury.2008, bury.2009, bury.2010, bury.2012, bury.2013, bury.2014)
df_wgreen<- rbind(wgreen.2007, wgreen.2008, wgreen.2009, wgreen.2010, wgreen.2011, wgreen.2012, wgreen.2013, wgreen.2014)

# I then add the predictor "Year" using the $ sign:
df_bury$Year <- c(rep(2007,9),rep(2008,9), rep(2009,9),rep(2010,9), rep(2012,9), rep(2013,9), rep(2014,9))
df_wgreen$Year <- c(rep(2007,9), rep(2008,9), rep(2009,9), rep(2010,9), rep(2011,9), rep(2012,9), rep(2013,9), rep(2014,9))

# Upon further introspection of the dataframe we find that most of the numerical variables are of type "Character":
summary(df_bury)
summary(df_wgreen)

# Thus, we convert each numeric variable incorrectly classified as class "character" to class "numeric":
df_bury[,c("Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", "Female_Passes", 
           "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")] <- as.numeric( 
unlist(df_bury[,c("Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", 
  "Female_Passes", "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")])) 

df_wgreen[,c("Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", "Female_Passes", 
           "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")] <- as.numeric( 
 unlist(df_wgreen[,c("Age","Male_Tests_Conducted", "Male_Passes", "Male_Pass_Rate","Female_Tests_Conducted", 
      "Female_Passes", "Female_Pass_Rate", "Total_Tests_Conducted", "Total_Passes", "Pass_Rate")])) 


summary(df_bury)
summary(df_wgreen)

#1),2) Expected(Mean) passing rate for 22 year old females at Bury (Manchester) and Wood Green (London) 
# respectively, weher the latter is the nearest test centre to the LSE:
mean(df_bury$`Female_Pass_Rate`[df_bury$Age==22])
# 40.07% approximately
mean(df_wgreen$`Female_Pass_Rate`[df_wgreen$Age])
# 37.77 % approximately

# Note that the above calculation was made on the strict assumption that the mean should be for the exact category
# that Sarah belongs to . If she wanted to find out what the mean pass rate was just for her gender then it would be:
mean(df_bury$`Female_Pass_Rate`)
# 43.28 % approximately
mean(df_wgreen$`Female_Pass_Rate`)
# 37.47 % approximately

# Thus, based on these findings, Sarah will have a better chance of passing the test given that she goes to the 
#Centre at Bury.


############ Part 3) Deciding where it would be best for Sarah to take the test ##############

####Exploratory Data Analysis #########

# I will now do some exploratory Data Analysis to get a sense of what the data shows(find any outliers or interesting
# correlations).
# The plot function is used to display a scatter plot showing the relationship between two variables
plot(df_bury$Age, df_bury$Female_Pass_Rate, col="blue")
#EXPLAIN BOX PLOT
# This plot interestingly seems to suggest that as individuals get older(within the given range of 
# 17-25), the probability of passing the test declines.Notice the two outliers represented by the 
# circles; one states that a 17 year old individual has a passing rate of 60 % whereas the mean passing rate for the Age=17 
#category is only around 51%

#The pairs() function can be used to create multiple scatter plots showing the relationship between  
#independent continuous variables. How it works is that a variable on the horizontal axis is taken as an independent variable
#whereas one on the vertical axis (irrespective of whether it is positive/negative) is taken as the dependent/
#Response variable.
pairs(df_bury[,-1],col="brown")

#Correlations can only be performed for Numerix variables so we will remove the Test Centre and Age variables as 
# they are categoical.
cor(df_bury[-c(1,2)])
cor(df_wgreen[-c(1,2)])





# I am going to use the method of linear regression to decide which test Centre Sarah should do the test.
# In order to make a prediction on which centre is better, we will need to have a training set which will be used 
# to train the model, and a testing set which (as its name implies) we can use to test the accuracy of the model.
# The lm() function can be used to run a linear regression of the Y(dependent) variable on a set of predictors(X)/
# Independent variables. We will first decide which predictors are significant and use that same set of predictors
# for the model in Bury and Wood Green.

lm.bury <- lm(Female_Pass_Rate~ Age + Male_Tests_Conducted+ Male_Passes + Male_Pass_Rate +Female_Tests_Conducted 
              +Female_Passes + Pass_Rate+Year, data=df_bury )
summary(lm.bury)
# Notice that the "Test _Centre" variable is categorical and hence we will not include it in this calculation.

# Age , Male_Tests_Conducted and Year not significant

lm.wgreen <- lm(Female_Pass_Rate~ Age + Male_Tests_Conducted+ Male_Passes + Male_Pass_Rate +Female_Tests_Conducted 
              +Female_Passes + Pass_Rate+Year, data=df_wgreen )
summary(lm.wgreen)
# Age , Male_Tests_Conducted, Male_Passes and Year not significant

#Remove insignificant variables in each models to improve predictive power:
lm.bury <- lm(Female_Pass_Rate~ Male_Passes + Male_Pass_Rate +Female_Tests_Conducted 
              +Female_Passes + Pass_Rate, data=df_bury )
summary(lm.bury)

lm.wgreen <- lm(Female_Pass_Rate~ Male_Pass_Rate +Female_Tests_Conducted 
                +Female_Passes + Pass_Rate , data=df_wgreen )
summary(lm.wgreen)

# Let the training set be a random sample of 75% of the observations in each model and the other 25% be the testing
# set.
set.seed(201316007) # To reproduce the exact set of random
# values each time
split.bury <- sample(nrow(df_bury), size = floor(0.75*nrow(df_bury)))
train.bury = df_bury[split.bury,]
test.bury = df_bury[-split.bury,]
head(train.bury)

# We do the same for Wood Green 
set.seed(201316007) # To reproduce the exact set of random
# values each time
split.wgreen <- sample(nrow(df_wgreen), size = floor(0.75*nrow(df_wgreen)))
train.wgreen = df_wgreen[split.wgreen,]
test.wgreen = df_wgreen[-split.wgreen,]
head(train.wgreen)

# We now build the predictive models for Bury and Wood Green using only the significant variables as predictors and the training data
predictionModel.bury <- lm(Female_Pass_Rate~ Male_Passes + Male_Pass_Rate +Female_Tests_Conducted 
                           +Female_Passes + Pass_Rate, data=train.bury )
summary(predictionModel.bury)
predictionModel.wgreen <-lm(Female_Pass_Rate~ Male_Pass_Rate +Female_Tests_Conducted 
                            +Female_Passes + Pass_Rate, data=train.wgreen )
summary(predictionModel.wgreen)

# Testing the predictive models:
#Bury
prediction.bury <- predict(predictionModel.bury, newdata= test.bury)
head(prediction.bury)
head(test.bury$Female_Pass_Rate)

# CAlculate R-squared for the predictive model on the test set
RSS.bury <- sum((test.bury$Female_Pass_Rate -prediction.bury)^2)
TSS.bury <- sum((test.bury$Female_Pass_Rate -mean(test.bury$Female_Pass_Rate))^2)
ESS.bury <- 1-(RSS.bury/TSS.bury)

#Wood Green
prediction.wgreen <- predict(predictionModel.wgreen, newdata= test.wgreen)
head(prediction.wgreen)
head(test.wgreen$Female_Pass_Rate)

# CAlculate R-squared for the predictive model on the test set
RSS.wgreen <- sum((test.wgreen$Female_Pass_Rate -prediction.wgreen)^2)
TSS.wgreen <- sum((test.wgreen$Female_Pass_Rate -mean(test.wgreen$Female_Pass_Rate))^2)
ESS.wgreen <- 1-(RSS.wgreen/TSS.wgreen)