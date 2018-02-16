# install and load the ODS package 
install.packages("readODS")
library(readODS)

#set the working directory to the folder used for this project, containing the data files
setwd("/Users/akeelahamed/Desktop/ST447 Final Project/dvsa1203")

# Enter Student ID to get XYZ's profile
ID = 201316007

#Load the XYZ function to generate XYZ's profile
source("XYZProfile.r")
XYZprofile(201316007)

# The profile of XYZ:
#- Age:  22
#- Gender:  Female
#- Home address:  Bury (Manchester)

#Read in Files 
#Data in year 2014/15
sheet_1_201415 <- read.csv("sheet_1.csv", stringsAsFactors = FALSE,header = T )

#Data in year 2013/14
sheet_2_201314 <- read.csv("sheet_2.csv", stringsAsFactors = FALSE,header = T )

#Data in year 2012/13
sheet_3_201213 <- read.csv("sheet_3.csv", stringsAsFactors = FALSE,header = T )

#2011/12 Data not available for Bury(Manchester)

# Data in year 2010/11
sheet_5_201011 <-read.csv("sheet_5.csv", stringsAsFactors = FALSE,header = T )

# Data in year 2009/10
sheet_6_200910 <-read.csv("sheet_6.csv", stringsAsFactors = FALSE,header = T )

# Data in year 2008/09
sheet_7_200809 <-read.csv("sheet_7.csv", stringsAsFactors = FALSE,header = T )

# Data in year 2007/08
sheet_8_200708 <-read.csv("sheet_8.csv", stringsAsFactors = FALSE,header = T )


#Rename columns
colnames(sheet_1_201415) <- c("Test Centre","Age","Male tests conducted", "Male Passes", "Male Pass rate(%)","Female tests Conducted", "Female Passes", "Female Pass rate(%)", "Total tests conducted", "Total Passes", "Pass rate(%)")
colnames(sheet_2_201314) <- c("Test Centre","Age","Male tests conducted", "Male Passes", "Male Pass rate(%)","Female tests Conducted", "Female Passes", "Female Pass rate(%)", "Total tests conducted", "Total Passes", "Pass rate(%)")
colnames(sheet_3_201213) <- c("Test Centre","Age","Male tests conducted", "Male Passes", "Male Pass rate(%)","Female tests Conducted", "Female Passes", "Female Pass rate(%)", "Total tests conducted", "Total Passes", "Pass rate(%)")
colnames(sheet_5_201011) <- c("Test Centre","Age","Male tests conducted", "Male Passes", "Male Pass rate(%)","Female tests Conducted", "Female Passes", "Female Pass rate(%)", "Total tests conducted", "Total Passes", "Pass rate(%)")
colnames(sheet_6_200910) <- c("Test Centre","Age","Male tests conducted", "Male Passes", "Male Pass rate(%)","Female tests Conducted", "Female Passes", "Female Pass rate(%)", "Total tests conducted", "Total Passes", "Pass rate(%)")
colnames(sheet_7_200809) <- c("Test Centre","Age","Male tests conducted", "Male Passes", "Male Pass rate(%)","Female tests Conducted", "Female Passes", "Female Pass rate(%)", "Total tests conducted", "Total Passes", "Pass rate(%)")
colnames(sheet_8_200708) <- c("Test Centre","Age","Male tests conducted", "Male Passes", "Male Pass rate(%)","Female tests Conducted", "Female Passes", "Female Pass rate(%)", "Total tests conducted", "Total Passes", "Pass rate(%)")

# Extract Data for female tests conducted in Bury (Manchester) from years 2007 -2015
#2014/15
sheet_1_201415[sheet_1$`Test Centre`=="Bury (Manchester)" & sheet_1$Age==22,] 

#2013/14
sheet_2_201314[sheet_2_201314$`Test Centre`=="Bury (Manchester)" & sheet_2_201314$Age==22,] 

#2012/13
sheet_3_201213[sheet_3_201213$`Test Centre`=="Bury (Manchester)" & sheet_3_201213$Age==22,] 

#2011/12 Data not available for Bury(Manchester)

#2010/11
sheet_5_201011[sheet_5_201011$`Test Centre`=="Bury (Manchester)" & sheet_5_201011$Age==22,] 

#2009/10
sheet_6_200910[sheet_6_200910$`Test Centre`=="Bury (Manchester)" & sheet_6_200910$Age==22,] 

#2008/09
sheet_7_200809[sheet_7_200809$`Test Centre`=="Bury (Manchester)" & sheet_7_200809$Age==22,] 

#2007/08
sheet_8_200708[sheet_8_200708$`Test Centre`=="Bury (Manchester)" & sheet_8_200708$Age==22,] 


# Make a data frame to plot "Female pass rate(%)" in Bury (Manchester) against years :
# Note: as.double() enables fractional numbers to be passed. 
years <- c(2007,2008,2009,2010,2012,2013,2014)
female_pass_rates_bury <- c(as.double(sheet_8_200708[sheet_8_200708$`Test Centre`=="Bury (Manchester)" & sheet_8_200708$Age==22, 'Female Pass rate(%)']),
                            as.double(sheet_7_200809[sheet_7_200809$`Test Centre`=="Bury (Manchester)" & sheet_7_200809$Age==22, 'Female Pass rate(%)']),
                            as.double(sheet_6_200910[sheet_6_200910$`Test Centre`=="Bury (Manchester)" & sheet_6_200910$Age==22, 'Female Pass rate(%)']),
                            as.double(sheet_5_201011[sheet_5_201011$`Test Centre`=="Bury (Manchester)" & sheet_5_201011$Age==22, 'Female Pass rate(%)']),
                            as.double(sheet_3_201213[sheet_3_201213$`Test Centre`=="Bury (Manchester)" & sheet_3_201213$Age==22, 'Female Pass rate(%)']),
                            as.double(sheet_2_201314[sheet_2_201314$`Test Centre`=="Bury (Manchester)" & sheet_2_201314$Age==22, 'Female Pass rate(%)']),
                            as.double(sheet_1_201415[sheet_1_201415$`Test Centre`=="Bury (Manchester)" & sheet_1_201415$Age==22, 'Female Pass rate(%)']))

f_passes_bury <- c(sheet_8_200708[sheet_8_200708$`Test Centre`=="Bury (Manchester)" & sheet_8_200708$Age==22, 'Female Passes'],
                   (sheet_7_200809[sheet_7_200809$`Test Centre`=="Bury (Manchester)" & sheet_7_200809$Age==22, 'Female Passes']),
                   (sheet_6_200910[sheet_6_200910$`Test Centre`=="Bury (Manchester)" & sheet_6_200910$Age==22, 'Female Passes']),
                   (sheet_5_201011[sheet_5_201011$`Test Centre`=="Bury (Manchester)" & sheet_5_201011$Age==22, 'Female Passes']),
                   (sheet_3_201213[sheet_3_201213$`Test Centre`=="Bury (Manchester)" & sheet_3_201213$Age==22, 'Female Passes']),
                   (sheet_2_201314[sheet_2_201314$`Test Centre`=="Bury (Manchester)" & sheet_2_201314$Age==22, 'Female Passes']),
                   (sheet_1_201415[sheet_1_201415$`Test Centre`=="Bury (Manchester)" & sheet_1_201415$Age==22, 'Female Passes']))

df_f_bury <- data.frame(female_pass_rates_bury, years)
df_f_bury$passes <- f_passes_bury

#1) Expected passing rate at Bury
mean(df_f_bury$female_pass_rates_bury)
# 40.07%


# For the average pass rate at the test centre closest to LSE (Wood Green) we need to xtract Data for female 
# tests conducted in Wood Green (London) from years 2007-2015
#2014/15
sheet_1_201415[sheet_1$`Test Centre`=="Wood Green (London)" & sheet_1$Age==22,] 

#2013/14
sheet_2_201314[sheet_2_201314$`Test Centre`=="Wood Green (London)" & sheet_2_201314$Age==22,] 

#2012/13
sheet_3_201213[sheet_3_201213$`Test Centre`=="Wood Green (London)" & sheet_3_201213$Age==22,] 

#2010/11
sheet_5_201011[sheet_5_201011$`Test Centre`=="Wood Green (London)" & sheet_5_201011$Age==22,] 

#2009/10
sheet_6_200910[sheet_6_200910$`Test Centre`=="Wood Green (London)" & sheet_6_200910$Age==22,] 

#2008/09
sheet_7_200809[sheet_7_200809$`Test Centre`=="Wood Green (London)" & sheet_7_200809$Age==22,] 

#2007/08
sheet_8_200708[sheet_8_200708$`Test Centre`=="Wood Green (London)" & sheet_8_200708$Age==22,] 

# Make a data frame to plot "Female pass rate(%)" in Wood Green (London) against years :
years <- c(2007,2008,2009,2010,2012,2013,2014)
female_pass_rates_wgreen <- c(as.double(sheet_8_200708[sheet_8_200708$`Test Centre`=="Wood Green (London)" & sheet_8_200708$Age==22, 'Female Pass rate(%)']),
                              as.double(sheet_7_200809[sheet_7_200809$`Test Centre`=="Wood Green (London)" & sheet_7_200809$Age==22, 'Female Pass rate(%)']),
                              as.double(sheet_6_200910[sheet_6_200910$`Test Centre`=="Wood Green (London)" & sheet_6_200910$Age==22, 'Female Pass rate(%)']),
                              as.double(sheet_5_201011[sheet_5_201011$`Test Centre`=="Wood Green (London)" & sheet_5_201011$Age==22, 'Female Pass rate(%)']),
                              as.double(sheet_3_201213[sheet_3_201213$`Test Centre`=="Wood Green (London)" & sheet_3_201213$Age==22, 'Female Pass rate(%)']),
                              as.double(sheet_2_201314[sheet_2_201314$`Test Centre`=="Wood Green (London)" & sheet_2_201314$Age==22, 'Female Pass rate(%)']),
                              as.double(sheet_1_201415[sheet_1_201415$`Test Centre`=="Wood Green (London)" & sheet_1_201415$Age==22, 'Female Pass rate(%)']))



df_f_wgreen <- data.frame(female_pass_rates_wgreen, years)

#1) Expected passing rate at Bury
mean(df_f_wgreen$female_pass_rates_wgreen)
# 38.98 %









#In order to decide which Centre would give XYZ the greatest chance of passing, we first need to decide which 
# Centre gives the most consistent predictions based on the data obtained. This will enable us to make a better
#guess on where it would be best for XYZ to do the test. 

# We would thus like to regress Female Pass rate(%) in Bury and Wood Green on a number of predictors, namely:
# Gender & Age . 


# We are going to assess which place is better for a test using the method of logistic regression. However, we 
# first need to introduce a categorical variable as a response. 
#We will build a categorical variable called "Result" which states whether Femeale individuals of a particular 
#Age group and test centre Passed/Failed the test. The criteria for pass or fail in Bury and Wood Green 
# will be judged as follows:
# If the Female pass rate > mean(Female pass rate) == Pass, or else fail. The drawback to this method is that even
# if an Age group in a test centre has a higher than average pass rate this doesnt guarantee that all have passed.

#The cut() function takes three arguments x,breaks and labels. x is a numeric vector converted to a factor by 
#cutting. Breaks gives the number of intervals into which x is to be cut. labels gives the names of the categories
# made by cutting. 
#Note that we are making the crucial assumption here that if the Female pass rate is above the median, 
#then the individuals of that particular age group and area have passed the test. We use the median (which separates
#the first 50% of observations in the dataset from the last 50%) as it isn't affected by outliers/extreme values.
df_bury$Result <- cut(df_bury$`Female_Pass_Rate(%)`, c(0,median(df_bury$`Female_Pass_Rate(%)`),60.14), 
                      labels = c("Fail", "Pass"))
summary(df_bury)
df_wgreen$Result <- cut(df_wgreen$`Female_Pass_Rate(%)`, c(0,median(df_bury$`Female_Pass_Rate(%)`),47.77), labels = c("Fail", "Pass"))
summary(df_wgreen)

# In order to enable fair comparison of data we need to make sure that the proportion of pass/fail in each location 
# is approximately the same. Thus, I have chosen to increase the median pass rate of wood green in order to enable
# a fair comparison.
df_wgreen$Result <- cut(df_wgreen$`Female_Pass_Rate(%)`, c(0,median(df_wgreen$`Female_Pass_Rate(%)`)+0.2,47.77), 
                        labels = c("Fail", "Pass"))
summary(df_wgreen)
# Now notice that the pass rates for bury and wood green are approximately the same at 49% (31/63 and 35/72).

#Whats left: Run a logistic regression using glm(explain a little). Use it to train and test with same predictors
# for each area. Choose area with the lowest test set error rate. Automate work with functions where able 
#We are going to use logistic regression to predict the Result (pass/fail) of each test location based on the following
# predictors : Year, Age , Female tests conducted and Male tests conducted .
glm.bury1 <- glm(Result ~ Age + Year + Female_Tests_Conducted + Male_Tests_Conducted, data =df_bury,family = binomial)
summary(glm.bury1)

glm.wgreen1<- glm(Result ~ Age + Year + Female_Tests_Conducted + Male_Tests_Conducted
                  , data = df_wgreen, family= binomial)
summary(glm.wgreen1)

# In both cases it can be seen that the Age and Year are not very significant variables to our model as they have a 
# very high p-value. Thus we will remove them .
glm.bury1 <- glm(Result ~ Female_Tests_Conducted + Male_Tests_Conducted, data =df_bury,family = binomial)
summary(glm.bury1)

glm.wgreen1<- glm(Result ~ Female_Tests_Conducted + Male_Tests_Conducted
                  , data = df_wgreen, family= binomial)
summary(glm.wgreen1)

# We are now going to divide the data in wood green and Bury into two sets: a Training Set and Testing Set. But, the 
#dataset for Bury and wood green differ in the number of observations , with Bury having 63 observations and Wood Green
#having 72(this is because data for the year 2011/12 is not available for Bury). In order to have a fair comparison
# of both areas , we are going to ensure the training sets contain the first 75% of the observations in each dataset.

#Boolean vectors can be used to obtain a subset of rows or columns of a matrix. eg df_bury[train.bury,] would take 
# only the first 47 elements of df_bury as they correspond to the elements of df_bury which are TRUE. 
train.bury = c(rep(TRUE,47), rep(FALSE,63-47))  # Note that 47/63 is roughly 75%. rep()  creates a vector of 47 elements with the value TRUE
train.wgreen = c(rep(TRUE, 54), rep(FALSE,72-54)) # Note that 54/72 is exactly 75%

#test.bury = rep(TRUE, 63-47)
#test.wgreen = rep(TRUE, 72-54)

test.bury = df_bury[!train.bury,]
test.wgreen = df_wgreen[!train.wgreen,]

Result.bury = df_bury$Result[!train.bury]
Result.wgreen = df_wgreen$Result[!train.wgreen]
# We now fit a logistic regression model for the training data only , using the subset argument.
glm.bury2 <- glm(Result ~ Female_Tests_Conducted + Male_Tests_Conducted, data = df_bury,family = binomial,subset =train.bury)
#summary(glm.bury2)

glm.wgreen2<- glm(Result ~ Female_Tests_Conducted + Male_Tests_Conducted,data=df_wgreen
                  , family= binomial, subset = train.wgreen)
#summary(glm.wgreen2)

# The predict() function can predict the probability that the result is a Pass, given the value of the pedictors.
# The type="response" argumnet tells R to output probabilities of the form P(Y=1|X).
glm.probs.bury = predict(glm.bury2,test.bury, type = "response")
glm.probs.wgreen = predict(glm.wgreen2,test.wgreen, type = "response")

#We have thus trained and tested our model on two completely separate datasets: training was performed on the first 75% of observations, whereas
#testing was performed on the rest. Finally, we compute the predictions for the test set and compare them to the actual result of Pass or Fail.

glm.pred.bury = rep("Fail", 16)
glm.pred.wgreen = rep("Fail", 18)

glm.pred.bury[glm.probs.bury>0.5]="Pass"
glm.pred.wgreen[glm.probs.wgreen>0.5]="Pass"

table(glm.pred.bury, Result.bury )
table(glm.pred.wgreen, Result.wgreen)

mean(glm.pred.bury==Result.bury)
mean(glm.pred.wgreen==Result.wgreen)
# The Testing Accuracy for Bury is better than that of Wood Green. Thus Sarah should take the test at Wood Green .



#debug
a = c(1,2,3)
b = c(a, 4)

for(i in c(sheet_1_201415)){
  print(mode(i))
  print(mode(sheet_1_201415))
  print(dim(i))
}