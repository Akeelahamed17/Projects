setwd("~/Desktop/LSE_Courses/ST447 /ST447 Final Project/")

# Enter Student ID to get XYZ's profile
ID = 201316007

#Load the XYZ function to generate XYZ's profile
source("XYZProfile.r")
XYZprofile(201316007)

#Read Data 
#Sheet_names <-excel_sheets("dvsa1203.xls") 
#Sheet_names
sheet_1_201415 <- read.csv("sheet_1.csv", stringsAsFactors = FALSE,header = T )

#Data in year 2013/14
sheet_2_201314 <- read.csv("sheet_2.csv", stringsAsFactors = FALSE,header = T )

#Data in year 2012/13
sheet_3_201213 <- read.csv("sheet_3.csv", stringsAsFactors = FALSE,header = T )

#2011/12 Data not available for Bury(Manchester). 
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

#2011/12 Data not available for Bury(Manchester) but closest alternative at Bolton taken:
wgreen.2011 = sheet_4_201112[sheet_4_201112$`Test_Centre`=="Wood Green (London)" ,] 
bolton.2011 = sheet_4_201112[sheet_4_201112$Test_Centre=="Bolton",]
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
df_bury<- rbind(bury.2007, bury.2008, bury.2009, bury.2010, bolton.2011, bury.2012, bury.2013, bury.2014)
df_wgreen<- rbind(wgreen.2007, wgreen.2008, wgreen.2009, wgreen.2010, wgreen.2011, wgreen.2012, wgreen.2013, wgreen.2014)

# I then add the predictor "Year" using the $ sign:
df_bury$Year <-   c(rep(2007,9), rep(2008,9), rep(2009,9), rep(2010,9), rep(2011,9), rep(2012,9), rep(2013,9), rep(2014,9))
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


#Mean pass rate for Bury (Manchester) & Wood Green
# Mean Female Pass Rate for each Area according to Age
df_1_bury <-data.frame(aggregate(df_bury$Female_Pass_Rate, list(df_bury$Age), mean))
colnames(df_1_bury) = c("Age", "Female Pass Rate")
df_1_wgreen <- aggregate(df_wgreen$Female_Pass_Rate, list(df_wgreen$Age), mean)
colnames(df_1_wgreen) = c("Age", "Female Pass Rate")

makeplot <- function(x,y){
  plot(x, col = "brown", type = "b", pch = 1, ylim = c(25,55), lty = 1)
  lines(y, col = "Green", type = "b", pch = 2, lty = 2)
}

makeplot(df_1_bury, df_1_wgreen)
legend(22, 55, legend = c("Bury", "Wood Green"), fill = c("brown", "green"), lty = 1:2,
       col = c("brown", "green"), cex = 0.6, box.lty = 0, bg = "yellow", text.font = 4)

#Mean Female Pass Rate for each area by Year
df_2_bury <-data.frame(aggregate(df_bury$Female_Pass_Rate, list(df_bury$Year), mean))
colnames(df_2_bury) = c("Year", "Female Pass Rate")
df_2_wgreen <- aggregate(df_wgreen$Female_Pass_Rate, list(df_wgreen$Year), mean)
colnames(df_2_wgreen) = c("Year", "Female Pass Rate")

makeplot(df_2_bury, df_2_wgreen)
legend(2011, 30, legend = c("Bury", "Wood Green"), fill = c("brown", "green"), lty = 1:2,
       col = c("brown", "green"), cex = 0.6, box.lty = 0, bg = "yellow", text.font = 4)


#Mean for all Females Aged 22 in each Area from Years 2007 - 2014:
Bury.Mean = mean(df_bury$Female_Pass_Rate[df_bury$Age == 22])
# 41.44 %
WoodGreen.Mean = mean(df_wgreen$Female_Pass_Rate[df_wgreen$Age == 22])
# 37.42 %

# Fitting the line of best fit 
plot(df_1_bury, col = "brown", type = "b", pch = 1, ylim = c(25,55), lty = 1)
lm_1_bury <- lm(`Female Pass Rate` ~ Age, data = df_1_bury)
abline(coef(lm_1_bury), lwd = 2, lty = 3)
lines(df_1_wgreen, col = "green", type = "b", pch = 2, ylim = c(25,55), lty = 2)
lm_1_wgreen <- lm(`Female Pass Rate` ~ Age, data = df_1_wgreen)
abline(coef(lm_1_wgreen), lwd = 2, lty = 3)
legend(22, 55, legend = c("Bury", "Wood Green", "Best Fit Line"), fill = c("brown", "green", "black"), 
       lty = 1:3,col = c("brown", "green", "black"), cex = 0.6, box.lty = 0, bg = "yellow", text.font = 4)



# linear Regression Model for Female Pass Rate Against Age & Test Centre
#Make a new data frame combining Bury and Wood Green. 
df_bury_wgreen <- rbind(df_bury, df_wgreen)
#Rename the Bolton Factor to Bury (Manchester) to be consistent with our initial assumption -	
#Since there is no data for Bury (Manchester) in 2011-12, I am taking the data for Bolton, which is the next 
#closest Test Centre to Bury of approximately 6 miles.  
df_bury_wgreen$Test_Centre <- c(rep("Bury (Manchester)",72), rep("Wood Green (London)", 72))
# Convert the test centre variable from character to factor in order to display contrasts (shown below)
df_bury_wgreen$Test_Centre <- as.factor(df_bury_wgreen$Test_Centre)
# Run a linear Regression model on Female Pass Rate against Test Centre and Age from the df_bury_wgreen dataset.
# Note that the command to run linear regression in R is lm(y ~ x, data = ) where lm stands for linear model.
lm_bury_wgreen <- lm(Female_Pass_Rate ~ Age + Test_Centre , data = df_bury_wgreen)
# Return the summary statistics of our linear model
summary(lm_bury_wgreen)
# Display the coding scheme used for the dummy variables
contrasts(df_bury_wgreen$Test_Centre)


# Fitting the line of best fit 
plot(df_1_bury, col = "brown", type = "b", pch = 1, ylim = c(25,55), lty = 1)
lm_1_bury <- lm(`Female Pass Rate` ~ Age, data = df_1_bury)
abline(coef(lm_1_bury), lwd = 2, lty = 3)
lines(df_1_wgreen, col = "green", type = "b", pch = 2, ylim = c(25,55), lty = 2)
lm_1_wgreen <- lm(`Female Pass Rate` ~ Age, data = df_1_wgreen)
abline(coef(lm_1_wgreen), lwd = 2, lty = 3)
legend(22, 55, legend = c("Bury", "Wood Green", "Best Fit Line"), fill = c("brown", "green", "black"), 
       lty = 1:3,col = c("brown", "green", "black"), cex = 0.6, box.lty = 0, bg = "yellow", text.font = 4)

#4 Main Assumptions for linear regression:

