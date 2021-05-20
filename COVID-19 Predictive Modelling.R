# ------------------------ COVID-19 Predictive Modelling ------------------------------# 

# First, we created a new repository called CA2 in GitHub.
# Then created a new project in R.
# Get the current working directory
# The working directory can be set if required

setwd("C:\\Users\\deepa\\Documents\\R\\CA2")
getwd()

# We will load some libraries that we are going to use during the research.
install.packages("ggplot2")   # Install package for Data visualization
install.packages("mice")      # Install mice package and displayed the missing values
install.packages("VIM")       # Install VIM package and displayed the missing values
install.packages("psych")     # Install psych package to find correlations between multiple variables
install.packages("magrittr")  # Install magrittr package are only needed the first time you use it
install.packages("dplyr")     # Install dplyr package to decrease development time and improve readability of code
library(magrittr)             # needs to be run every time you start R and want to use %>%
library(dplyr)                # alternatively, this also loads %>%
library(psych)
library(VIM)
library(ggplot2)
library(mice)


# File "covid.csv" downloaded from blackboard, 
# storing the data file into the working directory and reading it as data frame called as "covid_data".
# Covid data set holds lots of variables that contain an empty space.
# Hence, replaced each empty space/missing content with NA.
# Also, converting the character type into factor for better analysis.

covid_data <- read.csv("covid.csv", na = "", header = T, stringsAsFactors = T) # Reading covid.csv file
covid_data [covid_data == ""] <- NA      # Assigning blank spaces with NA
head(covid_data, n = 15)                 # Display the first 15 records of the dataframe

class(covid_data)                        # Confirm the class of covid_data
str(covid_data)                          # Check the structure of data frame
nrow(covid_data)                         # Count the number of rows within the covid data frame 



# ------------------------------------- Raw Data Visualization -----------------------------------------------#


Deaths<-aggregate(covid_data$total_deaths~covid_data$location,covid_data,FUN = max)
Deaths10<-Deaths[order(-Deaths$`covid_data$total_deaths`),][1:10,]

Deaths10

barplot(Deaths10$`covid_data$total_deaths`,names.arg = Deaths10$`covid_data$location`,
        main="Highest covid death cases",las=3,col="red")



#-------------------------------------------- Data Preparation -----------------------------------------------#

# Structure displays that there are total 84529 observations and 59 variables in the Covid dataset.
# Already all the string variables are converted to Factors while reading the data into dataframe. 
# The `date` field is in "YYYY-mm-dd" format as char type
# converted to a `date` variable to date from char type.

covid_data$date <- as.Date(covid_data$date)
str(covid_data$date)

# There are some values in `location` field which is a continent not a country
# Hence, filling the `continent` field with values from `location` its a continent name.
covid_data[(covid_data$location=="Asia"),2] <- "Asia"
covid_data[(covid_data$location=="Europe"),2] <- "Europe"
covid_data[(covid_data$location=="Africa"),2] <- "Africa"
covid_data[(covid_data$location=="North America"),2] <- "North America"
covid_data[(covid_data$location=="South America"),2] <- "South America"
covid_data[(covid_data$location=="Oceania"),2] <- "Oceania"
sum(is.na(covid_data$continent))


# ---------------------------------------- Identifying the missing values------------------------------------#

# Lets find out if there are any NA's in the data
# Using na.omit() to store any full rows into new_data frame

final_df<-na.omit(covid_data)
dim(final_df)

# It is observed that there are missing data in all the records, 
# so na.omit() function is dropping all the rows from the data frame.
# Hence, not an option to proceed with. 

# complete.cases() returns a vector with no missing values, can be swapped by using the `!`
# Using complete.cases() to show all complete rows store in complete_data
# and `!` complete_cases() for missing_data accordingly.
# Then using nrow() to show a total of all complete and missing rows

complete_data <-covid_data[complete.cases(covid_data),]
nrow(complete_data)
missing_data <-covid_data[!complete.cases(covid_data),]
nrow(missing_data)

nrow(complete_data) - nrow(missing_data)
# Here as well its evident that the none of the rows are complete out of 84529 observations. 


# Now, getting the total number of `NA` values to see, how many null values were there in the entire dataset.
# Finding which columns contain `NA` values

sum(is.na(covid_data))                     # Count of `NA` is 2009585
names(which(sapply(covid_data, anyNA)))    # Almost all the variables contains `NA`, 


# --------------------------------- Data Subsetting and Imputing ---------------------------------------------#
#------------------------------------------ Data Analysis ------------------------------------------------------#

# Let's create a subset of covid_data, 
# considering the information required for further Hypothesis testing.

# Using the subset function to extract all records 
# from covid_data and only select the listed attributes for Europe


attach(covid_data)


# The variable `new_cases` are the counts of new confirmed cases of covid-19 reported daily, country wise. 
# So it will not be wrong to consider the null values as no new cases in the country on particular date, 
# hence replacing NA values with 0 for this column. 

# Similarly, the variable `people_fully_vaccinated` is total number of people who received all doses prescribed by the vaccination protocol. 
# The NA cases for this variable would be considered as if a country has number of people
# fully vaccinated is none then this column takes NA as value, can be replaced by 0 for analysis purpose.

covid_data$new_cases[is.na(covid_data$new_cases)] <- 0
covid_data$new_cases
covid_data$people_fully_vaccinated[is.na(covid_data$people_fully_vaccinated)] <- 0
covid_data$people_fully_vaccinated

#------------------------------------------------- Hypothesis Testing --------------------------------------#
#----------------------------------------------  Research Question 1 ----------------------------------------#

# Research Question 1: Is there any correlation between total cases and total deaths in different continents of the world.

# Null Hypothesis (H0): There is no correlation between total cases and total deaths in different continents of the world.
# Alternate Hypothesis (H1): There is correlation between total cases and total deaths in different continents of the world.

# Analyzing the variables used in each variable
# total_cases = Continuous interval variable,
# total_deaths = Continuous interval variable 
#-----------------------------------------------------------------------------------------------------------------------------------#

# use statistical methods to examine 
# the relationship between our variables of interest

# creating a subset of covid_data for convenient hypothesis testing

attach(covid_data)
names(covid_data)

covid_subset <- subset(covid_data,
                       select = c(iso_code, location, date, total_cases, total_deaths))
str(covid_subset)
head(covid_subset)
dim(covid_subset)
sum(is.na(covid_subset))

# Check for missing data
incomplete_data <- covid_subset[!complete.cases(covid_subset),]
nrow(incomplete_data)

#Using mice library to display NA values and its count
md.pattern(covid_subset)

# Using VIM library and displayed the missing values
missing_values <- aggr(covid_subset, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)

# ----------------------------------------------- Linearity check --------------------------------------------------------------#

# Check whether the variables used for the hypothesis test are normally distributed or not. 
# Doing this visually and using a relevant statistical analysis test. 
# Then decide on which statistical test you will use.

# ChecK linearity of the variables 

attach(covid_subset)

plot(total_cases, total_deaths, pch = 9, col= "lightblue",
     main = "Comparision of total_cases with total_deaths in various continents",
     xlab = "Total confirmed cases of COVID-19",
     ylab = "Total deaths attributed to COVID-19")


options(scipen = 999)
ggplot(covid_subset, aes(x=total_cases, y=total_deaths))+ geom_point(col="lightblue", size=3)


# we can also examine the linear correlation between both variables using Quantile-quantile plot (Q-Q plot)
with (covid_subset, {qqplot (total_cases, total_deaths,
                             main = "Comparision of total_cases with total_deaths in various continents",
                             xlab = "Total confirmed cases of COVID-19",
                             ylab = "Total deaths attributed to COVID-19")})

# Also using psych library to get correlation coefficient between the 2 variables
covid_corr <- subset(covid_subset,
                     select = c(total_cases, total_deaths))

my_sample<-covid_corr[sample(1:nrow(covid_corr), 10000, replace = FALSE),]
my_sample

head(covid_corr)
dim(covid_corr) 

pairs.panels(my_sample,
             smooth = TRUE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font    
             density = TRUE, # If TRUE, adds density plots and histograms    
             ellipses = TRUE, # If TRUE, draws ellipses    
             method = "spearman",# Correlation method (also "pearson" or "kendall")    
             pch = 21, # pch symbol    
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit    
             cor = TRUE, # If TRUE, reports correlations    
             jiggle = FALSE, # If TRUE, data points are jittered    
             factor = 2, # Jittering factor    
             hist.col = 4, # Histograms color    
             stars = TRUE, # If TRUE, adds significance level with stars    
             ci = TRUE) # If TRUE, adds confidence intervals   

# 0.96***

#----------------------------  Normal Distribution --------------------#


# Plotting histograms to view if the variables are normally Distributed 

#arrange the plots in 1 rows by 2 cols
opar = par(no.readonly = TRUE)
par(mfrow = c(1,2))

hist(total_cases, col = "blue", main = "distribution of total_cases" , 
     xlab = "total_cases")
hist(total_deaths, col = "blue", main = "distribution of total_deaths",
     xlab = "total_deaths")

par = opar

# Using Quantile-quantile plot (Q-Q plot) allows us to check
# if the data is normally distributed or not 

#Is total_cases normally distributed?
with (covid_subset, {qqnorm (total_cases,
                             main = "Normal QQ-plot of total_cases",
                             xlab = "Theoritical Quantiles",
                             ylab = "Samples Quantiles")})
# Add line that represents normal distribution
qqline(total_cases, col = "red")
# total_cases appears not to be normally distributed


#Is total_deaths normally distributed?
with (covid_subset, {qqnorm (total_deaths,
                             main = "Normal QQ-plot of total_deaths",
                             xlab = "Theoritical Quantiles",
                             ylab = "Samples Quantiles")})

# Add line that represents normal distribution
qqline(total_deaths, col = "red")
# total_deaths appears not to be normally distributed


# ------------------------------------------ shapiro-wilks test ---------------------------------------------------#
# we can run the formal test of normality provided through the widely used
# shapiro-wilks test

my_sample<-covid_subset[sample(1:nrow(covid_subset), 5000, replace = FALSE),]
my_sample

# normality test for total_cases
normality_test <- shapiro.test(my_sample$total_cases)
normality_test$p.value


#p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is not normally distributed

# In this example p-value= 0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001095636
# 0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001095636 > 0.05 (False)
# Therefore the var total_cases is not normally distributed

# normality test for total_deaths
normality_test <- shapiro.test(my_sample$total_deaths)
normality_test$p.value


#p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is not normally distributed

# In this example p-value = 0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000001243357
# 0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000001243357 > 0.05 (False)
# Therefore the variable total_deaths is not normally distributed


# Need to decide a test for calulating p-value
# Here both variables are continuous but the dependent variable is not normally distributed. 
# Hence going for a non parametric test i.e. 
# Spearman’s Correlation Coefficient (also use for ordinal data) 


corr1 <- cor.test(x=covid_subset$total_cases, 
                  y=covid_subset$total_deaths, method = 'spearman')
corr1

#Spearman's rank correlation rho

#data:  covid_subset$total_cases and covid_subset$total_deaths
#S = 2963840232128, p-value < 0.00000000000000022
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#     rho 
#0.953974 


# Spearman's rank correlation = 0.00000000000000022
# p-value = 0.00000000000000022
# cut off = 0.05 

# 0.00000000000000022 < 0.05  (true)
# reject null, and accept alternative
# hence reject H0 and accept H1

detach(covid_subset)

#------------------------------------------------- Research Question 2 --------------------------------------#

# Research Question 2: Is there any correlation between people_fully_vaccinated and new_cases in Europe?
# H0 : There is no correlation between people_fully_vaccinated and new_cases in Europe.
# H1 : There is correlation between people_fully_vaccinated and new_cases in Europe.

# Analyzing the variables used in null and alternate hypothesis
# people_fully_vaccinated = continuous interval variable
# new_cases = continuous interval variable

detach(covid_subset)

#-----------------------------------------------------------------------------------------------------------#

# creating a subset of covid_data for convenient hypothesis testing

attach(covid_data)
names(covid_data)

covid_subset <- subset(covid_data, continent %in% c("Europe"),
                       select = c(iso_code, location, date, people_fully_vaccinated, new_cases))
str(covid_subset)
head(covid_subset)
dim(covid_subset)
sum(is.na(covid_subset))

# Check for missing data
incomplete_data <- covid_subset[!complete.cases(covid_subset),]
nrow(incomplete_data)

#Using mice library to display NA values and its count
md.pattern(covid_subset)

# Using VIM library and displayed the missing values
missing_values <- aggr(covid_subset, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)

# ----------------------------------------------- Linearity check --------------------------------------------------------------#

# Check whether the variables used for the hypothesis test are normally distributed or not. 
# Doing this visually and using a relevant statistical analysis test. 
# Then decide on which statistical test you will use.

# ChecK linearity of the variables 

attach(covid_subset)

plot(people_fully_vaccinated, new_cases, pch = 9, col= "lightblue",
     main = "Comparision of people_fully_vaccinated with new_cases",
     xlab = "People_fully_vaccinated in Europe",
     ylab = "New_cases in Europe")


options(scipen = 999)
ggplot(covid_subset, aes(x=people_fully_vaccinated,y=new_cases))+ geom_point(col="lightblue", size=3)

# we can also examine the linear correlation between both variables using Quantile-quantile plot (Q-Q plot)
with (covid_subset, {qqplot (total_cases, total_deaths,
                             main = "Comparision of people_fully_vaccinated with new_cases",
                             xlab = "People_fully_vaccinated in Europe",
                             ylab = "New_cases in Europe")})

# Also using psych library to get correlation coefficient between the 2 variables
covid_corr <- subset(covid_subset,
                     select = c(people_fully_vaccinated, new_cases))

my_sample<-covid_corr[sample(1:nrow(covid_corr), 10000, replace = FALSE),]
my_sample

head(covid_corr)
dim(covid_corr) 

pairs.panels(my_sample,
             smooth = TRUE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font    
             density = TRUE, # If TRUE, adds density plots and histograms    
             ellipses = TRUE, # If TRUE, draws ellipses    
             method = "spearman",# Correlation method (also "pearson" or "kendall")    
             pch = 21, # pch symbol    
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit    
             cor = TRUE, # If TRUE, reports correlations    
             jiggle = FALSE, # If TRUE, data points are jittered    
             factor = 2, # Jittering factor    
             hist.col = 4, # Histograms color    
             stars = TRUE, # If TRUE, adds significance level with stars    
             ci = TRUE) # If TRUE, adds confidence intervals   

# 0.96***

#----------------------------  Normal Distribution --------------------#


# plotting histograms to view if the variables are normally Distributed 

#arrange the plots in 1 rows by 2 cols
opar = par(no.readonly = TRUE)
par(mfrow = c(1,2))

hist(people_fully_vaccinated, col = "blue", main = "Distribution of people_fully_vaccinated" ,
     xlab = "people_fully_vaccinated in Europe")
hist(new_cases, col = "blue", main = "Distribution of new_cases in Europe",
     , xlab = "new_cases in Europe")

par = opar


# Using Quantile-quantile plot (Q-Q plot) allows us to check
# if the data is normally distributed or not 

#Is people_fully_vaccinated normally distributed?
with (covid_subset, {qqnorm (people_fully_vaccinated,
                             main = "Normal QQ-plot of people_fully_vaccinated in Europe",
                             xlab = "Theoritical Quantiles",
                             ylab = "Samples Quantiles")})

# Add line that represents normal distribution
qqline(people_fully_vaccinated, col = "red")
# people_fully_vaccinated appears not to be normally distributed


#Is new_cases normally distributed?
with (covid_subset, {qqnorm (new_cases,
                             main = "Normal QQ-plot of new_cases in Europe",
                             xlab = "Theoritical Quantiles",
                             ylab = "Samples Quantiles")})

# Add line that represents normal distribution
qqline(new_cases, col = "red")
# new_cases appears not to be normally distributed


# ----------------------------------------------- shapiro-wilks test ---------------------------------------------------#
# we can run the formal test of normality provided through the widely used
# shapiro-wilks test

my_sample<-covid_subset[sample(1:nrow(covid_subset), 10000, replace = FALSE),]
my_sample

# normality test for people_fully_vaccinated
normality_test <- shapiro.test(my_sample$people_fully_vaccinated)
normality_test$p.value

#

#p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is not normally distributed

# In this example p-value = 0.0000000000000000000000000000000000000000000000000000000000002180336
# 0.0000000000000000000000000000000000000000000000000000000000002180336 > 0.05 (False)
# Therefore the variable people_fully_vaccinated is not normally distributed

# normality test for new_cases

my_sample<-covid_subset[sample(1:nrow(covid_subset), 5000, replace = FALSE),]
my_sample

normality_test <- shapiro.test(my_sample$new_cases)
normality_test$p.value


#p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is not normally distributed

# In this example p-value = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006308943
# 0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006308943 > 0.05 (False)
# Therefore the variable new_cases is not normally distributed


# Need to decide a test for calulating p-value
# Here both variables are continuous but the dependent variable is not normally distributed. 
# Hence going for a non parametric test i.e. 
# Spearman’s Correlation Coefficient (also use for ordinal data) 


corr1 <- cor.test(x=covid_subset$people_fully_vaccinated, 
                  y=covid_subset$new_cases, method = 'spearman')
corr1

#Spearman's rank correlation rho

#data:  covid_subset$people_fully_vaccinated and covid_subset$new_cases
#S = 1853690090, p-value < 0.00000000000000022
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#      rho 
#0.5876566  

# Spearman's rank correlation = 0.00000000000000022
# p-value = 0.00000000000000022
# cut off = 0.05 

# 0.00000000000000022 < 0.05  (true)
# reject null, and accept alternative
# hence reject H0 and accept H1

detach(covid_subset)



#----------------------------------------- Research Question 3 ----------------------------------------------#
# Research Question 3: Does hand-washing facilities affect new cases numbers in America?

# H0: The hand-washing facilities does not affect new cases numbers in America
# H1: The hand-washing facilities affect new cases numbers in America

# Analyzing the variables used in each variable
# handwashing_facilities = categorical variable, represented as proportions
# new_cases = continuous interval variable

#--------------------------------------------------------------------------------------------------------------#


# Using statistical methods to examine the relationship between our variables of interest
# creating a subset of covid_data for convenient hypothesis testing

attach(covid_data)
names(covid_data)

covid_subset <- subset(covid_data, continent %in% c("South America", "North America"),
                       select = c(iso_code, location, date, handwashing_facilities, new_cases))
str(covid_subset)
head(covid_subset)
dim(covid_subset)
sum(is.na(covid_subset))

# Check for missing data
incomplete_data <- covid_subset[!complete.cases(covid_subset),]
nrow(incomplete_data)

#Using mice library to display NA values and its count
md.pattern(covid_subset)

# Using VIM library and displayed the missing values
missing_values <- aggr(covid_subset, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)

# ----------------------------------------------- Linearity check --------------------------------------------------------------#

# Check whether the variables used for the hypothesis test are normally distributed or not. 
# Doing this visually and using a relevant statistical analysis test. 
# Then decide on which statistical test you will use.

# ChecK linearity of the variables 

attach(covid_subset)

plot(handwashing_facilities, new_cases, pch = 9, col= "lightblue",
     main = "Comparision of handwashing_facilities with new_cases in America",
     xlab = "handwashing_facilities",
     ylab = "new_cases")


options(scipen = 999)
ggplot(covid_subset, aes(x=total_deaths,y=diabetes_prevalence))+ geom_point(col="lightblue", size=3)


# we can also examine the linear correlation between both variables using Quantile-quantile plot (Q-Q plot)
with (covid_subset, {qqplot (handwashing_facilities, new_cases,
                             main = "Comparing handwashing_facilities and new_cases in America",
                             xlab = "handwashing_facilities",
                             ylab = "new_cases")})

# Also using psych library to get correlation coefficient between the 2 variables
covid_corr <- subset(covid_subset,
                     select = c(handwashing_facilities, new_cases))
head(covid_corr)
dim(covid_corr)

pairs.panels(covid_corr,
             smooth = TRUE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font    
             density = TRUE, # If TRUE, adds density plots and histograms    
             ellipses = TRUE, # If TRUE, draws ellipses    
             method = "spearman",# Correlation method (also "pearson" or "kendall")    
             pch = 21, # pch symbol    
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit    
             cor = TRUE, # If TRUE, reports correlations    
             jiggle = FALSE, # If TRUE, data points are jittered    
             factor = 2, # Jittering factor    
             hist.col = 4, # Histograms color    
             stars = TRUE, # If TRUE, adds significance level with stars    
             ci = TRUE) # If TRUE, adds confidence intervals   
# -0.21***


#----------------------------  Normal Distribution --------------------------#
# plotting histograms to view if the variables are normally Distributed 

#arrange the plots in 1 rows by 2 cols
opar = par(no.readonly = TRUE)
par(mfrow = c(1,2))


hist(total_deaths, col = "blue", main = "distribution of handwashing_facilities" , 
     xlab = "handwashing_facilities in America")
hist(new_cases, col = "blue", main = "distribution of new_cases",
     xlab = "new_cases in America")

par = opar

# Using Quantile-quantile plot (Q-Q plot) allows us to check
# if the data is normally distributed or not 

#Is handwashing_facilities normally distributed?
with (covid_subset, {qqnorm (handwashing_facilities,
                             main = "Normal QQ-plot of handwashing_facilities in America",
                             xlab = "Theoritical Quantiles",
                             ylab = "Samples Quantiles")})
# Add line that represents normal distribution
qqline(handwashing_facilities, col = "red")
# handwashing_facilities appears not to be normally distributed


#Is new_cases normally distributed?
with (covid_subset, {qqnorm (new_cases,
                             main = "Normal QQ-plot of new_cases in America",
                             xlab = "Theoritical Quantiles",
                             ylab = "Samples Quantiles")})

# Add line that represents normal distribution
qqline(new_cases, col = "red")
# new_cases appears not to be normally distributed


# ------------------------------------------ shapiro-wilks test ---------------------------------------------------#
# we can run the formal test of normality provided through the widely used
# shapiro-wilks test

my_sample<-covid_subset[sample(1:nrow(covid_subset), 10000, replace = FALSE),]
my_sample

# normality test for people_fully_vaccinated
normality_test <- shapiro.test(my_sample$handwashing_facilities )
normality_test$p.value

# p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is not normally distributed

# In this example p-value = 0.00000000000000000000000000000000000000000000000000000000000000001628196
# 0.00000000000000000000000000000000000000000000000000000000000000001628196 > 0.05 (False)
# Therefore the variable handwashing_facilities is not normally distributed

# normality test for new_cases
my_sample<-covid_subset[sample(1:nrow(covid_subset), 5000, replace = FALSE),]
my_sample

normality_test <- shapiro.test(my_sample$new_cases)
normality_test$p.value

# p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is not normally distributed

# In this example p-value = 0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000002331127
# 0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000002331127 > 0.05 (False)
# Therefore the variable new_cases is not normally distributed


# Need to decide a test for calulating p-value
# Here, one variable is continuous and other is categorical.
# The dependent variable is not normally distributed. 
# Hence going for a non parametric test i.e. 
# Mann-Whitney test 

wilcox.test(handwashing_facilities, new_cases, paired = TRUE)  # not working


# Wilcoxon signed rank test with continuity correction

#data:  handwashing_facilities and new_cases
#V = 245188587, p-value < 0.00000000000000022
#alternative hypothesis: true location shift is not equal to 0

# p-value = 0.00000000000000022
# cut off = 0.05 

# 0.00000000000000022 < 0.05  (true)
# reject null/accept alternative
# hence reject H0 and accept H1

detach(covid_subset)



#----------------------------------------- Research Question 4 ----------------------------------------------#
# Research Question 4: Is there any link between life expectancy at birth in 2019 
# and human development index?

# H0: Covid does not affect life expectancy at birth in 2019 and human_development_index
# H1: Covid-19 affect life expectancy at birth in 2019 and human_development_index

# Analyzing the variables used in each variable
# life_expectancy = categorical nominal variable, represented as proportions
# human_development_index = categorical nominal variable, represented as proportions

#--------------------------------------------------------------------------------------------------------------#


# Using statistical methods to examine the relationship between our variables of interest
# creating a subset of covid_data for convenient hypothesis testing

attach(covid_data)
names(covid_data)

covid_subset <- subset(covid_data, 
                       select = c(iso_code, location, date, life_expectancy, human_development_index))
str(covid_subset)
head(covid_subset)
dim(covid_subset)
sum(is.na(covid_subset))

# Check for missing data
incomplete_data <- covid_subset[!complete.cases(covid_subset),]
nrow(incomplete_data)

#Using mice library to display NA values and its count
md.pattern(covid_subset)

# Using VIM library and displayed the missing values
missing_values <- aggr(covid_subset, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)

# ----------------------------------------------- Linearity check --------------------------------------------------------------#

# Check whether the variables used for the hypothesis test are normally distributed or not. 
# Doing this visually and using a relevant statistical analysis test. 
# Then decide on which statistical test you will use.

# ChecK linearity of the variables 

attach(covid_subset)

plot(life_expectancy, human_development_index, pch = 9, col= "lightblue",
     main = "Comparision of life_expectancy with human_development_index",
     xlab = "life_expectancy",
     ylab = "human_development_index")


options(scipen = 999)
ggplot(covid_subset, aes(x=total_deaths,y=diabetes_prevalence))+ geom_point(col="lightblue", size=3)


# we can also examine the linear correlation between both variables using Quantile-quantile plot (Q-Q plot)
with (covid_subset, {qqplot (life_expectancy, human_development_index,
                             main = "Comparing life_expectancy and human_development_index",
                             xlab = "life_expectancy",
                             ylab = "human_development_index")})

# Also using psych library to get correlation coefficient between the 2 variables
covid_corr <- subset(covid_subset,
                     select = c(life_expectancy, human_development_index))
head(covid_corr)
dim(covid_corr)

my_sample<-covid_corr[sample(1:nrow(covid_subset), 10000, replace = FALSE),]
my_sample


pairs.panels(my_sample,
             smooth = TRUE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font    
             density = TRUE, # If TRUE, adds density plots and histograms    
             ellipses = TRUE, # If TRUE, draws ellipses    
             method = "spearman",# Correlation method (also "pearson" or "kendall")    
             pch = 21, # pch symbol    
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit    
             cor = TRUE, # If TRUE, reports correlations    
             jiggle = FALSE, # If TRUE, data points are jittered    
             factor = 2, # Jittering factor    
             hist.col = 4, # Histograms color    
             stars = TRUE, # If TRUE, adds significance level with stars    
             ci = TRUE) # If TRUE, adds confidence intervals   


# 0.92***
# plotting histograms to view if the variables are normally Distributed 

#arrange the plots in 1 rows by 2 cols
opar = par(no.readonly = TRUE)
par(mfrow = c(1,2))
par = opar

hist(total_deaths, col = "cyan", main = "distribution of life_expectancy" , 
     xlab = "life_expectancy")
hist(new_cases, col = "cyan", main = "distribution of human_development_index",
     xlab = "human_development_index")


# Using Quantile-quantile plot (Q-Q plot) allows us to check
# if the data is normally distributed or not 

#Islife_expectancy normally distributed?
with (covid_subset, {qqnorm (life_expectancy,
                             main = "Normal QQ-plot of life_expectancy",
                             xlab = "Theoritical Quantiles",
                             ylab = "Samples Quantiles")})
# Add line that represents normal distribution
qqline(life_expectancy, col = "red")
# life_expectancy appears not to be normally distributed


#Is human_development_index normally distributed?
with (covid_subset, {qqnorm (human_development_index,
                             main = "Normal QQ-plot of human_development_index",
                             xlab = "Theoritical Quantiles",
                             ylab = "Samples Quantiles")})

# Add line that represents normal distribution
qqline(new_cases, col = "red")
# human_development_index appears not to be normally distributed


# ------------------------------------------ shapiro-wilks test ---------------------------------------------------#
# we can run the formal test of normality provided through the widely used
# shapiro-wilks test

my_sample<-covid_subset[sample(1:nrow(covid_subset), 1000, replace = FALSE),]
my_sample

# normality test for life_expectancy
normality_test <- shapiro.test(my_sample$life_expectancy )
normality_test$p.value

#p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is not normally distributed

# In this example p-value= 0.0000000000000009363117
# 0.0000000000000009363117 > 0.05 (False)
# Therefore the var life_expectancy is not normally distributed

# normality test for new_cases
my_sample<-covid_subset[sample(1:nrow(covid_subset), 1000, replace = FALSE),]
my_sample

normality_test <- shapiro.test(my_sample$human_development_index)
normality_test$p.value

#p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is not normally distributed

# In this example p-value= 0.0000000000000001671074
# 0.0000000000000001671074 > 0.05 (False)
# Therefore the var human_development_index is not normally distributed


# Need to decide a test for calulating p-value
# Here, both the variablea are categorical nominal.
# The dependent variable is not normally distributed. 
# Hence going for a non parametric test i.e. 
# Chi-squared test 

chisq <- chisq.test(covid_subset$life_expectancy, covid_subset$human_development_index)
chisq

#Pearson's Chi-squared test

#data:  covid_subset$life_expectancy and covid_subset$human_development_index
#X-squared = 10923255, df = 26274, p-value < 0.00000000000000022

detach(covid_data)




#----------------------------------------- Research Question 5 ----------------------------------------------#
# Research Question 5: Does Stringency Index  impacted the new cases in Ireland?

# H0: Stringency Index does not impact the new cases in Ireland.
# H1: Stringency Index impacts the new cases in Ireland.

# Analyzing the variables used in each variable
# stringency_index = categorical nominal variable, represented as proportions
# new_cases = continuous interval variable 

#--------------------------------------------------------------------------------------------------------------#

# Using statistical methods to examine the relationship between our variables of interest
# creating a subset of covid_data for convenient hypothesis testing

attach(covid_data)
names(covid_data)

covid_subset <- subset(covid_data, location %in% c("Ireland"), 
                       select = c(iso_code, location, date, stringency_index, new_cases))
str(covid_subset)
head(covid_subset)
dim(covid_subset)
sum(is.na(covid_subset))

# Check for missing data
incomplete_data <- covid_subset[!complete.cases(covid_subset),]
nrow(incomplete_data)

#Using mice library to display NA values and its count
md.pattern(covid_subset)

# Using VIM library and displayed the missing values
missing_values <- aggr(covid_subset, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)

# ----------------------------------------------- Linearity check --------------------------------------------------------------#

# ChecK linearity of the variables 

attach(covid_subset)

plot(stringency_index, new_cases, pch = 9, col= "lightblue",
     main = "comparision of stringency_index with new_cases in Ireland",
     xlab = "stringency_index",
     ylab = "new_cases")


options(scipen = 999)
ggplot(covid_subset, aes(x=stringency_index,y=new_cases))+ geom_point(col="lightblue", size=3)


# we can also examine the linear correlation between both variables using Quantile-quantile plot (Q-Q plot)
with (covid_subset, {qqplot (life_expectancy, new_cases,
                             main = "comparision of stringency_index with new_cases in Ireland",
                             xlab = "stringency_index",
                             ylab = "new_cases")})

# Also using psych library to get correlation coefficient between the 2 variables
covid_corr <- subset(covid_subset,
                     select = c(stringency_index, new_cases))
head(covid_corr)
dim(covid_corr)

pairs.panels(covid_corr,
             smooth = TRUE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font    
             density = TRUE, # If TRUE, adds density plots and histograms    
             ellipses = TRUE, # If TRUE, draws ellipses    
             method = "spearman",# Correlation method (also "pearson" or "kendall")    
             pch = 21, # pch symbol    
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit    
             cor = TRUE, # If TRUE, reports correlations    
             jiggle = FALSE, # If TRUE, data points are jittered    
             factor = 2, # Jittering factor    
             hist.col = 4, # Histograms color    
             stars = TRUE, # If TRUE, adds significance level with stars    
             ci = TRUE) # If TRUE, adds confidence intervals   


# 0.60***
# ---------------------------- Normal Distribution ------------------------------------------------#

# Check whether the variables used for the hypothesis test are normally distributed or not. 
# Doing this visually and using a relevant statistical analysis test. 
# Then decide on which statistical test you will use.

# plotting histograms to view if the variables are normally Distributed 

#arrange the plots in 1 rows by 2 cols
opar = par(no.readonly = TRUE)
par(mfrow = c(1,2))


hist(stringency_index, col = "blue", main = "distribution of stringency_index" , 
     xlab = "stringency_index")
hist(new_cases, col = "blue", main = "distribution of new_cases",
     xlab = "new_cases")

par = opar

# Using Quantile-quantile plot (Q-Q plot) allows us to check
# if the data is normally distributed or not 

#Is stringency_index normally distributed?
with (covid_subset, {qqnorm (stringency_index,
                             main = "Normal QQ-plot of stringency_index in Ireland",
                             xlab = "Theoritical Quantiles",
                             ylab = "Samples Quantiles")})
# Add line that represents normal distribution
qqline(stringency_index, col = "red")
# stringency_index appears not to be normally distributed


#Is new_cases normally distributed?
with (covid_subset, {qqnorm (new_cases,
                             main = "Normal QQ-plot of new_cases in Ireland",
                             xlab = "Theoritical Quantiles",
                             ylab = "Samples Quantiles")})

# Add line that represents normal distribution
qqline(new_cases, col = "red")
# new_cases appears not to be normally distributed


# ------------------------------------------ shapiro-wilks test ---------------------------------------------------#
# we can run the formal test of normality provided through the widely used
# shapiro-wilks test

# normality test for life_expectancy
normality_test <- shapiro.test(covid_subset$stringency_index )
normality_test$p.value

#p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is not normally distributed

# In this example p-value= 0.00000000000000000001340043
# 0.00000000000000000001340043 > 0.05 (False)
# Therefore the var life_expectancy is not normally distributed

# normality test for new_cases
normality_test <- shapiro.test(covid_subset$new_cases)
normality_test$p.value

#p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is not normally distributed

# In this example p-value= 0.00000000000000000000000000000001248359
# 0.00000000000000000000000000000001248359 > 0.05 (False)
# Therefore the var new_cases is not normally distributed


# Need to decide a test for calulating p-value
# Here, one of the variable is continuous and other is categorical nominal.
# The dependent variable is not normally distributed. 
# Hence going for a non parametric test i.e. 
# Wilcoxon signed  rank test 


wilcox.test(stringency_index, new_cases)  

# p-value = 0.00000000000000022
# cut off = 0.05 

# 0.00000000000000022 < 0.05  (true)
# reject null/accept alternative
# hence reject H0 and accept H1




#======================================================= The End =======================================================#

