# ----------------------------- COVID-19 Predictive Modeling ------------------------------# 

# First, we created a new repository called CA2 in GitHub.
# Then created a new project in R.
# Get the current working directory
# The working directory can be set if required

setwd("C:\\Users\\deepa\\Documents\\R\\CA2")
getwd()

# ----------------------------------- Research Question --------------------------------# 

# Prediction of new_cases in Europe by analyzing the variables 
# like population, people_fully_vaccinated, stringency_index, handwashing_facilities, 
# aged_70_older, diabetes_prevalence, Smokers etc. in first quarter of year 2021.
# Further, there is also forecasting on new_deaths, seeing the trends in new_cases.

# We will load some libraries that we are going to use during the study.
install.packages("ggplot2")   # Install package for Data visualization
install.packages("mice")      # Install mice package and displayed the missing values
install.packages("VIM")       # Install VIM package and displayed the missing values
install.packages("psych")     # Install psych package to find correlations between multiple variables
install.packages("gvlma")
install.packages("leaps")

library(ggplot2)
library(mice)
library(VIM)
library(psych)
library(gvlma)
library(leaps)
library(car)
library(MASS)
library(e1071)

# ----------------------------------- Data Gathering --------------------------------# 

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

#----------------------------------------- Data Preparation -----------------------------------------------#

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

#-------------------------------------- Data Wrangling, Subsetting, Imputing -----------------------------------------------#
# As per the research question requirement, the concerned variables are selected
# from covid_data for the continent Europe in the first 4 months of 2021 i.e. (Jan-Apr)

attach(covid_data)
names(covid_data)

covid_EU <- subset(covid_data, continent %in% c("Europe") , 
                   select = c(iso_code, location, date, 
                              new_cases,
                              total_cases,
                              total_deaths,
                              new_deaths,
                              icu_patients,
                              hosp_patients,
                              new_tests,
                              total_tests,
                              people_fully_vaccinated,
                              population,
                              stringency_index,
                              aged_70_older,
                              diabetes_prevalence,
                              female_smokers,
                              male_smokers,
                              handwashing_facilities
                   )) 

covid_EU_2021 <- subset(covid_EU, format.Date(date, "%Y") == "2021" )

str(covid_EU_2021)
head(covid_EU_2021)
dim(covid_EU_2021)
sum(is.na(covid_EU_2021))
names(which(sapply(covid_EU_2021, anyNA)))    # Almost all the variables contains `NA`

# Check for missing data
incomplete_data <- covid_EU_2021[!complete.cases(covid_EU_2021),]
nrow(covid_EU_2021)

#Using mice library to display NA values and its count
md.pattern(covid_EU_2021, plot = TRUE, rotate.names = TRUE)

# Using VIM library and displayed the missing values
missing_values <- aggr(covid_EU_2021,cex.axis=.5, prop = FALSE, numbers = TRUE)


# show summary of the content of missing_values 
summary(missing_values)

attach(covid_EU_2021)
# The variable `new_cases` are the counts of new confirmed cases of covid-19 reported daily, country wise. 
# So it will not be wrong to consider the null values as no new cases in the country on particular date, 
# hence replacing NA values with 0 for this column. 

# Similarly, the variable `people_fully_vaccinated` is total number of people who received all doses prescribed by the vaccination protocol. 
# The NA cases for this variable would be considered as if a country has number of people
# fully vaccinated is none then this column takes NA as value, can be replaced by 0 for analysis purpose.

# Likewise, `total_cases`, `total_deaths`, `new_deaths`, `icu_patients`, `hosp_patients`,
# `new_tests`, `total_tests`, `aged_70_older`, `stringency_index`, `diabetes_prevalence`,
# `female_smokers`, `male_smokers`, and `handwashing_facilities` 
# NA values, are replaced by 0 for analysis purpose.

covid_EU_2021$total_cases[is.na(covid_EU_2021$total_cases)] <- 0
covid_EU_2021$new_cases[is.na(covid_EU_2021$new_cases)] <- 0
covid_EU_2021$total_deaths[is.na(covid_EU_2021$total_deaths)] <- 0
covid_EU_2021$new_deaths[is.na(covid_EU_2021$new_deaths)] <- 0
covid_EU_2021$people_fully_vaccinated[is.na(covid_EU_2021$people_fully_vaccinated)] <- 0

covid_EU_2021$icu_patients[is.na(covid_EU_2021$icu_patients)] <- 0
covid_EU_2021$hosp_patients[is.na(covid_EU_2021$hosp_patients)] <- 0
covid_EU_2021$new_tests[is.na(covid_EU_2021$new_tests)] <- 0
covid_EU_2021$total_tests[is.na(covid_EU_2021$total_tests)] <- 0

covid_EU_2021$aged_70_older[is.na(covid_EU_2021$aged_70_older)] <- 0
covid_EU_2021$stringency_index[is.na(covid_EU_2021$stringency_index)] <- 0
covid_EU_2021$diabetes_prevalence[is.na(covid_EU_2021$diabetes_prevalence)] <- 0
covid_EU_2021$female_smokers[is.na(covid_EU_2021$female_smokers)] <- 0
covid_EU_2021$male_smokers[is.na(covid_EU_2021$male_smokers)] <- 0
covid_EU_2021$handwashing_facilities[is.na(covid_EU_2021$handwashing_facilities)] <- 0


# Calculating the actual numbers for the variables having values 
# as share of total population provided in the data set description.
# Share of the population that is 70 years and older
covid_EU_2021$aged_70_older <- (covid_EU_2021$population  *  covid_EU_2021$aged_70_older)/100
covid_EU_2021$aged_70_older <- as.integer(covid_EU_2021$aged_70_older)

#Diabetes prevalence (% of population aged 20 to 79)
covid_EU_2021$diabetes_prevalence <- (covid_EU_2021$population  *  covid_EU_2021$diabetes_prevalence)/100
covid_EU_2021$diabetes_prevalence <- as.integer(covid_EU_2021$diabetes_prevalence)

# Share of the population with basic handwashing facilities on premises, most recent year available
covid_EU_2021$handwashing_facilities <- (covid_EU_2021$population  *  covid_EU_2021$handwashing_facilities)/100
covid_EU_2021$handwashing_facilities <- as.integer(covid_EU_2021$handwashing_facilities)

# Share of women who smoke, most recent year available
covid_EU_2021$female_smokers <- (covid_EU_2021$population  *  covid_EU_2021$female_smokers)/100
covid_EU_2021$female_smokers <- as.integer(covid_EU_2021$female_smokers)

#Share of men who smoke, most recent year available
covid_EU_2021$male_smokers <- (covid_EU_2021$population  *  covid_EU_2021$male_smokers)/100
covid_EU_2021$male_smokers <- as.integer(covid_EU_2021$male_smokers)

# Total number of smokers by adding the males and female smokers
covid_EU_2021$total_smokers <- covid_EU_2021$male_smokers + covid_EU_2021$female_smokers

str(covid_EU_2021)
head(covid_EU_2021)
dim(covid_EU_2021)
sum(is.na(covid_EU_2021))

# ----------------------------------------------- Linearity check --------------------------------------------------------------#

# Also using psych library to get correlation coefficient between the variables
# Then using the scatter plot with the dependent variable in the y axis 
# and an independent variable in the x axis. 
# If the relation appears to be linear, the assumption is validated.

#covid_corr <- subset(covid_subset,
#                    select = c(people_fully_vaccinated, new_cases))

#my_sample<-covid_corr[sample(1:nrow(covid_corr), 10000, replace = FALSE),]
#my_sample

#head(covid_corr)
#dim(covid_corr) 


library(psych)
pairs.panels(covid_EU_2021,
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

# This chart provides a general level of detail on linearity of the independent variable with the depend
# variables.
# It is observed that the covid-19 total cases may be bimodal and that each of the predictor variables is skewed
# to some extent.
# Impact on Total/New cases with population and people_fully_vaccinated, and they fall with stringency_index levels and handwashing_facilities.
# At the same time, colder states have lower illiteracy rates and population and higher incomes.
# We can check each one in more detail using a scatter plot. 

# Plot between the new_cases and new_deaths is really alarming. 
# There is a strong correlation of #0.9422073
options(scipen = 999)
scatter.smooth(x = covid_EU_2021$new_cases,
               y = covid_EU_2021$new_deaths,
               main = "new_cases ~ new_deaths",
               xlab = "new_cases",
               ylab = "new_deaths")

cor(covid_EU_2021$new_cases, covid_EU_2021$new_deaths) #0.9422073

# It is very important to analyze the reason for increasing new_cases and predictions to control the new_deaths.
# Here’s the independent variable new_cases
# plotted against the dependent variables people_fully_vaccinated

scatter.smooth(x = covid_EU_2021$new_cases,
               y = covid_EU_2021$people_fully_vaccinated,
               main = "new_cases ~ people_fully_vaccinated",
               xlab = "new_cases",
               ylab = "people_fully_vaccinated")

# The chart does not appear to be correlated. We can measure correlation using the cor() function.
# Recall that a low correlation (-0.2 < x < 0.2) suggests that much of variation of the response
# variable (Y) is unexplained by the predictor (X), in which case, we should probably look for better
# explanatory variables.

# Checking numerically the correlation of these variables
# Values of -0.2 < x < 0.2 - low correlation
cor(covid_EU_2021$new_cases, covid_EU_2021$people_fully_vaccinated) #0.6385971

# Medium correlation. Value = 0.6385971.
# The correlation test shows that the correlation between the new_cases and 
# people_fully_vaccinated variables = 0.6385971 indicating a medium correlation.


# Similarly, checking for other variables 

scatter.smooth(x = covid_EU_2021$new_cases,
               y = covid_EU_2021$population,
               main = "new_cases ~ population",
               xlab = "new_cases",
               ylab = "population")

cor(covid_EU_2021$new_cases, covid_EU_2021$population) #0.9356457

# High correlation. Value = 0.9356457
# The correlation test shows that the correlation between the new_cases and 
# population variables = 0.9356457 indicating a High correlation.


# Lets examine new_cases and stringency_index
scatter.smooth(x = covid_EU_2021$new_cases,
               y = covid_EU_2021$stringency_index,
               main = "new_cases ~ stringency_index",
               xlab = "new_cases",
               ylab = "stringency_index")

cor(covid_EU_2021$new_cases, covid_EU_2021$stringency_index) # -0.1706173

# This variable shows a negative correlation of -0.1706173
# It clearly shows that with the Government Response Stringency Index: 
#composite measure based on 9 response indicators including school closures, 
# workplace closures, and travel bans reduced the number of new cases

# Similarly, examining new_cases and handwashing_facilities
scatter.smooth(x = covid_EU_2021$new_cases,
               y = covid_EU_2021$handwashing_facilities,
               main = "new_cases ~ handwashing_facilities",
               xlab = "new_cases",
               ylab = "handwashing_facilities")
#summary(covid_EU_2021$handwashing_facilities)
#table(covid_EU_2021$handwashing_facilities)
cor(covid_EU_2021$new_cases, covid_EU_2021$handwashing_facilities) # -0.04546042

# Similarly, examining new_cases and icu_patients
scatter.smooth(x = covid_EU_2021$new_cases,
               y = covid_EU_2021$icu_patients,
               main = "new_cases ~ icu_patients",
               xlab = "new_cases",
               ylab = "icu_patients")

cor(covid_EU_2021$new_cases, covid_EU_2021$icu_patients) # 0.1381871

# Similarly, examining new_cases and hosp_patients
scatter.smooth(x = covid_EU_2021$new_cases,
               y = covid_EU_2021$hosp_patients,
               main = "new_cases ~ hosp_patients",
               xlab = "new_cases",
               ylab = "hosp_patients")

cor(covid_EU_2021$new_cases, covid_EU_2021$hosp_patients) # 0.1488507

# Similarly, examining new_cases and aged_70_older
scatter.smooth(x = covid_EU_2021$new_cases,
               y = covid_EU_2021$aged_70_older,
               main = "new_cases ~ aged_70_older",
               xlab = "new_cases",
               ylab = "aged_70_older")

cor(covid_EU_2021$new_cases, covid_EU_2021$aged_70_older) # 0.1125412

# Similarly, examining new_cases and diabetes_prevalence
scatter.smooth(x = covid_EU_2021$new_cases,
               y = covid_EU_2021$diabetes_prevalence,
               main = "new_cases ~ diabetes_prevalence",
               xlab = "new_cases",
               ylab = "diabetes_prevalence")

cor(covid_EU_2021$new_cases, covid_EU_2021$diabetes_prevalence) # 0.09135375

# Similarly, examining new_cases and total_smokers
scatter.smooth(x = covid_EU_2021$new_cases,
               y = covid_EU_2021$total_smokers,
               main = "new_cases ~ total_smokers",
               xlab = "new_cases",
               ylab = "total_smokers")

cor(covid_EU_2021$new_cases, covid_EU_2021$total_smokers) # 0.09356765

# similarly, the correlation is checked between newcases and other variables

cor(covid_EU_2021$new_cases, covid_EU_2021$icu_patients) # 0.1381871
cor(covid_EU_2021$new_cases, covid_EU_2021$hosp_patients) # 0.1488507
cor(covid_EU_2021$new_cases, covid_EU_2021$new_tests) # 0.05560787
cor(covid_EU_2021$new_cases, covid_EU_2021$total_tests) # 0.0389979

cor(covid_EU_2021$new_cases, covid_EU_2021$aged_70_older) # 0.1125412
cor(covid_EU_2021$total_cases, covid_EU_2021$aged_70_older) # 0.13300941
cor(covid_EU_2021$new_deaths, covid_EU_2021$aged_70_older) # 0.1312088
cor(covid_EU_2021$total_deaths, covid_EU_2021$aged_70_older) # 0.1474991

cor(covid_EU_2021$new_cases, covid_EU_2021$diabetes_prevalence) # 0.09135375
cor(covid_EU_2021$total_cases, covid_EU_2021$diabetes_prevalence) # 0.1176815
cor(covid_EU_2021$total_cases, covid_EU_2021$total_smokers) # 0.1241381

# We can also examine all other correlations using the cor() function.

paste("Correlation for new_cases and population: ", cor(covid_EU_2021$new_cases, covid_EU_2021$population))
paste("Correlation for new_cases and people_fully_vaccinated: ", cor(covid_EU_2021$new_cases, covid_EU_2021$people_fully_vaccinated))
paste("Correlation for new_cases and stringency_index: ", cor(covid_EU_2021$new_cases, covid_EU_2021$stringency_index))
paste("Correlation for new_cases and handwashing_facilities: ", cor(covid_EU_2021$new_cases, covid_EU_2021$handwashing_facilities))

paste("Correlation for new_cases and icu_patients: ", cor(covid_EU_2021$new_cases, covid_EU_2021$icu_patients))
paste("Correlation for new_cases and hosp_patients: ", cor(covid_EU_2021$new_cases, covid_EU_2021$hosp_patients))
paste("Correlation for new_cases and new_tests: ", cor(covid_EU_2021$new_cases, covid_EU_2021$new_tests))
paste("Correlation for new_cases and total_tests: ", cor(covid_EU_2021$new_cases, covid_EU_2021$total_tests))
paste("Correlation for new_cases and aged_70_older: ", cor(covid_EU_2021$new_cases, covid_EU_2021$aged_70_older))
paste("Correlation for new_cases and diabetes_prevalence: ", cor(covid_EU_2021$new_cases, covid_EU_2021$diabetes_prevalence))
paste("Correlation for new_cases and total_smokers: ", cor(covid_EU_2021$new_cases, covid_EU_2021$total_smokers))


# "Correlation for new_cases and population:  0.935645664980254"
# "Correlation for new_cases and people_fully_vaccinated:  0.63859714086781"
# "Correlation for new_cases and stringency_index:  -0.170617307822532"
# "Correlation for new_cases and handwashing_facilities:  -0.0454604198937569"
# "Correlation for new_cases and icu_patients:  0.1381871"
# "Correlation for new_cases and hosp_patients:  0.1488507"

# "Correlation for new_cases and new_tests:  0.0556078745519732"
# "Correlation for new_cases and total_tests:  0.0389979033826804"

# "Correlation for new_cases and aged_70_older:  0.1125412"
# "Correlation for new_cases and diabetes_prevalence:  0.09135375"
# "Correlation for new_cases and total_smokers:  0.09356765"


# It appears that the variable  new_tests, total_tests  has a vary low correlation with new_cases.
# Therefore, let's remove it from the dataset. Alternatively we can choose to exclude these dependent variables when
# we are constructing the linear model.

covid_EU_2021 <- subset(covid_EU_2021, select = -c( new_tests, total_tests))
head(covid_EU_2021)
detach (covid_EU_2021)

# ----------------------------------------------- Outliers --------------------------------------------------------------#

# Checking for outliers
# Generally, any data point that lies outside the **1.5 * interquartile-range (1.5 * IQR)** is considered
# an outlier. Examining all variables in the dataset.
attach(covid_EU_2021)
dim(covid_EU_2021)
min(covid_EU_2021$new_cases)
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # divide graph area in 3 rows by 2 columns
#par<-opar

# box plot for 'new_cases'
boxplot(new_cases,
        main = "new_cases",
        sub = paste("Outlier rows: ",
                    boxplot.stats(new_cases)$out)) 

# box plot for 'population'
boxplot(population,
        main = "population",
        sub = paste("Outlier rows: ",
                    boxplot.stats(population)$out))

# box plot for 'people_fully_vaccinated'
boxplot(people_fully_vaccinated,
        main = "people_fully_vaccinated",
        sub = paste("Outlier rows: ",
                    boxplot.stats(people_fully_vaccinated)$out)) 

# box plot for 'stringency_index'
boxplot(stringency_index,
        main = "stringency_index",
        sub = paste("Outlier rows: ",
                    boxplot.stats(stringency_index)$out)) 

# Outlier rows: 0 

# box plot for 'handwashing_facilities'
boxplot(handwashing_facilities,
        main = "handwashing_facilities",
        sub = paste("Outlier rows: ",
                    boxplot.stats(handwashing_facilities)$out))


# box plot for 'icu_patients'
boxplot(icu_patients,
        main = "icu_patients",
        sub = paste("Outlier rows: ",
                    boxplot.stats(icu_patients)$out)) 

# box plot for 'hosp_patients'
boxplot(hosp_patients,
        main = "hosp_patients",
        sub = paste("Outlier rows: ",
                    boxplot.stats(hosp_patients)$out))

# box plot for 'aged_70_older'
boxplot(aged_70_older,
        main = "aged_70_older",
        sub = paste("Outlier rows: ",
                    boxplot.stats(aged_70_older)$out))


# box plot for 'diabetes_prevalence'
boxplot(diabetes_prevalence,
        main = "diabetes_prevalence",
        sub = paste("Outlier rows: ",
                    boxplot.stats(diabetes_prevalence)$out)) 

# box plot for 'total_smokers'
boxplot(total_smokers,
        main = "total_smokers",
        sub = paste("Outlier rows: ",
                    boxplot.stats(total_smokers)$out))

str(covid_EU_2021)
detach(covid_EU_2021)
par(opar)
# ------------------------------ outliers stats --------------------------------#
# Use boxplot.stats() function to generate relevant outliers for new_cases
outlier_values <- boxplot.stats(covid_EU_2021$new_cases)$out 
paste("new_cases: ", paste(outlier_values, collapse=", "))

# ’new_cases outliers: Too many, cannot remove all’

# Repeating for other variables with identified outliers.

# Use boxplot.stats() function to generate relevant outliers for people_fully_vaccinated
outlier_values <- boxplot.stats(covid_EU_2021$people_fully_vaccinated)$out 
paste("people_fully_vaccinated outliers: ", paste(outlier_values, collapse=", "))

# people_fully_vaccinated outliers: Too many, cannot remove all’

# Use boxplot.stats() function to generate relevant outliers for population
outlier_values <- boxplot.stats(covid_EU_2021$population)$out 
paste("population outliers: ", paste(outlier_values, collapse=", "))

# population outliers: Too many, cannot remove all’

# Use boxplot.stats() function to generate relevant outliers for stringency_index
outlier_values <- boxplot.stats(covid_EU_2021$stringency_index)$out 
paste("stringency_index outliers: ", paste(outlier_values, collapse=", "))

# stringency_index outliers: 0’

# Use boxplot.stats() function to generate relevant outliers for handwashing_facilities
outlier_values <- boxplot.stats(covid_EU_2021$handwashing_facilities)$out 
paste("handwashing_facilities outliers: ", paste(outlier_values, collapse=", "))

# handwashing_facilities outliers: Too many, cannot remove all’

# Use boxplot.stats() function to generate relevant outliers for icu_patients
outlier_values <- boxplot.stats(covid_EU_2021$icu_patients)$out 
paste("icu_patients outliers: ", paste(outlier_values, collapse=", "))

# Use boxplot.stats() function to generate relevant outliers for hosp_patients
outlier_values <- boxplot.stats(covid_EU_2021$hosp_patients)$out 
paste("hosp_patients outliers: ", paste(outlier_values, collapse=", "))

# Use boxplot.stats() function to generate relevant outliers for aged_70_older
outlier_values <- boxplot.stats(covid_EU_2021$aged_70_older)$out 
paste("aged_70_older outliers: ", paste(outlier_values, collapse=", "))

# aged_70_older outliers: Too many, cannot remove all’

# Use boxplot.stats() function to generate relevant outliers for diabetes_prevalence
outlier_values <- boxplot.stats(covid_EU_2021$diabetes_prevalence)$out 
paste("diabetes_prevalence outliers: ", paste(outlier_values, collapse=", "))

# diabetes_prevalence outliers: Too many, cannot remove all’

# Use boxplot.stats() function to generate relevant outliers for total_smokers
outlier_values <- boxplot.stats(covid_EU_2021$total_smokers)$out 
paste("total_smokers outliers: ", paste(outlier_values, collapse=", "))

# total_smokers outliers: Too many, cannot remove all’

# -------------------------- Remove Outliers -------------------------------#
# Now remove all of these outliers using this R code.
# Need to revisit the outliers check by using some other methods
# Remove new_cases outliers
#covid_EU_2021 <- subset(covid_EU_2021,
#                       covid_EU_2021$new_cases != -74347
#                       & covid_EU_2021$new_cases != 131033
#                       & covid_EU_2021$new_cases != 199224
#                       & covid_EU_2021$new_cases != 262816
#                       & covid_EU_2021$new_cases != 269592 
#                       & covid_EU_2021$new_cases != 274846
#                       & covid_EU_2021$new_cases != 275691
#                       & covid_EU_2021$new_cases != 282881
#                       & covid_EU_2021$new_cases != 320755)

covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$new_cases > 0
                        & covid_EU_2021$new_cases < 4000
)
#summary(covid_EU_2021$new_cases)

plot(density(covid_EU_2021$new_cases),
     main = "Density plot : new_cases",
     ylab = "Frequency", xlab = "new_cases",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$new_cases), 2)))



# Remove population  outliers
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$population  < 16948445)




# density plot for population
plot(density(covid_EU_2021$population),
     main = "Density plot : population",
     ylab = "Frequency", xlab = "population",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$population), 2)))

# Remove people_fully_vaccinated outliers

covid_EU_20213 <- subset(covid_EU_2021, 
                         covid_EU_2021$people_fully_vaccinated < 300000)
#& covid_EU_2021$people_fully_vaccinated > 0)



summary(covid_EU_2021$people_fully_vaccinated)

# density plot for people_fully_vaccinated
plot(density(covid_EU_20213$people_fully_vaccinated),
     main = "Density plot : people_fully_vaccinated",
     ylab = "Frequency", xlab = "people_fully_vaccinated",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_20213$people_fully_vaccinated), 2)))




# Remove stringency_index  outliers
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$stringency_index  != 0
                        & covid_EU_2021$stringency_index  != 22.22)


# density plot for stringency_index
plot(density(covid_EU_2021$stringency_index),
     main = "Density plot : stringency_index",
     ylab = "Frequency", xlab = "stringency_index",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$stringency_index), 2)))



# Remove handwashing_facilities  outliers
covid_EU_20216 <- subset(covid_EU_2021,
                         covid_EU_2021$handwashing_facilities > 1000)

str(covid_EU_2021$handwashing_facilities)
summary(covid_EU_2021$handwashing_facilities)

# density plot for handwashing_facilities
plot(density(covid_EU_20216$handwashing_facilities ),
     main = "Density plot : handwashing_facilities",
     ylab = "Frequency", xlab = "handwashing_facilities",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_20216$handwashing_facilities), 2)))


# Remove icu_patients  outliers
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$icu_patients <400)



summary(covid_EU_2021$icu_patients)

# density plot for icu_patients
plot(density(covid_EU_2021$icu_patients ),
     main = "Density plot : icu_patients",
     ylab = "Frequency", xlab = "icu_patients",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$icu_patients), 2)))

# Remove hosp_patients  outliers
covid_EU_20218 <- subset(covid_EU_2021,
                         covid_EU_2021$hosp_patients < 750)
#& covid_EU_2021$hosp_patients > 0)

summary(covid_EU_2021$hosp_patients)

# density plot for hosp_patients
plot(density(covid_EU_2021$hosp_patients ),
     main = "Density plot : hosp_patients",
     ylab = "Frequency", xlab = "hosp_patients",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$hosp_patients), 2)))


# Remove aged_70_older  outliers
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$aged_70_older  != 13707623
                        & covid_EU_2021$aged_70_older  != 13369404
                        & covid_EU_2021$aged_70_older  != 9819000
                        & covid_EU_2021$aged_70_older  != 8913035
                        & covid_EU_2021$aged_70_older  != 8504079
                        & covid_EU_2021$aged_70_older  != 6451692
                        & covid_EU_2021$aged_70_older  != 4868879
                        & covid_EU_2021$aged_70_older  != 3861110)

# Remove diabetes_prevalence  outliers
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$diabetes_prevalence  != 9018749
                        & covid_EU_2021$diabetes_prevalence  != 1873750)

# Remove total_smokers  outliers
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$total_smokers  != 73029)


# --------------------------------- Normality and Skewness check ------------------------------------------------

detach(covid_EU_2021)
attach(covid_EU_2021)

dim(covid_EU_2021)

# Check for normality
# Skewness function to examine normality
install.packages("e1071")

#library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2)) # divide graph area into 1 row x 2 cols
#par <- opar

# minimally skewed to the left
# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symmetrical


# density plot for new_cases
plot(density(covid_EU_2021$new_cases),
     main = "Density plot : new_cases",
     ylab = "Frequency", xlab = "new_cases",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$new_cases), 2)))

# fill the area under the plot
polygon(density(covid_EU_2021$new_cases), col = "red")

# density plot for population
plot(density(covid_EU_2021$population),
     main = "Density plot : population",
     ylab = "Frequency", xlab = "population",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$population), 2)))

# fill the area under the plot for population
polygon(density(covid_EU_2021$population), col = "red")

# density plot for people_fully_vaccinated
plot(density(covid_EU_2021$people_fully_vaccinated),
     main = "Density plot : people_fully_vaccinated",
     ylab = "Frequency", xlab = "people_fully_vaccinated",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$people_fully_vaccinated), 2)))

# fill the area under the plot	 
polygon(density(covid_EU_2021$people_fully_vaccinated), col = "red")

# density plot for stringency_index
plot(density(covid_EU_2021$stringency_index),
     main = "Density plot : stringency_index",
     ylab = "Frequency", xlab = "stringency_index",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$stringency_index), 2)))

# fill the area under the plot
polygon(density(covid_EU_2021$stringency_index ), col = "red")

# density plot for handwashing_facilities
plot(density(covid_EU_2021$handwashing_facilities ),
     main = "Density plot : handwashing_facilities",
     ylab = "Frequency", xlab = "handwashing_facilities",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$handwashing_facilities), 2)))

# fill the area under the plot	 
polygon(density(covid_EU_2021$handwashing_facilities), col = "red")

# density plot for icu_patients
plot(density(covid_EU_2021$icu_patients ),
     main = "Density plot : icu_patients",
     ylab = "Frequency", xlab = "icu_patients",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$icu_patients), 2)))

# fill the area under the plot for icu_patients
polygon(density(covid_EU_2021$icu_patients), col = "red")

# density plot for hosp_patients
plot(density(covid_EU_2021$hosp_patients ),
     main = "Density plot : hosp_patients",
     ylab = "Frequency", xlab = "hosp_patients",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$hosp_patients), 2)))

# fill the area under the plot for hosp_patients	 
polygon(density(covid_EU_2021$hosp_patients), col = "red")

# density plot for aged_70_older
plot(density(covid_EU_2021$aged_70_older ),
     main = "Density plot : aged_70_older",
     ylab = "Frequency", xlab = "aged_70_older",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$aged_70_older), 2)))

# fill the area under the plot for aged_70_older	 
polygon(density(covid_EU_2021$aged_70_older), col = "red")

# density plot for diabetes_prevalence
plot(density(covid_EU_2021$diabetes_prevalence ),
     main = "Density plot : diabetes_prevalence",
     ylab = "Frequency", xlab = "diabetes_prevalence",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$diabetes_prevalence), 2)))

# fill the area under the plot for diabetes_prevalence
polygon(density(covid_EU_2021$diabetes_prevalence), col = "red")

# density plot for total_smokers
plot(density(covid_EU_2021$total_smokers ),
     main = "Density plot : total_smokers",
     ylab = "Frequency", xlab = "total_smokers",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$total_smokers), 2)))

# fill the area under the plot for total_smokers 
polygon(density(covid_EU_2021$total_smokers), col = "red")

par <- opar

# It is difficult to read all skewness values. We can display them numerically using the following code.

paste("Skewness for new_cases: ", round(e1071::skewness(covid_EU_2021$new_cases), 2))
paste("Skewness for population: ", round(e1071::skewness(covid_EU_2021$population), 2))
paste("Skewness for people_fully_vaccinated : ", round(e1071::skewness(covid_EU_2021$people_fully_vaccinated),2))
paste("Skewness for stringency_index)) : ", round(e1071::skewness(covid_EU_2021$stringency_index),2))
paste("Skewness for handwashing_facilities)) : ", round(e1071::skewness(covid_EU_2021$handwashing_facilities), 2))

paste("Skewness for icu_patients: ", round(e1071::skewness(covid_EU_2021$icu_patients), 2))
paste("Skewness for hosp_patients: ", round(e1071::skewness(covid_EU_2021$hosp_patients), 2))
paste("Skewness for aged_70_older : ", round(e1071::skewness(covid_EU_2021$aged_70_older),2))
paste("Skewness for diabetes_prevalence)) : ", round(e1071::skewness(covid_EU_2021$diabetes_prevalence),2))
paste("Skewness for total_smokers)) : ", round(e1071::skewness(covid_EU_2021$total_smokers), 2))

# "Skewness for new_cases:  3.39"
# "Skewness for population:  0.67"
# "Skewness for people_fully_vaccinated :   2.88"
# "Skewness for stringency_index)) :  -0.38"
# "Skewness for handwashing_facilities)) :  5.29"
# "Skewness for icu_patients:  3.21"
# "Skewness for hosp_patients:  2.7"
# "Skewness for aged_70_older :   0.67"
# "Skewness for diabetes_prevalence)) :  0.81"
# "Skewness for total_smokers)) :  0.8"

# Here’s the standard metrics for skewness.
# Minimal skewness = -0.11 - slightly skewed to the left. 
# NB a skewness value <-1 or >1 = highly skewed. 
# Skewness -1 to -05 and 0.5 to 1 = moderately skewed. 
# Skewness -0.5 to 0-5 = approx symmetrical.

# Now lets use a scatterplot to see if the distribution of data points could be described with a straight
# line. Using a scatter plot with the dependent variable in the y axis and an independent variable
# in the x axis. If the relation appears to be linear, the assumption is validated.
# Also examine normality using the qqnorm() function and a histogram of the data for
# distribution


opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # divide graph area in 2 columns

hist(covid_EU_2021$new_cases, main = "Normality proportion of new_cases", xlab = "new_cases")
qqnorm(covid_EU_2021$new_cases)
qqline(covid_EU_2021$new_cases)
par <- opar

attach(covid_EU_2021)

#Training and testing datasets
set.seed(1)
no_rows_data <- nrow(covid_EU_2021)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)
training_data <- covid_EU_2021[sample, ]
testing_data <- covid_EU_2021[-sample, ]


# Now that the data variables are examined, so build the multiple regresion model.
# Building the MLR model

fit <- lm(new_cases ~ people_fully_vaccinated + population + stringency_index + handwashing_facilities +
                  icu_patients + hosp_patients + aged_70_older + diabetes_prevalence +
                  total_smokers, data=training_data)


# Lets have a look at the the results in detail, we use the summary() function on the model object:
# 

summary(fit)

# AIC and BIC
AIC(fit) #44442.59
BIC(fit) #44500.84

# Model summary
confint(fit)

#-------------------------------- Regression diagnostics -----------------------------#
# Normality and studentized residuals
# use methods available through the car package to do this analysis.
str(covid_EU_2021)
#library(car)
qqPlot(fit, labels=row.names(location), id.method="identify", 
       simulate=TRUE, main="Q-Q Plot")

#73581 73672  
#  593  1611 

training_data[593,]
training_data[1611,]

# The fitted() function extracts fitted values from objects returned by modeling functions. 
# It returns the predicted new cases rate for a particular state.

fitted(fit)[593]  #73581  4070.025  
fitted(fit)[1611]  #73672  3650.808 

dim(covid_EU_2021)
# Remove diabetes_prevalence  outliers
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$location != "Sweden"
                        & covid_EU_2021$date != "2021-01-05")
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$location != "Sweden"
                        & covid_EU_2021$date != "2021-04-06")

#Split the data into Training and testing datasets
set.seed(1)
no_rows_data <- nrow(covid_EU_2021)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)
training_data <- covid_EU_2021[sample, ]
testing_data <- covid_EU_2021[-sample, ]

#Then rebuild the MLR model again
fit <- lm(new_cases ~ people_fully_vaccinated + population + stringency_index + handwashing_facilities +
                  icu_patients + hosp_patients + aged_70_older + diabetes_prevalence +
                  total_smokers, data=training_data)
outlierTest(fit)

#We can view these errors using a histogram. This code generates a histogram of the studentized
#residuals and superimposes a normal curve, kernel-density curve, and rug plot.

student_fit <- rstudent(fit)
hist(student_fit,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")
rug(jitter(student_fit), col="brown")
curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE,col="blue", lwd=2)
lines(density(student_fit)$x, density(student_fit)$y, col="red", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)

# The car package also provides a statistical test for outliers. The outlierTest() function reports
# the Bonferroni adjusted p-value for the largest absolute studentized residual:


#Linearity
crPlots(fit)

# Influential observations

cutoff <- 4/(nrow(training_data) - length(fit$coefficients) - 2)
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

#library(car)
avPlots(fit, ask=FALSE)


# Influence plot

#library(car)
influencePlot(fit, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

# Homoscedasticity
ncvTest(fit)

# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 4186.282, Df = 1, p = < 2.22e-16

# The score test is nonsignificant (p = 0.45229), suggesting that we’ve met the constant variance
# assumption. If the p value is significant (p < 0.05), we would assume that the error variance
# changes with the level of the fitted values.

spreadLevelPlot(fit)

# Suggested power transformation:  0.07075301 
# Warning message:
#        In spreadLevelPlot.lm(fit) : 
#        726 negative fitted values removed


# Global validation of linear model assumption

#library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)


# Multicollinearity

#library(car)
vif(fit)

#people_fully_vaccinated              population        stringency_index 
#1.993006                2.100581                1.256987 
#handwashing_facilities            icu_patients           hosp_patients 
#1.062101                3.509519                2.636406 
#aged_70_older     diabetes_prevalence           total_smokers 
#23.737084               39.695411               19.063749 


# We can check whether any of the variables indicate a multicollinearity problem
# if the value > 2
sqrt(vif(fit)) > 2

#people_fully_vaccinated              population        stringency_index 
#FALSE                   FALSE                   FALSE 
#handwashing_facilities            icu_patients           hosp_patients 
#FALSE                   FALSE                   FALSE 
#aged_70_older     diabetes_prevalence           total_smokers 
#TRUE                    TRUE                    TRUE 

# Transforming variables

#library(car)
summary(powerTransform(training_data$new_cases))
summary(powerTransform(training_data$population))

#fit <- lm(new_cases ~ people_fully_vaccinated + population + stringency_index + handwashing_facilities, data=training_data)

# Comparing models using AIC

#Transform Murder varaible as indicated by spreadLevelPlot() function
sqrt_transform_new_cases <- sqrt(training_data$new_cases)
training_data$new_cases_sqrt <- sqrt_transform_new_cases
training_data$new_cases_sqrt

fit_model1 <- lm(new_cases ~ people_fully_vaccinated + population + 
                         stringency_index + handwashing_facilities + 
                         icu_patients + hosp_patients + aged_70_older +  
                         diabetes_prevalence + total_smokers, data=training_data)
fit_model2 <- lm(new_cases_sqrt ~ people_fully_vaccinated + population + 
                         stringency_index + handwashing_facilities + 
                         icu_patients + hosp_patients + aged_70_older +  
                         diabetes_prevalence + total_smokers, data=training_data)
AIC(fit_model1,fit_model2)


spreadLevelPlot(fit_model1) # Suggested power transformation:  0.3712377
spreadLevelPlot(fit_model2) # Suggested power transformation:  0.6918342 


# Comparing multiple models
# STEPWISE REGRESSION

#library(MASS)
fit_test <- lm(new_cases ~ people_fully_vaccinated + population + 
                       stringency_index + handwashing_facilities + 
                       icu_patients + hosp_patients + aged_70_older +  
                       diabetes_prevalence + total_smokers, data=training_data)
stepAIC(fit_test, direction="backward")


#library(leaps)
leaps <-regsubsets(new_cases ~ people_fully_vaccinated + population + 
                           stringency_index + handwashing_facilities + 
                           icu_patients + hosp_patients + aged_70_older +  
                           diabetes_prevalence + total_smokers, data=training_data, nbest=4)
plot(leaps, scale="adjr2")


#library(MASS)
fit_test <- lm(new_cases_sqrt ~ people_fully_vaccinated + population + 
                       stringency_index + handwashing_facilities + 
                       icu_patients + hosp_patients + aged_70_older +  
                       diabetes_prevalence + total_smokers, data=training_data)
stepAIC(fit_test, direction="backward")


#library(leaps)
leaps <-regsubsets(new_cases_sqrt ~ people_fully_vaccinated + population + 
                           stringency_index + handwashing_facilities + 
                           icu_patients + hosp_patients + aged_70_older +  
                           diabetes_prevalence + total_smokers, data=training_data, nbest=4)
plot(leaps, scale="adjr2")


# Lets examine predicted accuracy.

fit_model <- lm(new_cases ~ people_fully_vaccinated + population + 
                        stringency_index + handwashing_facilities + 
                        icu_patients + hosp_patients + aged_70_older +  
                        diabetes_prevalence + total_smokers, data=training_data)

fit_model_sqrt <- lm(new_cases_sqrt ~ people_fully_vaccinated + population + 
                             stringency_index + handwashing_facilities + 
                             icu_patients + hosp_patients + aged_70_older +  
                             diabetes_prevalence + total_smokers, data=training_data)

predicted_new_cases <- predict(fit_model, testing_data)
predicted_new_cases_sqrt <- predict(fit_model_sqrt, testing_data)
converted_new_cases_sqrt <- predicted_new_cases_sqrt ^2
converted_new_cases_sqrt


# make actuals_predicted dataframe.
actuals_predictions <- data.frame(cbind(actuals = testing_data$new_cases, predicted = predicted_new_cases))
head(actuals_predictions)

# make actuals_predicted dataframe for sqrt(Murder)
actuals_predictions_sqrt <- data.frame(cbind(actuals = testing_data$new_cases, predicted = converted_new_cases_sqrt))
head(actuals_predictions_sqrt)

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy
#actuals predicted
#actuals   1.000000  0.796065
#predicted 0.796065  1.000000

correlation_accuracy <- cor(actuals_predictions_sqrt)
correlation_accuracy
#actuals predicted
#actuals   1.0000000 0.8095174
#predicted 0.8095174 1.0000000


# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
min_max_accuracy

# 0.4590679

# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_predictions_sqrt, 1, min) / apply(actuals_predictions_sqrt, 1, max))
min_max_accuracy

# 0.5001728

# Residual Standard Error (RSE), or sigma

sigma(fit_model)/ mean(testing_data$new_cases)
# 0.8842141


sigma(fit_model_sqrt)/ mean(testing_data$new_cases)
# 0.008234087
# This estimates an error rate of 4% with the data we have at hand.

# Run some output with the final model
detach (covid_EU_2021)
summary(covid_EU_2021)
attach (covid_EU_2021)

df <- data.frame(people_fully_vaccinated = c(695854), population = c(29406628), 
                 stringency_index = c(88.89), handwashing_facilities = c(6649383),
                 icu_patients = c(361.9), hosp_patients = c(2497),
                 aged_70_older = c(1806649), diabetes_prevalence = c(912165),
                 total_smokers = c(9055349))
predicted_new_cases <- predict(fit_model, df)
predicted_new_cases

# 8783.776 

df <- data.frame(people_fully_vaccinated = c(695854), population = c(29406628), 
                 stringency_index = c(88.89), handwashing_facilities = c(6649383),
                 icu_patients = c(361.9), hosp_patients = c(2497),
                 aged_70_older = c(1806649), diabetes_prevalence = c(912165),
                 total_smokers = c(9055349))
predicted_new_cases <- predict(fit_model_sqrt, df)
predicted_new_cases

# 140.0911 

df <- data.frame(people_fully_vaccinated = c(695854), population = c(29406628), 
                 stringency_index = c(52.42), handwashing_facilities = c(261640),
                 icu_patients = c(361.9), hosp_patients = c(2497),
                 aged_70_older = c(1806649), diabetes_prevalence = c(912165),
                 total_smokers = c(9055349))
predicted_new_cases <- predict(fit_model, df)
predicted_new_cases

# 8495.424


df <- data.frame(people_fully_vaccinated = c(695854), population = c(29406628), 
                 stringency_index = c(52.42), handwashing_facilities = c(261640),
                 icu_patients = c(361.9), hosp_patients = c(2497),
                 aged_70_older = c(1806649), diabetes_prevalence = c(912165),
                 total_smokers = c(9055349))
predicted_new_cases <- predict(fit_model_sqrt, df)
predicted_new_cases

# 48.21254