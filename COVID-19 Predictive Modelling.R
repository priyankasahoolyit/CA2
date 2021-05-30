# --------------------------------- COVID-19 Predictive Modeling ------------------------------# 

# First, we created a new repository called CA2 in GitHub.
# Then created a new project in R.
# Get the current working directory
# The working directory can be set if required

setwd("C:\\Users\\deepa\\Documents\\R\\CA2")
getwd()

# ------------------------------------------ Research Question -----------------------------------# 

# Prediction of new_cases in Europe by analyzing the variables 
# like population, people_fully_vaccinated, stringency_index, handwashing_facilities, 
# aged_70_older, diabetes_prevalence, Smokers etc. in first quarter of year 2021.
# Further, there is also forecasting on new_deaths, seeing the trends in new_cases.

# We will load some libraries that we are going to use during the study.
install.packages("ggplot2")   # Install package for Data visualization
install.packages("mice")      # Install mice package and displayed the missing values
install.packages("VIM")       # Install VIM package and displayed the missing values
install.packages("psych")     # Install psych package to find correlations coefficients between multiple variables
install.packages("gvlma")     # Install package designed to detect skewness, kurtosis, a nonlinear link function, and heteroscedasticity
install.packages("leaps")     # performs an exhaustive search for the best subsets of the variables in x for predicting y in
                              # linear regression, using an efficient branch-and-bound algorithm.
install.packages("e1071")     # Install package for computation power for decision and probability values for predictions

library(ggplot2)
library(mice)
library(VIM)
library(psych)
library(gvlma)
library(leaps)
library(car)
library(MASS)
library(e1071)

# ------------------------------------ Data Gathering ---------------------------------------------# 

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

#----------------------------------------- Data Preparation ------------------------------------------#

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

#------------------------------------------- Data Sub-setting ---------------------------------------#

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
                              new_tests_smoothed,
                              new_tests,
                              total_tests,
                              positive_rate,
                              tests_per_case,
                              new_vaccinations_smoothed,
                              people_fully_vaccinated,
                              population,
                              population_density,
                              stringency_index,
                              aged_70_older,
                              diabetes_prevalence,
                              female_smokers,
                              male_smokers,
                              handwashing_facilities,
                              life_expectancy,
                              human_development_index
                   )) 

covid_EU_2021 <- subset(covid_EU, format.Date(date, "%Y") == "2021" )

str(covid_EU_2021)            # Check the structure of data frame
head(covid_EU_2021)           # Displays first 5 rows of data frame
dim(covid_EU_2021)            # Provides the attributes on the data frame,
                              # i.e. 5968 observations and 26 columns

# -------------------------------------- Identifying the missing values------------------------------------#

# Lets find out if there are any NA's in the data
# Using na.omit() to store any full rows into new_data frame
final_df<-na.omit(covid_EU_2021)
dim(final_df)

# It is observed that there are missing data in all the records, 
# so na.omit() function is dropping all the rows from the data frame.
# Hence, not an option to proceed with. 



# complete.cases() returns a vector with no missing values, can be swapped by using the `!`
# Using complete.cases() to show all complete rows store in complete_data
# and `!` complete_cases() for missing_data accordingly.
# Then using nrow() to show a total of all complete and missing rows
complete_data <-covid_EU_2021[complete.cases(covid_EU_2021),]
nrow(complete_data)
missing_data <-covid_EU_2021[!complete.cases(covid_EU_2021),]
nrow(missing_data)

nrow(complete_data) - nrow(missing_data) 
# -5968, Here as well its evident that the none of the rows are complete out of 5968 observations. 



# Now, getting the total number of `NA` values to see, how many null values were there in the entire dataset.
# Finding which columns contain `NA` values
sum(is.na(covid_EU_2021))                                   # Count of `NA` is 35234
names(which(sapply(covid_EU_2021, anyNA)))                  # Almost all the variables contains `NA`,
                                                            # except iso_code, location, date


# Various diagnostic plots are available to inspect the quality of the imputations.
# Here using mice library to display NA values and its count.
# As there is large number of variables so it would be difficult to visualize all, hence subsetting
covid_pattern <- subset(covid_EU_2021,
                        select = c(new_cases,  icu_patients, hosp_patients, new_tests_smoothed,
                                   positive_rate, new_vaccinations_smoothed, population,
                                   people_fully_vaccinated,  stringency_index, 
                                   aged_70_older, diabetes_prevalence, female_smokers, male_smokers,
                                   handwashing_facilities, life_expectancy, human_development_index))


md.pattern(covid_pattern, plot = TRUE, rotate.names = TRUE)


# The visualization produced by mice library is not very clear and readable.
# Hence, using VIM library and displayed the missing values
missing_values <- aggr(covid_EU_2021,cex.axis=.5, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)


# -------------------------------------------- Data Wrangling and Imputing ------------------------------------#

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
covid_EU_2021$population[is.na(covid_EU_2021$population)] <- 0
covid_EU_2021$population_density[is.na(covid_EU_2021$population_density)] <- 0

covid_EU_2021$icu_patients[is.na(covid_EU_2021$icu_patients)] <- 0
covid_EU_2021$hosp_patients[is.na(covid_EU_2021$hosp_patients)] <- 0
covid_EU_2021$new_tests[is.na(covid_EU_2021$new_tests)] <- 0
covid_EU_2021$total_tests[is.na(covid_EU_2021$total_tests)] <- 0

covid_EU_2021$new_tests_smoothed[is.na(covid_EU_2021$new_tests_smoothed)] <- 0
covid_EU_2021$positive_rate[is.na(covid_EU_2021$positive_rate)] <- 0
covid_EU_2021$tests_per_case[is.na(covid_EU_2021$tests_per_case)] <- 0
covid_EU_2021$new_vaccinations_smoothed[is.na(covid_EU_2021$new_vaccinations_smoothed)] <- 0
covid_EU_2021$people_fully_vaccinated[is.na(covid_EU_2021$people_fully_vaccinated)] <- 0

covid_EU_2021$aged_70_older[is.na(covid_EU_2021$aged_70_older)] <- 0
covid_EU_2021$stringency_index[is.na(covid_EU_2021$stringency_index)] <- 0
covid_EU_2021$diabetes_prevalence[is.na(covid_EU_2021$diabetes_prevalence)] <- 0
covid_EU_2021$female_smokers[is.na(covid_EU_2021$female_smokers)] <- 0
covid_EU_2021$male_smokers[is.na(covid_EU_2021$male_smokers)] <- 0
covid_EU_2021$handwashing_facilities[is.na(covid_EU_2021$handwashing_facilities)] <- 0
covid_EU_2021$life_expectancy[is.na(covid_EU_2021$life_expectancy)] <- 0
covid_EU_2021$human_development_index[is.na(covid_EU_2021$human_development_index)] <- 0


# Calculating the actual numbers for the categorical nominal variables having values 
# as share of total population provided in the data set description.

# Share of the population that is 70 years and older
covid_EU_2021$aged_70_older <- (covid_EU_2021$population  *  covid_EU_2021$aged_70_older)/100
covid_EU_2021$aged_70_older <- as.integer(covid_EU_2021$aged_70_older)

# Diabetes prevalence (% of population aged 20 to 79)
covid_EU_2021$diabetes_prevalence <- (covid_EU_2021$population  *  covid_EU_2021$diabetes_prevalence)/100
covid_EU_2021$diabetes_prevalence <- as.integer(covid_EU_2021$diabetes_prevalence)

# Share of the population with basic handwashing facilities on premises, most recent year available
covid_EU_2021$handwashing_facilities <- (covid_EU_2021$population  *  covid_EU_2021$handwashing_facilities)/100
covid_EU_2021$handwashing_facilities <- as.integer(covid_EU_2021$handwashing_facilities)

# Share of women who smoke, most recent year available
covid_EU_2021$female_smokers <- (covid_EU_2021$population  *  covid_EU_2021$female_smokers)/100
covid_EU_2021$female_smokers <- as.integer(covid_EU_2021$female_smokers)

# Share of men who smoke, most recent year available
covid_EU_2021$male_smokers <- (covid_EU_2021$population  *  covid_EU_2021$male_smokers)/100
covid_EU_2021$male_smokers <- as.integer(covid_EU_2021$male_smokers)

# Total number of smokers by adding the males and female smokers
covid_EU_2021$total_smokers <- covid_EU_2021$male_smokers + covid_EU_2021$female_smokers

str(covid_EU_2021)
head(covid_EU_2021)
dim(covid_EU_2021)
sum(is.na(covid_EU_2021))

# ---------------------------- Statistical Methods to find Correlation Coefficient------------------------------------#

# Using psych library to get correlation coefficient between the variables
# It is a package for personality, psychometric theory and experimental psychology.
# Functions are primarily for multivariate analysis and scale construction using factor analysis, 
# principal component analysis, cluster analysis and reliability analysis.

covid_corr <- subset(covid_EU_2021,
                     select = c(iso_code, location, date, total_cases, new_cases, 
                                total_deaths, new_deaths, people_fully_vaccinated,
                                stringency_index, population_density, 
                                diabetes_prevalence, handwashing_facilities, 
                                life_expectancy, human_development_index))

my_sample<- covid_corr[sample(1:nrow(covid_EU_2021), 3500, replace = FALSE),]
my_sample

library(psych)
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

# This chart provides a general level of detail on linearity of the independent variable with the depend
# variables. It is observed that each of the predictor variables is skewed to some extent.
# Impact on Total/New cases with population and people_fully_vaccinated, 
# and they fall with stringency_index levels and handwashing_facilities.
# We can check each one in more detail using a scatter plot. 

# ---------------------------------- Statistical Methods to check Linearity------------------------------------#


# Then using the scatter plot with the dependent variable in the y axis 
# and an independent variable in the x axis. 
# If the relation appears to be linear, the assumption is validated.

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
# Here the independent variable new_cases
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

# High correlation. Value = 0.6385971.
# The correlation test shows that the correlation between the new_cases and 
# people_fully_vaccinated variables = 0.6385971 indicating a High correlation.


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

# This variable shows a negative low correlation of -0.1706173
# It clearly shows that with the Government Response Stringency Index: 
# composite measure based on 9 response indicators including school closures, 
# workplace closures, and travel bans reduced the number of new cases

# Similarly, examining new_cases and handwashing_facilities
scatter.smooth(x = covid_EU_2021$new_cases,
               y = covid_EU_2021$handwashing_facilities,
               main = "new_cases ~ handwashing_facilities",
               xlab = "new_cases",
               ylab = "handwashing_facilities")

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

paste("Correlation for new_cases and new_tests_smoothed: ", cor(covid_EU_2021$new_cases, covid_EU_2021$new_tests_smoothed))
paste("Correlation for new_cases and new_vaccinations_smoothed: ", cor(covid_EU_2021$new_cases, covid_EU_2021$new_vaccinations_smoothed))
paste("Correlation for new_cases and life_expectancy: ", cor(covid_EU_2021$new_cases, covid_EU_2021$life_expectancy))
paste("Correlation for new_cases and human_development_index: ", cor(covid_EU_2021$new_cases, covid_EU_2021$human_development_index))


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
# "Correlation for new_cases and new_vaccinations_smoothed:  0.788974516399076"
# "Correlation for new_cases and life_expectancy:  -0.44763391217123"
# "Correlation for new_cases and human_development_index:  -0.220532299034713"


# It appears that the variable  handwashing_facilities, new_tests, total_tests  has a vary low correlation with new_cases.
# Therefore, let's remove it from the dataset. Alternatively we can choose to exclude these dependent variables when
# we are constructing the linear model.

covid_EU_2021 <- subset(covid_EU_2021, select = -c( new_tests, total_tests ))
head(covid_EU_2021)
detach (covid_EU_2021)


# ----------------------------------------------- Outliers -------------------------------------------------#

# Checking for outliers
# Generally, any data point that lies outside the **1.5 * interquartile-range (1.5 * IQR)** is considered
# an outlier. Examining all variables in the dataset.

attach(covid_EU_2021)
dim(covid_EU_2021)
min(covid_EU_2021$new_cases)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2)) # divide graph area in 3 rows by 2 columns
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

# box plot for 'new_tests_smoothed'
boxplot(new_tests_smoothed,
        main = "new_tests_smoothed",
        sub = paste("Outlier rows: ",
                    boxplot.stats(new_tests_smoothed)$out))

# box plot for 'new_vaccinations_smoothed'
boxplot(new_vaccinations_smoothed,
        main = "new_vaccinations_smoothed",
        sub = paste("Outlier rows: ",
                    boxplot.stats(new_vaccinations_smoothed)$out))

# box plot for 'life_expectancy'
boxplot(life_expectancy,
        main = "life_expectancy",
        sub = paste("Outlier rows: ",
                    boxplot.stats(life_expectancy)$out))

# box plot for 'human_development_index'
boxplot(human_development_index,
        main = "human_development_index",
        sub = paste("Outlier rows: ",
                    boxplot.stats(human_development_index)$out))

str(covid_EU_2021)
detach(covid_EU_2021)
par(opar)
# -------------------------------------------------------- Outliers stats ------------------------------------------#


# Use boxplot.stats() function to generate relevant outliers for new_cases
# new_cases outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$new_cases)$out 
paste("new_cases: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for people_fully_vaccinated
# people_fully_vaccinated outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$people_fully_vaccinated)$out 
paste("people_fully_vaccinated outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for population
# population outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$population)$out 
paste("population outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for stringency_index
# stringency_index outliers: 0
outlier_values <- boxplot.stats(covid_EU_2021$stringency_index)$out 
paste("stringency_index outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for handwashing_facilities
# handwashing_facilities outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$handwashing_facilities)$out 
paste("handwashing_facilities outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for icu_patients
# icu_patients outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$icu_patients)$out 
paste("icu_patients outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for hosp_patients
# hosp_patients outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$hosp_patients)$out 
paste("hosp_patients outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for aged_70_older
# aged_70_older outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$aged_70_older)$out 
paste("aged_70_older outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for diabetes_prevalence
# diabetes_prevalence outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$diabetes_prevalence)$out 
paste("diabetes_prevalence outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for total_smokers
# total_smokers outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$total_smokers)$out 
paste("total_smokers outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for new_tests_smoothed
# new_tests_smoothed outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$new_tests_smoothed)$out 
paste("new_tests_smoothed outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for new_vaccinations_smoothed
# new_vaccinations_smoothed outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$new_vaccinations_smoothed)$out 
paste("new_vaccinations_smoothed outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for life_expectancy
# life_expectancy outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$life_expectancy)$out 
paste("life_expectancy outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers for human_development_index
# human_development_index outliers: Too many, cannot remove all
outlier_values <- boxplot.stats(covid_EU_2021$human_development_index)$out 
paste("human_development_index outliers: ", paste(outlier_values, collapse=", "))



# ---------------------------------------------- Remove Outliers ------------------------------------------#
# Now remove all of these outliers by choosing the maximum/ minimum deviation from the values list above  .

# Remove new_cases outliers
summary( covid_EU_2021$new_cases)
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$new_cases > 0
                        & covid_EU_2021$new_cases < 8000)

plot(density(covid_EU_2021$new_cases),
     main = "Density plot : new_cases",
     ylab = "Frequency", xlab = "new_cases",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$new_cases), 2)))


# Remove population  outliers
summary( covid_EU_2021$population)
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$population  < 25449321  )

# density plot for population
plot(density(covid_EU_2021$population),
     main = "Density plot : population",
     ylab = "Frequency", xlab = "population",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$population), 2)))


# Remove people_fully_vaccinated outliers
summary( covid_EU_2021$people_fully_vaccinated)
covid_EU_2021 <- subset(covid_EU_2021, 
                        covid_EU_2021$people_fully_vaccinated < 200000)

# density plot for people_fully_vaccinated
plot(density(covid_EU_2021$people_fully_vaccinated),
     main = "Density plot : people_fully_vaccinated",
     ylab = "Frequency", xlab = "people_fully_vaccinated",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$people_fully_vaccinated), 2)))


# Remove stringency_index  outliers
summary( covid_EU_2021$stringency_index)
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$stringency_index  > 0)

# density plot for stringency_index
plot(density(covid_EU_2021$stringency_index),
     main = "Density plot : stringency_index",
     ylab = "Frequency", xlab = "stringency_index",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$stringency_index), 2)))



# Remove handwashing_facilities  outliers
#covid_EU_2021 <- subset(covid_EU_2021,
#  covid_EU_2021$handwashing_facilities > 1000)

plot(density(covid_EU_2021$handwashing_facilities ),
     main = "Density plot : handwashing_facilities",
     ylab = "Frequency", xlab = "handwashing_facilities",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$handwashing_facilities), 2)))


# Remove icu_patients  outliers
summary(covid_EU_2021$icu_patients)
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$icu_patients <400)

# density plot for icu_patients
plot(density(covid_EU_2021$icu_patients ),
     main = "Density plot : icu_patients",
     ylab = "Frequency", xlab = "icu_patients",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$icu_patients), 2)))


# Remove hosp_patients  outliers
summary(covid_EU_2021$hosp_patients)
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$hosp_patients < 1000)

# density plot for hosp_patients
plot(density(covid_EU_2021$hosp_patients ),
     main = "Density plot : hosp_patients",
     ylab = "Frequency", xlab = "hosp_patients",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$hosp_patients), 2)))


# Remove aged_70_older  outliers
summary(covid_EU_2021$aged_70_older)
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$aged_70_older  < 993889) 


# density plot for aged_70_older
summary(covid_EU_2021$diabetes_prevalence)
plot(density(covid_EU_2021$aged_70_older ),
     main = "Density plot : aged_70_older",
     ylab = "Frequency", xlab = "aged_70_older",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$aged_70_older), 2)))


# Remove diabetes_prevalence  outliers
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$diabetes_prevalence  <1873750) 

# density plot for diabetes_prevalence
plot(density(covid_EU_2021$diabetes_prevalence ),
     main = "Density plot : diabetes_prevalence",
     ylab = "Frequency", xlab = "diabetes_prevalence",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$diabetes_prevalence), 2)))   


# Remove total_smokers  outliers
summary(covid_EU_2021$total_smokers)
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$total_smokers  < 5300500)

# density plot for total_smokers
plot(density(covid_EU_2021$total_smokers ),
     main = "Density plot : total_smokers",
     ylab = "Frequency", xlab = "total_smokers",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$total_smokers), 2)))


# Remove new_tests_smoothed  outliers
summary(covid_EU_2021$new_tests_smoothed)
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$new_tests_smoothed  < 20000 )

# density plot for new_tests_smoothed
plot(density(covid_EU_2021$new_tests_smoothed ),
     main = "Density plot : new_tests_smoothed",
     ylab = "Frequency", xlab = "new_tests_smoothed",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$new_tests_smoothed), 2)))


# Remove new_vaccinations_smoothed  outliers
summary(covid_EU_2021$new_vaccinations_smoothed)
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$new_vaccinations_smoothed  < 10000 )

# density plot for new_vaccinations_smoothed
plot(density(covid_EU_2021$new_vaccinations_smoothed ),
     main = "Density plot : new_vaccinations_smoothed",
     ylab = "Frequency", xlab = "new_vaccinations_smoothed",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$new_vaccinations_smoothed), 2)))


# Remove life_expectancy  outliers
summary(covid_EU_2021$life_expectancy)
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$life_expectancy  >0)

# density plot for life_expectancy
plot(density(covid_EU_2021$life_expectancy ),
     main = "Density plot : life_expectancy",
     ylab = "Frequency", xlab = "life_expectancy",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$life_expectancy), 2)))


# Remove human_development_index  outliers
summary(covid_EU_2021$human_development_index)
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$human_development_index  >0)

# density plot for human_development_index
plot(density(covid_EU_2021$human_development_index ),
     main = "Density plot : human_development_index",
     ylab = "Frequency", xlab = "human_development_index",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$human_development_index), 2)))



# --------------------------------- Normality and Skewness check ------------------------------------------------#

detach(covid_EU_2021)
attach(covid_EU_2021)
dim(covid_EU_2021)

# Using Skewness function to examine normality
# library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))                      # divide graph area into 1 row x 2 cols


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


# density plot for new_tests_smoothed
plot(density(covid_EU_2021$new_tests_smoothed ),
     main = "Density plot : new_tests_smoothed",
     ylab = "Frequency", xlab = "new_tests_smoothed",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$new_tests_smoothed), 2)))

# fill the area under the plot for new_tests_smoothed 
polygon(density(covid_EU_2021$new_tests_smoothed), col = "red")


# density plot for new_vaccinations_smoothed
plot(density(covid_EU_2021$new_vaccinations_smoothed ),
     main = "Density plot : new_vaccinations_smoothed",
     ylab = "Frequency", xlab = "new_vaccinations_smoothed",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$new_vaccinations_smoothed), 2)))

# fill the area under the plot for new_vaccinations_smoothed 
polygon(density(covid_EU_2021$new_vaccinations_smoothed), col = "red")


# density plot for life_expectancy
plot(density(covid_EU_2021$life_expectancy ),
     main = "Density plot : life_expectancy",
     ylab = "Frequency", xlab = "life_expectancy",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$life_expectancy), 2)))

# fill the area under the plot for life_expectancy 
polygon(density(covid_EU_2021$life_expectancy), col = "red")


# density plot for human_development_index
plot(density(covid_EU_2021$human_development_index ),
     main = "Density plot : human_development_index",
     ylab = "Frequency", xlab = "human_development_index",
     sub = paste("Skewness : ", round(e1071::skewness(covid_EU_2021$human_development_index), 2)))

# fill the area under the plot for human_development_index 
polygon(density(covid_EU_2021$human_development_index), col = "red")

par <- opar

# It is difficult to read all skewness values. We can display them numerically using the following code.

paste("Skewness for new_cases: ", round(e1071::skewness(covid_EU_2021$new_cases), 2))
paste("Skewness for icu_patients: ", round(e1071::skewness(covid_EU_2021$icu_patients), 2))
paste("Skewness for hosp_patients: ", round(e1071::skewness(covid_EU_2021$hosp_patients), 2))
paste("Skewness for new_tests_smoothed)) : ", round(e1071::skewness(covid_EU_2021$new_tests_smoothed), 2))
paste("Skewness for new_vaccinations_smoothed)) : ", round(e1071::skewness(covid_EU_2021$new_vaccinations_smoothed), 2))
paste("Skewness for people_fully_vaccinated : ", round(e1071::skewness(covid_EU_2021$people_fully_vaccinated),2))
paste("Skewness for population: ", round(e1071::skewness(covid_EU_2021$population), 2))
paste("Skewness for stringency_index)) : ", round(e1071::skewness(covid_EU_2021$stringency_index),2))
paste("Skewness for aged_70_older : ", round(e1071::skewness(covid_EU_2021$aged_70_older),2))
paste("Skewness for diabetes_prevalence)) : ", round(e1071::skewness(covid_EU_2021$diabetes_prevalence),2))
paste("Skewness for total_smokers)) : ", round(e1071::skewness(covid_EU_2021$total_smokers), 2))
paste("Skewness for handwashing_facilities)) : ", round(e1071::skewness(covid_EU_2021$handwashing_facilities), 2))
paste("Skewness for life_expectancy)) : ", round(e1071::skewness(covid_EU_2021$life_expectancy), 2))
paste("Skewness for human_development_index)) : ", round(e1071::skewness(covid_EU_2021$human_development_index), 2))

# "Skewness for new_cases:  1.66"
# "Skewness for icu_patients:  2.49"
# "Skewness for hosp_patients:  1.07"
# "Skewness for new_tests_smoothed)) :  0.94"
# "Skewness for new_vaccinations_smoothed)) :  1"
# "Skewness for people_fully_vaccinated :  2.01"
# "Skewness for population:  0.52"
# "Skewness for stringency_index)) :  0.54"
# "Skewness for aged_70_older :  0.96"
# "Skewness for diabetes_prevalence)) :  0.5"
# "Skewness for total_smokers)) :  0.37"
# "Skewness for handwashing_facilities)) :  1.81"
# "Skewness for life_expectancy)) :  -0.7"
# "Skewness for human_development_index)) :  -0.64"	

# Here is the standard metrics for skewness.
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


#-------------------------------------------Multiple Linear Regression MOdel building ----------------------------------------#

# Splitting Training and testing datasets
set.seed(1)
no_rows_data <- nrow(covid_EU_2021)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)
training_data <- covid_EU_2021[sample, ]
testing_data <- covid_EU_2021[-sample, ]

# Now that the data variables are examined, so build the multiple regresion model.
# Building the MLR model

fit <- lm(new_cases ~ people_fully_vaccinated + population + stringency_index + handwashing_facilities +
              icu_patients + hosp_patients + aged_70_older + diabetes_prevalence +
              total_smokers + new_tests_smoothed + new_vaccinations_smoothed + life_expectancy + 
              human_development_index, data=training_data)


# Lets have a look at the the results in detail, we use the summary() function on the model object:
summary(fit)


# AIC and BIC, both criteria depend on the maximised value of the likelihood function L for the estimated model.
#For model comparison, the model with the lowest AIC and BIC score is preferred.

AIC(fit) #14098.32
BIC(fit) #14170.69

# Model summary : Output from the confint() function.
confint(fit)

#---------------------------------------------------------- Regression diagnostics --------------------------------#
#-------------------------------------------- Normality and studentized residuals ----------------------------------#
# use methods available through the car package to do this analysis.
str(covid_EU_2021)
library(car)
qqPlot(fit, labels=row.names(covid_EU_2021), id.method="identify", 
       simulate=TRUE, main="Q-Q Plot")

# 25980 25978 
# 1419  3320

training_data[1419,]
training_data[3320,]

# The fitted() function extracts fitted values from objects returned by modeling functions. 
# It returns the predicted new cases rate for a particular state.

fitted(fit)[207]   # 45051 611.1062 
fitted(fit)[755]  # 50045  966.5526

dim(covid_EU_2021)
# Remove  outliers
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$location != "Europe  "
                        & covid_EU_2021$date != "2021-01-07")
covid_EU_2021 <- subset(covid_EU_2021,
                        covid_EU_2021$location != "Europe  "
                        & covid_EU_2021$date != "2021-01-05")

#Split the data into Training and testing datasets again
set.seed(1)
no_rows_data <- nrow(covid_EU_2021)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)
training_data <- covid_EU_2021[sample, ]
testing_data <- covid_EU_2021[-sample, ]

#Then rebuild the MLR model again
fit <- lm(new_cases ~ people_fully_vaccinated + population + stringency_index + handwashing_facilities +
              icu_patients + hosp_patients + aged_70_older + diabetes_prevalence +
              total_smokers + new_tests_smoothed + new_vaccinations_smoothed + life_expectancy +
              human_development_index, data=training_data)

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


#--------------------------------------------------- Linearity ------------------------------------------------------------#
crPlots(fit)

#--------------------------------------------- Influential observations ---------------------------------------------------#

cutoff <- 4/(nrow(training_data) - length(fit$coefficients) - 2)
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

#library(car)
avPlots(fit, ask=FALSE)


#----------------------------------------------- Influence plot -----------------------------------------------------------#

#library(car)
influencePlot(fit, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

#------------------------------------------------ Homoscedasticity ---------------------------------------------------------#
ncvTest(fit)

# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 387.539, Df = 1, p = < 2.22e-16

# The score test is nonsignificant (p = 0.45229), suggesting that we have met the constant variance
# assumption. If the p value is significant (p < 0.05), we would assume that the error variance
# changes with the level of the fitted values.

spreadLevelPlot(fit)

# Suggested power transformation:  0.007314462 


#------------------------------------------ Global validation of linear model assumption ----------------------------------------#

# The gvlma() function performs a global validation of linear model assumptions as well as
# separate evaluations of skewness, kurtosis, and heteroscedasticity

library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)

#Value          p-value                   Decision
#Global Stat        20765.253 0.0000000 Assumptions NOT satisfied!
#Skewness            1190.911 0.0000000 Assumptions NOT satisfied!
#Kurtosis           19559.512 0.0000000 Assumptions NOT satisfied!
#Link Function         11.857 0.0005744 Assumptions NOT satisfied!
#Heteroscedasticity     2.972 0.0846948    Assumptions acceptable.

#------------------------------------------------ Multicollinearity -------------------------------------------#

library(car)
vif(fit)

#people_fully_vaccinated population          stringency_index    handwashing_facilities              icu_patients 
#2.322684                 36.822746                  1.816813                  4.945676                  3.666989 
#hosp_patients            aged_70_older       diabetes_prevalence             total_smokers        new_tests_smoothed 
#5.470502                 43.211124                 17.485645                 22.832712                  1.821410 
#new_vaccinations_smoothed life_expectancy   human_development_index 
#2.895943                  6.538118                 11.134527 



# We can check whether any of the variables indicate a multicollinearity problem
# if the value > 2
sqrt(vif(fit)) > 2

#people_fully_vaccinated              population        stringency_index  handwashing_facilities 
#FALSE                    TRUE                   FALSE                   FALSE 
#icu_patients           hosp_patients           aged_70_older     diabetes_prevalence 
#FALSE                   FALSE                    TRUE                    TRUE 
#total_smokers 
#TRUE 

#------------------------------------------------ Transforming variables -------------------------------------------#

library(car)
summary(powerTransform(training_data$new_cases))


#--------------------------------------------- Comparing models using AIC ------------------------------------------#

# Transform new_cases variable as indicated by spreadLevelPlot() function

sqrt_transform_new_cases <- sqrt(training_data$new_cases)
training_data$new_cases_sqrt <- sqrt_transform_new_cases
training_data$new_cases_sqrt

fit_model1 <- lm(new_cases ~ people_fully_vaccinated + population + 
                     stringency_index + handwashing_facilities + 
                     icu_patients + hosp_patients + aged_70_older +  
                     diabetes_prevalence + total_smokers+
                     new_tests_smoothed + new_vaccinations_smoothed +
                     life_expectancy + human_development_index, data=training_data)


fit_model2 <- lm(new_cases_sqrt ~ people_fully_vaccinated + population + 
                     stringency_index + handwashing_facilities + 
                     icu_patients + hosp_patients + aged_70_older +  
                     diabetes_prevalence + total_smokers+
                     new_tests_smoothed + new_vaccinations_smoothed +
                     life_expectancy + human_development_index, data=training_data)

AIC(fit_model1,fit_model2)


spreadLevelPlot(fit_model1) # Suggested power transformation:  0.007314462 
spreadLevelPlot(fit_model2) # Suggested power transformation:  0.2768732 


#-------------------------------------------- Comparing multiple models ----------------------------------------------#
# STEPWISE REGRESSION
# Backward

library(MASS)
fit_test <- lm(new_cases ~ people_fully_vaccinated + population + 
                   stringency_index + handwashing_facilities + 
                   icu_patients + hosp_patients + aged_70_older +  
                   diabetes_prevalence + total_smokers + new_tests_smoothed +
                   new_vaccinations_smoothed + life_expectancy + human_development_index, data=training_data)
stepAIC(fit_test, direction="backward")


# SUBSETS REGRESSION
library(leaps)
leaps <-regsubsets(new_cases ~ people_fully_vaccinated + population + 
                       stringency_index + handwashing_facilities + 
                       icu_patients + hosp_patients + aged_70_older +  
                       diabetes_prevalence + total_smokers + new_tests_smoothed + new_vaccinations_smoothed +
                       life_expectancy + human_development_index, data=training_data, nbest=4)
plot(leaps, scale="adjr2")




# STEPWISE REGRESSION on transformed response variable
# Backward

#library(MASS)
fit_test <- lm(new_cases_sqrt ~ people_fully_vaccinated + population + 
                   stringency_index + handwashing_facilities + 
                   icu_patients + hosp_patients + aged_70_older +  
                   diabetes_prevalence + total_smokers + new_tests_smoothed + new_vaccinations_smoothed +
                   life_expectancy + human_development_index, data=training_data)
stepAIC(fit_test, direction="backward")

# SUBSETS REGRESSION on transformed response variable
#library(leaps)
leaps <-regsubsets(new_cases_sqrt ~ people_fully_vaccinated + population + 
                       stringency_index + handwashing_facilities + 
                       icu_patients + hosp_patients + aged_70_older +  
                       diabetes_prevalence + total_smokers + new_tests_smoothed +
                       new_vaccinations_smoothed + life_expectancy + human_development_index,
                   data=training_data, nbest=4)
plot(leaps, scale="adjr2")


#---------------------------------------------------- Examining Predicted Accuracy ---------------------------------#

fit_model <- lm(new_cases ~ people_fully_vaccinated + population + 
                    stringency_index + handwashing_facilities + 
                    icu_patients + hosp_patients + aged_70_older +  
                    diabetes_prevalence + total_smokers + new_tests_smoothed +
                    new_vaccinations_smoothed + life_expectancy + human_development_index, data=training_data)

fit_model_sqrt <- lm(new_cases_sqrt ~ people_fully_vaccinated + population + 
                         stringency_index + handwashing_facilities + 
                         icu_patients + hosp_patients + aged_70_older +  
                         diabetes_prevalence + total_smokers + new_tests_smoothed +
                         new_vaccinations_smoothed + life_expectancy +
                         human_development_index, data=training_data)

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
#actuals   1.0000000 0.6191986
#predicted 0.6191986 1.0000000

correlation_accuracy <- cor(actuals_predictions_sqrt)
correlation_accuracy
#actuals predicted
#actuals   1.0000000 0.6000706
#predicted 0.6000706 1.0000000


# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
min_max_accuracy

# 0.5269857

# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_predictions_sqrt, 1, min) / apply(actuals_predictions_sqrt, 1, max))
min_max_accuracy

# 0.5961353

#----------------------------------------- Residual Standard Error (RSE), or sigma --------------------------------------#

sigma(fit_model)/ mean(testing_data$new_cases)
# 0.6511408
# This estimates the error of 65% on the existing data set.


sigma(fit_model_sqrt)/ mean(testing_data$new_cases)
# 0.01186012
# This estimates an error rate of 1% with the data we have at hand.

#-------------------------------------- Executing final model with varied inputs ----------------------------------------#
detach (covid_EU_2021)
attach (covid_EU_2021)


# Checking the ranges in input data bu using Summary function
summary(covid_EU_2021)

df <- data.frame(people_fully_vaccinated = c(41397), population = c(3921055), 
                 stringency_index = c(88.89), handwashing_facilities = c(6649383),
                 icu_patients = c(42.17), hosp_patients = c(601.6),
                 new_tests_smoothed = c(5142), new_vaccinations_smoothed = c(2594),
                 aged_70_older = c(226878), diabetes_prevalence = c(135926),
                 total_smokers = c(1199276), life_expectancy = c(79.37),
                 human_development_index = c(0.8669))
predicted_new_cases <- predict(fit_model, df)
predicted_new_cases

df <- data.frame(people_fully_vaccinated = c(41397), population = c(3921055), 
                 stringency_index = c(88.89), handwashing_facilities = c(6649383),
                 icu_patients = c(42.17), hosp_patients = c(601.6),
                 new_tests_smoothed = c(5142), new_vaccinations_smoothed = c(2594),
                 aged_70_older = c(226878), diabetes_prevalence = c(135926),
                 total_smokers = c(1199276), life_expectancy = c(79.37),
                 human_development_index = c(0.8669))
predicted_new_cases <- predict(fit_model_sqrt, df)
predicted_new_cases^2

# 16.68512 



# 593.4031

df <- data.frame(people_fully_vaccinated = c(41397), population = c(3921055), 
                 stringency_index = c(88.89), handwashing_facilities = c(6649383),
                 icu_patients = c(42.17), hosp_patients = c(601.6),
                 new_tests_smoothed = c(5142), new_vaccinations_smoothed = c(2594),
                 aged_70_older = c(226878), diabetes_prevalence = c(135926),
                 total_smokers = c(1199276), life_expectancy = c(79.37),
                 human_development_index = c(0.8669))
predicted_new_cases <- predict(fit_model_sqrt, df)
predicted_new_cases

# 29.63811


df <- data.frame(people_fully_vaccinated = c(41397), population = c(3921055), 
                 stringency_index = c(88.89), handwashing_facilities = c(6649383),
                 icu_patients = c(42.17), hosp_patients = c(601.6),
                 new_tests_smoothed = c(5142), new_vaccinations_smoothed = c(2594),
                 aged_70_older = c(226878), diabetes_prevalence = c(135926),
                 total_smokers = c(1199276), life_expectancy = c(79.37),
                 human_development_index = c(0.8669))
predicted_new_cases <- predict(fit_model_sqrt, df)
predicted_new_cases
#---------------------------------------------------------- The End -------------------------------------------------#