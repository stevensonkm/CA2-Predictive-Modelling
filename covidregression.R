# load the dataset "covid.csv"

covid_data <- read.csv("covid.csv", na = "", header = T, stringsAsFactors = T)  
covid_data [covid_data == ""] <- NA 

str(covid_data)


# change the date variable to date

covid_data$date <- as.Date(covid_data$date)
str(covid_data$date)
str(covid_data)

head(covid_data, n = 5)

class(covid_data)

nrow(covid_data)
# data preparation
#choosing the variables and Africa continent
#data cleaning
# data processing


covid_Africa <- subset.data.frame(covid_data,continent== 'Africa', select = c( new_cases, new_deaths, reproduction_rate,         
                                                                               icu_patients,hosp_patients,new_tests,total_tests,
                                                                               positive_rate,total_vaccinations,people_fully_vaccinated,
                                                                               new_vaccinations, stringency_index, population, 
                                                                               population_density, aged_65_older, gdp_per_capita, 
                                                                               aged_70_older, extreme_poverty, diabetes_prevalence, 
                                                                               female_smokers, male_smokers, median_age, 
                                                                               handwashing_facilities, human_development_index))

str(covid_Africa)
names(covid_Africa)
attach(covid_Africa)

# check the missing values using vim library
library(VIM)
missing_values <- aggr(covid_Africa, prop = FALSE, numbers = TRUE)
# show the summary of missing values in Europe data
summary(missing_values)
#show the summary by each column
summary(covid_Africa)
# Replacing NA's with "0" in variables
attach(covid_Africa)
covid_Africa$new_cases[is.na(new_cases)] <- 0
covid_Africa$new_deaths[is.na(new_deaths)] <- 0
covid_Africa$reproduction_rate[is.na(reproduction_rate)] <-  0
covid_Africa$icu_patients[is.na(icu_patients)] <- 0
covid_Africa$hosp_patients[is.na(hosp_patients)] <- 0
covid_Africa$new_tests[is.na(new_tests)] <- 0
covid_Africa$total_tests[is.na(total_tests)] <- 0
covid_Africa$female_smokers[is.na(female_smokers)] <- 0
covid_Africa$male_smokers[is.na(male_smokers)] <- 0
covid_Africa$positive_rate[is.na(positive_rate)] <- 0
covid_Africa$population_density[is.na(population_density)] <- 0
covid_Africa$aged_65_older[is.na(aged_65_older)] <- 0
covid_Africa$extreme_poverty[is.na(extreme_poverty)] <- 0
covid_Africa$total_vaccinations[is.na(total_vaccinations)] <- 0
covid_Africa$people_fully_vaccinated[is.na(people_fully_vaccinated)] <- 0
covid_Africa$new_vaccinations[is.na(new_vaccinations)] <- 0
covid_Africa$stringency_index[is.na(stringency_index)] <- 0
covid_Africa$population[is.na(population)] <- 0
covid_Africa$gdp_per_capita[is.na(gdp_per_capita)] <- 0
covid_Africa$diabetes_prevalence[is.na(diabetes_prevalence)] <- 0
covid_Africa$median_age[is.na(median_age)] <- 0
covid_Africa$handwashing_facilities[is.na(handwashing_facilities)] <- 0
covid_Africa$human_development_index [is.na(human_development_index )] <- 0


#identify the missing values using vim
any(is.na(covid_Africa))
covid_Africa <- na.omit(covid_Africa)
missing_values <- aggr(covid_Africa, prop = FALSE, numbers = TRUE)             
summary(missing_values) 




attach(covid_Africa)
# checking correlation
library(psych)                                                                  
#pdf('pairs.pdf')
#pairs(covid_Africa)
#pairs.panels(covid_Africa, 
#             smooth = TRUE, # If TRUE, draws loess smooths  
#             scale = FALSE, # If TRUE, scales the correlation text font  
#             density = TRUE, # If TRUE, adds density plots and histograms  
#             ellipses = TRUE, # If TRUE, draws ellipses   
#             method = "spearman",# Correlation method (also "pearson" or "kendall") 
#             pch = 21, # pch symbol   
#             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit 
#             cor = TRUE, # If TRUE, reports correlations
#             jiggle = FALSE, # If TRUE, data points are jittered  
#             factor = 2, # Jittering factor  
#             hist.col = 4, # Histograms color   
#             stars = TRUE,
#             ci = TRUE) # If TRUE, adds confidence intervals 

# dev.off()

# correlation between variables
scatter.smooth(x = covid_Africa$new_cases,                                       
               y = covid_Africa$new_deaths,                                     
               main = "new_cases ~ new_deaths",                                  
               xlab = "new_cases",
               ylab = "new_deaths")                                             
cor(covid_Africa$new_cases, covid_Africa$new_deaths)  # corr =  0.8162359                           
scatter.smooth(x = covid_Africa$new_cases,                                      
               y = covid_Africa$reproduction_rate,                              
               main = "new_cases ~ reproduction_rate",                          
               xlab = "new_cases",
               ylab = "reproduction_rate")                                      
cor(covid_Africa$new_cases, covid_Africa$reproduction_rate) # corr= 0.1259661                  

scatter.smooth(x = covid_Africa$new_cases,                                      
               y = covid_Africa$new_tests,                                      
               main = "new_cases ~ new_tests",
               xlab = "new_cases",
               ylab = "new_tests")
cor(covid_Africa$new_cases, covid_Africa$new_tests)   #corr= 0.8029776                          

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$total_tests,
               main = "new_cases ~ total_tests",
               xlab = "new_cases",
               ylab = "total_tests")
cor(covid_Africa$new_cases, covid_Africa$total_tests)  #corr=0.5290334                         

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$positive_rate,
               main = "new_cases ~ positive_rate",
               xlab = "new_cases",
               ylab = "positive_rate")
cor(covid_Africa$new_cases, covid_Africa$positive_rate)  #corr = 0.3603126                       


scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$new_vaccinations,
               main = "new_cases ~ new_vaccinations",
               xlab = "new_cases",
               ylab = "new_vaccinations")
cor(covid_Africa$new_cases, covid_Africa$new_vaccinations)  #corr=0.01799528                  

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$stringency_index,
               main = "new_cases ~ stringency_index",
               xlab = "new_cases",
               ylab = "stringency_index")
cor(covid_Africa$new_cases, covid_Africa$stringency_index) #corr=0.0992528                   

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$population,
               main = "new_cases ~ population",
               xlab = "new_cases",
               ylab = "population")
cor(covid_Africa$new_cases, covid_Africa$population)  #corr=0.178875                         

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$population_density,
               main = "new_cases ~ population_density",
               xlab = "new_cases",
               ylab = "population_density")
cor(covid_Africa$new_cases, covid_Africa$population_density)  #corr=-0.05522673                  

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$median_age,
               main = "new_cases ~ median_age",
               xlab = "new_cases",
               ylab = "median_age")
cor(covid_Africa$new_cases, covid_Africa$median_age)  #corr = 0.1907985                          

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$aged_65_older,
               main = "new_cases ~ aged_65_older",
               xlab = "new_cases",
               ylab = "aged_65_older")
cor(covid_Africa$new_cases, covid_Africa$aged_65_older) #corr= 0.1669321                        

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$aged_70_older,
               main = "new_cases ~ aged_70_older",
               xlab = "new_cases",
               ylab = "aged_70_older")
cor(covid_Africa$new_cases, covid_Africa$aged_70_older)   #corr = 0.1585393                      

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$extreme_poverty,
               main = "new_cases ~ extreme_poverty",
               xlab = "new_cases",
               ylab = "extreme_poverty")
cor(covid_Africa$new_cases, covid_Africa$extreme_poverty)  #corr= -0.08079204                   

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$diabetes_prevalence,
               main = "new_cases ~ diabetes_prevalence",
               xlab = "new_cases",
               ylab = "diabetes_prevalence")
cor(covid_Africa$new_cases, covid_Africa$diabetes_prevalence)   #corr=0.04530466               

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$female_smokers,
               main = "new_cases ~ female_smokers",
               xlab = "new_cases",
               ylab = "female_smokers")
cor(covid_Africa$new_cases, covid_Africa$female_smokers)  #corr= 0.1789466                       

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$male_smokers,
               main = "new_cases ~ male_smokers",
               xlab = "new_cases",
               ylab = "male_smokers")
cor(covid_Africa$new_cases, covid_Africa$male_smokers)    # corr=0.1553251                      

scatter.smooth(x = covid_Africa$new_cases,
               y = covid_Africa$handwashing_facilities,
               main = "new_cases ~ handwashing_facilities",
               xlab = "new_cases",
               ylab = "handwashing_facilities")
cor(covid_Africa$new_cases, covid_Africa$handwashing_facilities)  # corr=0.1553251              

# we can observe that the variables population density, extreme poverty in africa  has low correlation
# so we remove these variables
covid_Africa <- subset(covid_Africa, select = -c( diabetes_prevalence, gdp_per_capita ))
covid_Africa <- subset(covid_Africa, select = -c(extreme_poverty        ))
covid_Africa <- subset(covid_Africa, select = -c(population_density, stringency_index, ) )                       
head(covid_Africa)
str(covid_Africa)

# Now we check for outliners
# install.packages("e1071")
library(e1071)
attach(covid_Africa)

opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))    # divide graph area in 3 rows by 2 columns

boxplot(new_cases, 
        main = "new_cases", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(new_cases)$out))


boxplot(new_deaths, 
        main = "new_deaths", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(new_deaths)$out))


boxplot(stringency_index, 
        main = "stringency_index", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(stringency_index)$out))


boxplot(population, 
        main = "population", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(population)$out))


boxplot(positive_rate, 
        main = "positive_rate", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(positive_rate)$out))


boxplot(aged_65_older, 
        main = "aged_65_older", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(aged_65_older)$out))


boxplot(aged_70_older, 
        main = "aged_70_older", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(aged_70_older)$out))


boxplot(female_smokers, 
        main = "female_smokers", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(female_smokers)$out))


boxplot(male_smokers, 
        main = "male_smokers", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(male_smokers)$out))


boxplot(median_age, 
        main = "median_age", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(median_age)$out))


boxplot(handwashing_facilities, 
        main = "handwashing_facilities", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(handwashing_facilities)$out))


boxplot(new_tests, 
        main = "new_tests", 
        sub = paste("new_tests rows: ", 
                    boxplot.stats(new_tests)$out))


# check for normality
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))
plot(density(new_cases), 
     main = "Density plot : new_cases", 
     ylab = "Frequency", xlab = "new_cases",
     sub = paste("Skewness : ", round(e1071::skewness(new_cases), 2)))
polygon(density(new_cases), col = "red")


plot(density(new_deaths), 
     main = "Density plot : new_deaths", 
     ylab = "Frequency", xlab = "new_deaths",
     sub = paste("Skewness : ", round(e1071::skewness(new_deaths), 2)))
polygon(density(new_deaths), col = "red")

plot(density(stringency_index), 
     main = "Density plot : stringency_index", 
     ylab = "Frequency", xlab = "stringency_index",
     sub = paste("Skewness : ", round(e1071::skewness(stringency_index), 2)))
polygon(density(stringency_index), col = "red")

plot(density(population), 
     main = "Density plot : population", 
     ylab = "Frequency", xlab = "population",
     sub = paste("Skewness : ", round(e1071::skewness(population), 2)))
polygon(density(population), col = "red")

plot(density(positive_rate), 
     main = "Density plot : positive_rate", 
     ylab = "Frequency", xlab = "positive_rate",
     sub = paste("Skewness : ", round(e1071::skewness(positive_rate), 2)))
polygon(density(positive_rate), col = "red")

plot(density(aged_65_older), 
     main = "Density plot : aged_65_older", 
     ylab = "Frequency", xlab = "aged_65_older",
     sub = paste("Skewness : ", round(e1071::skewness(aged_65_older), 2)))
polygon(density(aged_65_older), col = "red")


plot(density(aged_70_older), 
     main = "Density plot : aged_70_older", 
     ylab = "Frequency", xlab = "aged_70_older",
     sub = paste("Skewness : ", round(e1071::skewness(aged_70_older), 2)))
polygon(density(aged_70_older), col = "red")

plot(density(female_smokers), 
     main = "Density plot : female_smokers", 
     ylab = "Frequency", xlab = "female_smokers",
     sub = paste("Skewness : ", round(e1071::skewness(female_smokers), 2)))
polygon(density(female_smokers), col = "red")

plot(density(median_age), 
     main = "Density plot : median_age", 
     ylab = "Frequency", xlab = "median_age",
     sub = paste("Skewness : ", round(e1071::skewness(median_age), 2)))
polygon(density(median_age), col = "red")

plot(density(male_smokers), 
     main = "Density plot : male_smokers", 
     ylab = "Frequency", xlab = "male_smokers",
     sub = paste("Skewness : ", round(e1071::skewness(male_smokers), 2)))
polygon(density(male_smokers), col = "red")

plot(density(handwashing_facilities), 
     main = "Density plot : handwashing_facilities", 
     ylab = "Frequency", xlab = "handwashing_facilities",
     sub = paste("Skewness : ", round(e1071::skewness(handwashing_facilities), 2)))
polygon(density(handwashing_facilities), col = "red")

plot(density(new_tests), 
     main = "Density plot : new_tests", 
     ylab = "Frequency", xlab = "new_tests",
     sub = paste("Skewness : ", round(e1071::skewness(new_tests), 2)))
polygon(density(new_tests), col = "red")

par <- opar



# Now displaying the results from the graph
paste("Skewness for New_cases: ", round(e1071::skewness(new_cases), 2))                             #12.29 
paste("Skewness for New_deaths: ", round(e1071::skewness(new_deaths), 2))                           #14.52
paste("Skewness for stringency_index: ", round(e1071::skewness(stringency_index), 2))               #-0.43 
paste("Skewness for population: ", round(e1071::skewness(population), 2))                           #3
paste("Skewness for positive_rate: ", round(e1071::skewness(positive_rate), 2))                     #2.87
paste("Skewness for aged_65_older: ", round(e1071::skewness(aged_65_older), 2))                     #2.43
paste("Skewness for aged_70_older: ", round(e1071::skewness(aged_70_older), 2))                     #2.08
paste("Skewness for female_smokers: ", round(e1071::skewness(female_smokers), 2))                   #1.75 
paste("Skewness for male_smokers: ", round(e1071::skewness(male_smokers), 2))                       #0.68
paste("Skewness for median_age: ", round(e1071::skewness(median_age), 2))                           #1.34
paste("Skewness for handwashing_facilities: ", round(e1071::skewness(handwashing_facilities), 2))   #1.42
paste("Skewness for new_tests: ", round(e1071::skewness(new_tests), 2))                             # 6.82


# showing the visual analysis by histogram
# showing the data is normally distributed or not 
# by qqnorm and qqline 

opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2)) # divide the graph area in 2 cols

# NEW CASES
hist(new_cases, 
     main = "Normalility proportion of New cases", 
     xlab = "New Cases")

qqnorm(new_cases)
qqline(new_cases, col = "red")

# NEW DEATHS
hist(new_deaths, 
     main = "Normalility proportion of New Deaths", 
     xlab = "New Deaths")

qqnorm(new_deaths)
qqline(new_deaths, col = "red")
# STRINGENCY INDEX
hist(stringency_index, 
     main = "Normalility proportion of stringency_index", 
     xlab = "stringency_index")

qqnorm(stringency_index)
qqline(stringency_index, col = "red")
# POPULATION
hist(population, 
     main = "Normalility proportion of population", 
     xlab = "population")

qqnorm(population)
qqline(population, col = "red")

# POSITIVE RATE
hist(positive_rate, 
     main = "Normalility proportion of positive_rate", 
     xlab = "positive_rate)")

qqnorm(positive_rate)
qqline(positive_rate, col = "red")

# AGED 65 AND OLDER
hist(aged_65_older, 
     main = "Normalility proportion of aged_65_older", 
     xlab = "aged_65_older")

qqnorm(aged_65_older)
qqline(aged_65_older, col = "red")

# AGED 70 AND OLDER
hist(aged_70_older, 
     main = "Normalility proportion of aged_70_older", 
     xlab = "aged_70_older")

qqnorm(aged_70_older)
qqline(aged_70_older, col = "red")

# FEMALE SMOKERS
hist(female_smokers, 
     main = "Normalility proportion of female_smokers", 
     xlab = "female_smokers")

qqnorm(female_smokers)
qqline(female_smokers, col = "red")

# MALE SMOKERS
hist(male_smokers, 
     main = "Normalility proportion of male_smokers", 
     xlab = "male_smokers")

qqnorm(male_smokers)
qqline(male_smokers, col = "red")

# median age
hist(median_age, 
     main = "Normalility proportion of median_age", 
     xlab = "median_age")

qqnorm(median_age)
qqline(median_age, col = "red")

# handwashing
hist(handwashing_facilities, 
     main = "Normalility proportion of handwashing_facilities", 
     xlab = "handwashing_facilities")

qqnorm(handwashing_facilities)
qqline(handwashing_facilities, col = "red")

# total tests
hist(total_tests, 
     main = "Normalility proportion of total_tests", 
     xlab = "total_tests")

qqnorm(total_tests)
qqline(total_tests, col = "red")


par <- opar

summary(covid_Africa)

covid_Africa <- covid_Africa[is.finite(rowSums(covid_Africa)),]
detach(covid_Africa)




# model fitting  


attach(covid_Africa)
mlr_model <- lm(new_cases ~  total_vaccinations + new_deaths + female_smokers +
                  + total_tests + aged_70_older + aged_65_older  +
                  population + new_tests,  
                data = covid_Africa)


summary(mlr_model)


set.seed(1)
no_rows_data <- nrow(covid_Africa)
my_sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace =FALSE)

training_data <- covid_Africa[my_sample,]
testing_data <- covid_Africa[-my_sample, ]

model_fit <- lm(new_cases ~  total_vaccinations + male_smokers + female_smokers 
                + total_tests + aged_70_older + aged_65_older +
                  population,    data = training_data)
summary(model_fit)
confint(model_fit)


library(ggplot2)
library(car)
qqPlot(model_fit, labels=row.names(location),                                           
       id.method="identify", 
       simulate=TRUE, 
       main="Q-Q Plot")
training_data[70124,]                                                            
training_data[70118,]                                                            


fitted(model_fit)[70124]                                                               
fitted(model_fit)[70118]
covid_Africa <- covid_Africa[!(row.names(covid_Africa) %in% c("70120", "70117,", "70119","70111","5189","5184","51873", "51870", "7118","7125")),]

attach(covid_Africa)


set.seed(1)
no_row_data <- nrow(covid_Africa)
my_sample <- sample(1:no_row_data, 
                    size = round(0.7 * no_row_data), 
                    replace = FALSE)
training_data <- covid_Africa[my_sample, ]
testing_data <- covid_Africa[-my_sample, ]

model_fit <- lm(new_cases ~  male_smokers + female_smokers 
                + total_tests + aged_70_older + aged_65_older + total_vaccinations,
                data = training_data)
outlierTest(model_fit)

student_fit <- rstudent(model_fit)
hist(student_fit,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")
rug(jitter(student_fit), col="brown")
curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE,col="blue", lwd=2)
lines(density(student_fit)$x, density(student_fit)$y, col="red", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)

library(car)
crPlots(model_fit)
cutoff <- 4/(nrow(training_data) - length(model_fit$coefficients) - 2)
plot(model_fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")
avPlots(model_fit, ask=FALSE)                                                         
influencePlot(model_fit, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")              
ncvTest(model_fit)

#
#
spreadLevelPlot(model_fit)
#install.packages("gvlma")
library(gvlma) 
library(mice) 
gvmodel <- gvlma(model_fit)  
summary(gvmodel)
#
#
library(car)
vif(model_fit)
sqrt(vif(model_fit)) > 2
#
#
fit_model <- lm(new_cases ~  male_smokers + female_smokers 
                + total_tests + aged_70_older + aged_65_older, data= testing_data)
fit_model_sqrt <- lm(new_cases ~  male_smokers + female_smokers 
                     + total_tests + aged_70_older + aged_65_older , data= testing_data)
predicted_cases <- predict(fit_model, testing_data)
predicted_cases_sqrt <- predict(fit_model_sqrt, testing_data)
converted_cases_sqrt <- predicted_cases_sqrt ^2

#
actuals_predictions <- data.frame(cbind(actuals = testing_data$new_cases, predicted = predicted_cases))
head(actuals_predictions,100)

#
actuals_predictions_sqrt <- data.frame(cbind(actuals = testing_data$new_cases, predicted = predicted_cases_sqrt))
head(actuals_predictions_sqrt,100)

# checking the correlation accuracy
correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

# checking the correlation accuracy_sqrt
correlation_accuracy_sqrt <- cor(actuals_predictions_sqrt)
correlation_accuracy_sqrt
# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
min_max_accuracy


# check the Min - max accuracy
min_max_accuracy <- mean(apply(actuals_predictions_sqrt, 1, min) / apply(actuals_predictions_sqrt, 1, max))
min_max_accuracy


# checking the Residual Standard Error (RSE), or sigma
sigma(fit_model)/ mean(testing_data$new_cases)

# checking Residual Standard Error (RSE), or sigma for transformed variable
sigma(fit_model_sqrt)/ mean(testing_data$new_cases)

head(actuals_predictions)

