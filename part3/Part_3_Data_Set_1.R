#########################################################
# FINAL PROJECT: PART 3.Data Set 1 - STAT 3201(DONGES)
# HARSHIL PATEL, 11/24/2018  
#########################################################

#set working directory
setwd("C:/Users/offic/OneDrive - The Ohio State University/data analytics/R programming Files/project/part3")
set.seed(35)

#################################
# Part 3 Data Set 1:
#################################

#Project Data Set 1.csv gives 25 observations of the time spent studying for an exam of students. Use this data to take 1,000 resamples (i.e.,
#sample with replacement) of size 25 from the observed data. Plot the empirical distribution of the sample mean, estimate the mean of the sample mean, 
#and estimate the standard deviation of the sample mean. Also ind the estimated 5 number summary and the estimated 2.5th and 97.5th
#percentiles. Compare and contrast your results for the two sample sizes.

#EXTRACT DATA FROM DATA SET FILE(.CSV)
dataset1<-read.csv("Project Data 1.csv", header = TRUE)
studyTimeValues<- dataset1$x #vector of 25 observations of a student's time spent studying for exam.

numberOfResamples<-1000
vectorOfSampleMeans<-vector()
samples<-vector()

# use a loop to generate and record 1000 random sample means from observed data using resampling technique from part two .
for (i in 1:numberOfResamples) {
  samples<- sample(studyTimeValues,25, replace = TRUE)
  vectorOfSampleMeans[i]<-mean(samples)}

#Determine the estimated mean & st. deviation of sample mean, and estimated 5 number summary.
avgOfSampleMean<-mean(vectorOfSampleMeans)
sdtOfSampleMean<-sd(vectorOfSampleMeans)
summary(vectorOfSampleMeans)

#Determine estimated estimated 2.5th and 97.5th percentiles
quantile(vectorOfSampleMeans, c(.0205, .975))

#Data Visualization, create a histrogram & boxplot of sample mean generated from previous operation.
par(mfrow=c(2,1))
hist(vectorOfSampleMeans,
     main = expression("Distribution of the Average Time Spent Studying for an Exam (Project Data Set 1)"),
     ylab='f(x)',
     xlab = "Sample Mean in Hours (with resampling and replacement)",
     breaks = 20,
     xlim = c(min(vectorOfSampleMeans),max(vectorOfSampleMeans)),
     freq = FALSE)
boxplot(vectorOfSampleMeans,
        main = expression("Distribution of the Average Time Spent Studying for an Exam (Project Data Set 1)"),
        xlab = "Sample Mean in Hours (with resampling and replacement)",
        horizontal = TRUE)

















