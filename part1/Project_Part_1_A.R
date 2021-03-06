###################
# FINAL PROJECT: PART 1A - STAT 3201(DONGES)
# HARSHIL PATEL, 11/23/2018   
###################

#set working directory
setwd("C:/Users/offic/OneDrive - The Ohio State University/data analytics/R programming Files/project")

set.seed(35) #this ensures you'll get the same random process every time you run the code

#################################
# Part 1A:
#################################

#Model the time spent studying for an exam. Take 1,000 random samples of size 25 from the distributions below. 
#Plot the empirical distribution of the sample mean, estimate the mean of the sample mean, and estimate the
#standard deviation of the sample mean. Compare the results to the theoretical results.

#THe distribution of one student's time spent studying for an exam is 
#normally distrubuted, with mean 5 hours and standard deviation of 1.5 hours.

mean<-5
stdeviation<-1.5
sampleSize<-25
numberOfSamples<-1000
vectorOfSampleMeans<-vector()
samples<-vector()
# use a loop to generate and record 1000 random sample means with x[i]~N(5,1.5^2) from samples of size 25.
for (i in 1:numberOfSamples) {
  samples<- rnorm(sampleSize,mean = mean,sd = stdeviation)
  vectorOfSampleMeans[i]<-mean(samples)
}
#Determine the estimated mean & st. deviation of sample mean.
avgOfSampleMean<-mean(vectorOfSampleMeans)
sdtOfSampleMean<-sd(vectorOfSampleMeans)

#Determine the theoretical mean & st. deviation of sample mean.
theoreticalMeanOfSampleMean<-mean
theoreticalSDTOfSampleMean<-(1.5)/(sqrt(sampleSize))

#Data Visualization, create a histrogram & boxplot of sample mean generated from previous operation.
par(mfrow=c(2,1))
hist(vectorOfSampleMeans,
    main = expression("Distribution of the Average Time Spent Studying for an Exam"~ (X[i]~"~"~N(5,1.5^{2}))),
    ylab='f(x)',
    xlab = "Sample Mean in Hours (n=25)",
    breaks = 25,
    xlim = c(min(vectorOfSampleMeans),max(vectorOfSampleMeans)),
    freq = FALSE)
boxplot(vectorOfSampleMeans,
    main = expression("Distribution of the Average Time Spent Studying for an Exam"~ (X[i]~"~"~N(5,1.5^{2}))),
    xlab = "Sample Mean in Hours (n=25)",
    horizontal = TRUE)