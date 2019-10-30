###################
# FINAL PROJECT: PART 1B - STAT 3201(DONGES)
# HARSHIL PATEL, 11/23/2018   
###################

#set working directory
setwd("C:/Users/offic/OneDrive - The Ohio State University/data analytics/R programming Files/project")

set.seed(35) #this ensures you'll get the same random process every time you run the code

#################################
# Part 1B:
#################################

#Model the time spent studying for an exam. Take 1,000 random samples of size 25 from the distributions below. 
#Plot the empirical distribution of the sample mean, estimate the mean of the sample mean, and estimate the
#standard deviation of the sample mean. Compare the results to the theoretical results.

#THe distribution of one student's time spent studying for an exam is 
#uniformally distrubuted, with time spend can be between 0 to 10 hours.

alpha<-0 #min amount student could study
beta<-10 #max amount student could study
sampleSize<-25
numberOfSamples<-1000
vectorOfSampleMeans<-vector()
samples<-vector()
# use a loop to generate and record 1000 random sample means with x[i]~Unif(0,10) from samples of size 25.
for (i in 1:numberOfSamples) {
  samples<- runif(sampleSize, min = alpha, max = beta)
  vectorOfSampleMeans[i]<-mean(samples)
}
#Determine the estimated mean & st. deviation of sample mean.
avgOfSampleMean<-mean(vectorOfSampleMeans)
sdtOfSampleMean<-sd(vectorOfSampleMeans)

#Determine the theoretical mean & st. deviation of sample mean.
theoreticalMeanOfSampleMean<-(beta+alpha)/2
theoreticalSDTOfSampleMean<-sqrt((((beta-alpha)^2)/12)/(sampleSize))

#Data Visualization, create a histrogram & boxplot of sample mean generated from previous operation.
par(mfrow=c(2,1))
hist(vectorOfSampleMeans,
    main = expression("Distribution of the Average Time Spent Studying for an Exam"~ (X[i]~"~"~Unif(0,10))),
    ylab='f(x)',
    xlab = "Sample Mean in Hours (n=25)",
    breaks = 25,
    xlim = c(min(vectorOfSampleMeans),max(vectorOfSampleMeans)),
    freq = FALSE)
boxplot(vectorOfSampleMeans,
    main = expression("Distribution of the Average Time Spent Studying for an Exam"~ (X[i]~"~"~Unif(0,10))),
    xlab = "Sample Mean in Hours (n=25)",
    horizontal = TRUE)