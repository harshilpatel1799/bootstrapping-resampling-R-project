######################################
# FINAL PROJECT: PART 2C - STAT 3201(DONGES)
# HARSHIL PATEL, 11/23/2018   
######################################

#set working directory
setwd("C:/Users/offic/OneDrive - The Ohio State University/data analytics/R programming Files/project")

set.seed(35) #this ensures you'll get the same random process every time you run the code

#################################
# Part 2C:
#################################

#Model the time spent studying for an exam. Take one random sample of size 25 from the distribution below. Then, take 1,000 resamples (i.e.,
#sample with replacement) of size 25 from the sample. Plot the empirical distribution of the sample mean, estimate the mean of the sample mean, 
#and estimate the standard deviation of the sample mean. 

#the distribution of a student's time spent studying for an exam is 
#distrubuted with a gamma distribution, with parameters: alpha=5, beta=1

alpha<-5 
beta<-1 
sampleSize<-25
originalSample<-rgamma(sampleSize,alpha,scale = beta) #generate one random sample of size 25 with x[i]~Gamma(5,1)
numberOfResamples<-1000
vectorOfSampleMeans<-vector()
samples<-vector()

# use a loop to generate and record 1000 random sample means with from orginal sample of size 25.
for (i in 1:numberOfResamples) {
  samples<-sample(originalSample,25, replace = TRUE)
  vectorOfSampleMeans[i]<-mean(samples)}

#Determine the estimated mean & st. deviation of sample mean.
avgOfSampleMean<-mean(vectorOfSampleMeans)
sdtOfSampleMean<-sd(vectorOfSampleMeans)

#Determine the theoretical mean & st. deviation of sample mean.
theoreticalMeanOfSampleMean<-(beta*alpha)
theoreticalSDTOfSampleMean<-sqrt((alpha*(beta^2))/(sampleSize))

#Data Visualization, create a histrogram & boxplot of sample mean generated from previous operation.
par(mfrow=c(2,1))
hist(vectorOfSampleMeans,
    main = expression("Distribution of the Average Time Spent Studying for an Exam "~ (X[i]~"~"~Gamma(5,1))),
    ylab='f(x)',
    xlab = "Sample Mean in Hours (n=25, with resampling and replacement)",
    breaks = 25,
    xlim = c(min(vectorOfSampleMeans),max(vectorOfSampleMeans)),
    freq = FALSE)
boxplot(vectorOfSampleMeans,
    main = expression("Distribution of the Average Time Spent Studying for an Exam"~ (X[i]~"~"~Gamma(5,1))),
    xlab = "Sample Mean in Hours (n=25, with resampling and replacement)",
    horizontal = TRUE)