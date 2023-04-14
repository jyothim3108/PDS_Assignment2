#Load the data-set to data
data = read.csv("C:\\diabetes.csv")
data

summary(data)

#Check for empty and null values
any(is.na(data))
any(is.null(data))

#Setting the seed value to last 4 digits of student ID
set.seed(0132)

#Take a random sample with 25 observation
randSample <- data[sample(nrow(data), 25, replace = FALSE), ]

summary(randSample)
nrow(randSample)

# Calculating the mean of data and random samples
data_mean_glucose <- mean(data$Glucose)
data_max_glucose <- max(data$Glucose)
random_sample_mean_glucose <- mean(data$Glucose)
random_sample_max_glucose <- max(data$Glucose)

#Print the calculated results
sprintf("Population mean glucose is %f", data_mean_glucose)
sprintf("Population max glucose is %f", data_max_glucose)
sprintf("Random sample mean glucose is %f", random_sample_mean_glucose)
sprintf("Random sample max glucose is %f", random_sample_max_glucose)

#Plotting the calculations in a bar graph
stats <- c(data_mean_glucose, random_sample_mean_glucose, data_max_glucose, random_sample_max_glucose)
names(stats) <- c("D.Mean", "R.Mean", "D.Max", "R.Max")
barplot(stats, main="Comparison of Glucose Statistics", ylab="Glucose", col=c("red", "green", "red", "green"), ylim=c(0, 250))

#B
#Find the 98th percentile of BMI of your sample and the population and compare the results using charts.

#Extract the BMI
bmi <- data$BMI
bmi

#Calculate the 98th percentile of BMI column
data_98th_percentile <- quantile(bmi, 0.98)
samp_98th_percentile <- quantile(sample(bmi, length(bmi), replace=TRUE), 0.98)

data_98th_percentile
samp_98th_percentile

#Plot the BMI distribution --> histogram plot
hist(bmi, breaks=20, xlab="BMI", main="BMI Distribution for Population",col="red",border = "green")
abline(v=data_98th_percentile, col="darkgreen", lty=2)
legend("topright", legend=c("98th percentile"), col="darkgreen", lty=2)

hist(sample(bmi, length(bmi), replace=TRUE), breaks=20, xlab="BMI", main="BMI Distribution for Sample", col="blue",border = "yellow")
abline(v=samp_98th_percentile, col="black", lty=2)
legend("topright", legend=c("98th percentile"), col="black", lty=2)

#C
#Using bootstrap (replace= True), create 500 samples (of 150 observation each) from the population and find the average mean, 
#standard deviation and percentile for Blood Pressure and compare this with these statistics from the population for the same variable. 
#Again, you should create charts for this comparison. Report on your findings.

#Statistics of population - mean, standard deviation, percentile
mean_blood_pressure <- mean(data$BloodPressure)
sample_blood_pressure <- sd(data$BloodPressure)
percentile_blood_pressure <- quantile(data$BloodPressure, c(0.25, 0.5, 0.75))
sprintf("%f, %f, %f are the mean_blood_pressure, sample_blood_pressure, percentile_blood_pressure(25,50,75) of the population ",mean_blood_pressure,sample_blood_pressure,percentile_blood_pressure )

#Creating empty vectors and iterating over the samples
bp_blank_mean_boot <- rep(NA, 500)
bp_blank_sd_boot <- rep(NA, 500)
bp_blank_quant_boot <- matrix(NA, nrow = 3, ncol = 500)

for (i in 1:500) {
  sample_iterator <- sample(data$BloodPressure, size = 150, replace = TRUE)
  bp_blank_mean_boot[i] <- mean(sample_iterator)
  bp_blank_sd_boot[i] <- sd(sample_iterator)
  bp_blank_quant_boot[, i] <- quantile(sample_iterator, probs = c(0.25, 0.5, 0.75))
}

# Comparing bootstrap and population stats for blood pressure
par(mfrow = c(3, 2))
hist(data$BloodPressure, main = "Population", xlab = "Blood Pressure")
hist(bp_blank_mean_boot, main = "Bootstrap Mean", xlab = "Blood Pressure")
abline(v = mean_blood_pressure, col = "blue")
hist(bp_blank_sd_boot, main = "Bootstrap SD", xlab = "Blood Pressure")
abline(v = sample_blood_pressure, col = "yellow")
matplot(bp_blank_quant_boot, type = "l", main = "Bootstrap Percentiles", xlab = "Sample",
        ylab = "Blood Pressure", lty = 1, col = 1:3)
lines(percentile_blood_pressure, lty = 2, col = "green")