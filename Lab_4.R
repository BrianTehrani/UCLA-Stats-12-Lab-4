##### Exercise 1 #####
pawnee <- read.csv (file.choose())

#a
head(pawnee)
dim(pawnee)

#b
size <- c(30)
set.seed(1337)
sample.index <- sample(541, size)
sample.index
pawnee[sample.index,]

#c
pawnee.sample.mean <- mean(pawnee[sample.index,]$Arsenic)
  
  #proportion of major health issue
mean(pawnee[sample.index,]$New_hlth_issue == 'Y') 


#d
  # the symbol we would use is "x-bar" for a sample 
  # The symbol we would use for a sample proportion is p-hat

#e
  # Sample data is not normally distruibuted
pawnee.sample.sd <- sd(pawnee[sample.index,]$Arsenic)

  # 90% CI
error.90 <- qt(0.950, df = size - 1) * pawnee.sample.sd / sqrt(size)
left.90 <- pawnee.sample.mean - error.90
right.90 <- pawnee.sample.mean + error.90

print(paste("90% CI: (", formatC(left.90, digits = 3, format = "f"), ",", 
            formatC(right.90, digits = 3, format = "f"),  ")"), sep = "")

  # 95% CI
error.95 <- qt(0.975, df = size - 1) * pawnee.sample.sd / sqrt(size)
left.95 <- pawnee.sample.mean - error.95
right.95 <- pawnee.sample.mean + error.95

print(paste("95% CI: (", formatC(left.95, digits = 3, format = "f"), ",", 
            formatC(right.95, digits = 3, format = "f"),  ")"), sep = "")

  # 99% CI
error.99 <- qt(0.999, df = size - 1) * pawnee.sample.sd / sqrt(size)
left.99 <- pawnee.sample.mean - error.99
right.99 <- pawnee.sample.mean + error.99

print(paste("99% CI: (", formatC(left.99, digits = 3, format = "f"), ",", 
            formatC(right.99, digits = 3, format = "f"),  ")"), sep = "")

#f
error.100 <- qt(1, df = size - 1) * pawnee.sample.sd / sqrt(size)
left.100 <- pawnee.sample.mean - error.100
right.100 <- pawnee.sample.mean + error.100

print(paste("100% CI: (", formatC(left.100, digits = 3, format = "f"), ",", 
            formatC(right.100, digits = 3, format = "f"),  ")"), sep = "")

#g
mean(pawnee$New_hlth_issue == "Y")

#f
plot(density(pawnee$Arsenic), main = "Distribution of Arsenic Levels",
     xlab = "Concentration in ppm")

##### Exercise 2 #####
#a
# We first create objects for common quantities we will use for this exercise.
n <- 30 # The sample size
N <- 541 # The population size
M <- 1000 # Number of samples/repetitions
# Create vectors to store the simulated proportions from each repetition.
phats <- c() # for sample proportions
# Set the seed for reproduceability
set.seed(123)
# Always set the seed OUTSIDE the for loop.
# Now we start the loop. Let i cycle over the numbers 1 to 1000 (i.e., iterate 1000 times).
for(i in 1:M){
  # The i-th iteration of the for loop represents a single repetition.
  # Take a simple random sample of size n from the population of size N.
  index <- sample(N, size = n)
  # Save the random sample in the sample_i vector.
  sample_i <- pawnee[index,]
  # Compute the proportion of the i-th sample of households with a new health issue.
  phats[i] <- mean(sample_i$New_hlth_issue == "Y")
}

  # Create a relative frequency histogram

h <- hist(phats, col =  "grey")
h$density <- h$counts / sum(h$counts) *15
plot(h, freq = FALSE, main = "Relative Frequency Historgam for the\n Sampling Distribution",
     xlab = "phats", ylab = " Relative Frequency scaled by factor of 15")
lines(density(phats), col = "blue")

#b

mean_phats <- mean(phats)
sd_phats <- sd(phats)
mean_phats
sd_phats

#c
  # The sample distributions of the proportions appears to be normal based on the histogram 
  # plot. Additonally, there appears to be no skew or major outliers, as much of the
  # data is centered about the mean.

#d
 # Sample sixe is n = 30. The mean of the sample proportions is 

##### Exercise 3 #####
#a
n <- 30 # The sample size
N <- 541 # The population size
M <- 1000 # Number of samples/repetitions
# Create vectors to store the simulated proportions from each repetition.
xbar<- c() # for sample mean
# Set the seed for reproduceability
set.seed(123)
# Always set the seed OUTSIDE the for loop.
# Now we start the loop. Let i cycle over the numbers 1 to 1000 (i.e., iterate 1000 times).
for(i in 1:M){
  # The i-th iteration of the for loop represents a single repetition.
  # Take a simple random sample of size n from the population of size N.
  index <- sample(N, size = n)
  # Save the random sample in the sample_i vector.
  sample_i <- pawnee[index,]
  # Compute the means of the i-th sample of households with a new health issue.
  xbar[i] <- mean(sample_i$Arsenic)
}

#b
h1 <- hist(xbar, col = "grey")
h1$density <- h1$counts / sum(h1$counts)
plot(h1, freq = FALSE, main = "Relative Frequency distributions of\n Sample Means of Arsenic Concentration",
     xlab = "Average Arsenic Concentration [ppm]", ylab = "Relative Frequencies")
lines(density(xbar), col = "red")

#c
  #The sample dist of the means of arsenic are not normal

