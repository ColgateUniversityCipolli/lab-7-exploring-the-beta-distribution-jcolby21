#Task One
library(tidyverse)

#create a function that takes alpha and beta as inputs and computes the 
#mean, variance, skewness, and excess kurtosis
Beta = function(alpha, beta){
mean=alpha/(alpha+beta)
variance= (alpha*beta)/((alpha+beta+1)*(alpha+beta)^2)
skewness= (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha+beta+2)*sqrt(alpha*beta))
kurtosis= ((6*((alpha-beta)^2*(alpha+beta+1)-alpha*beta*(alpha+beta+2)))/(alpha*beta*(alpha+beta+2)*(alpha+beta+3)))
return(c(alpha=alpha, beta=beta, mean = mean, variance = variance, skewness = skewness, kurtosis = kurtosis))
}


#Case 1
alpha=2
beta=5
case_1=Beta(alpha,beta)
q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

ggplot(data= q1.fig.dat)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) +  # plot gaussian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom

#Case 2
alpha=5
beta=5
case_2=Beta(alpha,beta)
q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

ggplot(data= q1.fig.dat)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,5)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("red", "grey"))+                 # change colors
  theme(legend.position = "bottom")

#Case 3
alpha=5
beta=2
case_3=Beta(alpha,beta)
q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

ggplot(data= q1.fig.dat)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,2)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("blue", "grey"))+                 # change colors
  theme(legend.position = "bottom")

#Case 4
alpha=0.5
beta=0.5
case_4=Beta(alpha,beta)
q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

ggplot(data= q1.fig.dat)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(0.5,0.5)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("orange", "grey"))+                 # change colors
  theme(legend.position = "bottom")

#create a table containing all four cases
combined_matrix=rbind(case_1, case_2, case_3, case_4)
results_table=as.data.frame(combined_matrix)

#Task Two
# function
beta.moment=function(alpha, beta, k, centered){
  if (centered==F){
    integrand= function(x) dbeta(x, alpha, beta)*x^k #function under integral
    uncentered_moment=integrate(integrand,lower=0, upper=1)#integrates over support [0,1]
    return(uncentered_moment$value)}
  if (centered==T){
    mu_x=alpha/(alpha+beta) #E(x) mean 
    integrand=function(x) dbeta(x, alpha, beta)*(x-mu_x)^k #function under integral
    centered_moment=integrate(integrand,lower=0, upper=1) #integrates over support [0,1]
    return(centered_moment$value)}
}

#Case 1
alpha=2
beta=5
part_2_case_1=beta.moment(alpha, beta, k=4, centered=T)#change for each moment

#Case 2
alpha=5
beta=5
part_2_case_2=beta.moment(alpha, beta, k=2, centered=F)#change for each moment

#Case 3
alpha=5
beta=2
part_2_case_3=beta.moment(alpha, beta, k=2, centered=F)#change for each moment

#Case 4
alpha=0.5
beta=0.5
part_2_case_4=beta.moment(alpha, beta, k=2, centered=F)#change for each moment


#Task Three
library(e1071) #used for some statistics calculations
#Case 1
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 2
beta <- 5
beta.sample <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha,   # alpha parameter
                     shape2 = beta)    # beta parameter
# Create a data frame for plotting
data_case_1 <- tibble(data_case_1 = beta.sample)

# Plot the histogram, density estimate, and Beta(2,5) distribution
ggplot(data_case_1, aes(x = data_case_1)) +
  # Histogram with density fill
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  
  # Estimated density from the data
  stat_density(aes(color = "Estimated Density"), size = 1, geom="line") +
  
  # Superimpose the Beta(2,5) distribution
  stat_function(
    fun = dbeta, args = list(shape1 = alpha, shape2 = beta), 
    size = 1,aes(color = "Beta(2, 5) Distribution")) +
  
  # Labels and themes
  labs(
    title = "Histogram and Density of Beta Sample with Beta(2, 5) Distribution",
    x = "x",
    y = "Density"
  ) +
  theme_minimal()+
  theme(legend.position = "bottom")



summary_stats_case_1 <- tibble(data = beta.sample) |>
  summarize(
    mean_sample = mean(data),
    variance_sample = var(data),
    skewness_sample = skewness(data),
    kurtosis_sample = kurtosis(data)  # Excess kurtosis
  )

#Case 2
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 5
beta <- 5
beta.sample <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha,   # alpha parameter
                     shape2 = beta)    # beta parameter

# Create a data frame for plotting
data_case_2 <- tibble(data_case_2 = beta.sample)

# Plot the histogram, density estimate, and Beta(5,5) distribution
ggplot(data_case_2, aes(x = data_case_2)) +
  # Histogram with density fill
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  
  # Estimated density from the data
  stat_density(aes(color = "Estimated Density"), size = 1, geom="line") +
  
  # Superimpose the Beta(5,5) distribution
  stat_function(
    fun = dbeta, args = list(shape1 = alpha, shape2 = beta), 
   size = 1,aes(color = "Beta(5, 5) Distribution")) +
  
  # Labels and themes
  labs(
    title = "Histogram and Density of Beta Sample with Beta(5, 5) Distribution",
    x = "x",
    y = "Density"
  ) +
  theme_minimal()+
  theme(legend.position = "bottom")

summary_stats_case_2 <- tibble(data = beta.sample) |>
  summarize(
    mean_sample = mean(data),
    variance_sample = var(data),
    skewness_sample = skewness(data),
    kurtosis_sample = kurtosis(data)  # Excess kurtosis
  )
#Case 3
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 5
beta <- 2
beta.sample <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha,   # alpha parameter
                     shape2 = beta)    # beta parameter
# Create a data frame for plotting
data_case_3 <- tibble(data_case_3 = beta.sample)
# Plot the histogram, density estimate, and Beta(5,2) distribution
ggplot(data_case_3, aes(x = data_case_3)) +
  # Histogram with density fill
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  
  # Estimated density from the data
  stat_density(aes(color = "Estimated Density"), size = 1, geom="line") +
  
  # Superimpose the Beta(5,2) distribution
  stat_function(
    fun = dbeta, args = list(shape1 = alpha, shape2 = beta), 
    size = 1,aes(color = "Beta(5, 2) Distribution")) +
  
  # Labels and themes
  labs(
    title = "Histogram and Density of Beta Sample with Beta(5, 2) Distribution",
    x = "x",
    y = "Density"
  ) +
  theme_minimal()+
  theme(legend.position = "bottom")


summary_stats_case_3 <- tibble(data = beta.sample) |>
  summarize(
    mean_sample = mean(data),
    variance_sample = var(data),
    skewness_sample = skewness(data),
    kurtosis_sample = kurtosis(data)  # Excess kurtosis
  )

#Case 4
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 0.5
beta <- 0.5
beta.sample <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha,   # alpha parameter
                     shape2 = beta)    # beta parameter

#plot a histogram
# Create a data frame for plotting
data_case_4 <- tibble(data_case_4 = beta.sample)

# Plot the histogram, density estimate, and Beta(0.5,0.5) distribution
ggplot(data_case_4, aes(x = data_case_4)) +
  # Histogram with density fill
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  
  # Estimated density from the data
  stat_density(aes(color = "Estimated Density"), size = 1, geom="line") +
  
  # Superimpose the Beta(0.5,0.5) distribution
  stat_function(
    fun = dbeta, args = list(shape1 = alpha, shape2 = beta), 
    size = 1,aes(color = "Beta(0.5, 0.5) Distribution")) +
  
  # Labels and themes
  labs(
    title = "Histogram and Density of Beta Sample with Beta(0.5, 0.5) Distribution",
    x = "x",
    y = "Density"
  ) +
  theme_minimal()+
  theme(legend.position = "bottom")

summary_stats_case_4 <- tibble(data = beta.sample) |>
  summarize(
    mean_sample = mean(data),
    variance_sample = var(data),
    skewness_sample = skewness(data),
    kurtosis_sample = kurtosis(data)  # Excess kurtosis
  )


#combine all four of the cases of summary stats into one table
combined_summary_matrix=rbind(summary_stats_case_1, summary_stats_case_2, summary_stats_case_3, summary_stats_case_4)
summarize_table=as.data.frame(combined_summary_matrix)

#Task Four
library(cumstats)
library(patchwork)
alpha=2
beta=5

set.seed(7272)
n <- 500
beta.sample <- rbeta(n, alpha, beta)

# Create a data frame with the cumulative statistics
cum_stats_df <- data.frame(
  x=1:n,
  mean = cummean(beta.sample),
  variance = cumvar(beta.sample),
  skewness = cumskew(beta.sample),
  kurtosis = cumkurt(beta.sample)-3
)

# real values #3rd column mean, 4th column variance, 5th column skewness, 6th column kurtosis
case_1=Beta(alpha,beta)

p_mean <- ggplot(cum_stats_df, aes(x = x, y = mean)) +
  geom_line() +
  geom_hline(yintercept = case_1[3], linetype = 'dashed', color = 'red') +
  labs(title = 'Cumulative Mean')

p_variance <- ggplot(cum_stats_df, aes(x = x, y = variance)) +
  geom_line() +
  geom_hline(yintercept = case_1[4], linetype = 'dashed', color = 'red') +
  labs(title = 'Cumulative Variance')

p_skewness <- ggplot(cum_stats_df, aes(x = x, y = skewness)) +
  geom_line() +
  geom_hline(yintercept = case_1[5], linetype = 'dashed', color = 'red') +
  labs(title = 'Cumulative Skewness')

p_kurtosis <- ggplot(cum_stats_df, aes(x = x, y = kurtosis)) +
  geom_line() +
  geom_hline(yintercept = case_1[6], linetype = 'dashed', color = 'red') +
  labs(title = 'Cumulative Kurtosis')

# Combine plots into a 2x2 grid
grid_plot = p_mean + p_variance + p_skewness + p_kurtosis + plot_layout(ncol = 2)

#prints the plot
print(grid_plot)


#create a for loop
alpha=2
beta=5
n=500

for (i in 2:50) {
  set.seed(7272 + i)
  new_data <- rbeta(n, alpha, beta)
  new_cum_stats_df <- data.frame(
    x = 1:n,
    mean = cummean(new_data),
    variance = cumvar(new_data),
    skewness = cumskew(new_data),
    kurtosis = cumkurt(new_data)-3
  )
  # Add the new lines for the statistics
  p_mean = p_mean +
    geom_line(data=new_cum_stats_df, aes(x=x, y=mean), color=i)
  p_variance = p_variance +
    geom_line(data=new_cum_stats_df, aes(x=x, y=variance), color=i)
  p_skewness = p_skewness +
    geom_line(data=new_cum_stats_df, aes(x=x, y=skewness), color=i)
  p_kurtosis = p_kurtosis +
    geom_line(data=new_cum_stats_df, aes(x=x, y=kurtosis), color=i)
    
  }
#combines new plots into 2x2
new_grid_plot= p_mean + p_variance + p_skewness + p_kurtosis + plot_layout(ncol = 2)
# Prints plot
print(new_grid_plot)

#Task Five
library(tidyverse)
library(ggplot2)
library(moments)
n=500
alpha=2
beta=5

#creates blank data frame to store data with 1000 entries
means <- numeric(1000)
variances <- numeric(1000)
skewnesses <- numeric(1000)
kurtoses <- numeric(1000)

# Loop to generate the data and compute statistics
for (i in 1:1000) {
  set.seed(7272 + i)  # set seed
  
  # Simulate data from the Beta distribution
  data <- rbeta(n, shape1 = alpha, shape2 = beta)
  
  # Compute statistics
  means[i] <- mean(data)
  variances[i] <- var(data)
  skewnesses[i] <- skewness(data)
  kurtoses[i] <- kurtosis(data) - 3  # Excess kurtosis is kurtosis - 3
}

# Create a data frame with the statistics
stats_df <- data.frame(
  means = means,
  variances = variances,
  skewnesses = skewnesses,
  kurtoses = kurtoses
)

# Plot histograms with density estimates for each statistic
#plot for mean
ggplot(stats_df) +
  geom_histogram(aes(x = means, y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(aes(x = means), color = "red", size = 1) +
  ggtitle("Sampling Distribution of Means") +
  theme_minimal()

#Plot for variance
ggplot(stats_df) +
  geom_histogram(aes(x = variances, y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(aes(x = variances), color = "red", size = 1) +
  ggtitle("Sampling Distribution of Variances") +
  theme_minimal()

#plot for skewness
ggplot(stats_df) +
  geom_histogram(aes(x = skewnesses, y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(aes(x = skewnesses), color = "red", size = 1) +
  ggtitle("Sampling Distribution of Skewnesses") +
  theme_minimal()

#plot for excess kurtosis
ggplot(stats_df) +
  geom_histogram(aes(x = kurtoses, y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(aes(x = kurtoses), color = "red", size = 1) +
  ggtitle("Sampling Distribution of Excess Kurtoses") +
  theme_minimal()

#Optional Challenge 1
library(ggplot2)
library(gganimate)
library(gifski)

alpha_values <- seq(1, 10, by = 1)
beta <- 5

beta_data <- data.frame(
  x = rep(seq(0, 1, length.out = 100), times = length(alpha_values)),
  alpha = rep(alpha_values, each = 100),
  beta = beta
)

beta_data$y <- mapply(function(x, alpha, beta) {
  dbeta(x, shape1 = alpha, shape2 = beta)
}, beta_data$x, beta_data$alpha, beta_data$beta)

# ggplot with animation
p1 <- ggplot(beta_data, aes(x = x, y = y, group = alpha, color = as.factor(alpha))) +
  geom_line() +
  labs(title = 'Beta Distribution: α = {frame_time}', x = 'x', y = 'Density') +
  transition_time(alpha) +
  ease_aes('linear')

animate(p1, nframes = 100, duration = 5, renderer = gifski_renderer())

#optional challenge 2
beta_values <- seq(1, 10, by = 1)
alpha <- 5

beta_data <- data.frame(
  x = rep(seq(0, 1, length.out = 100), times = length(beta_values)),
  alpha = alpha,
  beta = rep(beta_values, each = 100)
)

beta_data$y <- mapply(function(x, alpha, beta) {
  dbeta(x, shape1 = alpha, shape2 = beta)
}, beta_data$x, beta_data$alpha, beta_data$beta)

# ggplot with animation
p2 <- ggplot(beta_data, aes(x = x, y = y, group = beta, color = as.factor(beta))) +
  geom_line() +
  labs(title = 'Beta Distribution: β = {frame_time}', x = 'x', y = 'Density') +
  transition_time(beta) +
  ease_aes('linear')

animate(p2, nframes = 100, duration = 5, renderer = gifski_renderer())

#optional challenge 3
# Plot the Beta(1, 1) distribution (Uniform Distribution)
x <- seq(0, 1, length.out = 100)
y <- dbeta(x, shape1 = 1, shape2 = 1)

ggplot(data.frame(x, y), aes(x, y)) +
  geom_line() +
  labs(title = "Beta(α = 1, β = 1) Distribution (Uniform)", x = "x", y = "Density")

#Lab 8
#Task 6
library(readr)
library(tidyverse)
library(ggplot2)

#load data and add desired column
data=read_csv("deathcrudedata.csv")|>
  mutate(col_2022_rate=`2022`/1000)

#Calculate the sample mean and variance
mean_rate <- mean(data$col_2022_rate, na.rm = TRUE)
var_rate <- var(data$col_2022_rate, na.rm = TRUE)

# Solving for alpha and beta using formulas
alpha <- (mean_rate * (mean_rate * (1 - mean_rate) / var_rate - 1))
beta <- (alpha * (1 - mean_rate) / mean_rate)

#plot the data with the beta distribution
ggplot(data, aes(x=col_2022_rate)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta), 
                color = "red", size = 1) +
  labs(title = "Beta Distribution Fit to Country Death Rates (2022)",
       x = "Death Rate (per 1000 citizens scaled to [0,1])",
       y = "Density")

#Task Seven
library(nleqslv)
col_2022_rate=data$col_2022_rate

#Using MOM
#for the first example
#first moment E(X)]
m1=mean(col_2022_rate, na.rm=T) #263 observations that are not NA
#second moment E(X^2)
m2=mean(col_2022_rate^2, na.rm=T) #squared xs and 263 observations that are not NA

#Use equation solver to find parameters
MOM.beta=function(data, par){
  m1=mean(data, na.rm=T)
  m2=mean(data^2, na.rm=T)
  alpha=par[1]
  beta=par[2]
  
  EX= alpha/(alpha+beta)
  EX2= (alpha*(alpha+1))/((alpha+beta)*(alpha+beta+1))
  return(c(EX-m1, EX2-m2)) #goal to be 0, find alpha and beta
}
par=c(1,1) #initial guess
nleqslv(x=par,
        fn = MOM.beta,
        data=data$col_2022_rate)
#get alpha=8.426105, beta=1003.461384

#Using MLE

llbeta=function(data, par, neg=F){
  alpha=par[1]
  beta=par[2]
  
  loglik=sum(log(dbeta(x=data, alpha, beta)), na.rm=T)
  return(ifelse(neg, -loglik, loglik))
}
par=c(1,1) #initial guess 

optim(par = par,
      fn = llbeta,
      data=data$col_2022_rate,
      neg=T,
      )
#I got alpha=8.271409, beta=985.047671

alpha_mom <- 8.426105  # Example value for alpha from MOM
beta_mom <- 1003.461384  # Example value for beta from MOM
alpha_mle <- 8.271409  # Example value for alpha from MLE
beta_mle <- 985.047671   # Example value for beta from MLE

#create things needed for plot
x_vals <- seq(0, 0.02, length.out = 1000) #all x are between 0 and 0.02
beta_mom_pdf <- dbeta(x_vals, shape1 = alpha_mom, shape2 = beta_mom)
beta_mle_pdf <- dbeta(x_vals, shape1 = alpha_mle, shape2 = beta_mle)

# Create a tibble to store x-values and the corresponding PDFs
df <- tibble(
  x = x_vals,
  beta_mom = beta_mom_pdf,
  beta_mle = beta_mle_pdf
)

# Plot the histogram of the data and overlay the beta distributions for MOM and MLE
ggplot(data.frame(data), aes(x = col_2022_rate)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_line(data = df, aes(x = x, y = beta_mom), color = "red", size = 1) +
  geom_line(data = df, aes(x = x, y = beta_mle), color = "green", size = 1) +
  labs(title = "Histogram with Beta Distributions", x = "Death Rate (per 1000 citizens scaled to [0,1])", y = "Density") +
  theme_minimal()

#Task Eight
alpha=8
beta=950
n=266

momalpha=numeric()
mombeta=numeric()
mlealpha=numeric()
mlebeta=numeric()

for (i in 1:1000){
  set.seed(7272+i)
  beta.sample <- rbeta(n = n,  # sample size
                     shape1 = alpha,   # alpha parameter
                     shape2 = beta)    # beta parameter
  par=c(1,1) #initial guess
  momsol=nleqslv(x=par,
          fn = MOM.beta,
          data=beta.sample)
  momalpha= c(momalpha, momsol$x[1])
  mombeta= c(mombeta, momsol$x[2])
  par=c(1,1) #initial guess 
  mlesol=optim(par = par,
        fn = llbeta,
        data=beta.sample,
        neg=T,
  )
  mlealpha=c(mlealpha, mlesol$par[1])
  mlebeta=c(mlebeta, mlesol$par[2])
}


taskeighttibble= tibble(mlealpha=mlealpha, mlebeta=mlebeta, 
                        momalpha=momalpha, mombeta=mombeta)

alpha_mom_plot <- ggplot(taskeighttibble) +
  geom_density(aes(x = momalpha), fill = "blue", alpha = 0.5) +
  labs(title = "Density of Alpha (MOM)", 
       x = "Alpha Estimate", y = "Density") +
  theme_minimal()

alpha_mle_plot <- ggplot(taskeighttibble) +
  geom_density(aes(x = mlealpha), fill = "red", alpha = 0.5) +
  labs(title = "Density of Alpha (MLE)", 
       x = "Alpha Estimate", y = "Density") +
  theme_minimal()

beta_mom_plot <- ggplot(taskeighttibble) +
  geom_density(aes(x = mombeta), fill = "blue", alpha = 0.5) +
  labs(title = "Density of Beta (MOM)", 
       x = "Beta Estimate", y = "Density") +
  theme_minimal()

beta_mle_plot <- ggplot(taskeighttibble) +
  geom_density(aes(x = mlebeta), fill = "red", alpha = 0.5) +
  labs(title = "Density of Beta (MLE)", 
       x = "Beta Estimate", y = "Density") +
  theme_minimal()

library(patchwork)
#combines the plots into a 2x2 grid
alpha_mom_plot + alpha_mle_plot + beta_mom_plot + beta_mle_plot + 
  plot_layout(ncol = 2, nrow = 2)


# Calculate Bias, Precision, and MSE for alpha
bias_mom_alpha <- mean(momalpha) - alpha
bias_mle_alpha <- mean(mlealpha) - alpha
var_mom_alpha <- var(momalpha)
var_mle_alpha <- var(mlealpha)

precision_mom_alpha <- 1 / var_mom_alpha
precision_mle_alpha <- 1 / var_mle_alpha

mse_mom_alpha <- var_mom_alpha + bias_mom_alpha^2
mse_mle_alpha <- var_mle_alpha + bias_mle_alpha^2

# Calculate Bias, Precision, and MSE for beta
bias_mom_beta <- mean(mombeta) - beta
bias_mle_beta <- mean(mlebeta) - beta
var_mom_beta <- var(mombeta)
var_mle_beta <- var(mlebeta)

precision_mom_beta <- 1 / var_mom_beta
precision_mle_beta <- 1 / var_mle_beta

mse_mom_beta <- var_mom_beta + bias_mom_beta^2
mse_mle_beta <- var_mle_beta + bias_mle_beta^2

# Create a table for bias, precision, and MSE
dt <- tibble(
  Parameter = c("Alpha", "Beta", "Alpha", "Beta"),
  Method = c("MOM", "MOM", "MLE", "MLE"),
  Bias = c(bias_mom_alpha, bias_mom_beta, bias_mle_alpha, bias_mle_beta),
  Precision = c(precision_mom_alpha, precision_mom_beta, precision_mle_alpha, precision_mle_beta),
  MSE = c(mse_mom_alpha, mse_mom_beta, mse_mle_alpha, mse_mle_beta)
)
