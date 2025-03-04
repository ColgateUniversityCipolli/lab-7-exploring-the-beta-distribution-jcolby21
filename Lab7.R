#Task One
library(tidyverse)
library()
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
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) +  # plot guassian dist
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
part_2_case_1=beta.moment(alpha, beta, k=2, centered=F)#change for each moment

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


#Write a for loop to simulate new data, add new line for iterations 2-50
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
# Show the final plot with simulated data lines
print(new_grid_plot)

#Task Five



