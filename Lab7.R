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