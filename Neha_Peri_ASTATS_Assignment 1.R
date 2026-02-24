#Install packages
install.packages("tidyverse")
install.packages("AER")
#Load packages (every session)
library(tidyverse)
library(AER)
#Load the CPS1985 dataset into R's memory (from AER)
data("CPS1985")
#View the first 6 rows of the data
head(CPS1985)
#See the structure: variable names, types etc
str(CPS1985)
#Summary statistics for every variable
summary(CPS1985)
#Question 1. There are 534 rows or observations. There are 11 columns or variables as follows: wage, education, experience, age, ethnicity, region, gender, occupation, sector, union, married.

#Select only the wage, education and experience columns
wage_data = select(CPS1985, wage, education, experience)
head(wage_data)
#Keep only rows where the worker is a union member
union_workers= filter(CPS1985, union =="yes")
#Keep only rows where experience > 20
experienced = filter(CPS1985, experience>20)
#Count how many rows are in each filtered dataset
nrow(union_workers)
nrow(experienced)
#Take CPS1985, and then add a new column called log_wage
CPS1985 = CPS1985%>%
  mutate(log_wage=log(wage))
#Take CPS1985, and then add a column: 1 if wage>10, else 0
CPS1985=CPS1985%>%
  mutate(high_wage=ifelse(wage>10,1,0))
#The mean of a 0/1 variable gives the fraction of 1s
mean(CPS1985$high_wage)
#Question 3. 31% of workers in the dataset have a wage above $10/hour
#Compute overall summary statistics for wage
summarize(CPS1985,
          mean_wage=mean(wage),
          sd_wage=sd(wage),
          median_wage=median(wage),
          n=n()
          )
#Compute mean wage separately for union vs non-union
CPS1985%>%
  group_by(union) %>%
  summarize(mean_wage=mean(wage), sd_wage=sd(wage), n=n())
#Question 4. The mean wage for union members is 10.8 and for non-union members is 8.64. Union members have a higher wage.
# Make a histogram of the wage variable
ggplot(CPS1985, aes(x=wage))+
  geom_histogram(bins=30)+
  labs(title="Distribution of Hourly Wages",
       x="Wage($/hour)", y="Count")
#Question 5. left-skewed

#PART 2
#Compute the fraction of workers who are union members
#This is our "true" population proportion p
p_union=mean(CPS1985$union == "yes")
p_union

#Set the random seed so results are reproducible
set.seed(42)
#Make a 0/1 vector: 1 = union member, 0=not
union_binary=ifelse(CPS1985$union=="yes",1,0)
#Randomly draw 20 workers (with replacement)
n=20
one_sample=sample(union_binary, size=n, replace=TRUE)
#Print the same and count how many are union members
one_sample
sum(one_sample)
#Question 6. 4 members appeared in the sample of 20. Theoretical expected value would be 4*0.0.1797753 which is 0.7191012. Theoretical variance would be 0.7191012(1-0.1797753) is 0.58982456604

set.seed(42)
n=20
num_sims=10000

#Repeat 10,000 times: draw 20 workers, count union members
sim_counts=replicate(num_sims,{
  s=sample(union_binary, size=n, replace=TRUE)
  sum(s)
})
#Plot the results as a histogram
ggplot(data.frame(x=sim_counts), aes(x=x))+
  geom_histogram(aes(y=after_stat(density)), binwidth=1)+
  labs(title="Simulated Binomial Distribution (n=20)",
       x= "Number of Union Members", y="Density")
#Create the theoretical Binomial probabilities
x_vals=0:n
binom_probs=dbinom(x_vals, size=n, prob=p_union)
#Plot: histogram of simulation with Binomial PMF overlaid
ggplot(data.frame(x=sim_counts), aes(x=x))+
  geom_histogram(aes(y=after_stat(density)), binwidth=1)+
  geom_point(data=data.frame(x=x_vals, y=binom_probs),
             aes(x=x, y=y), color="red", linewidth=1)+
  labs(title="Simulated vs. Theoretical Binomial(20,p)",
  x="Number of Union Members", y="Density")
#Question 7.
#Compare simulated moments to theoretical values
mean(sim_counts)
var(sim_counts)