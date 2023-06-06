rm(list = ls())
library(tidyverse)

BMI <- read.csv("insurance_with_date.csv")
library(ggplot2)

plot_BMI_density_v0 <- ggplot(data = BMI, aes(x = bmi)) + 
  geom_density(aes(colour = sex, fill = sex), alpha = 0.5) +
  scale_fill_manual(name = "sex",
                    breaks = c("male", "female"),
                    values = c(unibeMintS()[1], unibePastelS()[1])) +
  scale_colour_manual(name = "sex",
                      breaks = c("male", "female"),
                      values = c(unibeMintS()[1], unibePastelS()[1])) +
  xlab(label = "BMI (kg/m^2") +
  ylab(label = "density") 

plot_BMI_age