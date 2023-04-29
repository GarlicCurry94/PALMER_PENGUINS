getwd()
setwd("~/R PROJ/rprojext")
getwd()

ls()

install.packages("learningr")
install.packages("gapminder")
install.packages("rmarkdown")
install.packages("tinytex")
install.packages("ISLR2")

library(readxl)
library(tidyverse)
library(janitor)
library(palmerpenguins)
library(learningr)
library(gapminder)
library(ggplot2)
library(rmarkdown)
library(tinytex)
library(dplyr)
library(ISLR2)

data()

data(penguins) 
View(penguins)
View(penguins_raw)
glimpse(penguins)

penguins_sm <- filter(penguins, species != "Adelie")
glimpse(penguins_sm)
View(penguins_sm)

model <- lm(body_mass_g ~ flipper_length_mm * species, data = penguins_sm)
summary(model)

penguins %>%
  group_by(species) %>%
  summarise(var(flipper_length_mm, na.rm = TRUE))

pp_model <- aov(flipper_length_mm ~ species, data = penguins)
summary(pp_model)

TukeyHSD(pp_model)

adelie <- filter(penguins, species == "Adelie",
                 !is.na(bill_length_mm))

View (adelie)

ggplot(adelie, aes(x = bill_depth_mm,
                   y = bill_length_mm)) + geom_point()

pp_bill_model <- lm(bill_length_mm ~ bill_depth_mm,
                    data = adelie)
plot(pp_bill_model)

ggplot(adelie, aes(x = bill_depth_mm,
                   y = bill_length_mm)) + geom_point()+
  geom_smooth(method = "lm",
              level = .99)

predict(pp_bill_model)

predict(pp_bill_model, interval = "prediction")

adelie_new <- cbind(adelie,
                    predict(pp_bill_model, interval = "prediction"))

View(adelie_new)

ggplot(adelie_new, aes(x = bill_depth_mm)) +
  geom_point(aes(y = bill_length_mm))+
  geom_line(aes(y = fit),
            col = "purple") +
  geom_line(aes(y = upr),
            col = "brown",
            linetype = "dashed")+
  geom_line(aes(y = lwr),
            col = "navy",
            linetype = "dashed")

ggplot(penguins, aes(x = species, y = flipper_length_mm)) +
  geom_boxplot()
ggsave("p_penguins_boxplot.pdf")

ggplot(penguins, aes(x = flipper_length_mm)) +
  geom_histogram() +
  facet_wrap(~species, ncol = 1)
ggsave("p_penguins_species_dist.pdf")

ggplot(penguins, aes(x = species, y = bill_length_mm, fill = species))+
  geom_violin()+
  geom_boxplot(width =.5)
ggsave("p_penguins_violinplot.pdf")
ggsave("p_penguins_violinplot.png")

ggplot(penguins_sm, aes(x = flipper_length_mm, y = body_mass_g, color = species))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)
ggsave("p_penguins_LM.pdf")
ggsave("p_penguins_LM.png")

ggplot(penguins_sm, aes(x = flipper_length_mm, y = body_mass_g, color = species))+
  geom_point()+
  geom_abline(aes(intercept = -3037.196,
                  slope = 34.573,
                  col = "Chinstrap"))+
  geom_abline(aes(intercept = -3037.196-3750.085,
                  slope = 34.573 + 20.049,
                  col = "Gentoo"))
ggsave("p_penguins_regression.pdf")
ggsave("p_penguins_regression.png")
