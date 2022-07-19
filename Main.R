# Import appropriate libraries
library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(car)
library(tidyverse)
library(rstatix)

# Import data and set up data frames
IRConc180 <-read_csv("csv/IR Conc 180.csv")
IRConc300 <-read_csv("csv/IR Conc 300.csv")
IREcc180 <-read_csv("csv/IR Ecc 180.csv")
IREcc300 <-read_csv("csv/IR Ecc 300.csv")

ERConc180 <-read_csv("csv/ER Conc 180.csv")
ERConc300 <-read_csv("csv/ER Conc 300.csv")
EREcc180 <-read_csv("csv/ER Ecc 180.csv")
EREcc300 <-read_csv("csv/ER Ecc 300.csv")

Cocking180 <- read_csv("csv/Cocking 180.csv")
Cocking300 <- read_csv("csv/Cocking 300.csv")

Spiking180 <- read_csv("csv/Spiking 180.csv")
Spiking300 <- read_csv("csv/Spiking 300.csv")

########## 1.0 DESCRIPTIVE ANALYSIS ########## 

# 1.1 Function to generate information regarding Central Tendancy and Dispersion of Data for a given data set and argument
result <- function (df, gen) { #df = data set, gen = the gender of participants to be analysed
  w <- df %>% subset(Gender == gen & Group == "Control") %>% # Control, Pre
    summarize(Mean = mean(Pre), SD = sd(Pre)) %>% 
    add_column(Group = "Control", Time = "Pre", .before = "Mean")
  
  x <- df %>% subset(Gender == gen & Group == "Plyo") %>% # Plyo, Pre
    summarize(Mean = mean(Pre), SD = sd(Pre)) %>% 
    add_column(Group = "Plyo", Time = "Pre", .before = "Mean")
  
  y <- df %>% subset(Gender == gen & Group == "Control") %>% # Control, Post
    summarize(Mean = mean(Post), SD = sd(Post)) %>% 
    add_column(Group = "Control", Time = "Post", .before = "Mean")
  
  z <- df %>% subset(Gender == gen & Group == "Plyo") %>% # Plyo, Post
    summarize(Mean = mean(Post), SD = sd(Post)) %>% 
    add_column(Group = "Plyo", Time = "Post", .before = "Mean")
  
  return(rbind(w,x,y,z))
}

test <- result(IREcc180, "Male") %>% print() # Example

########## 2.0 INFERENTIAL ANALYSIS ########## 

# 2.1 Shapiro-Wilk Test (For Normality) - Example
shapiro_test(subset(IRConc180, Gender == "Male")$Pre) # IRConc180 and associated arguments (Male and Pre) were changed for each test

# 2.2 Paired T Test - Function and Example
pairedT <- function (df, gen, grp) { #df = data set, gen = the gender of participants to be analysed, grp = the group
  subset(df,Group == grp) %>%
  subset(Gender == gen) %>%
  gather(key = "Group", value = "Result", Pre, Post) %>%
  select(Group, Result) %>%
  t_test(Result ~ Group, paired = TRUE) %>% # Compute the T Statistic
  select(statistic, p) %>%
    print()
}
pairedT(Spiking300, "Male", "Plyo") # Example

# 2.3 Cohen's D - Function and Example
cohens <- function (df, gen, grp) { #df = data set, gen = the gender of participants to be analysed, grp = the group
  subset(df,Group == grp) %>%
    subset(Gender == gen) %>%
    gather(key = "Group", value = "Result", Pre, Post) %>%
    select(Group, Result) %>%
    cohens_d(Result ~ Group, paired = TRUE) %>% # Compute Cohen's D
    select(effsize, magnitude) %>%
    print()
}
cohens(EREcc180, "Male", "Control") # Example

########## 3.0 VISUAL ANALYSIS ########## 

# 3.1 The function below generates a line plot comparing pre to post for the Control and Intervention group: Points are used to represent the mean with the Error Bar representing +/- 1 SD
test <- result(IREcc180, "Male") %>% print() # Function to generate a data set for visualization

# 3.2 Generate visualization of data selected above
ggplot(test, aes(x = factor(Time, level = c("Pre", "Post")), y = Mean, group = Group, colour = Group)) +
  geom_errorbar( aes(ymin = Mean-SD, ymax = Mean+SD), width = 0.1, show.legend = F) +
  ylim(0.25, 1) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  labs(x = "Time", y = "Nm (Normalised)")