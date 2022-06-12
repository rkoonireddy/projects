# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Marketing Experiments FS22
# Analyzing experimental data
# Prof. Dr. Anne Scherer & Claudia Wenzel



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 1) set working directory ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Different options:
###Task: Create a folder on your desktop called mktexp_2022 and set it as your working directory. 
# Download the data from OLAT into your working directory.

# 1) Set the working directory to where your script is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 2) Set from R interface: Session --> Set Working Directory --> Choose Directory ...

# 3) directly call path
setwd("~/Desktop/mktexp_2022")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 2) install & load packages ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Note: You only have to install packages the first time using them. Once they are installed, 
# loading them is sufficient. You can always check packages in "Packages" tab on the right.

### Task: Install & load the following packages####
# 1) Install packages
install.packages("data.table")
install.packages("ggplot2")
install.packages("vtable")

# 2) Load packages
library(data.table)
library(ggplot2)
library(vtable)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 3) Load data ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
###Task: Read in the data and store it as mktexp in your R environment. #####
mktexp <- fread("mktexpData.csv") # make sure your data is stored in the working directory


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 4) Get an overview of your data ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Show (an excerpt of) the data in the R console.
mktexp
# Show the whole data in a separate window.
View(mktexp)
# Show a list of the variables in the data set and their type.
str(mktexp)
# Look at the summary statistics (min, mean, median, max) of all variables.
summary(mktexp)
# Look at the distribution of the campaigns
table(mktexp[,campaign])

## Selecting rows and columns ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Select by row / column number.
mktexp[1,] ; mktexp[,1] 
# Select by multiple row / numbers.
mktexp[c(1:3,5),] ; mktexp[,c(1:3,5)]
# Select row by condition.
mktexp[Clicks > 1,]; mktexp[age == "30-34",]
# Select column by column name.
mktexp[,list(age)]; mktexp[,c("age")]


## Manipulating data ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Calculate a new data.table with the columns Avg_age & Treatment.
mktexp[,list(avg_clicks = mean(Clicks)), by = campaign]
# Calculate column Avg_age in the data.table input.
mktexp[,avg_click := mean(Clicks), by = campaign]
# Apply the aggregation to the whole data.table.
mktexp[,mean(Clicks)]

### Task: calculate variable for experimental data ####
# Cost per total conversion (CTC)
### ENTER YOUR CODE HERE ###


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 5) Analyzing experimental data ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
## Simple graphs ####
ggplot(mktexp, aes(y=emt_cnt, x= campaign)) +
  stat_summary(fun = mean, geom = "bar") +
  geom_label(data = mktexp[, .(N=.N), by=campaign], aes(label =  paste("N=", N), y=5.25), cex=2.5) +
  labs(title = "Perceptions across campaigns (mean)", x="", y="How emotional did you perceive the ad?")

### Task: use gender instead of campaign ####
### ENTER YOUR CODE HERE ###

## Advanced graphs ####
ggplot(mktexp, aes(y=emt_cnt, x= campaign, color=campaign)) +
  stat_summary(fun = mean, geom = "point") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  geom_label(data = mktexp[, .(N=.N), by=campaign], aes(label =  paste("N=", N), y=4.75), cex=2.5) +
  labs(title = "Perceptions across campaigns (mean incl. 95% CI)", x="", y="How emotional did you perceive the ad?")

## Simple tables ####
st(mktexp[,list(campaign, age, gender, emt_cnt,
                Impressions, Clicks, Spent, Total_Conversion, Approved_Conversion,
                CTC, CR, CPC
                )],
   group="campaign")

### Task: use gender instead of campaign ####
### ENTER YOUR CODE HERE ###

## Advanced tables ####
st(mktexp[,list(campaign, age, gender, emt_cnt,
                Impressions, Clicks, Spent, Total_Conversion, Approved_Conversion,
                CTC, CR, CPC
                )],
group="campaign", group.test = TRUE)

## Statistical tests ####
# comparison of 2 groups
t.test(emt_cnt~campaign, mktexp)

# anova: analysis of variance for treatment differences in cost per total conversion
summary(aov(CR~ campaign, mktexp))
# How do groups differ? --> post hoc test
TukeyHSD(aov(CR~ campaign, mktexp))

# Plot two-way anova
summary(aov(CR~ campaign*gender, mktexp))
# How do groups differ? --> post hoc test
TukeyHSD(aov(CR~ campaign*gender, mktexp))

## Graphs ####
ggplot(mktexp, aes(y=CR, color= gender, x=gender)) +
  stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  geom_label(data = mktexp[, .(N=.N), by=list(campaign,gender)], aes(label =  paste("N=", N), y=0.1), cex=2.5) +
  facet_wrap(~campaign, nrow=1) +
  labs(title = "Conversion rates across campaigns and gender (mean incl. 95% CI)", x="", y="Convesion rate")

