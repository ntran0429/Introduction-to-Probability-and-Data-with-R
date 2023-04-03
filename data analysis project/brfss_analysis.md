---
title: "Exploring the BRFSS data"
output: 
  html_document: 
    fig_height: 4
    highlight: pygments
    theme: spacelab
---

## Setup

### Load packages

```{r load-packages, message = FALSE}
library(ggplot2)
library(dplyr)
library(survey)
library(mice)
library(reshape2)
library(usmap)
#library(tidyverse)
```

### Load data

Download Links:

[brfss2013.Rdata](https://d3c33hcgiwev3.cloudfront.net/4tiY2fqCQa-YmNn6gnGvzQ_1e7320c30a6f4b27894a54e2de50a805_brfss2013.RData?Expires=1680048000&Signature=Q6uMfhbadJr5DsLbo~MbEY2iLCFpCaIF7rKM35idwX-oYdyKgpaeOWm4RhVWj7oyCHxHNXDmx6gS3wtZH9iwpR6Jy8ZnvoAkExm~zSLtU2UO99DAz4NzYWx2Cm9HeNaFyyUX3YxNy0tJdOyBspcqDWk0dj8~AOi5Kd8ZtKaWU24_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A)

[Codebook](https://d3c33hcgiwev3.cloudfront.net/_e34476fda339107329fc316d1f98e042_brfss_codebook.html?Expires=1680048000&Signature=fBbiheWuPcAS1SyAuwq8UmcsZ9ZB9kTmdyWLLnumKLs6m~UzpvWkBqdPfmko9Pu83GdS~Ta5kp-s6-LW1u5E5e~2kIMSngsG65jj8njKRMDcWNwmGs4Tiubj4jTdkB3EtuE~hcfcbs0xM174p5JVItmjgaY~e-doRhd4CDG6waA_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A)


```{r load-data}
load("brfss2013.RData")
```



* * *

## Part 1: Data
**How the observations in the sample are collected**

* The Behavioral Risk Factor Surveillance System (BRFSS) objective is to collect uniform, state-specific data on preventive health practices and risk behaviors that are linked to chronic diseases, injuries, and preventable infectious diseases that affect the adult population.

* All 50 states, the District of Columbia, Puerto Rico, and Guam collect data annually and American Samoa, Federated States of Micronesia, and Palau collect survey data over a limited point- in-time.

* BRFSS conducts both landline telephone- and cellular telephone-based surveys.

* Because BRFSS merely observes the data that arise through surveys, this is an observational study. Thus, there is no random assignment and we can only make correlation assumptions.

*Landline sample*

* Disproportionate stratified sampling with two strata, high tele-density and medium tele-density.

* Telephone numbers in the high density stratum are sampled at the highest rate (hence, disproportionate).

* Since landline telephones are often shared among persons in a residence, interviewers collect information on the number of adults
living within the residence and then select randomly from all eligible adults (simple random sampling conducted in each household).

* The success of this sampling method depends on the researcher’s precision at sampling rate allocation. If the allotted sampling rates aren’t accurate, the results may be biased due to the overrepresented or underrepresented strata.

*Cellular sample*

* Interviewers collect data from an adult who participates by using a cellular telephone and resides in a private residence or college housing.

* The cellular telephone sample is randomly generated from a sampling frame of confirmed cellular area code and prefix combinations.

* Cellular telephone sample is stratified by substate geographic specificity.

* Cellular telephone respondents are randomly selected with each having equal probability of selection (simple random sampling in each stratum).

**The implications of this data collection method on generalizability and causality**

Since there is no random assignment,

* Generalizability can be made for the population based on this study.

* Causality cannot be made for the population based on this study.

**Potential sources of bias**

Potential non-response bias may be considered for this data since the surveys were conducted using land lines and cell phones. However, this can be avoided by using the 'survey' package and the provided weighting variables in the brfss2013 data set.

* * *

## Part 2: Research questions

**Research quesion 1:**
What are the probabilities of owning or renting a home given an individual’s income level and race?

Higher income individuals may be more able to afford owning a house. There may also be differences in home-ownership rates based on race due to systemic inequalities and historical factors.

**Research quesion 2:**
Is there any association between physical activity level and tobacco use among young adults?

It is reasonable to assume people who engage in regular physical activities also adopt other healthy lifestyle choices.

**Research quesion 3:**
Is there a pattern in sleep duration, education level, and state residence among individuals who self-report excellent versus poor general health status?

This question explores the three separate variable types on general health: health behavior, demographics, and geographic region.

* * *

## Part 3: Exploratory data analysis 
<!-- our EDA should contain numerical summaries and visualizations. Each R output and plot should be accompanied by a brief interpretation. -->
**Research quesion 1:**
What are the probabilities of owning or renting a home given an individual’s income level and race?

Let's first look into our three variables separately.

```{r}
# summary for variable renthom1
brfss2013 %>% count(renthom1) %>% mutate(percent = n/sum(n))
```

There are three levels of housing types with the frequency and percent for each.

```{r}
# summary for variable income2
brfss2013 %>% count(income2) %>% mutate(percent = n/sum(n))
```

We can see that the $75,000 group accounts for roughly a quarter of our data set.

```{r}
# summary for variable race
# rename levels
levels(brfss2013$X_race) <- c("White","Black","American Indian or Alaskan Native","Asian","Native Hawaiian or Pacific Islander","Other race","Multiracial","Hispanic")

brfss2013 %>% count(X_race) %>% mutate(percent = n/sum(n))
```

The 'White' group accounts for the majority of the data set.


We will use 'mice package' to fill in the NA values. 

The basic idea behind the algorithm is to treat each variable that has missing values as a dependent variable in regression and treat the others as independent (predictors).

We will use an imputation method called predictive mean matching.

```{r}
set1 <- subset(brfss2013, select = c("income2","X_race","renthom1"))
table(is.na(set1))


# list the variable being imputed on the left-hand side of the equation, and the predictor variables on the right-hand side of the equation, separated by the tilde (~) symbol.
imp1 <- mice(set1, method= "pmm", seed=1234)

imputed_data1 <- complete(imp1)

# rename columns
imputed_data1 <- rename(imputed_data1,income_lev = income2, race = X_race, housing_type = renthom1)

# rename income column's levels
imputed_data1 <- imputed_data1 %>% mutate(income_lev = as.factor(case_when(
  income_lev == "Less than $10,000" ~ "$0 - $10,000",
  income_lev == "Less than $15,000" ~ "$10,000 - $15,000",
  income_lev == "Less than $20,000" ~ "$15,000 - $20,000",
  income_lev == "Less than $25,000" ~ "$20,000 - $25,000",
  income_lev == "Less than $35,000" ~ "$25,000 - $35,000",
  income_lev == "Less than $50,000" ~ "$35,000 - $50,000",
  income_lev == "Less than $75,000" ~ "$50,000 - $75,000",
  income_lev == "$75,000 or more"   ~ "$75,000 +"    ) ) )

imputed_data1 %>% head()
```

The first six rows of our desired subset with the columns renamed for better readability

Here are the histograms for the three variables

```{r}
# histograms
imputed_data1 %>% ggplot(aes(y = housing_type)) + 
  geom_bar()

imputed_data1 %>% ggplot(aes(y = income_lev)) + 
  geom_bar()

imputed_data1 %>% ggplot(aes(y = race)) + 
  geom_bar()
```

Now let's explore our method of finding the probability of owning/renting given income level and race.

Recall the fact that the "White" race category has the highest number of people in the data set. This can affect the calculation of our probabilities. If one race category has a much larger frequency than the others, it may dominate the conditional probabilities and make it difficult to make meaningful comparisons between the different race categories.

It is more useful to normalize the frequencies by the total number of people in each income+race combination. This would give us the proportion of people who own/rent in each race category within each income level, which can help to reveal any patterns or differences between the race categories and/or income levels.


```{r}
# Define the categories for income and race
income_categories <- c("$0 - $10,000", "$10,000 - $15,000", "$15,000 - $20,000", "$20,000 - $25,000", "$25,000 - $35,000", "$35,000 - $50,000", "$50,000 - $75,000", "$75,000 +")
race_categories <- c("White", "Black", "American Indian or Alaskan Native", "Asian", "Native Hawaiian or Pacific Islander", "Other race", "Multiracial", "Hispanic")

# Create two empty matrices to store the probabilities
prob_matrix_own <- matrix(NA, nrow = length(income_categories), ncol = length(race_categories))
prob_matrix_rent <- matrix(NA, nrow = length(income_categories), ncol = length(race_categories))


# Loop over the income and race categories and calculate the probabilities
for (i in seq_along(income_categories)) {
  for (j in seq_along(race_categories)) {
    # Subset the data to only include a certain (income,race) pair
    subset_data <- subset(imputed_data1, income_lev == income_categories[i] & race == race_categories[j])
    
    # Calculate the total number of people in the subset
    n <- nrow(subset_data)
    
    # Calculate the number of people who own or rent their home in the subset
    rent_own_table <- table(subset_data$housing_type)
    n_own <- rent_own_table[1]
    n_rent <- rent_own_table[2]
    
    # Calculate the probabilities and store them in the matrix
    prob_own <- n_own / n
    prob_rent <- n_rent / n
    prob_matrix_own[i, j] <- prob_own
    prob_matrix_rent[i,j] <- prob_rent
  }
}

# Add row and column names to the probability matrices
rownames(prob_matrix_own) <- income_categories
colnames(prob_matrix_own) <- race_categories
rownames(prob_matrix_rent) <- income_categories
colnames(prob_matrix_rent) <- race_categories

# View the probability matrices
prob_matrix_own
prob_matrix_rent

```

Here is the plot of owning a home given each income level and race category

```{r}
# Convert the probability matrix to a data frame
prob_df <- melt(prob_matrix_own, varnames = c("income", "race"), value.name = "probability")
prob_df$income <- factor(prob_df$income, levels = income_categories)

# Plot the probability distribution by income level
ggplot(prob_df, aes(x = income, y = probability, group = race, color = race)) +
  geom_line() +
  geom_point() +
  ggtitle(expression(atop(
    paste("Probability of ", underline("Owning a Home"), " Given Income Level and Race Category")))) +
  labs(x = "Income Level", y = "Probability", color = "Race") +
  theme(axis.text.x = element_text(angle = 90))
```

* 'White' group's percentage of owning a home increases steadily as we go up the income level.

* 'Other race' group seems to have the most unstable increase of percentage as we go up the income level.

* The discrepancy in the 75,000+ income level is minimal across different racial groups, as expected, except for 'White'.

* Individuals of all racial groups break the 50% once everyone makes at least 50,000.

* Overall, as expected, we see a positive association between income level and probability of an individual of any racial group to own a home.

Plot for renting a home.

```{r}
# Convert the probability matrix to a data frame
prob_df <- melt(prob_matrix_rent, varnames = c("income", "race"), value.name = "probability")
prob_df$income <- factor(prob_df$income, levels = income_categories)

# Plot the probability distribution by income level
ggplot(prob_df, aes(x = income, y = probability, group = race, color = race)) +
  geom_line() +
  geom_point() +
  ggtitle(expression(atop(
    paste("Probability of ", underline("Renting a Home"), " Given Income Level and Race Category")))) +
  labs(x = "Income Level", y = "Probability", color = "Race") +
  theme(axis.text.x = element_text(angle = 90))
```

* As expected, 'White' group has the lowest percentage of renting, as they hold the biggest percentage of owning a home across the income level.

* The two most stable decreases are the 'Multiracial' and 'White' groups.

* 'Hispanic', 'Other race', and 'Native Hawaiian or Pacific Islander' groups seem to have the most unstable decrease.

* Again, we observe a minimal difference once reaching 75,000+ for all racial groups, except for 'White'.

* Overall, as expected, we see a negative association between income level and probability of an individual of any racial group to rent a home.

**Research quesion 2:**
Is there any association between physical activity level and tobacco use among young adults?

```{r}
#filter the data to include only young adults, defined as those between the ages of 18 and 24
young_adults <- filter(brfss2013, X_age_g == "Age 18 to 24")

# check to see if the 'X_llcpwt' weighting variable has any missing values
sum(is.na(young_adults$X_llcpwt))
```

This output means there are no missing values in the weighting variable.

```{r}
set2 <- subset(young_adults, select = c("seqno", "X_psu","X_ststr","X_llcpwt","X_pacat1","X_rfsmok3","X_smoker3"))
sum(is.na(set2))

imp2 <- mice(set2, method = c("","","","","sample", "logreg.boot", "sample"), seed=4321) 
imputed_data2 <- complete(imp2)

imputed_data2 %>% head()
```

We used random sample from observed values for X_pacat1 and X_smoker3 and logistic regression for X_rfsmok3 since it is binary.
We did not impute these variables "X_seqno", "X_psu","X_ststr","X_llcpwt" because they are inputs to create a survey design object.

Let's look at the summary statistics for physical activity level and tobacco use.

```{r}
summary(imputed_data2$X_pacat1)
summary(imputed_data2$X_rfsmok3)
summary(imputed_data2$X_smoker3)
```

* In terms of physical activity level distribution, we have a lot of 'highly active' young adults followed by 'insufficiently active' as the second biggest group.

* Majority of young adults are non-smokers (21825 No's vs 5378 Yes's).

Let's now create a bar plot to investigate the potential association.
```{r}
# create a weighted survey design object
design2 <- svydesign(
  data = imputed_data2,
  id = ~seqno,
  cluster = imputed_data2$X_psu,
  strata = imputed_data2$X_ststr,
  weights = imputed_data2$X_llcpwt,
  nest = TRUE
)

# create a weighted contingency table
# returns a table with the weighted frequencies for each combination of categories
tab <- svytable(~X_rfsmok3+X_pacat1, design2)

# normalize the weighted frequencies by computing the proportion of each table entry relative to the total sum of all entries
normalized_tab <- prop.table(tab)


# adjust plot margins
par(mar = c(5,4,4,5) + 0.5)
# visualize the association using a bar plot
ggplot(as.data.frame(normalized_tab), aes(x = X_pacat1, y = Freq, fill = X_rfsmok3)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Association in Physical Activity Level and Tobacco Use",
       x = "Physical Activity Levels",
       y = "Percentages",
       fill = "Tobacco Use") +
  scale_fill_manual(values = c("mediumseagreen", "red"))
```

* The legend to the right indicates that the green color represents non-smokers, while the red color represents current smokers.

* The bar plot suggests that, among young adults aged 18-24, those who are 'highly active' are more likely to be non-smokers.

* However, among young adults aged 18-24, those who are 'highly active' are also more likely to be current smokers compared to those in other physical activity levels.

* In general, as physical activity levels increase, the percentage of non-smokers increases, and the percentage of current smokers decreases, except where there is a sharp jump from 'active' to 'highly active'.

* It is possible there is a negative association here. However, further statistical analysis is required to confirm.


Let's dig a little bit deeper.

```{r}
tab2 <- svytable(~X_smoker3+X_pacat1, design2, 1)
df_tab2 <- as.data.frame(tab2)

# visualize the association using a relative frequency segmented bar plot
ggplot(df_tab2, aes(x = X_pacat1, y = Freq, fill = X_smoker3)) +
  geom_col(position = position_fill()) +
  labs(title = "Relative Frequency Segmented Bar Plot",
       x = "Physical Activity Level", y = "", fill = "Tobacco Use Status") +
  scale_fill_manual(values = c("red","orange","yellow","mediumseagreen"))
```

* Non-smokers represent the majority in all four categories of physical activity levels.

* In general, We find that young adults who report a higher level of physical activity are less likely to use tobacco compared to those who report a lower level of physical activity.

* This effect may be stronger or weaker depending on the level of tobacco use.

* Similarly, the effect of tobacco use on physical activity level may depend on the level of physical activity.

* Thus, there can be a potential multiplicative relationship between physical activity level and tobacco use here.


Due to the general nature of our question, in which we don't assume which variable is the explanatory variable and which is the response, let's create both the column and row proportion contingency tables.

```{r}
# create column proportion contingency table
# interpret the relationship from side to side
# does physical activity level affect tobacco use status?
colp_tab <- prop.table(tab, margin = 2)

# print the normalized table
print(colp_tab)

ggplot(as.data.frame(colp_tab), aes(x = X_pacat1, y = Freq, fill = X_rfsmok3)) + 
  geom_bar(stat = "identity") +
  labs(title = "Column Proportion Contingency Plot",
       x = "Physical Activity Levels",
       y = "Percentages",
       fill = "Tobacco Use") +
  scale_fill_manual(values = c("mediumseagreen", "red"))
```

* If we suspect physical activity level potentially affects tobacco use status, then column proportion is the appropriate choice.

* We see an increase in the proportion of non-smokers as we go up the physical activity level, except for a decrease when going from 'active' to 'highly active'.

* There is a decrease in the proportion of smokers as we go up the physical activity level, except for an increase when going from 'active' to 'highly active'.

* Because there is a considerable difference between the proportion of young smokers who are 'inactive' and the rest of the smokers in each physical activity level (22.7% vs 17%, 15.7%, 19.8%), this provides some evidence that physical activity level affects tobacco use.

```{r}
# create row proportion contingency table
# interpret the relationship from top to bottom
# does tobacco use status affect physical activity level?
rowp_tab <- prop.table(tab, margin = 1)

# print the normalized table
print(rowp_tab)

ggplot(as.data.frame(rowp_tab), aes(x = X_pacat1, y = Freq, fill = X_rfsmok3)) + 
  geom_bar(stat = "identity",position = "dodge") +
  labs(title = "Row Proportion Contingency Plot",
       x = "Physical Activity Levels",
       y = "Percentages",
       fill = "Tobacco Use") +
  scale_fill_manual(values = c("mediumseagreen", "red"))
```

* If we suspect tobacco use status potentially affects physical activity level, then row proportion is the appropriate choice.

* As we look down each column in the row proportion contingency table, we see a noticeable difference in the 'Inactive' (22.2% vs 27.8%), 'Insufficiently Active' (25.2% vs 22.1%), and 'Active' group (20.9% vs 16.7%).

* However, the 'Highly Active' group seems to have negligible difference (31.7% vs 33.4%), which does not help with our suspicion.

**Research question 3:** Is there a pattern in sleep duration, education level, and state residence among individuals who self-report excellent versus poor general health status?

Cleaning and Pre-processing.
```{r}
# filter the data to include only individuals who self report 'Excellent' or Poor general health
exc_pr <- subset(brfss2013, genhlth == "Excellent" | genhlth == "Poor")
# remove other levels in "genhlth" variable to only have two levels: excellent, poor
exc_pr$genhlth <- factor(exc_pr$genhlth, levels = c("Excellent", "Poor"))


# select only variables of interest
set3 <- exc_pr[,c("seqno","X_llcpwt", "X_ststr", "X_psu","genhlth", "sleptim1", "X_educag", "X_state")]

# Count the number of missing values for each variable
missing_counts <- colSums(is.na(set3))
# Print the missing value counts for each variable
print(missing_counts)

# remove missing values for the variables "seqno", "X_llcpwt", "X_llcpwt"
set3 <- set3[complete.cases(set3[,c("seqno", "X_llcpwt", "X_ststr")]), ]
```

Impute missing values.
```{r}
# impute missing values
imp3 <- mice(set3, method = "rf", seed = 123)
imputed_data3 <- complete(imp3)

# rename variables/columns
imputed_data3 <- rename(imputed_data3, gen_health = genhlth, sleep_hrs = sleptim1, edu_lv = X_educag, state = X_state)
```


Summary statistics and plots for sleep duration by general health status.
```{r}
# create a design survey object 
design3 <- svydesign(
  data = imputed_data3,
  id = ~seqno,
  cluster = imputed_data3$X_psu,
  strata = imputed_data3$X_ststr,
  weights = imputed_data3$X_llcpwt,
  nest = TRUE)

########################## ############# ############# ############# ############# sleep and general health
# svymean(~sleep_hrs, by = ~gen_health, design = design3)

# Summary statistics for sleep duration by general health status
# Histogram of sleep_hrs as summary statistics
ggplot(imputed_data3, aes(x = sleep_hrs)) +
  geom_histogram(binwidth = 1, fill = "#69b3a2", color = "black") +
  labs(title = "Distribution of Sleep Duration", x = "Sleep Duration (Hours)", y = "Count")
# Bar plot of genhlth
ggplot(set3, aes(x = genhlth, fill = genhlth)) +
  geom_bar() +
  labs(title = "General Health Status", x = "General Health Status", y = "Count") +
  scale_fill_manual(values = c("mediumseagreen", "red")) # custom colors

# Create a box plot of sleptim1 for each level of genhlth
ggplot(imputed_data3, aes(x = gen_health, y = sleep_hrs)) +
  geom_boxplot() +
  labs(x = "General Health Status", y = "Sleep Duration") +
  ggtitle("Sleep Duration by General Health Status")


```

* As expected, the center of the sleep duration distribution is within the 6-9 hours range.

* Individuals self report "Excellent" general health about 3 times more than "Poor" general health.

* The box plot indicates that the "Poor" health group has a more variable sleep duration than the "Excellent" health group.

* More specifically, the "Poor" health group first quartile is lower, indicating they tend to have less sleep than the "Excellent" health group.

* Furthermore, the sleep duration median for the "Excellent" health group is higher than the median for the "Poor" health group means. So, on average, people in the "Excellent" group tend to have longer sleep duration compared to those in the "Poor" health group.

* Thus, this implies a correlation between sleep duration and general health status


Summary statistics and plot for education level by general health status.
```{r}
########################## ############# ############# ############# ############# education level and general health
# the weighted counts indicate the number of people in the population that each combination of categories represents
# the weighted counts are population estimates for each combination of the variables of interest
educ_table <- svytable(~edu_lv + gen_health, design = design3)
norm_educ_table <- prop.table(educ_table)

# Create a data frame from the svytable object
educ_table_df <- as.data.frame(norm_educ_table)

# Create bar chart for education level by general health status
ggplot(educ_table_df, aes(x = Freq, y = edu_lv, fill = gen_health)) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("mediumseagreen", "red")) +
  labs(x = "Percentage", y = "Education Level", 
       title = "Distribution of Education Level by General Health Status")
```

* As we go up the education level, more individuals self report "Excellent" health and less self report "Poor" health.

* Thus, there's a correlation between the two variables.

Create choropleth map to explore if there is any pattern in where individuals who self report excellent/poor health reside in the US.
```{r}
########################## ############# ############# ############# #############  state residence and general health
# Calculate percentage of individuals who self-report excellent or poor general health status by state

state_table <- svytable(~state + gen_health, design = design3)
rowp_state_df <- as.data.frame(prop.table(state_table, margin = 1))

# create a vector of levels to remove
levels_to_remove <- c("0", "80", "Puerto Rico", "Guam")
# remove the levels
rowp_state_df <- rowp_state_df[!(rowp_state_df$state %in% levels_to_remove), ]


# Subset the state_data data frame to include only poor health status percentages
poor_health_data <- subset(rowp_state_df, gen_health == "Poor")
colnames(poor_health_data)[3] <- "percentage"
# Subset the state_data data frame to include only excellent health status percentages
excellent_health_data <- subset(rowp_state_df, gen_health == "Excellent")
colnames(excellent_health_data)[3] <- "percentage"


# This will create a choropleth map that shows the percentage of individuals who self-report excellent or poor general health status by state.
# Create a choropleth map for poor health
plot_usmap(data = poor_health_data, values = "percentage", include = poor_health_data$state) +
  scale_fill_gradient(low = "#F0FFF0", high = "#8B0000") +
  theme(legend.position = "right",plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
  ggtitle("The South more likely to have 'Poor' General Health")

# choropleth map for excellent health
plot_usmap(data = excellent_health_data, values = "percentage", include = excellent_health_data$state) +
  scale_fill_gradient(low = "#F0FFF0", high = "#1E4620") +
  theme(legend.position = "right",plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +  
  ggtitle("Northeast, Midwest more likely to have 'Excellent' General Health")

```

* The South is more likely to have the highest proportion of self-reported "Poor" health individuals

* Utah, Idaho, Colorado, Minnesota, South Dakota, and some states in the Northeast have the highest proportion of self-reported "Excellent" health individuals

