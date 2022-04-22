
install.packages("tidyverse")
install.packages("openxlsx")

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(openxlsx)

# Q1. - Response Rate

# Number of Respondents #
survey <- read.csv(file = 'C:/Users/zacha/OneDrive/Documents/Professional Docs/CU Interview - Insights Analyst/survey_data.csv')
head(survey)
completed <- count(survey,vars = "ID")
completed #178

# Total Surveys Distributed #
population <- read.csv(file = 'C:/Users/zacha/OneDrive/Documents/Professional Docs/CU Interview - Insights Analyst/population_data.csv')
head(population)
total_pop <- count(population,vars = "ID")
total_pop #400

178/400
  # .445 OR 45% response rate #

# Q2. Student Satisfaction
  head(survey) 
  
  # Descriptive Stats #
  summary(survey$satoverall)
  table(survey$satoverall)
  70/178
  29/178
  99/178
  29/178
  
  # Visual Distribution #
  survey$satoverall2 <- factor(survey$satoverall, c(1,2,3,4,5),
                               labels=c('Poor', 'Fair', 'Good', 
                                        'Very Good','Excellent'))
  
  # Bar Plot - Proportion #
  sat_prop <- ggplot(data = survey) +
    ggtitle("Overall Satisfaction") +
    ylab("Proportion") +
    xlab("") +
    labs(caption = "n = 178") +
    scale_y_continuous(labels = scales::percent)+
    geom_bar(
      mapping =aes(x=satoverall2, y = ..prop.., group = 1)
      )
  sat_prop
  

# Q3. TA and Satisfaction
  # view var:
  survey$ta
    # ta = 1 = 'Yes' ta during graduate school
    # ta = 2 = 'No' not ta during graduate school

  # recode so dummy
  # ta = 1 = 'Yes' ta during graduate school
  # ta = 0 = 'No' not ta during graduate school
  
  #duplicate database for recode#
  ta_survey <- survey
  ta_survey$ta
    
  ta_survey$ta <- recode(ta_survey$ta, '2' = 0, '1' = 1)
  ta_survey$ta

  # Prepare Data for Table %  
  ta_survey$satoverall2 <- factor(ta_survey$satoverall, c(1,2,3,4,5),
                               labels=c('Poor', 'Fair', 'Good', 
                                        'Very Good','Excellent'))
  
  ta_survey$ta2 <- factor(ta_survey$ta, c(0,1),
                         labels=c('Never TA', 'TA'))
  
  # TA Distribution #
  table(ta_survey$ta2)
  139+38
  139/177
  
  # Proportion Table to show distribution across satisfaction by TA status
  ta_sat_table <- table(ta_survey$satoverall2, ta_survey$ta2)
  ta_table <- round(100*prop.table(ta_sat_table, 2),digits = 2)
  ta_table
  
  # Regression #
  ta_sat <- lm(satoverall ~ ta, data = ta_survey)
  summary(ta_sat)
  
# Q4. Graduate Curriculum and Overall Satisfaction
  # curriculum = ranked variable 
    # 1 = Poor, 2 = Fair, 3 = Good, 4 = Very Good, 5 = Excellent
  # satoverall = ranked variable
    # 1 = Poor, 2 = Fair, 3 = Good, 4 = Very Good, 5 = Excellent
  
  summary(survey$satoverall)
  summary(survey$curriculum)
  
  # Make curriculum a factor with labels #
  survey$curriculum <- factor(survey$curriculum, c(1,2,3,4,5),
                              labels=c('Poor', 'Fair', 'Good', 
                                   'Very Good','Excellent'))
  
  # Create a variable for overall sat as a factor with labels #
  survey$satoverall2 <- factor(survey$satoverall, c(1,2,3,4,5),
                              labels=c('Poor (Overall)', 'Fair', 'Good', 
                                       'Very Good','Excellent'))
  

  curr_sat_table <- table(survey$satoverall2, survey$curriculum)
  
  round(prop.table(curr_sat_table,1),digits=3)
  
  round(100*prop.table(curr_sat_table, 1),digits=1)
  
  curr_sat <- lm(satoverall ~ curriculum, data = survey)
  anova(curr_sat)
  summary(curr_sat)
  

# Q5. Curriculum and Overall Sat depend on school? 
  # https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html #
  # duplicate dataset for recode
  q5_data <- survey

  class(q5_data)  
  
  # 'Program' appears to be identical to SchCol
  q5_data$program <- q5_data$Program 
  head(q5_data)
  
  # recode program to be numeric #
  q5_data$program <- recode(q5_data$program, 
                            'Arts & Sciences' = 1, 
                            'Engineering' = 2, 
                            'Music' = 3, 
                            'Education' = 4)
  
  class(q5_data$program)
  q5_data$program <- factor(q5_data$program, c(1,2,3,4), 
                            labels=c('Arts & Sciences','Engineering',
                                     'Music', 'Education'))
  
  class(q5_data$program)
    # variable now has 'levels' as 'factor'
  head(q5_data)
  
  # Interaction Model #
  q5_mod <- lm(satoverall ~ curriculum * program, data = q5_data)
  summary(q5_mod)
  #no significance#  
  
  
#### TASK 3: REFORMATTING DATA
  head(survey)
  head(population)  
  
  # data is currently wide since ID is only shown once
    # validated that in the survey data we have 178 unique IDs and 178 obvs
  
  # Transformed data to combine population and survey data #
    # index-match raw data
  
  # Import Data #
  wide_data <- read.csv(file = 'C:/Users/zacha/OneDrive/Documents/Professional Docs/CU Interview - Insights Analyst/reformat_data.csv')
  class(wide_data)
  
  # look at data  
  head(wide_data)

  # use reshape to convert wide data to long
  ?reshape
  
  long_data <- reshape(data = wide_data,
                       idvar = "ID",
                       varying = c("satacad","satlife","satoverall",
                                   "ta","tahelp",	"ra",	"rahelp",
                                   "curriculum",	"relFacGradStudents",
                                   "teaching",	"advising", "candidacy",
                                   "interdisc",	"employment",	"progqual"),
                       times = c("satacad","satlife","satoverall",
                                 "ta","tahelp",	"ra",	"rahelp",
                                 "curriculum",	"relFacGradStudents",
                                 "teaching",	"advising", "candidacy",
                                 "interdisc",	"employment",	"progqual"),
                       ids = "ID",
                       v.name = c("Response"),
                       direction = "long")
                   
  head(long_data)
 
   # Remove unused columns #
  long_data <- subset(long_data, select = -X)
  long_data <- subset(long_data, select = -X.1)
  long_data <- subset(long_data, select = -Program)
  class(long_data)

  # Convert to Tibble #
  long_data <- as_tibble(long_data)
  ?rename 
  # Rename Column Name #
  long_data <- rename(long_data,
                Variable = time)
  head(long_data)

  # Reorder tibble by ID #
  long_data <- long_data[order(long_data$ID),]
  head(long_data)
  
  # VALIDATED: 178 UNIQUE IDs
  
  # Save Long Data #
  write.csv(long_data, "C:/Users/zacha/OneDrive/Documents/Professional Docs/CU Interview - Insights Analyst/final_long_data.csv")
    