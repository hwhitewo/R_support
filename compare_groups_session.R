library(janitor)
library(data.table)

medsamp = fread("C:/Users/shwol/Desktop/CFA Temp/exams.csv") %>% janitor::clean_names()

head(medsamp)


# t-test
# here our numeric values are the test scores
# we can test the means of the scores based on categorical variables
# * with 2 levels *
# H0: (mean score of females - mean score of males) = 0
#      equivalently: 
#       mean score of females = mean score of males
#       i.e. no significant difference between scores of females & males
# Ha: (mean score of females - mean score of males) != 0
#     equivalently, :
#      mean score of females != mean score of males
#      i.e. there is a significant difference between scores of females & males

# t test assumes that numeric variable is normally distributed,
# can check this assumption visually
# check assumption on each score
ggplot(medsamp,aes(x=math_score))+geom_histogram(bins = 15)
ggplot(medsamp,aes(x=reading_score))+geom_histogram(bins = 15)
ggplot(medsamp,aes(x=writing_score))+geom_histogram(bins = 15)

# can check normality with qqplots too
qqnorm(medsamp$math_score); qqline(medsamp$math_score)
qqnorm(medsamp$reading_score); qqline(medsamp$reading_score)
qqnorm(medsamp$writing_score); qqline(medsamp$writing_score)


# next assumption assumes that the variance of the score
# within each group (e.g. male and female) is about the same
# visually, can look at the range of the scores, check that distributions look pretty similar
ggplot(medsamp,aes(x=math_score))+geom_histogram(bins = 15)+facet_grid(~gender)
ggplot(medsamp,aes(x=reading_score))+geom_histogram(bins = 15)+facet_grid(~gender)
ggplot(medsamp,aes(x=writing_score))+geom_histogram(bins = 15)+facet_grid(~gender)



# TEST OF COEFFICIENTS  (beta)
# t-test of coefficient more of a marginal test
# uses a t-statistic
# H0: beta = 0, e.g. this coefficient does not add any benefit to the model
# Ha: beta != 0, e.g. this coefficient is significant in predicting the response

# This is the test that you see in the summary output.
# NOTE: if you have 1 single variable in your model, that has 2 levels,
# the p-value from the summary output will be the same as the p-value from
# the anova output, see below
sex1 = lm(math_score ~ gender, data = medsamp)
summary(sex1)
anova(sex1)



# ANOVA 
# helps test significance of variables with more than 2 categories.
# when running anova on categorical variables with more than 2 levels the hypothesis can be written:
# H0: There is no significant difference between any levels of the variable
# Ha: At LEAST 1 pair of levels are significantly different in their response.
sex2 = lm(math_score ~ race_ethnicity + gender , data = medsamp)
summary(sex2)
anova(sex2)

# IF you found from ANOVA that the categorical variable with more than 2 levels is 
# significant - you can use the Tukey HSD test to compare each combination
# of the levels
# you can try this below, even though nothing appears to come out significant
# these are basically doing a bunch of t-tests on each individual pair of categories,
# but it adjusts for MULTIPLE COMPARISONS
# multiple comparisons is when you do a lot of t-tests or hypothesis tests within
# the same data, so the chance of you finding spuriosly signiciant results increases 
# with each individual t-test you would do.
# tukey hsd accounts for this and provides "adjusted", more appropriate, p-values
# note TukeyHSD only takes in the "aov(...)" function which works practically the same as the
# anova function
TukeyHSD(aov(math_score ~ gender + race_ethnicity, data = medsamp))

sex3 = lm(math_score ~ gender+race_ethnicity, data = medsamp)
summary(sex3)
anova(sex3)




sex3a = lm(math_score ~ race_ethnicity*gender, data = medsamp)
summary(sex3a)
anova(sex3a)

all2 = lm(reading_score ~ gender + test_preparation_course + parental_level_of_education +
            race_ethnicity , data = medsamp)
summary(all2)
anova(all2)





# race model
race1 = lm(math_score~race_ethnicity, data = medsamp)
anova(race1)


#### interaction model

# looking at the interaction plots
interaction.plot(medsamp$gender, medsamp$race_ethnicity, medsamp$reading_score)
interaction.plot(medsamp$gender, medsamp$race_ethnicity, medsamp$writing_score)
interaction.plot(medsamp$gender, medsamp$race_ethnicity, medsamp$math_score)

# interaction terms help determine if, in this conetxt, the test scores
# vary not only between gender (ignoring race) or race (ignoring gender)
# but they vary within race and gender, e.g. white girls perform differently
# than black girls rather than just saying blacks perform differently from whites
# or boys perform differently from girls
intModel1 = lm(reading_score~race_ethnicity*gender, data = medsamp)
summary(intModel1)
anova(intModel1)
TukeyHSD(aov(reading_score~race_ethnicity*gender,data = medsamp))

intModel2 = lm(math_score~race_ethnicity*gender, data = medsamp)
anova(intModel2)
TukeyHSD(aov(math_score~race_ethnicity*gender, data = medsamp))




# If you have imbalances in the categorical variable that you're measuring over -
# like we can see the imbalances of the race variable here
# you'd be safer using "Anova III"
# they have a good explanation here of the differences
# https://www.r-bloggers.com/anova-%E2%80%93-type-iiiiii-ss-explained/
table(medsamp$race_ethnicity)

# to use type 3 anova you can use the "Anova" function within the car package
# note: this has a capital A, which makes it different from the "anova" function.


car::Anova(intModel1, type = "III")



