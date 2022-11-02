library(dplyr)
library(lme4)

dat <- read.csv(file.choose())

# Number of fact introduced
# unique(dat$fact_id)

#Threshold for items to be considered as learned
threshold = 0.5
nrow(subset(dat, alpha>threshold))
learned <- subset(dat, alpha>threshold)

#Keep unique values (each fact is only counts as learned once)
learned = distinct(learned, subject,question, .keep_all= TRUE)


#Number of learned facts, per subject
learnedPerSubject <- learned %>% group_by(condition, subject) %>% 
  summarise(countLearned=n(),.groups = 'drop') %>% 
  as.data.frame()


###Statistical Analysis
learnedPerSubject$condition <- as.factor(learnedPerSubject$condition)
learnedPerSubject$subject <- as.factor(learnedPerSubject$subject)

##Simple ANOVA
anovaModel <- aov(countLearned ~ condition + subject, data = learnedPerSubject)
summary(anovaModel)


## Creation of two linear mixed effects models
linearModel1 <- lmer(countLearned ~ condition + (1|condition), data=learnedPerSubject, REML=FALSE)
linearModel2 <- lmer(countLearned ~ (1|condition), data=learnedPerSubject, REML=FALSE)

anova(linearModel1, linearModel2)

summary(linearModel2)


