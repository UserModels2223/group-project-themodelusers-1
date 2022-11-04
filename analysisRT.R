library(dplyr)
library(lme4)
library(ggplot2)

dat <- read.csv(file.choose())

# Number of fact introduced
# unique(dat$fact_id)

#Threshold for items to be considered as learned
threshold = 0
nrow(subset(dat, alpha>threshold))
learned <- subset(dat, alpha>threshold)

#Keep unique values (each fact is only counts as learned once)
learned = distinct(learned, subject, question, .keep_all= TRUE)

### Removing outliers that are outside of  2*IQR of Q1 and Q3
Q1 <- quantile(learned$rt, .25)
Q3 <- quantile(learned$rt, .75)
IQR <- IQR(learned$rt)
learned <- subset(learned, learned$rt > (Q1 - 2*IQR) & learned$rt < (Q3 + 2*IQR))


# 
# #Number of learned facts, per subject
# learnedPerSubject <- learned %>% group_by(condition, subject) %>% 
#   summarise(countLearned=n(),.groups = 'drop') %>% 
#   as.data.frame()

#Basic visualisation of data

customDataForBoxplot <- learned
customDataForBoxplot$condition <- as.character(customDataForBoxplot$condition)
customDataForBoxplot$condition[customDataForBoxplot$condition == '1'] <- 'Default'
customDataForBoxplot$condition[customDataForBoxplot$condition == '2'] <- 'LD Model'
customDataForBoxplot$condition[customDataForBoxplot$condition == '3'] <- 'Col. Feedback'
customDataForBoxplot$condition[customDataForBoxplot$condition == '4'] <- 'LD Model and\nCol. Feedback'
customDataForBoxplot$condition <- factor(customDataForBoxplot$condition , 
                                         levels=c("Default", "LD Model", "Col. Feedback",
                                                  "LD Model and\nCol. Feedback"))

boxplot <- ggplot(customDataForBoxplot, aes(x=condition, y=rt)) + 
  geom_boxplot() +
  labs(title = "Reaction Time (first keypress) per Condition", x="Condition", y="Reaction Time (ms)") +
  theme_grey(base_size = 22)
  
boxplot

###Statistical Analysis
learned$condition <- as.factor(learned$condition)
learned$subject <- as.factor(learned$subject)

##Simple ANOVA
anovaModel <- aov(rt ~ condition + subject, data = learned)
summary(anovaModel)
TukeyHSD(anovaModel, "condition")


# 
# ## Creation of two linear mixed effects models
# linearModel1 <- lmer(rt ~ condition + (1|condition), data=learned, REML=FALSE)
# linearModel2 <- lmer(rt ~ condition + subject + (1|condition), data=learned, REML=FALSE)
# 
# anova(linearModel1, linearModel2)
# 
# summary(linearModel2)