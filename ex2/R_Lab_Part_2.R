#################################
###   R INTRODUCTION SESSION  ###
### STATISTICAL DATA ANALYSIS ###
###   TEL-AVIV UNIVERSITY     ###
###        OFER LITVER        ###
#################################

# First of all - let's ask R to install missing packages:
install.packages("gplots")
install.packages("HH")

# Tip: use the TAB button for auto-completion (functions, variables, etc.)

### Part 3 - ANOVA Continued ###

ToothGrowth
?ToothGrowth
table(ToothGrowth$supp, ToothGrowth$dose)   # The "$" sign is the method to call 
                                            # a column within a dataframe

attach(ToothGrowth) # Allows us to refer to the dataframe more easily
aggregate(len, by = list(supp, dose), FUN = mean)
aggregate(len, by = list(supp, dose), FUN = sd)
fit <- aov(len ~ supp*dose) # This is the command for fitting a two-way ANOVA model
summary(fit)
interaction.plot(dose, supp, len, type = "b", 
                 col = c("red", "blue"), pch = c(16, 18), 
                 main = "interaction between Dose and Supplement Type")

install.packages("gplots")
library(gplots)
plotmeans(len ~ interaction(supp, dose, sep = " "), 
          connect = list(c(1, 3, 5), c(2, 4, 6)), 
          col = c("red", "darkgreen"), 
          main = "Interaction Plot with 95% CIs", 
          xlab = "Treatment and Dose Combination")
library(HH)
interaction2wt(len ~ supp*dose)

detach(ToothGrowth) # Very important to type in as-soon as we type the "attach" command


### Part 4 - Logistic Regression ###

# We are going to work with the built-in dataset: mtcars
mtcars
?mtcars
pairs(mtcars)

# Generalized linear model:
?glm
am.glm <- glm(formula=am ~ wt, 
              data=mtcars, 
              family=binomial)
summary(am.glm)

am.glm <- glm(formula=am ~ hp + wt, 
             data=mtcars, 
             family=binomial)
summary(am.glm)

newdata <- data.frame(hp=120, wt=2.8)
predict(am.glm, newdata, type="response")

?predict.glm   # the "predict" function might be different for each model

newdata <- data.frame(hp=c(178, 120, 103), 
                      wt=c(2.28, 2.8, 3.1))
predict(am.glm, newdata, type="response")
