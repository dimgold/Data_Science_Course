#################################
###   R INTRODUCTION SESSION  ###
### STATISTICAL DATA ANALYSIS ###
###   TEL-AVIV UNIVERSITY     ###
###        OFER LITVER        ###
#################################

### Part 1 - Basic operations ###
3+4
x <- 3+4
x

myvector <- c(8, 6, 9, 10, 5)
myvector
myvector[4]

mylist <- list(name = "Fred", wife = "Mary", myvector)
mylist
mylist[[2]]
mylist$wife

mynames <- c("Mary", "John", "Ann", "Sinead", "Joe", "Mary", "Jim", "John", "Simon")
table(mynames)
mytable <- table(mynames)
mytable[[4]]

# Functions
log10(1000)
mean(myvector)
rnorm(10)
rnorm(10, 100, 5)
?rnorm

myfunction <- function(x){
    return(x^2 + 20)
}

myfunction(10)
myfunction(25)


### Part 2 - Analysis of Variance ###

# download class_example_1.csv from the Moodle and move it
# to your working directory (usually "My Documents").
# You can change your working directory under
# "Session -> Set Working Directory"
example1 <- read.csv("class_example_1.csv")
example1
summary(example1)

### CHECKING ASSUMPTIONS
# First, we'd like to plot the data to Check it for equal variance:
plot(coag~diet, data=example1)
# We can see that the interquartile range looks approximately equal

# Plotting the QQ-plot:
qqnorm(example1$coag)
qqline(example1$coag)

# Let's run a one-way ANOVA:
model1 <- aov(coag ~ diet, data = example1)
summary(model1)
# or:
model1 <- lm(coag ~ diet, data = example1)
anova(model1)


# Duncan's test
install.packages("agricolae")
library(agricolae)
model1.duncan <- duncan.test(model1, trt = "diet")
model1.duncan

# Another way:
print(duncan.test(model1, trt = "diet"))


## Chicken weight ANOVA example:

chickwts
summary(chickwts)
plot(weight ~ feed, data = chickwts)
# The equal variance assumption - is it practical?

model <- lm(weight ~ feed, data = chickwts)
anova(model)
summary(model)
