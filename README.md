# Permutations-model
This is a permutations model that I am using to test out Github
This model is testing the weight of fishes based on the fishing method.

# Importing data and checking base assumptions
mydata <- read.csv("testdata.csv")  
fitmodel <- lm(Weight~Type, data=mydata)  
par(mfrow=c(1,2))  
names(fitmodel)  
plot(fitmodel$res~fitmodel$fit, xlab="Fitted value", ylab="Residuals")  
abline(h=0,col="gray")  
qqnorm(fitmodel$res,main="")  
qqline(fitmodel$res)  
// Not sure how to attach plots, but will do so when I figure that out. For the time being,
// trust that I know the assumptions fail. As such, a permutations test must be carried out.
# Running a for loop in r  
fitmodel <- lm(Weight~Type, data=mydata)  
fitperm <- anova(fitmodel)$F[1]  
fitperm  
N <- 1000  
Fnull <- rep(NA,N) // The assignment of N=1000 is unnecessary, but is done here to show how the loop works.  
Fnull  
// If you want the same numbers to be generated, set.seed(1000) would be placed here.  
for(t in 1:N) {  
  typereorder <- sample(mydata$Type)  
  Fnull[t] <- (anova(lm(mydata$Weight~as.factor(typereorder))))$F[1]  
}  
Fnull  
p <-sum(Fnull>=fitperm)/1000  
p  
// This will randomly generate 1000 F-stat values, and calculate a p-value based off the number of generated stats that are larger than
// the variable fitperm, which is our f-value for our linear model.
// Alternatively, a quicker method would be to simply use the lmp function, but the purpose of this was to use a for loop. 
