#EXAMPLE 2: Beetles
#This analysis looks at the number of carrion feeding beetles 
#caught in pitfall traps at 6 distances from a pig corpse 
#experimentally placed in woodland

#Read in the data

# attach the data
attach(beetle_data)

#Look at the data:
plot(dist,beetles,pch=16)
#check to see if it looks like variance changes for 
#different mean values of beetles.  
boxplot(beetles~dist)
hist(beetles, breaks=10)

#Note can also try other links e.g. sqrt.  

#what kind of model would it be good to fit? 
#Fit the model using the FAMILY and LINK of your choice:
beetle.glm <-glm(beetles~dist,poisson(link="log"))  
summary(beetle.glm)

#What does the summary tell us? Does it look like beetles decline with distance?  

#CHECK FOR OVERDISPERSION
# Is this data overdipersed? (If residual deviance/residual df >>1) If so, then fit the model again but
#this time use the family quasipoisson and a log link function 

beetle.glm.od <-glm(beetles~dist,quasipoisson(link="log")) 
summary(beetle.glm.od)

# compare the output with the first model that was fit.  What 
#is different and what is the same? 
summary(beetle.glm)  #this is the first model
#Let's look at the diagnistic plots for the second model
par(mfrow=c(2,2))
plot(beetle.glm.od) 
#how do the diagnostic plots for the first model look? 
plot(beetle.glm) 

#Finally, let's look at the fit of the model
par(mfrow=c(1,2)) #back to two plots per page

#We can look at it on the transformed link scale...(lp stands for linear predictor)
xpt <- seq(min(dist), max(dist), length.out = 100)
pr.lp <- predict(beetle.glm.od, newdata=data.frame(dist = xpt), se.fit = TRUE)
#Calculate 95% confidence intervals.  We can assume a normal distrubution on the transformed scale
lower.lp <- pr.lp$fit - qnorm(0.95) * pr.lp$se.fit
upper.lp <- pr.lp$fit + qnorm(0.95) * pr.lp$se.fit 
plot(dist,log(beetles),pch=16, xlab="Distance class", ylab = "log(Number of beetles)", main="Transformed scale")
lines(xpt,pr.lp$fit,lwd=2,col="blue")
lines(xpt, upper.lp,lwd = 1.5, lty = 2)
lines(xpt, lower.lp,lwd = 1.5, lty = 2)

#Or we can look at the fit of the data on the untransformed scale - we normally do this as it is
#more useful since this is the realtionship we're interested in
#We do this by asking the predict function to predict type"response" which means
#transfomr the predictions to the response scale

linefit.glm.od <- predict(beetle.glm.od, type="response")
plot(dist,beetles,pch=16, xlab="Distance class", ylab = "Number of beetles",main="Response scale")
lines(dist,linefit.glm.od,lwd=2,col="green")

#we also need to transform back the 95% confidence intervals we made
lower =exp(lower.lp)
upper = exp(upper.lp)
#now we can add them to the plot
lines(xpt, upper,lwd = 1.5, lty = 2)
lines(xpt, lower,lwd = 1.5, lty = 2)


#clean up
detach(beetle_data)

