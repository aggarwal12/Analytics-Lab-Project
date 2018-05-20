#############Classification Algorithm on ISLR data set################
library(ISLR)
names(Smarket)
View(Smarket)
######################################################################
#To form the summary we will get the following
##################Smarket###############################
summary(Smarket)
#plot(x=Smarket)
#help(plot)
head(Smarket$Today)
boxplot(Smarket$Today~Smarket$Direction,data=Smarket)
######################################################################
plot(Smarket$Lag1,type = 'h')
cor(Smarket)
cor(Smarket[-9])
#########money###########################################################
#From the correlation we can see that every year there is an increase in the Stock rate 
attach(Smarket)

plot(Volume)

########################################################################
glm.fit=glm(formula = Direction~Lag1 + Lag2 + Lag3 +Lag4 +Lag5+Volume,family=binomial,data = Smarket)
glm.fit
summary(glm.fit)
###########################################################################
glm.probs=predict(glm.fit,type="response")
View(glm.probs)
####################################################################
###########To view the first 10 probabilities#######################
glm.probs[1:10]
contrasts(Direction)

#######################################################################
glm.pred=rep("Down",1250)
glm.pred
########################################################################
##############To create a vector of Down Probabilities##################
glm.up=glm.pred[glm.probs>0.5]="Up"
glm.pred
########################################################################
table(glm.pred,Direction)
#######################################################################
########################################################################
(594+49)/(49+54+553+594)
#########################################################################
#So prediction is equal to 0.5144 or 51.44%#############################
#####To create a vector to seperate the data of less than the year 2005#############
trainingdata=(Smarket$Year<2005)
trainingdata
Smarket.2005=Smarket[!trainingdata,]
Smarket.trainingdata=Smarket[trainingdata,]
dim(Smarket.2005)
dim(Smarket.trainingdata)
Direction.2005=Direction[!trainingdata]
Year.2005=Year[!trainingdata]
Year.2005
Direction.2005
Smarket.2005
###############################################################
View(Smarket.trainingdata)
#################################################################
##########Fitting a logistic regression model using the subset of the observations that corresponds to dates before 2005########
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family = binomial,subset = trainingdata)
################################################################
glm.probs=predict(glm.fit,Smarket.2005,type="response")
dim(Smarket.2005)
names(glm.probs)
names(Smarket.2005)
##################################################################
glm.pred=rep("Down",252)
glm.pred
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Direction.2005)
##################################################################
mean(glm.pred==Direction.2005)
###################################################################
######################################################
#####Since the mean coming out to be 56% of the daily movements have been correctly predicting #######
######With these parameter market will be able to correctly predict 56% of the time#######
#########Now calculatuping accuracy on
(106)/(106+76)

#x <- c(0:10, 50)
#x
#xm <- mean(x)
#xm


