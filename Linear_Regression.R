mydata = read.csv("C:\\Users\\Sexy Laudav\\Desktop\\Centaur_CEI Calculation_Updated.csv")
View(mydata)
attach(mydata)
head(mydata)
lm(formula = mydata$Appeal_Score ~ mydata$Sentiment_Web_Score.Header. + mydata$Award_Score + mydata$celebrity_is_audience_Score + mydata$Sentiment_Score +mydata$Actual.Engagemt.score,data = mydata)
lm(formula = mydata$Appeal_Score ~ mydata$Sentiment_Web_Score.Header. + mydata$Award_Score + mydata$celebrity_is_audience_Score + mydata$Sentiment_Score +mydata$Normalised.Engagement_Score,data = mydata)
lm(formula = mydata$Awareness_Score~mydata$Relative_Follower_Change_Score+mydata$Absolute_Follower_Change_Score+mydata$Normalise.Reach.Added+mydata$Web_Score,data = mydata)
