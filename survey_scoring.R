# Survey Analysis Script
# SURV400

################## Reverse Scoring Items ################## 
# Sometimes you write questions in a way that is contrary to your other 
# questions. (e.g., In a sadness survey, you may have a question about how 
# happy someone is.) This can help us make sure participants are paying 
# attention, but it also may help us increase content or criterion validity if 
# the construct is multidimensional. However, we cannot treat the score on this 
# item like the others. In fact, we have to change each score to its opposing 
# value on the other half of the scale in a process called reverse scoring. So, 
# on a 5-point scale, 5s become 1s (strongly agrees become strongly disagrees), 
# 4s become 2s (agrees become disagrees), and so on.

# We can easily reverse score in R by subtracting one more than the number of 
# positions on the scale (e.g., 6 for a 5-point scale, 8 for a 7-point scale, 
# and 101 for a 100-point scale) from the question(s) that are reverse worded. 
# Examples of this are below.

### create fake data 
df_5point <- data.frame(pred1=c(5, 4, 4, 5, 4, 3, 2, 1, 2, 1),
                        pred2=c(1, 2, 2, 1, 2, 3, 4, 5, 4, 5),
                        pred3=c(4, 4, 4, 5, 4, 3, 2, 4, 3, 1),
                        pred4=c(3, 4, 2, 2, 1, 2, 5, 4, 3, 2),
                        out1=c(2, 2, 3, 2, 3, 1, 4, 5, 3, 4),
                        out2=c(1, 2, 4, 1, 5, 1, 1, 5, 4, 4),
                        out3=c(5, 1, 3, 2, 3, 2, 4, 4, 3, 2),
                        out4=c(4,4, 3, 2, 4, 1, 4, 5, 4, 5),
                        out5=c(2, 1, 2, 1, 2, 1, 2, 1, 2, 3))
### assume item 5 for the outcome variable is reverse coded
### for a 5 point scale, subtract 6 from every item
df_5point$out5_rev <- 6-df_5point$out5  # this will create a new variable called out5_rev that is the reverse score of out5
### we will now use this instead of out5 when creating our total variable

### but if you have a 7 point Likert-type scale...
df_7point <- data.frame(pred1=c(5, 4, 4, 5, 4, 3, 2, 1, 2, 1),
                        pred2=c(1, 2, 2, 1, 2, 3, 6, 7, 5, 3),
                        pred3=c(4, 4, 2, 7, 6, 3, 1, 4, 6, 1),
                        pred4=c(6, 4, 2, 2, 1, 2, 7, 4, 6, 2),
                        out1=c(2, 1, 6, 2, 7, 1, 4, 6, 3, 4),
                        out2=c(7, 2, 4, 1, 7, 1, 1, 6, 4, 4),
                        out3=c(1, 1, 6, 7, 3, 2, 4, 4, 3, 2),
                        out4=c(4,7, 3, 2, 6, 1, 6, 5, 4, 5),
                        out5=c(2, 1, 2, 1, 6, 1, 7, 1, 2, 3))
### assume item 4 for the outcome variable is reverse coded
### for a 7 point scale, subtract 8 from every item
df_7point$out4_rev <- 8-df_7point$out4 # this will create a new variable called out4_rev that is the reverse score of out4
# we will now use this instead of out4 when creating our total variable

################## Creating Sum Scores ################## 
### let's assume we're going to sum up our items to create the total variable
### for the five-point scale...
# recall in R we can manipulate specific columns since these themselves are objects. There are multiple ways to do this...
# if your predictor questions are in a row like they are in this dataframe
df_5point$predictor_TOT <- rowSums(df_5point[,1:4])
### however, you can do the same thing by calling the variables by name...
df_5point$predictor_TOT <- df_5point$pred1 + df_5point$pred2 + df_5point$pred3 + df_5point$pred4
### these will give you the same value. You can do this for the outcome too, but REMEMBER we reverse coded the last outcome question so we can't use the first method
df_5point$outcome_TOT <- df_5point$out1 + df_5point$out2 + df_5point$out3 + df_5point$out4 + df_5point$out5_rev
# you can then use these total variables to test the correlation between your predictor and outcome
cor.test(df_5point$predictor_TOT,df_5point$outcome_TOT)

### for the seven-point scale...
# if your predictor questions are in a row like they are in this dataframe
df_7point$predictor_TOT <- rowSums(df_7point[,1:4])
### however, you can do the same thing by calling the variables by name...
df_7point$predictor_TOT <- df_7point$pred1 + df_7point$pred2 + df_7point$pred3 + df_7point$pred4
### these will give you the same value. You can do this for the outcome too, but REMEMBER we reverse coded the fourth outcome question so we can't use the first method
df_7point$outcome_TOT <- df_7point$out1 + df_7point$out2 + df_7point$out3 + df_7point$out4_rev + df_7point$out5
# you can then use these total variables to test the regress your outcome on your predictor
summary(glm(outcome_TOT~predictor_TOT, data = df_7point))

################## Reliability/Internal Consistency ################## 
# don't forget to check the reliability script (on Jupyter) for how to calculate
# the internal consistency of our scale (McDonald's omega) from the MBESS package.

### end of script