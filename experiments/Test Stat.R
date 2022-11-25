library(tidyverse)
library(dplyr)

#Reading in the data
stats_df = read.csv("sequence_df.csv", header = TRUE)
head(stats_df)

#Converting the receiving_shot into a factor
stats_df$receiving_shot = as.factor(stats_df$receiving_shot)
re_level = levels(stats_df$receiving_shot) #Checking the levels of the factor

#for loop
result = array(0, dim = c(length(re_level), 4))
colnames(result) = c("Receiving Shot", "Best Shot", "Frequency", "Probability")

for (i in 1:length(re_level)){
  test = stats_df$combined[which(startsWith(stats_df$combined, re_level[i]),TRUE)]
  test = as.factor(test)
  test_level = levels(test)
  
  result[i,1] = re_level[i]
}

#Convert it to a data frame
result = data.frame(result)
result$Frequency = as.numeric(result$Frequency)

#Obtain the sequence that will give the highest winning probability for each case
#Reading in the stat data
prob_df = read.csv("stats_df.csv", header = TRUE)
head(prob_df)

#for loop - Highest Probability with total occurrence greater than 60
for (i in 1:length(re_level)){
  combs = prob_df$combined[which(startsWith(prob_df$combined, re_level[i]), TRUE)]
  probs = prob_df$p[which(startsWith(prob_df$combined, re_level[i]), TRUE)]
  times = prob_df$total_occurences[which(startsWith(prob_df$combined, re_level[i]), TRUE)]
  len = nchar(re_level[i])+1
  if(max(times) < 50){
    seq = combs[which(probs == max(probs))]
    if(length(seq) > 1){
      samp = combs[sample(which(probs == max(probs)), size = 1)]
      result[i,2] = substring(samp,len)
      result[i,3] = times[sample(which(probs == max(probs)), size = 1)]
      result[i,4] = max(probs)
    }
    else{
      result[i,2] = substring(seq, len)
      result[i,3] = times[which(probs == max(probs))]
      result[i,4] = max(probs)
    }
  }else if (max(times) >= 50){
    seq = combs[which(probs == max(probs[which(times >= 50)]))]
    if(length(seq) > 1){
      samp = combs[sample(which(probs == max(probs[which(times >= 50)])), size = 1)]
      result[i,2] = substring(samp, len)
      result[i,3] = times[sample(which(probs == max(probs[which(times >= 50)])), size = 1)]
      result[i,4] = max(probs[which(times >= 50)])
    }
    else{
      seq = combs[which(probs == max(probs[which(times >= 50)]))]
      result[i,2] = substring(seq, len)
      result[i,3] = times[which(probs == max(probs[which(times >= 50)]))]
      result[i,4] = max(probs[which(times >= 50)])
    }
  }
}

result2 = arrange(result, desc(Frequency))
result2

write.csv(result2, "/Users/stephaniejeong/SFU/Fall 2022/MATH303/Final Project/prob.csv", row.names = FALSE)












