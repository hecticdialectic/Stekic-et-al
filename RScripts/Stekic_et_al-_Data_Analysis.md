---
title: "Words of a Feather Flock Together"
subtitle: 'Data Analysis and Exploration'
output: 
  html_document:
    keep_md: true
    theme: journal
    toc: true
    toc_depth: 2
    toc_float: 
      collapsed: false
    code_folding: hide
---

#Introduction

The following is data analysis for Stekic, Kovic, and Nielsen (2019), where we explored the learnability of artificial languages that differed in the relationship between those labels and the objects that they describe. All files for this experiment can be found at our [GitHub Repository](https://github.com/hecticdialectic/Stekic-et-al) and further details of the experimental design can be found at our OSF Repository.

Below in two code chunks we load libraries and read in our files. 

Note that in this document all code is "folded" by default- you can unfold sections (and look at the code) by clicking on the "Code" button on the Top Right of each block.

####Loading Libraries

```r
library(data.table)
library(tidyverse)
library(ggthemes)
library(outliers)
library(lme4)
library(lmerTest)
library(afex)
library(kableExtra)
```

####Loading Data

```r
# Our data is in two formats because of a change in jsPsych version halfway through our data collection - thus we read those separate data file types (distinguishable by file size) in here separately, then combine them into a single large data frame

setwd("C:/Users/Alan/Documents/GitHub/Stekic-et-al/Data/50s/")
files50  <- list.files(pattern = '\\.csv')
tables50 <- lapply(files50, read.csv, header = TRUE)
combined.df.50 <- do.call(rbind , tables50)

combined.df.50[] <- lapply(combined.df.50, function(x) gsub("\\\\", "", x))

setwd("C:/Users/Alan/Documents/GitHub/Stekic-et-al/Data/40s/")
files40  <- list.files(pattern = '\\.csv')
tables40 <- lapply(files40, read.csv, header = TRUE)
combined.df.40 <- do.call(rbind , tables40)

colnames(combined.df.50)<- c("rtD", "key_press", "trialtype2", "TrialIndex", "elapsed", "node_id", "viewhist", "responses", "Yoking", "RT", "RespKey", "RespCorr", "TrialType", "Image", "Label", "Location", "CorrectResponse", "Block", "BlockTrial", "Condition", "LabelType", "Subcondition", "TrialNum")


colnames(combined.df.40)<- c("rtD", "key_press", "trialtype2", "TrialIndex", "elapsed", "node_id", "viewhist", "responses", "Yoking", "RT", "RespKey", "RespCorr", "TrialType", "Image", "Label", "Location", "CorrectResponse", "Block", "BlockTrial", "Condition", "LabelType", "Subcondition", "TrialNum")
  
combined.df <- rbind(combined.df.50, combined.df.40)
```

## Cleaning Up the Data

```r
#1- Substitute out some special characters
combined.df[] <- lapply(combined.df, function(x) gsub("\\\\", "", x))
combined.df[] <- lapply(combined.df, function(x) gsub("[{}]", "", x))
combined.df[] <- lapply(combined.df, function(x) gsub("\"", "", x))

#2- Add in a column with the biographical data (which is currently stored in a single value on the fourth line of each participant's file)
biodata <- combined.df[seq(4, nrow(combined.df), 246),]
biodata <- as.data.frame(biodata$responses)
colnames(biodata) <- "biodata"

biodata <- separate(biodata, col=biodata, into = c("Age", "Gender", "Specify"), sep = ",")

biodata$Age <- sub("age:", "", biodata$Age)
biodata$Gender <- sub("gender:", "", biodata$Gender)
biodata$Specify <- sub("specify:", "", biodata$Specify)

combined.df$Age <- rep(biodata$Age, each = 246)
combined.df$Gender <- rep(biodata$Gender, each = 246)

#3- Add a unique participantID (actually the name of each file)
files <- c(files40, files50)
files <- sub(".csv", "", files)

combined.df$ParticipantID <- rep(files, each= 246)

#4- Clean up our Data, Get rid of some useless columns, and re-sort the remaining columns into ones we will actually use
CleanData <- subset(combined.df, select = c("ParticipantID", "Condition", "Subcondition", "Yoking", "TrialNum", "TrialType", "Block", "BlockTrial", "Image", "Label", "Location", "CorrectResponse", "RespKey", "RespCorr", "RT"))

#5- Get rid of extra lines from the jsPsych output- leaving us with only our Trial data (everything else of use we've extracted and added as columns)
CleanData <- subset(CleanData, TrialNum > 0)

#6- Set the data types of our various columns
CleanData$ParticipantID <- as.factor(CleanData$ParticipantID)
CleanData$Condition <- as.factor(CleanData$Condition)
CleanData$Subcondition <- as.factor(CleanData$Subcondition)
CleanData$Yoking <- as.factor(CleanData$Yoking)
CleanData$TrialType <- as.factor(CleanData$TrialType)
CleanData$Location <- as.factor(CleanData$Location)

CleanData$TrialNum <- as.numeric(CleanData$TrialNum)
CleanData$Block <- as.numeric(CleanData$Block)
CleanData$BlockTrial <- as.numeric(CleanData$BlockTrial)
CleanData$RespCorr <- as.numeric(CleanData$RespCorr)
CleanData$RT <- as.numeric(CleanData$RT)
```

We have all of our data combined, but it has to be cleaned up considerably (tidied)

1) remove all of the slashes and backslashes everywhere to have data that we can actually read and parse properly with later commands
2) Extract the biological data from the participants, which is on line 4 of each participant's dataframe, then put it back into the main dataframe as the Age and Gender columns
3) Add a unique participantID column (from the filenames)
4) Trim down to the required columns and re-order them
5) Delete the useless rows
6) Make sure the columns are all the right data types

####Exploring the Data

```r
#1- Removing participant 7gtriiTixvBaQ
RespCorrStrange <- CleanData[is.na(CleanData$RespCorr),]
CleanData.RespCorr <- subset(CleanData, ParticipantID != "7gtriiTixvBaQ")

#2A- Removing Participants with impossible negative RT values
RTNegative <- subset(CleanData.RespCorr, RT < 0)

CleanData.RTNegCorr <- subset(CleanData.RespCorr, ParticipantID != "60CBPiTiO1gKw")
CleanData.RTNegCorr <- subset(CleanData.RTNegCorr, ParticipantID != "k3LHwiTiOx7fx")

# Verify that we have removed all negative values RT values from the data frame
RTNegative2 <- subset(CleanData.RTNegCorr, RT < 0)

#2B- Removing Participants with very large single RT values

RTHigh <- subset(CleanData.RespCorr, RT>120000 )

#Relevel this to get rid of factor levels that aren't there any longer
RTHigh$ParticipantID <- factor(RTHigh$ParticipantID)

RTHighs <- as.data.frame(table(RTHigh$ParticipantID))
colnames(RTHighs) <- c("Participant", "Count")

#re-order by count
RTHighs <- RTHighs[order(-RTHighs$Count),]

#Give shorter participantIDs
RTHighs$Participant <- substring(RTHighs$Participant, 1,3)

#Making the Participant Column into an Index
RTHighs2 <- RTHighs[-1]
row.names(RTHighs2) <- RTHighs$Participant

#Transpose for output
RTHighsT <- as.data.frame(t(RTHighs2))
```

The basics of tidying are now done, but we should take a visual look at the data- for example looking for impossible values

I leave this as an exercise to the reader, but point out things I noticed:

1) I sorted the "RespCorr" (Correctness of response where 0 = Incorrect and 1 = Correct) and noted that we have a single case where the value is "NA"- thsi is Testing Trial 1 for the participant 7gtriiTixvBaQ . Because we can't be certain what went wrong here, and whether the other data obtained from the participant is of good quality (visually it appears to be fairly good, with the participants fairly uniformly taking about 5 seconds per trial and being correct on about half of trials), the most conservative approach is the simply throw out all of the data from this participant, in case something larger went wrong

2) I sorted the RT (response time) column as well, revealing a number of strange values
    
    a) First, there are a number of **negative** RT values, which are actually impossible
      -These are from two participants- there are 17 from participant 60CBPiTiO1gKw and 1 from participant k3LHwiTiOx7fx. As above, we will simply remove          these participants from our data entirely
      
    b) Second, there are some very **high** RT values, with the highest being 2460.568 seconds (41.0094667 minutes or 0.6834911 hours). This is patently too long for a single trial - beyond the realm where we might consider that a participant could be simply evaluating possible correct responses- This long of a break also almost certainly has negative learning consequences. As such, we must set a baseline for removing participants who have individual trials over a certain length. Here we will use 2 minutes (1.2\times 10^{5} milliseconds) as a cutoff time to eliminate **all** trials for a participant. Note that we remove individual trials with lengths over 30 seconds in a section below- those we view as outliers for participants responding normally, whereas trials over 2 minutes in length suggest a failure of attention and/or multitasking that could deleteriously effect the results on **all** trials.
    
    The prospect of divided attention is broadly supported by looking at these trials- we can see that the 32 trials with RTs over 2 minutes are from 21 participants. 
    
    The offending participants can be seen below:

####Outputting a table with KableExtra

```r
knitr::kable(RTHighsT, caption = 'Number of Trials over 2 Minutes long by ParticipantID') %>%
  kable_styling() %>%
  scroll_box(width = "800px", height = "150px")
```

<div style="border: 1px solid #ddd; padding: 5px; overflow-y: scroll; height:150px; overflow-x: scroll; width:800px; "><table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Number of Trials over 2 Minutes long by ParticipantID</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> iN9 </th>
   <th style="text-align:right;"> YtJ </th>
   <th style="text-align:right;"> oOr </th>
   <th style="text-align:right;"> 5KG </th>
   <th style="text-align:right;"> Ecp </th>
   <th style="text-align:right;"> lDj </th>
   <th style="text-align:right;"> 5bM </th>
   <th style="text-align:right;"> 60C </th>
   <th style="text-align:right;"> 6T8 </th>
   <th style="text-align:right;"> 7U9 </th>
   <th style="text-align:right;"> 9wR </th>
   <th style="text-align:right;"> AVp </th>
   <th style="text-align:right;"> d3i </th>
   <th style="text-align:right;"> fTX </th>
   <th style="text-align:right;"> IOe </th>
   <th style="text-align:right;"> jwE </th>
   <th style="text-align:right;"> QBc </th>
   <th style="text-align:right;"> SWq </th>
   <th style="text-align:right;"> tHU </th>
   <th style="text-align:right;"> y9y </th>
   <th style="text-align:right;"> YVW </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Count </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table></div>
####Removing Long RT Participants

```r
#Get the participantIDs for participants with too-long RT values

participants <- unique(RTHigh$Participant)

#Write a for loop that removes all the lines of the data frame for each of these participants
CleanData.HighRTCorr <- CleanData.RTNegCorr

for (participant in participants) {
  
  CleanData.HighRTCorr <- subset(CleanData.HighRTCorr, ParticipantID != participant)
  
}

CleanData2 <- CleanData.HighRTCorr

#Output this clean Data to acsv
write.csv(CleanData2, file=("C:/Users/Alan/Documents/GitHub/Stekic-et-al/Data/CleanData.csv"))
```

    
In the code chunk above, we remove all of the data for these 21 participants - we could be more liberal with our criteria and only erase participants with multiple lapses, but given our large amount of data a conservative policy is likely best.
    
This gives us a first bash at clean data - we started with 429 participants, removed 1 participant (Participant ID "7gtriiTixvBaQ") based on an NA value on one of their trials, 2 participants (Participant IDs: 60CBPiTiO1gKw, k3LHwiTiOx7fx) with impossible Negative RT values, and 20 participants (Participant IDS: 5bMnRiTi8m8VY, fTXj1iTiH9yh4, jwEltiTiE1yXC, 5KGA3iTidksdQ, 60CBPiTiO1gKw, 6T8RyiTitqvgs, 7U90riTiU7b4A, 9wR2NiTi3j05w, AVpqMiTidvCFP, d3iMTiTi0J4mA, EcpbriTixVTkS, iN9miiTiQr9C6, IOeHuiTiGoOjS, lDjT0iTiBH9Bd, oOrsQiTiA7N0u, QBc3UiTitqvaT, SWqVniTixSi42, tHUnMiTikaIIL, y9y6biTidLSDu, YtJcdiTiDvWGS, YVWDNiTi5c584) with overly long RTs (at least 2 minutes long) for at least one trial.

This leaves us with a total of 406 participants between our 10 Experimental Conditions, which is still more than we thought we'd have access to.

##Removing Outliers

We have removed data for participants under a number of conditions, but what about outliers, especially for things like RT - how do we want to handle those?

The best way we can get some estimate of how they are likely to affect our data is by looking at some histograms of response time and maybe qq plots to get some idea of their normality.

####Plotting RT Distributions

```r
ggplot(CleanData2, aes(RT)) +
  geom_density() #+
```

![](Stekic_et_al-_Data_Analysis_files/figure-html/RT Histograms-1.png)<!-- -->

```r
  #xlim(0, 40000)
```

So it doesn't actually look like we have a lot of outliers, but this doesn't mean they won't have an effect on the data. 

As a first bash we'll use 1.5x the interquartile range as our cutoff. We can reconsider this as needed

####Removing outliers and replotting

```r
#Calculating the interquartile range

lowerquart <- quantile(CleanData2$RT)[2]
upperquart <- quantile(CleanData2$RT)[4]

Interquartile <- upperquart - lowerquart

#Calculating thresholds
#Mild thresholds are 1.5* interquartile range
mild.low <- lowerquart - (Interquartile * 1.5)
mild.high <- upperquart + (Interquartile * 1.5)

#Extremes are 3* interquartile range
extreme.low <- lowerquart - (Interquartile * 3)
extreme.high <- upperquart + (Interquartile * 3)


#1- Removing all Outliers ()
CleanData.RTTrim1 <- subset(CleanData2, RT > mild.low & RT < mild.high)

ggplot(CleanData.RTTrim1, aes(RT)) +
  geom_density() +
  ggtitle("Density Plot of RTs- All Outliers Removed")
```

![](Stekic_et_al-_Data_Analysis_files/figure-html/Removing RT outliers and replotting-1.png)<!-- -->

```r
#2- Replacing all Outliers with the Mean
CleanData.RTTrim2 <- CleanData2

#Compute non-outlier means
NOMean1 <- mean(CleanData.RTTrim1$RT)

CleanData.RTTrim2$RT <- ifelse(CleanData.RTTrim2$RT < mild.low,
                               NOMean1,
                               CleanData.RTTrim2$RT)

CleanData.RTTrim2$RT <- ifelse(CleanData.RTTrim2$RT > mild.high,
                               NOMean1,
                               CleanData.RTTrim2$RT)


ggplot(CleanData.RTTrim2, aes(RT)) +
  geom_density() +
  ggtitle("Density Plot of RTs- All Outliers Replaced with Mean")
```

![](Stekic_et_al-_Data_Analysis_files/figure-html/Removing RT outliers and replotting-2.png)<!-- -->

```r
#3- Removing only extreme outliers

CleanData.RTTrim3 <- subset(CleanData2, RT > extreme.low & RT < extreme.high)

ggplot(CleanData.RTTrim3, aes(RT)) +
  geom_density() +
  ggtitle("Density Plot of RTs- Extreme Outliers Removed")
```

![](Stekic_et_al-_Data_Analysis_files/figure-html/Removing RT outliers and replotting-3.png)<!-- -->

```r
#4- Setting outliers to the most extreme of the minimum outlier values

CleanData.RTTrim4 <- CleanData2

CleanData.RTTrim4$RT <- ifelse(CleanData.RTTrim4$RT < mild.low,
                               mild.low,
                               CleanData.RTTrim4$RT)

CleanData.RTTrim4$RT <- ifelse(CleanData.RTTrim4$RT > mild.high,
                               mild.high,
                               CleanData.RTTrim4$RT)

ggplot(CleanData.RTTrim4, aes(RT)) +
  geom_density() +
  ggtitle("Density Plot of RTs- Outliers Trimmed to Mild Outlier Boundary")
```

![](Stekic_et_al-_Data_Analysis_files/figure-html/Removing RT outliers and replotting-4.png)<!-- -->

```r
#5- Replacing all outliers with the mean on a by-subject basis

participantIDs <- unique(CleanData2$ParticipantID)

participantdata <- list()
CleanData.RTTrim5 <- list()

for (participant in participantIDs) {
  
  participantdata <- subset(CleanData2, ParticipantID == participant)
  
  #Get Non-Outlier Mean
  participantdataNO <- subset(participantdata, RT > mild.low & RT < mild.high)
  NOMean2 <- mean(participantdataNO$RT)
  
  

  participantdata$RT <- ifelse(participantdata$RT < mild.low,
                               NOMean2,
                               participantdata$RT)

  participantdata$RT <- ifelse(participantdata$RT > mild.high,
                                NOMean2,
                               participantdata$RT)
  

  
  CleanData.RTTrim5 <- rbind(CleanData.RTTrim5, participantdata)

}


ggplot(CleanData.RTTrim5, aes(RT)) +
  geom_density() +
  ggtitle("Density Plot of RTs- Outliers Replaced with Mean (By Subject)")
```

![](Stekic_et_al-_Data_Analysis_files/figure-html/Removing RT outliers and replotting-5.png)<!-- -->

```r
CleanData3 <- CleanData.RTTrim5
```

Above I output five graphs, which are informatively titled so that it should be clear what we've done

I think that the final option is probably the best one, although we'll likely find that no matter what data we run with won't make much of a statistical difference


####Splitting testing trials into new vs. old items (test of generalisation)

```r
#Splitting Testing Trials
participantIDs <- unique(CleanData3$ParticipantID)
participantdata <- list()
participantdata.training <- list()
participantdata.testing <- list()

testingtrials <- list()
trainingtrials <- list()


for (participant in participantIDs) {
  
  participantdata <- subset(CleanData3, ParticipantID == participant)
  participantdata.training <- subset(participantdata, TrialType == "Training")
  participantdata.testing <- subset(participantdata, TrialType == "Testing")
  
  
  TrainingFigures <- unique(participantdata.training$Image)
  
  participantdata.training$Generalisation <- NA
  
  participantdata.testing$Generalisation <- ifelse(
    participantdata.testing$Image %in% TrainingFigures,
    "Old",
    "New")
  
  trainingtrials <- rbind(trainingtrials, participantdata.training)
  testingtrials <- rbind(testingtrials, participantdata.testing)
  
  
}

CleanData4 <- rbind(trainingtrials, testingtrials)


#Factor levels, adding Trial Type 2

CleanData4$TrialType <- factor(CleanData4$TrialType, level = c ("Training", "Testing"))
CleanData4$TrialType2 <- paste(CleanData4$TrialType, CleanData4$Generalisation, sep = "-")
CleanData4$Generalisation <- as.factor(CleanData4$Generalisation)

CleanData4$TrialType2 <- factor(CleanData4$TrialType2, level = c ("Training-NA", "Testing-Old", "Testing-New"),
                                     labels = c("Training", "Testing-Old", "Testing-New"))


# Obtaining the curviness of images from our original script

CleanData4$Image <- sub("Stims/Figures/", "", CleanData4$Image)
CleanData4$Image <- sub(".bmp", "", CleanData4$Image)

CleanData4 <- separate(CleanData4, col=Image, into = c("ImageSeed", "JaggedvsCurved", "Curviness", "Set"), sep = "-")
```

Our experiment is split into Training Trials and Testing Trials, and that is already in the data frame, but there areafew additional differences required.

The first of these is splitting up our testing trials so we can look at Generalisation. Testing trials in this experiment use both the initial set of 16 stimuli per participant and also introduce 8 novel exemplars to test the ability of participants to generalise the rules that they have learned 

The second of these is adding in information about how curvy vs. jagged the various images used are.

Recall that our images were generated along a range of "Curviness" from 0 (Very Jagged) to 30 (Very Curvy) with 10 (somewhat jagged) and 20 (somewhat curvy) also used

We thus used 4 possible pairings of curviness:

0 vs 30
10 vs 20
0 vs 20
10 vs 30

Which means there may be many ways to look at this data.

This is actually already coded in the currently filename structure- so we just need to expand it out into separate columns

#Data Analysis- A First Look

```r
#Name the levels of Condition, rather than having them be numbers
ReplicationData2007 <- subset(CleanData4, Condition == 10|Condition == "3A"|Condition == "3B")

ReplicationData2007$Condition <- factor(ReplicationData2007$Condition,
                            levels= c("3A", "3B", 10),
                            labels = c("Conventional Category","Conventional Category","No Label"))
#There are 2 subconditions here - 3A and 3B - we're going to ignore thedifference for now

Rep2007Agg1 <- aggregate(RespCorr ~ Condition + TrialType + Block , data=ReplicationData2007, mean, na.rm= FALSE)

Rep2007Agg1$Block <- factor(Rep2007Agg1$Block)
```

That was a lot of work to get us here, but we're ready (in theory) to start looking at our data.

We already know because of the various crossings of the data that we're going to have a very hard time getting any models to converge- even though we have a lot of data our design isn't fully crossed and specifying things can be very difficult - I've previously attempted to get those models to converge and had no luck, so instead we'll go about this a little bit more systematically.

##Replication of Lupyan et al. (2007)

The first thing we can do in a fairly straightforward fashion is see how well we have replicated the results of the papers we are extending. The first of these is Lupyan et al. (2007): [Language is Not Just for Talking](https://www.ncbi.nlm.nih.gov/pubmed/18031415).

In this paper, from which we took our basic design of Training vs. Testing Trials, Lupyan et al. compared the learnability of categories with no label to those with a single label for each category. These are both included in our design as well, as No Label (Condition 10) and Conventional Category Label (Condition 3)

###Visualisation and Eyeballing
So lets take a look at our data


```r
ggplot(data=Rep2007Agg1, aes(x=Block, y=RespCorr, group= Condition)) +
  #geom_line(aes(color= Condition)) +
  #geom_point(size=1.75, aes(colour = Condition)) +
  geom_smooth(method='loess', formula= y ~ x, se= FALSE, aes(linetype = Condition)) +
 # scale_linetype_manual(values = c("solid", "solid", "solid",
  #                        "longdash", "longdash", "longdash", "dotdash",
  #                        "dotted")) +
  scale_color_manual(values= c("#0066CC", "#CC0033","#33FF00", "#0066CC", "#CC0033","#33FF00", "#33FF00", "#000000")) +
  labs(x="Block", y="Proportion of Correct Responses") +
  scale_y_continuous(limits = c(0.45,1), breaks=c(0.5,0.6,0.7,0.8,0.9,1.0)) +
  facet_grid(~TrialType, scales="free", space= "free_x") +
  theme_tufte()+
  ggtitle("Replication Performance split by Trial Type")
```

![](Stekic_et_al-_Data_Analysis_files/figure-html/Lupyan Replication Graph 1-1.png)<!-- -->


These are interesting results - Having a label is helpful, but when participants roll around to the testing phase their performance falls off a cliff (and doesn't recover), suggesting that maybe they are relying too much on the words and haven't learned very much about the image differences

For best comparison, lets plot this with Lupyan et al's data

####Plotting with data from Lupyan et al 2007

```r
#Reading in Gary's Data
Lupyan2007 <- read.csv("C:/Users/Alan/Documents/GitHub/Stekic-et-al/RScripts/Lupyan2007Data.csv")

Rep2007Agg2 <- Rep2007Agg1

#Adding a "study" column to our data
Rep2007Agg2$Study <- "Stekic et al (2019)"

#Combining the 2 dataframes into 1
CombinedData <- rbind(Rep2007Agg2, Lupyan2007)

CombinedData$Group <- paste(CombinedData$Study, CombinedData$Condition)

#Plotting the Data
ggplot(data=CombinedData, aes(x=Block, y=RespCorr, group= Group)) +
  #geom_line(aes(color= Condition)) +
  #geom_point(size=1.75, aes(colour = Condition)) +
  geom_smooth(method='loess', formula= y ~ x, se= FALSE, aes(linetype = Condition, colour= Study)) +
 # scale_linetype_manual(values = c("solid", "solid", "solid",
  #                        "longdash", "longdash", "longdash", "dotdash",
  #                        "dotted")) +
  scale_color_manual(values= c("#0066CC", "#CC0033","#33FF00", "#0066CC", "#CC0033","#33FF00", "#33FF00", "#000000")) +
  labs(x="Block", y="Proportion of Correct Responses") +
  scale_y_continuous(limits = c(0.45,1), breaks=c(0.5,0.6,0.7,0.8,0.9,1.0)) +
  facet_grid(~TrialType, scales="free", space= "free_x") +
  theme_tufte()+
  ggtitle("Replication Performance split by Trial Type, comparing our data to Lupyan et al. (2007)")
```

![](Stekic_et_al-_Data_Analysis_files/figure-html/Plotting with Lupyan 2007-1.png)<!-- -->

So some very intersting similarities and some interesting differences as well

Our no-label condition (dashed red line) is harder than Lupyan et al. (2007) by around 10% across the board, but the general trajectory is bang on, including the fact that performance peaks at the end of training and is basically maintained in testing trials

Our Conventional Category Label condition is almost identical in Training to Lupyan et al, but is *totally* different in testing - around a 20% difference with testing performance falling off a cliff for our participants, suggesting that in our experiment they might not be learning very much about the image types at all, and instead learning *only* the words, such that they can't generalise when given new stimuli

####Exploring the Test Trial Failure

```r
Rep2007Agg3 <- aggregate(RespCorr ~ Condition + TrialType2 + Block , data=ReplicationData2007, mean, na.rm= FALSE)

Rep2007Agg3$Block <- factor(Rep2007Agg3$Block)

ggplot(data=Rep2007Agg3, aes(x=Block, y=RespCorr, group= Condition)) +
  #geom_line(aes(color= Condition)) +
  #geom_point(size=1.75, aes(colour = Condition)) +
  geom_smooth(method='loess', formula= y ~ x, se= FALSE, aes(linetype = Condition)) +
 # scale_linetype_manual(values = c("solid", "solid", "solid",
  #                        "longdash", "longdash", "longdash", "dotdash",
  #                        "dotted")) +
  scale_color_manual(values= c("#0066CC", "#CC0033","#33FF00", "#0066CC", "#CC0033","#33FF00", "#33FF00", "#000000")) +
  labs(x="Block", y="Proportion of Correct Responses") +
  scale_y_continuous(limits = c(0.45,1), breaks=c(0.5,0.6,0.7,0.8,0.9,1.0)) +
  facet_grid(~TrialType2, scales="free", space= "free_x") +
  theme_tufte() +
  ggtitle("Replication Performance split by Trial Type")
```

![](Stekic_et_al-_Data_Analysis_files/figure-html/Looking at Test Trials-1.png)<!-- -->

This interpretation is supported by breaking apart the data and looking at the two types of testing trials separately.

For participants in the no-label condition, Testing Trials differ from Training Trials only in the lack of feedback given and the introduction of new exemplars to generalise to. Unsurprisingly, they find Old (familiar) images easier than new images which they have to generalise to (which they get better at).

Participants in the Conventional Category label condition however don't show this- performance on all testing trials falls off the cliff and there are no differences between familiar and new images - this suggests that they are actually learning very little about the image categories.

####Stimuli Extremeness

```r
#Aggregating CleanData4 to include this information
ExtremeData <- subset(CleanData4, Condition == 10|Condition == "3A"|Condition == "3B")

ExtremeData$Condition <- factor(ExtremeData$Condition,
                            levels= c("3A", "3B", 10),
                            labels = c("Conventional Category","Conventional Category","No Label"))


ExtremeAgg1 <- aggregate(RespCorr ~ Condition + TrialType2 + Block + Curviness  , data=ExtremeData, mean, na.rm= FALSE)

ExtremeAgg1$Group <- paste(ExtremeAgg1$Condition, ExtremeAgg1$Curviness, sep= "-")
ExtremeAgg1$Block <- factor(ExtremeAgg1$Block)

#Plotting
ggplot(data=ExtremeAgg1, aes(x=Block, y=RespCorr, group= Group)) +
  #geom_line(aes(color= Condition)) +
  #geom_point(size=1.75, aes(colour = Condition)) +
  geom_smooth(method='loess', formula= y ~ x, se= FALSE, aes(linetype = Condition, color= Curviness)) +
 # scale_linetype_manual(values = c("solid", "solid", "solid",
  #                        "longdash", "longdash", "longdash", "dotdash",
  #                        "dotted")) +
  scale_color_manual(values= c("#0066CC", "#CC0033","#33FF00", "#000000")) +
  labs(x="Block", y="Proportion of Correct Responses") +
  scale_y_continuous(limits = c(0.45,1), breaks=c(0.5,0.6,0.7,0.8,0.9,1.0)) +
  facet_grid(~TrialType2, scales="free", space= "free_x") +
  theme_tufte() +
  ggtitle("Replication Performance split by Trial Type and Image Curviness")
```

![](Stekic_et_al-_Data_Analysis_files/figure-html/Exploring the effect of stimuli extremeness-1.png)<!-- -->

```r
ExtremeAgg2 <- aggregate(RespCorr ~ Condition + TrialType2 + Block + Set  , data=ExtremeData, mean, na.rm= FALSE)

ExtremeAgg2$Group <- paste(ExtremeAgg2$Condition, ExtremeAgg2$Set, sep= "-")
ExtremeAgg2$Block <- factor(ExtremeAgg2$Block)
ExtremeAgg2$Set <- factor(ExtremeAgg2$Set)


#Plotting
ggplot(data=ExtremeAgg2, aes(x=Block, y=RespCorr, group= Group)) +
  #geom_line(aes(color= Condition)) +
  #geom_point(size=1.75, aes(colour = Condition)) +
  geom_smooth(method='loess', formula= y ~ x, se= FALSE, aes(linetype = Condition, color= Set)) +
 # scale_linetype_manual(values = c("solid", "solid", "solid",
  #                        "longdash", "longdash", "longdash", "dotdash",
  #                        "dotted")) +
  scale_color_manual(values= c("#0066CC", "#CC0033","#33FF00", "#000000")) +
  labs(x="Block", y="Proportion of Correct Responses") +
  scale_y_continuous(limits = c(0.45,1), breaks=c(0.5,0.6,0.7,0.8,0.9,1.0)) +
  facet_grid(~TrialType2, scales="free", space= "free_x") +
  theme_tufte() +
  ggtitle("Replication Performance split by Trial Type and Image Set")
```

![](Stekic_et_al-_Data_Analysis_files/figure-html/Exploring the effect of stimuli extremeness-2.png)<!-- -->

The above graph breaks our data down by image extremeness- recall that on a curviness scale images vary from 0 (very jagged) to 30 (very curvy).

![Examples of stimuli varying from 0 (Jagged) to 30 (Curvy)](exemplars.png)

There are two ways we can look at this- and both are above. The first graph we look only at the individual images, as they are seen by participants. i.e. on every trial what is the curviness (defined by the values of 0,10,20, and 30) of the image. Looking at it this way, it appears that when a label is present there is very little if any difference between the performance on various types of images (especially during training trials). Without a label however, there appear to be pretty clear differences between the image types- participants find the very jagged images the easiest, with everything else clustering together fairly closely.

The second way to look at this is by "Image Set". From our four possible curvinesses of image we selected pairs from each image seed (a row of images in Figure X created from the same starting point). Thus no participant saw all variations of a single image seed - instead they saw a pair of images from that seed that was either:

Set 1: Jagged = 0, Curvy = 30
Set 2: Jagged = 0, Curvy = 20
Set 3: Jagged = 10, Curvy = 30
Set 4: Jagged = 10, Curvy = 20

Looking at things this second way makes pretty clear that the first approach is better- Set 1 and Set 2 images perform best, and (unsurprisingly) these are the sets with the high-performing most jagged images.

###Replication Statistics

What interesting things seem to be going on in our replication data?

A full model of this data from Condition 3 (Conventional Category Label) and Condition 10 (No Label) 












