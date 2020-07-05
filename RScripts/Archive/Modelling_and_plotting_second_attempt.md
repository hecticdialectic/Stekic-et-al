---
title: "Stekic et al."
subtitle: 'Data modeling and plotting'
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
```

```
## -- Attaching packages ---------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 3.1.0       v purrr   0.3.1  
## v tibble  2.0.1       v dplyr   0.8.0.1
## v tidyr   0.8.3       v stringr 1.4.0  
## v readr   1.3.1       v forcats 0.4.0
```

```
## -- Conflicts ------------------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::between()   masks data.table::between()
## x dplyr::filter()    masks stats::filter()
## x dplyr::first()     masks data.table::first()
## x dplyr::lag()       masks stats::lag()
## x dplyr::last()      masks data.table::last()
## x purrr::transpose() masks data.table::transpose()
```

```r
library(ggthemes)
library(outliers)
library(lme4)
```

```
## Loading required package: Matrix
```

```
## 
## Attaching package: 'Matrix'
```

```
## The following object is masked from 'package:tidyr':
## 
##     expand
```

```r
library(lmerTest)
```

```
## 
## Attaching package: 'lmerTest'
```

```
## The following object is masked from 'package:lme4':
## 
##     lmer
```

```
## The following object is masked from 'package:stats':
## 
##     step
```

```r
library(afex)
```

```
## ************
## Welcome to afex. For support visit: http://afex.singmann.science/
```

```
## - Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()
## - Methods for calculating p-values with mixed(): 'KR', 'S', 'LRT', and 'PB'
## - 'afex_aov' and 'mixed' objects can be passed to emmeans() for follow-up tests
## - NEWS: library('emmeans') now needs to be called explicitly!
## - Get and set global package options with: afex_options()
## - Set orthogonal sum-to-zero contrasts globally: set_sum_contrasts()
## - For example analyses see: browseVignettes("afex")
## ************
```

```
## 
## Attaching package: 'afex'
```

```
## The following object is masked from 'package:lme4':
## 
##     lmer
```

```r
library(kableExtra)
```

```
## 
## Attaching package: 'kableExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     group_rows
```

```r
library(agricolae)
library(multcomp)
```

```
## Loading required package: mvtnorm
```

```
## Loading required package: survival
```

```
## Loading required package: TH.data
```

```
## Loading required package: MASS
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```
## 
## Attaching package: 'TH.data'
```

```
## The following object is masked from 'package:MASS':
## 
##     geyser
```

####Modeling Correctness Trainging data with Condition as fixed factor

```r
DataNew <- read.csv("C:/Users/775/Documents/GitHub/Stekic-et-al/Data/CleanData.csv")

DataNew$ParticipantID <- as.factor(DataNew$ParticipantID)
DataNew$Condition <- as.factor(DataNew$Condition)

DataSubset <- subset(DataNew, Condition == "1"|Condition == "2"|Condition == "3A"|Condition == "4"|Condition == "10"| Condition == "5"| 
                       Condition == "6A"|Condition == "7"|Condition == "8"|Condition == "9")

DataTraining <- subset(DataSubset, TrialType == "Training")

Model.Ress.Corr.Training <- glmer(RespCorr ~ Condition +
                          (1|ParticipantID),
                          data=DataTraining, 
                          family= binomial)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00105959
## (tol = 0.001, component 1)
```

```r
summary(Model.Ress.Corr.Training)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: RespCorr ~ Condition + (1 | ParticipantID)
##    Data: DataTraining
## 
##      AIC      BIC   logLik deviance df.resid 
##  46941.4  47038.2 -23459.7  46919.4    49237 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -7.7345 -0.8541  0.2847  0.6136  1.2217 
## 
## Random effects:
##  Groups        Name        Variance Std.Dev.
##  ParticipantID (Intercept) 1.067    1.033   
## Number of obs: 49248, groups:  ParticipantID, 342
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  2.53694    0.18465  13.740  < 2e-16 ***
## Condition10 -1.82735    0.26049  -7.015 2.30e-12 ***
## Condition2  -0.12117    0.26293  -0.461 0.644904    
## Condition3A  0.09587    0.26379   0.363 0.716279    
## Condition4  -0.89316    0.25645  -3.483 0.000496 ***
## Condition5  -0.87246    0.26549  -3.286 0.001016 ** 
## Condition6A -0.90336    0.25926  -3.484 0.000493 ***
## Condition7  -2.11190    0.25448  -8.299  < 2e-16 ***
## Condition8  -2.06071    0.25961  -7.938 2.06e-15 ***
## Condition9  -2.12201    0.26026  -8.153 3.54e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) Cndt10 Cndtn2 Cndt3A Cndtn4 Cndtn5 Cndt6A Cndtn7 Cndtn8
## Condition10 -0.711                                                        
## Condition2  -0.699  0.497                                                 
## Condition3A -0.701  0.498  0.492                                          
## Condition4  -0.718  0.511  0.503  0.504                                   
## Condition5  -0.694  0.494  0.486  0.487  0.499                            
## Condition6A -0.716  0.509  0.501  0.502  0.515  0.497                     
## Condition7  -0.727  0.516  0.508  0.509  0.522  0.505  0.520              
## Condition8  -0.711  0.505  0.497  0.498  0.511  0.494  0.509  0.517       
## Condition9  -0.712  0.506  0.498  0.499  0.511  0.494  0.510  0.517  0.506
## convergence code: 0
## Model failed to converge with max|grad| = 0.00105959 (tol = 0.001, component 1)
```

```r
anova(Model.Ress.Corr.Training)
```

```
## Analysis of Variance Table
##           Df Sum Sq Mean Sq F value
## Condition  9 216.49  24.055  24.055
```

```r
summary(glht(Model.Ress.Corr.Training, linfct = mcp(Condition = "Tukey")), test = adjusted("holm"))
```

```
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: glmer(formula = RespCorr ~ Condition + (1 | ParticipantID), data = DataTraining, 
##     family = binomial)
## 
## Linear Hypotheses:
##              Estimate Std. Error z value Pr(>|z|)    
## 10 - 1 == 0  -1.82735    0.26049  -7.015 8.05e-11 ***
## 2 - 1 == 0   -0.12117    0.26293  -0.461 1.000000    
## 3A - 1 == 0   0.09587    0.26379   0.363 1.000000    
## 4 - 1 == 0   -0.89316    0.25645  -3.483 0.008879 ** 
## 5 - 1 == 0   -0.87246    0.26549  -3.286 0.016249 *  
## 6A - 1 == 0  -0.90336    0.25926  -3.484 0.008879 ** 
## 7 - 1 == 0   -2.11190    0.25448  -8.299  < 2e-16 ***
## 8 - 1 == 0   -2.06071    0.25961  -7.938 7.99e-14 ***
## 9 - 1 == 0   -2.12201    0.26026  -8.153 1.82e-14 ***
## 2 - 10 == 0   1.70618    0.26247   6.501 2.72e-09 ***
## 3A - 10 == 0  1.92322    0.26261   7.324 8.70e-12 ***
## 4 - 10 == 0   0.93420    0.25574   3.653 0.005706 ** 
## 5 - 10 == 0   0.95490    0.26471   3.607 0.006361 ** 
## 6A - 10 == 0  0.92399    0.25760   3.587 0.006361 ** 
## 7 - 10 == 0  -0.28455    0.25329  -1.123 1.000000    
## 8 - 10 == 0  -0.23336    0.25869  -0.902 1.000000    
## 9 - 10 == 0  -0.29466    0.25883  -1.138 1.000000    
## 3A - 2 == 0   0.21704    0.26552   0.817 1.000000    
## 4 - 2 == 0   -0.77198    0.25894  -2.981 0.040620 *  
## 5 - 2 == 0   -0.75128    0.26789  -2.804 0.065532 .  
## 6A - 2 == 0  -0.78219    0.26081  -2.999 0.040620 *  
## 7 - 2 == 0   -1.99073    0.25668  -7.756 3.46e-13 ***
## 8 - 2 == 0   -1.93954    0.26201  -7.403 4.94e-12 ***
## 9 - 2 == 0   -2.00084    0.26215  -7.632 8.78e-13 ***
## 4 - 3A == 0  -0.98903    0.25905  -3.818 0.003096 ** 
## 5 - 3A == 0  -0.96833    0.26803  -3.613 0.006361 ** 
## 6A - 3A == 0 -0.99924    0.26092  -3.830 0.003080 ** 
## 7 - 3A == 0  -2.20778    0.25684  -8.596  < 2e-16 ***
## 8 - 3A == 0  -2.15658    0.26216  -8.226 9.33e-15 ***
## 9 - 3A == 0  -2.21789    0.26230  -8.456  < 2e-16 ***
## 5 - 4 == 0    0.02070    0.26133   0.079 1.000000    
## 6A - 4 == 0  -0.01021    0.25410  -0.040 1.000000    
## 7 - 4 == 0   -1.21875    0.24980  -4.879 3.52e-05 ***
## 8 - 4 == 0   -1.16756    0.25527  -4.574 0.000129 ***
## 9 - 4 == 0   -1.22886    0.25541  -4.811 4.80e-05 ***
## 6A - 5 == 0  -0.03091    0.26316  -0.117 1.000000    
## 7 - 5 == 0   -1.23945    0.25896  -4.786 5.10e-05 ***
## 8 - 5 == 0   -1.18826    0.26425  -4.497 0.000176 ***
## 9 - 5 == 0   -1.24956    0.26438  -4.726 6.40e-05 ***
## 7 - 6A == 0  -1.20854    0.25169  -4.802 4.88e-05 ***
## 8 - 6A == 0  -1.15735    0.25713  -4.501 0.000176 ***
## 9 - 6A == 0  -1.21865    0.25726  -4.737 6.29e-05 ***
## 8 - 7 == 0    0.05119    0.25281   0.202 1.000000    
## 9 - 7 == 0   -0.01011    0.25295  -0.040 1.000000    
## 9 - 8 == 0   -0.06130    0.25836  -0.237 1.000000    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- holm method)
```

####Modeling Correctness Test data with Condition as fixed factor

```r
DataNew <- read.csv("C:/Users/775/Documents/GitHub/Stekic-et-al/Data/CleanData.csv")

DataNew$ParticipantID <- as.factor(DataNew$ParticipantID)
DataNew$Condition <- as.factor(DataNew$Condition)

DataSubset <- subset(DataNew, Condition == "1"|Condition == "2"|Condition == "3A"|Condition == "4"|Condition == "10"| Condition == "5"| 
                       Condition == "6A"|Condition == "7"|Condition == "8"|Condition == "9")

DataTest <- subset(DataSubset, TrialType == "Testing")


Model.Ress.Corr.Testing <- glmer(RespCorr ~ Condition +
                          (1|ParticipantID),
                          data=DataTest, 
                          family= binomial)

summary(Model.Ress.Corr.Testing)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: RespCorr ~ Condition + (1 | ParticipantID)
##    Data: DataTest
## 
##      AIC      BIC   logLik deviance df.resid 
##  34901.6  34994.0 -17439.8  34879.6    32821 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.5353 -0.9108  0.2963  0.7395  3.7521 
## 
## Random effects:
##  Groups        Name        Variance Std.Dev.
##  ParticipantID (Intercept) 1.685    1.298   
## Number of obs: 32832, groups:  ParticipantID, 342
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   1.3440     0.2249   5.977 2.27e-09 ***
## Condition10   0.3946     0.3263   1.210   0.2264    
## Condition2   -0.1731     0.3213  -0.539   0.5901    
## Condition3A  -0.6323     0.3175  -1.991   0.0464 *  
## Condition4   -0.1841     0.3151  -0.584   0.5592    
## Condition5   -0.1498     0.3291  -0.455   0.6488    
## Condition6A  -0.6104     0.3179  -1.920   0.0548 .  
## Condition7   -0.5428     0.3157  -1.719   0.0855 .  
## Condition8   -0.3302     0.3237  -1.020   0.3077    
## Condition9   -0.6397     0.3221  -1.986   0.0470 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) Cndt10 Cndtn2 Cndt3A Cndtn4 Cndtn5 Cndt6A Cndtn7 Cndtn8
## Condition10 -0.690                                                        
## Condition2  -0.702  0.484                                                 
## Condition3A -0.708  0.489  0.497                                          
## Condition4  -0.711  0.491  0.499  0.504                                   
## Condition5  -0.685  0.473  0.481  0.485  0.487                            
## Condition6A -0.708  0.489  0.497  0.502  0.504  0.485                     
## Condition7  -0.713  0.492  0.501  0.505  0.507  0.489  0.505              
## Condition8  -0.695  0.480  0.488  0.493  0.494  0.476  0.493  0.496       
## Condition9  -0.698  0.482  0.490  0.495  0.496  0.478  0.495  0.498  0.485
```

```r
anova(Model.Ress.Corr.Testing)
```

```
## Analysis of Variance Table
##           Df Sum Sq Mean Sq F value
## Condition  9  18.97  2.1077  2.1077
```

```r
summary(glht(Model.Ress.Corr.Testing, linfct = mcp(Condition = "Tukey")), test = adjusted("holm"))
```

```
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: glmer(formula = RespCorr ~ Condition + (1 | ParticipantID), data = DataTest, 
##     family = binomial)
## 
## Linear Hypotheses:
##               Estimate Std. Error z value Pr(>|z|)  
## 10 - 1 == 0   0.394639   0.326255   1.210   1.0000  
## 2 - 1 == 0   -0.173083   0.321306  -0.539   1.0000  
## 3A - 1 == 0  -0.632287   0.317501  -1.991   1.0000  
## 4 - 1 == 0   -0.184056   0.315136  -0.584   1.0000  
## 5 - 1 == 0   -0.149845   0.329066  -0.455   1.0000  
## 6A - 1 == 0  -0.610434   0.317872  -1.920   1.0000  
## 7 - 1 == 0   -0.542836   0.315695  -1.719   1.0000  
## 8 - 1 == 0   -0.330184   0.323663  -1.020   1.0000  
## 9 - 1 == 0   -0.639743   0.322074  -1.986   1.0000  
## 2 - 10 == 0  -0.567722   0.328850  -1.726   1.0000  
## 3A - 10 == 0 -1.026925   0.325520  -3.155   0.0723 .
## 4 - 10 == 0  -0.578695   0.323839  -1.787   1.0000  
## 5 - 10 == 0  -0.544484   0.336526  -1.618   1.0000  
## 6A - 10 == 0 -1.005072   0.325719  -3.086   0.0873 .
## 7 - 10 == 0  -0.937475   0.323589  -2.897   0.1582  
## 8 - 10 == 0  -0.724823   0.331420  -2.187   1.0000  
## 9 - 10 == 0  -1.034382   0.330058  -3.134   0.0759 .
## 3A - 2 == 0  -0.459203   0.320300  -1.434   1.0000  
## 4 - 2 == 0   -0.010973   0.318611  -0.034   1.0000  
## 5 - 2 == 0    0.023238   0.331499   0.070   1.0000  
## 6A - 2 == 0  -0.437350   0.320510  -1.365   1.0000  
## 7 - 2 == 0   -0.369753   0.318329  -1.162   1.0000  
## 8 - 2 == 0   -0.157101   0.326337  -0.481   1.0000  
## 9 - 2 == 0   -0.466659   0.324896  -1.436   1.0000  
## 4 - 3A == 0   0.448231   0.315166   1.422   1.0000  
## 5 - 3A == 0   0.482442   0.328189   1.470   1.0000  
## 6A - 3A == 0  0.021853   0.317087   0.069   1.0000  
## 7 - 3A == 0   0.089451   0.314880   0.284   1.0000  
## 8 - 3A == 0   0.302102   0.322978   0.935   1.0000  
## 9 - 3A == 0  -0.007456   0.321516  -0.023   1.0000  
## 5 - 4 == 0    0.034211   0.326540   0.105   1.0000  
## 6A - 4 == 0  -0.426378   0.315379  -1.352   1.0000  
## 7 - 4 == 0   -0.358780   0.313166  -1.146   1.0000  
## 8 - 4 == 0   -0.146128   0.321295  -0.455   1.0000  
## 9 - 4 == 0   -0.455687   0.319838  -1.425   1.0000  
## 6A - 5 == 0  -0.460589   0.328395  -1.403   1.0000  
## 7 - 5 == 0   -0.392991   0.326268  -1.205   1.0000  
## 8 - 5 == 0   -0.180339   0.334080  -0.540   1.0000  
## 9 - 5 == 0   -0.489898   0.332678  -1.473   1.0000  
## 7 - 6A == 0   0.067598   0.315097   0.215   1.0000  
## 8 - 6A == 0   0.280249   0.323183   0.867   1.0000  
## 9 - 6A == 0  -0.029309   0.321728  -0.091   1.0000  
## 8 - 7 == 0    0.212652   0.321027   0.662   1.0000  
## 9 - 7 == 0   -0.096907   0.319553  -0.303   1.0000  
## 9 - 8 == 0   -0.309559   0.327540  -0.945   1.0000  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- holm method)
```

####Plotting Correctness for Training

```r
CleanData2 <- read.csv("C:/Users/775/Documents/GitHub/Stekic-et-al/Data/CleanData4.csv")

DataSubset <- subset(CleanData2, Condition == "1"|Condition == "2"|Condition == "3A"|Condition == "4"|Condition == "10"| Condition == "5"| 
                       Condition == "6A"|Condition == "7"|Condition == "8"|Condition == "9")

DataTraining <- subset(DataSubset, TrialType == "Training")

ggplot(DataTraining, aes(x= Block, y= RespCorr, colour= Condition, linetype = Condition)) + 
  geom_smooth(aes(colour = Condition),size = 1.0,se = TRUE, method= 'loess', formula =  y~x)+
  
    scale_linetype_manual(values = c("solid", "dotdash", "dotted",
                                    "solid", "dotdash", "dotted",
                                    "solid", "dotdash", "dotted","longdash")) +
  
  scale_color_manual(values= c("#0066CC", "#000000", "#0066CC", "#0066CC",
                               "#33FF00", "#33FF00", "#33FF00", 
                               "#CC0033", "#CC0033", "#CC0033")) +
  
                               
  ggtitle("Training Performance") +
  labs(x="Block", y="Proportion of Correctness") +
  theme(axis.title.y = element_text(size=12,  color="#666666")) +
  theme(axis.text = element_text(size=8)) +
  theme(plot.title = element_text(size=16, face="bold", hjust=0, color="#666666")) +
  theme(strip.text.x = element_text(size = 8, colour = "black"))
```

![](Modelling_and_plotting_second_attempt_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

####Plotting Correctness for Testing

```r
CleanData2 <- read.csv("C:/Users/775/Documents/GitHub/Stekic-et-al/Data/CleanData4.csv")

DataSubset <- subset(CleanData2, Condition == "1"|Condition == "2"|Condition == "3A"|Condition == "4"|Condition == "10"| Condition == "5"| 
                       Condition == "6A"|Condition == "7"|Condition == "8"|Condition == "9")

DataTesting <- subset(DataSubset, TrialType == "Testing")

ggplot(DataTesting, aes(x= Block, y= RespCorr, colour= Condition, linetype = Condition)) + 
  geom_smooth(aes(colour = Condition),size = 1.0,se = TRUE, method= 'loess', formula =  y~x)+
  
    scale_linetype_manual(values = c("solid", "dotdash", "dotted",
                                    "solid", "dotdash", "dotted",
                                    "solid", "dotdash", "dotted","longdash")) +
  
  scale_color_manual(values= c("#0066CC", "#000000", "#0066CC", "#0066CC",
                               "#33FF00", "#33FF00", "#33FF00", 
                               "#CC0033", "#CC0033", "#CC0033")) +
  
                               
  ggtitle("Test Performance") +
  labs(x="Block", y="Proportion of Correctness") +
  theme(axis.title.y = element_text(size=12,  color="#666666")) +
  theme(axis.text = element_text(size=8)) +
  theme(plot.title = element_text(size=16, face="bold", hjust=0, color="#666666")) +
  theme(strip.text.x = element_text(size = 8, colour = "black"))
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 6.705e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 6.705e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 4.1559e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 4.1559e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 2.8458e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 2.8458e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 1.322e-014
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 1.322e-014
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 6.705e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 6.705e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 4.6026e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 4.6026e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 1.322e-014
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 1.322e-014
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 6.705e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 6.705e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 4.1559e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 4.1559e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 4.1559e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 4.1559e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

![](Modelling_and_plotting_second_attempt_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

####Modeling Correctness Training data with Condition and Block as fixed factors

```r
DataNew <- read.csv("C:/Users/775/Documents/GitHub/Stekic-et-al/Data/CleanData.csv")

DataNew$ParticipantID <- as.factor(DataNew$ParticipantID)
DataNew$Condition <- as.factor(DataNew$Condition)

DataSubset <- subset(DataNew, Condition == "1"|Condition == "2"|Condition == "3A"|Condition == "4"|Condition == "10"| Condition == "5"| 
                       Condition == "6A"|Condition == "7"|Condition == "8"|Condition == "9")

DataTraining <- subset(DataSubset, TrialType == "Training")

Model.Ress.Corr.Training <- glmer(RespCorr ~ Condition + Block +
                          (1|ParticipantID),
                          data=DataTraining, 
                          family= binomial)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00149416
## (tol = 0.001, component 1)
```

```r
summary(Model.Ress.Corr.Training)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: RespCorr ~ Condition + Block + (1 | ParticipantID)
##    Data: DataTraining
## 
##      AIC      BIC   logLik deviance df.resid 
##  45797.9  45903.6 -22887.0  45773.9    49236 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.4309 -0.7142  0.2692  0.5724  1.6720 
## 
## Random effects:
##  Groups        Name        Variance Std.Dev.
##  ParticipantID (Intercept) 1.117    1.057   
## Number of obs: 49248, groups:  ParticipantID, 342
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.833890   0.187994   9.755  < 2e-16 ***
## Condition10 -1.867033   0.264498  -7.059 1.68e-12 ***
## Condition2  -0.123726   0.268066  -0.462 0.644402    
## Condition3A  0.096802   0.266983   0.363 0.716922    
## Condition4  -0.911360   0.261468  -3.486 0.000491 ***
## Condition5  -0.887007   0.270364  -3.281 0.001035 ** 
## Condition6A -0.920880   0.262584  -3.507 0.000453 ***
## Condition7  -2.159819   0.259164  -8.334  < 2e-16 ***
## Condition8  -2.106977   0.264919  -7.953 1.82e-15 ***
## Condition9  -2.169498   0.264268  -8.209 2.22e-16 ***
## Block        0.153000   0.004589  33.343  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) Cndt10 Cndtn2 Cndt3A Cndtn4 Cndtn5 Cndt6A Cndtn7 Cndtn8
## Condition10 -0.702                                                        
## Condition2  -0.693  0.492                                                 
## Condition3A -0.693  0.493  0.487                                          
## Condition4  -0.711  0.506  0.499  0.500                                   
## Condition5  -0.687  0.488  0.482  0.482  0.495                            
## Condition6A -0.707  0.503  0.496  0.497  0.509  0.492                     
## Condition7  -0.718  0.511  0.504  0.504  0.518  0.500  0.514              
## Condition8  -0.703  0.500  0.493  0.493  0.507  0.489  0.504  0.512       
## Condition9  -0.703  0.500  0.493  0.493  0.507  0.489  0.504  0.512  0.501
## Block       -0.103 -0.008  0.000  0.001 -0.004 -0.003 -0.003 -0.010 -0.010
##             Cndtn9
## Condition10       
## Condition2        
## Condition3A       
## Condition4        
## Condition5        
## Condition6A       
## Condition7        
## Condition8        
## Condition9        
## Block       -0.010
## convergence code: 0
## Model failed to converge with max|grad| = 0.00149416 (tol = 0.001, component 1)
```

```r
anova(Model.Ress.Corr.Training)
```

```
## Analysis of Variance Table
##           Df  Sum Sq Mean Sq  F value
## Condition  9  199.94   22.22   22.215
## Block      1 1111.60 1111.60 1111.599
```

```r
summary(glht(Model.Ress.Corr.Training, linfct = mcp(Condition = "Tukey")), test = adjusted("holm"))
```

```
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: glmer(formula = RespCorr ~ Condition + Block + (1 | ParticipantID), 
##     data = DataTraining, family = binomial)
## 
## Linear Hypotheses:
##               Estimate Std. Error z value Pr(>|z|)    
## 10 - 1 == 0  -1.867033   0.264498  -7.059 5.88e-11 ***
## 2 - 1 == 0   -0.123726   0.268066  -0.462 1.000000    
## 3A - 1 == 0   0.096802   0.266983   0.363 1.000000    
## 4 - 1 == 0   -0.911360   0.261468  -3.486 0.008349 ** 
## 5 - 1 == 0   -0.887007   0.270364  -3.281 0.016563 *  
## 6A - 1 == 0  -0.920880   0.262584  -3.507 0.008158 ** 
## 7 - 1 == 0   -2.159819   0.259164  -8.334  < 2e-16 ***
## 8 - 1 == 0   -2.106977   0.264919  -7.953 7.11e-14 ***
## 9 - 1 == 0   -2.169498   0.264268  -8.209 9.33e-15 ***
## 2 - 10 == 0   1.743307   0.268358   6.496 2.80e-09 ***
## 3A - 10 == 0  1.963835   0.267703   7.336 7.93e-12 ***
## 4 - 10 == 0   0.955673   0.261469   3.655 0.005658 ** 
## 5 - 10 == 0   0.980027   0.270541   3.622 0.006128 ** 
## 6A - 10 == 0  0.946153   0.262813   3.600 0.006362 ** 
## 7 - 10 == 0  -0.292786   0.258990  -1.130 1.000000    
## 8 - 10 == 0  -0.239944   0.264648  -0.907 1.000000    
## 9 - 10 == 0  -0.302465   0.264345  -1.144 1.000000    
## 3A - 2 == 0   0.220528   0.270996   0.814 1.000000    
## 4 - 2 == 0   -0.787634   0.265056  -2.972 0.041554 *  
## 5 - 2 == 0   -0.763280   0.274099  -2.785 0.069653 .  
## 6A - 2 == 0  -0.797154   0.266416  -2.992 0.041554 *  
## 7 - 2 == 0   -2.036093   0.262776  -7.748 3.64e-13 ***
## 8 - 2 == 0   -1.983250   0.268351  -7.391 5.41e-12 ***
## 9 - 2 == 0   -2.045772   0.268055  -7.632 8.78e-13 ***
## 4 - 3A == 0  -1.008162   0.264354  -3.814 0.003149 ** 
## 5 - 3A == 0  -0.983808   0.273445  -3.598 0.006362 ** 
## 6A - 3A == 0 -1.017682   0.265726  -3.830 0.003078 ** 
## 7 - 3A == 0  -2.256621   0.262113  -8.609  < 2e-16 ***
## 8 - 3A == 0  -2.203778   0.267702  -8.232 9.33e-15 ***
## 9 - 3A == 0  -2.266300   0.267405  -8.475  < 2e-16 ***
## 5 - 4 == 0    0.024354   0.267382   0.091 1.000000    
## 6A - 4 == 0  -0.009520   0.259530  -0.037 1.000000    
## 7 - 4 == 0   -1.248459   0.255726  -4.882 3.47e-05 ***
## 8 - 4 == 0   -1.195616   0.261454  -4.573 0.000130 ***
## 9 - 4 == 0   -1.258137   0.261147  -4.818 4.62e-05 ***
## 6A - 5 == 0  -0.033874   0.268701  -0.126 1.000000    
## 7 - 5 == 0   -1.272813   0.264989  -4.803 4.68e-05 ***
## 8 - 5 == 0   -1.219970   0.270522  -4.510 0.000166 ***
## 9 - 5 == 0   -1.282491   0.270225  -4.746 5.81e-05 ***
## 7 - 6A == 0  -1.238939   0.257098  -4.819 4.62e-05 ***
## 8 - 6A == 0  -1.186096   0.262796  -4.513 0.000166 ***
## 9 - 6A == 0  -1.248618   0.262491  -4.757 5.70e-05 ***
## 8 - 7 == 0    0.052843   0.258961   0.204 1.000000    
## 9 - 7 == 0   -0.009679   0.258651  -0.037 1.000000    
## 9 - 8 == 0   -0.062521   0.264317  -0.237 1.000000    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- holm method)
```

####Trying to model Correctness Testing data with Condition and Block as fixed factors

```r
DataNew <- read.csv("C:/Users/775/Documents/GitHub/Stekic-et-al/Data/CleanData.csv")

DataNew$ParticipantID <- as.factor(DataNew$ParticipantID)
DataNew$Condition <- as.factor(DataNew$Condition)

DataSubset <- subset(DataNew, Condition == "1"|Condition == "2"|Condition == "3A"|Condition == "4"|Condition == "10"| Condition == "5"| 
                       Condition == "6A"|Condition == "7"|Condition == "8"|Condition == "9")

DataTesting <- subset(DataSubset, TrialType == "Testing")

Model.Ress.Corr.Testing <- glmer(RespCorr ~ Condition + Block +
                          (1|ParticipantID),
                          data=DataTraining, 
                          family= binomial)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00149416
## (tol = 0.001, component 1)
```

```r
summary(Model.Ress.Corr.Testing)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: RespCorr ~ Condition + Block + (1 | ParticipantID)
##    Data: DataTraining
## 
##      AIC      BIC   logLik deviance df.resid 
##  45797.9  45903.6 -22887.0  45773.9    49236 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.4309 -0.7142  0.2692  0.5724  1.6720 
## 
## Random effects:
##  Groups        Name        Variance Std.Dev.
##  ParticipantID (Intercept) 1.117    1.057   
## Number of obs: 49248, groups:  ParticipantID, 342
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.833890   0.187994   9.755  < 2e-16 ***
## Condition10 -1.867033   0.264498  -7.059 1.68e-12 ***
## Condition2  -0.123726   0.268066  -0.462 0.644402    
## Condition3A  0.096802   0.266983   0.363 0.716922    
## Condition4  -0.911360   0.261468  -3.486 0.000491 ***
## Condition5  -0.887007   0.270364  -3.281 0.001035 ** 
## Condition6A -0.920880   0.262584  -3.507 0.000453 ***
## Condition7  -2.159819   0.259164  -8.334  < 2e-16 ***
## Condition8  -2.106977   0.264919  -7.953 1.82e-15 ***
## Condition9  -2.169498   0.264268  -8.209 2.22e-16 ***
## Block        0.153000   0.004589  33.343  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) Cndt10 Cndtn2 Cndt3A Cndtn4 Cndtn5 Cndt6A Cndtn7 Cndtn8
## Condition10 -0.702                                                        
## Condition2  -0.693  0.492                                                 
## Condition3A -0.693  0.493  0.487                                          
## Condition4  -0.711  0.506  0.499  0.500                                   
## Condition5  -0.687  0.488  0.482  0.482  0.495                            
## Condition6A -0.707  0.503  0.496  0.497  0.509  0.492                     
## Condition7  -0.718  0.511  0.504  0.504  0.518  0.500  0.514              
## Condition8  -0.703  0.500  0.493  0.493  0.507  0.489  0.504  0.512       
## Condition9  -0.703  0.500  0.493  0.493  0.507  0.489  0.504  0.512  0.501
## Block       -0.103 -0.008  0.000  0.001 -0.004 -0.003 -0.003 -0.010 -0.010
##             Cndtn9
## Condition10       
## Condition2        
## Condition3A       
## Condition4        
## Condition5        
## Condition6A       
## Condition7        
## Condition8        
## Condition9        
## Block       -0.010
## convergence code: 0
## Model failed to converge with max|grad| = 0.00149416 (tol = 0.001, component 1)
```

```r
anova(Model.Ress.Corr.Testing)
```

```
## Analysis of Variance Table
##           Df  Sum Sq Mean Sq  F value
## Condition  9  199.94   22.22   22.215
## Block      1 1111.60 1111.60 1111.599
```

```r
summary(glht(Model.Ress.Corr.Testing, linfct = mcp(Condition = "Tukey")), test = adjusted("holm"))
```

```
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: glmer(formula = RespCorr ~ Condition + Block + (1 | ParticipantID), 
##     data = DataTraining, family = binomial)
## 
## Linear Hypotheses:
##               Estimate Std. Error z value Pr(>|z|)    
## 10 - 1 == 0  -1.867033   0.264498  -7.059 5.88e-11 ***
## 2 - 1 == 0   -0.123726   0.268066  -0.462 1.000000    
## 3A - 1 == 0   0.096802   0.266983   0.363 1.000000    
## 4 - 1 == 0   -0.911360   0.261468  -3.486 0.008349 ** 
## 5 - 1 == 0   -0.887007   0.270364  -3.281 0.016563 *  
## 6A - 1 == 0  -0.920880   0.262584  -3.507 0.008158 ** 
## 7 - 1 == 0   -2.159819   0.259164  -8.334  < 2e-16 ***
## 8 - 1 == 0   -2.106977   0.264919  -7.953 7.11e-14 ***
## 9 - 1 == 0   -2.169498   0.264268  -8.209 9.33e-15 ***
## 2 - 10 == 0   1.743307   0.268358   6.496 2.80e-09 ***
## 3A - 10 == 0  1.963835   0.267703   7.336 7.93e-12 ***
## 4 - 10 == 0   0.955673   0.261469   3.655 0.005658 ** 
## 5 - 10 == 0   0.980027   0.270541   3.622 0.006128 ** 
## 6A - 10 == 0  0.946153   0.262813   3.600 0.006362 ** 
## 7 - 10 == 0  -0.292786   0.258990  -1.130 1.000000    
## 8 - 10 == 0  -0.239944   0.264648  -0.907 1.000000    
## 9 - 10 == 0  -0.302465   0.264345  -1.144 1.000000    
## 3A - 2 == 0   0.220528   0.270996   0.814 1.000000    
## 4 - 2 == 0   -0.787634   0.265056  -2.972 0.041554 *  
## 5 - 2 == 0   -0.763280   0.274099  -2.785 0.069653 .  
## 6A - 2 == 0  -0.797154   0.266416  -2.992 0.041554 *  
## 7 - 2 == 0   -2.036093   0.262776  -7.748 3.64e-13 ***
## 8 - 2 == 0   -1.983250   0.268351  -7.391 5.41e-12 ***
## 9 - 2 == 0   -2.045772   0.268055  -7.632 8.78e-13 ***
## 4 - 3A == 0  -1.008162   0.264354  -3.814 0.003149 ** 
## 5 - 3A == 0  -0.983808   0.273445  -3.598 0.006362 ** 
## 6A - 3A == 0 -1.017682   0.265726  -3.830 0.003078 ** 
## 7 - 3A == 0  -2.256621   0.262113  -8.609  < 2e-16 ***
## 8 - 3A == 0  -2.203778   0.267702  -8.232 9.33e-15 ***
## 9 - 3A == 0  -2.266300   0.267405  -8.475  < 2e-16 ***
## 5 - 4 == 0    0.024354   0.267382   0.091 1.000000    
## 6A - 4 == 0  -0.009520   0.259530  -0.037 1.000000    
## 7 - 4 == 0   -1.248459   0.255726  -4.882 3.47e-05 ***
## 8 - 4 == 0   -1.195616   0.261454  -4.573 0.000130 ***
## 9 - 4 == 0   -1.258137   0.261147  -4.818 4.62e-05 ***
## 6A - 5 == 0  -0.033874   0.268701  -0.126 1.000000    
## 7 - 5 == 0   -1.272813   0.264989  -4.803 4.68e-05 ***
## 8 - 5 == 0   -1.219970   0.270522  -4.510 0.000166 ***
## 9 - 5 == 0   -1.282491   0.270225  -4.746 5.81e-05 ***
## 7 - 6A == 0  -1.238939   0.257098  -4.819 4.62e-05 ***
## 8 - 6A == 0  -1.186096   0.262796  -4.513 0.000166 ***
## 9 - 6A == 0  -1.248618   0.262491  -4.757 5.70e-05 ***
## 8 - 7 == 0    0.052843   0.258961   0.204 1.000000    
## 9 - 7 == 0   -0.009679   0.258651  -0.037 1.000000    
## 9 - 8 == 0   -0.062521   0.264317  -0.237 1.000000    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- holm method)
```

####Trying to plot Correctness through both Training and Testing


```r
DataSubset <- subset(DataNew, Condition == "1"|Condition == "2"|Condition == "3A"|Condition == "4"|Condition == "10"| Condition == "5"| 
                       Condition == "6A"|Condition == "7"|Condition == "8"|Condition == "9")

ggplot(DataSubset, aes(x= Block, y= RT, colour= Condition, linetype = Condition)) + 
  geom_smooth(aes(colour = Condition),size = 1.0,se = TRUE, method= 'loess', formula =  y~x)+
  
  scale_linetype_manual(values = c("solid", "dotdash", "dotted",
                                    "solid", "dotdash", "dotted",
                                    "solid", "dotdash", "dotted","longdash")) +
  
  scale_color_manual(values= c("#0066CC", "#000000", "#0066CC", "#0066CC",
                               "#33FF00", "#33FF00", "#33FF00", 
                               "#CC0033", "#CC0033", "#CC0033")) +
  
                               
  ggtitle("Performance by Block") +
  labs(x="Block", y="Proportion of Correctness") +
  facet_grid(~TrialType, scales="free", space= "free_x") + #facet grid works, but it still has a problem with loess
  theme(axis.title.y = element_text(size=12,  color="#666666")) +
  theme(axis.text = element_text(size=8)) +
  theme(plot.title = element_text(size=16, face="bold", hjust=0, color="#666666")) +
  theme(strip.text.x = element_text(size = 8, colour = "black"))
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 6.705e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 6.705e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 4.1559e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 4.1559e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 2.8458e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 2.8458e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 1.322e-014
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 1.322e-014
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 6.705e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 6.705e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 4.6026e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 4.6026e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 1.322e-014
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 1.322e-014
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 6.705e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 6.705e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 4.1559e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 4.1559e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : pseudoinverse used at 0.985
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : neighborhood radius 2.015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : reciprocal condition number 4.1559e-015
```

```
## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
## parametric, : There are other near singularities as well. 4.0602
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used
## at 0.985
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
## 2.015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : reciprocal
## condition number 4.1559e-015
```

```
## Warning in predLoess(object$y, object$x, newx = if
## (is.null(newdata)) object$x else if (is.data.frame(newdata))
## as.matrix(model.frame(delete.response(terms(object)), : There are other
## near singularities as well. 4.0602
```

![](Modelling_and_plotting_second_attempt_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

