# Stekic et al. - Meta Analysis and Related Stuff
Alan Nielsen  
October 30, 2017  

First we'll load in some previous data to make some nice graphs (and load in some packages we will need)


```r
library(tidyverse)
library(ggthemes)
library(scales)

PrevData <- read.csv('F:/GitHub Repos/Stekic-et-al/Data/PrevWork.csv')

#Make a new graph theme


theme_alan <- function(base_size = 12 , base_family = "")
  {
  half_line <- base_size/2
  colors <- ggthemes_data$few
  gray <- colors$medium["gray"]
  black <- colors$dark["black"]
    
  theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", 
    colour = "black", size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "black", 
                        size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5,
                        angle = 0, margin = margin(), debug = FALSE),

    axis.line = element_blank(),
      axis.line.x = NULL,
      axis.line.y = NULL, 
    axis.text = element_text(size = rel(0.8), colour = "grey30"),
      axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1),
      axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line/2), vjust = 0),
      axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line/2), hjust = 0), 
    axis.ticks = element_line(colour = "grey20"), 
      axis.ticks.length = unit(half_line/2, "pt"),
    axis.title.x = element_text(margin = margin(t = half_line), vjust = 1),
    axis.title.x.top = element_text(margin = margin(b = half_line), vjust = 0),
    axis.title.y = element_text(angle = 90, margin = margin(r = half_line), vjust = 1),
    axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line), vjust = 0),

    legend.background = element_rect(colour = NA),
    legend.spacing = unit(0.4, "cm"), 
      legend.spacing.x = NULL, 
      legend.spacing.y = NULL,
    legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.key = element_rect(fill = "white", colour = NA), 
      legend.key.size = unit(1.2, "lines"), 
      legend.key.height = NULL,
      legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)), 
      legend.text.align = NULL,
    legend.title = element_text(hjust = 0),
      legend.title.align = NULL,
    legend.position = "right", 
    legend.direction = NULL,
    legend.justification = "center", 
    legend.box = NULL,
      legend.box.margin = margin(0, 0, 0, 0, "cm"),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(0.4, "cm"),
    
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(fill = NA, colour = "grey20"),
    panel.grid.major = element_line(colour = "grey92"),
    panel.grid.minor = element_line(colour = "grey92", size = 0.25),
    panel.spacing = unit(half_line, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
    panel.ontop = FALSE,
    
    strip.background = element_rect(fill = "NA", colour = "NA"),
    strip.text = element_text(colour = "grey10", size = rel(0.8)),
      strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
      strip.text.y = element_text(angle = 0, margin = margin(l = half_line, r = half_line)),
    strip.placement = "inside",
      strip.placement.x = NULL, 
      strip.placement.y = NULL,
    strip.switch.pad.grid = unit(0.1, "cm"), 
    strip.switch.pad.wrap = unit(0.1, "cm"), 
    
    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2), hjust = 0, vjust = 1, margin = margin(b = half_line * 1.2)),
    plot.subtitle = element_text(size = rel(0.9), hjust = 0, vjust = 1, margin = margin(b = half_line * 0.9)),
    plot.caption = element_text(size = rel(0.9), hjust = 1, vjust = 1, margin = margin(t = half_line * 0.9)), 
    plot.margin = margin(half_line, half_line, half_line, half_line),
    
    complete = TRUE)
  }
```

Okay so the first sensible thing to do are some individual plots by experiment, just to make sure we understand those previous results:


```r
L2007 <- subset(PrevData, Paper == "Lupyan et al. (2007)")

L2007Training <- subset(L2007, Train.vs..Test == "Training")
L2007Testing <- subset(L2007, Train.vs..Test == "Testing")
L2007Verification <- subset(L2007, Train.vs..Test == "Verification")

# Training - Performance
ggplot(data=L2007Training, aes(x=Label.Type, y=Performance, fill = SoundSym)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = Performance - Performance.SD,
                     ymax = Performance + Performance.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Performance") +
  facet_wrap(~Experiment) +
  ggtitle("Lupyan 2007 - Training - Performance")
```

![](Plotting_Previous_Results_files/figure-html/Lupyan et al 2007 Results-1.png)<!-- -->

```r
# Training - RT
ggplot(data=L2007Training, aes(x=Label.Type, y=RT, fill = SoundSym)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = RT - RT.SD,
                     ymax = RT + RT.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Response Time") +
  facet_wrap(~Experiment) +
  ggtitle("Lupyan 2007 - Training - Response Time")
```

```
## Warning: Removed 4 rows containing missing values (geom_bar).
```

```
## Warning: Removed 4 rows containing missing values (geom_errorbar).
```

![](Plotting_Previous_Results_files/figure-html/Lupyan et al 2007 Results-2.png)<!-- -->

```r
# Testing - Performance
ggplot(data=L2007Testing, aes(x=Label.Type, y=Performance, fill = SoundSym)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = Performance - Performance.SD,
                     ymax = Performance + Performance.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Performance") +
  facet_wrap(~Experiment) +
  ggtitle("Lupyan 2007 - Testing - Performance")
```

![](Plotting_Previous_Results_files/figure-html/Lupyan et al 2007 Results-3.png)<!-- -->

```r
# Testing - RT
ggplot(data=L2007Testing, aes(x=Label.Type, y=RT, fill = SoundSym)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = RT - RT.SD,
                     ymax = RT + RT.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Response Time") +
  facet_wrap(~Experiment) +
  ggtitle("Lupyan 2007 - Testing - Response Time")
```

![](Plotting_Previous_Results_files/figure-html/Lupyan et al 2007 Results-4.png)<!-- -->

```r
# Verification - Performance

ggplot(data=L2007Verification, aes(x=Presentation, y=Performance, fill = Label.Type)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = Performance - Performance.SD,
                     ymax = Performance + Performance.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Performance") +
  facet_wrap(~Experiment) +
  ggtitle("Lupyan 2007 - Verification - Performance")
```

![](Plotting_Previous_Results_files/figure-html/Lupyan et al 2007 Results-5.png)<!-- -->



```r
K2010 <- subset(PrevData, Paper == "Kovic et al. (2010)")

K2010Testing <- subset(K2010, Train.vs..Test == "Testing")

# Testing - Performance
ggplot(data=K2010Testing, aes(x=Congruency, y=Performance, fill = Congruency)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = Performance - Performance.SD,
                     ymax = Performance + Performance.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Performance") +
  facet_wrap(~Experiment) +
  ggtitle("Kovic et al. 2010 - Testing - Performance")
```

![](Plotting_Previous_Results_files/figure-html/Kovic et al 2010 Results-1.png)<!-- -->

```r
# Testing - RT
ggplot(data=K2010Testing, aes(x=Congruency, y=RT, fill = Congruency)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = RT - RT.SD,
                     ymax = RT + RT.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Response Time") +
  facet_wrap(~Experiment) +
  ggtitle("Kovic et al. 2010 - Testing - Response Time")
```

![](Plotting_Previous_Results_files/figure-html/Kovic et al 2010 Results-2.png)<!-- -->




```r
L2014 <- subset(PrevData, Paper == "Lupyan et al. (2014)")

# Training - Performance
ggplot(data=L2014, aes(x=SoundSym, y=Performance, fill = Congruency)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = Performance - Performance.SD,
                     ymax = Performance + Performance.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Performance") +
  facet_wrap(~Experiment) +
  ggtitle("Lupyan et al. 2014 - Training - Performance")
```

![](Plotting_Previous_Results_files/figure-html/Lupyan et al 2014 Results-1.png)<!-- -->

```r
# Training - RT
ggplot(data=L2014, aes(x=SoundSym, y=RT, fill = Congruency)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = RT - RT.SD,
                     ymax = RT + RT.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Response Time") +
  facet_wrap(~Experiment) +
  ggtitle("Lupyan et al. 201 - Training - Response Time")
```

```
## Warning: Removed 1 rows containing missing values (geom_bar).
```

```
## Warning: Removed 1 rows containing missing values (geom_errorbar).
```

![](Plotting_Previous_Results_files/figure-html/Lupyan et al 2014 Results-2.png)<!-- -->




```r
K2017 <- subset(PrevData, Paper == "Kovic et al. (2017)")

K2017$Trial.Type <- factor(K2017$Trial.Type, c("Label", "No Label", "Identical", "Mismatch"))

# Performance
ggplot(data=K2017, aes(x=Label.Type, y=Performance, fill = Congruency)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = Performance - Performance.SD,
                     ymax = Performance + Performance.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Performance") +
  facet_grid(Trial.Type ~ Train.vs..Test) +
  ggtitle("Kovic et al. 2017 - Performance")
```

![](Plotting_Previous_Results_files/figure-html/Kovic et al 2017 Results-1.png)<!-- -->

```r
# Response Time
ggplot(data=K2017, aes(x=Label.Type, y=RT, fill = Congruency)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = RT - RT.SD,
                     ymax = RT + RT.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Response Time") +
  facet_grid(Trial.Type ~ Train.vs..Test) +
  ggtitle("Kovic et al. 2017 - Response Time")
```

![](Plotting_Previous_Results_files/figure-html/Kovic et al 2017 Results-2.png)<!-- -->




```r
N2017 <- subset(PrevData, Paper == "Nielsen et al. (2017)")

# Performance
ggplot(data=N2017, aes(x=SoundSym, y=Performance, fill = Congruency)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = Performance - Performance.SD,
                     ymax = Performance + Performance.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Performance") +
  ggtitle("Nielsen et al. 2017 - Performance")
```

![](Plotting_Previous_Results_files/figure-html/Nielsen et al 2017 Results-1.png)<!-- -->

```r
# Response Time
ggplot(data=N2017, aes(x=SoundSym, y=RT, fill = Congruency)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = RT - RT.SD,
                     ymax = RT + RT.SD,
                     width = 0.5)) +
  labs(x="Label Type", y="Response Time") +
  ggtitle("Nielsen et al. 2017 - Response Time")
```

![](Plotting_Previous_Results_files/figure-html/Nielsen et al 2017 Results-2.png)<!-- -->


Okay those graphs are all pretty fucky and not well formatted, but we want to look at everything together anyways, so lets try that

First we will look at Performance


```r
# First we get rid of some stuff not super relevant for what we care about here
PData <- subset(PrevData, Performance != 'NA')
PData <- subset(PData, Congruency != "Location")
PData <- subset(PData, Congruency != "Mixed")
PData <- subset(PData, SoundSym != "Real")
PData <- subset(PData, Train.vs..Test != "Verification")

dataagg1 <- aggregate(Performance ~ Train.vs..Test + Congruency + Paper + Label.Type, data = PData, mean)
dataagg2 <- aggregate(Performance.SD ~ Train.vs..Test + Congruency + Paper + Label.Type, data = PData, mean)

PDataAgg <- dataagg1
PDataAgg$Performance.SD <- dataagg2$Performance.SD
PDataAgg <-droplevels(PDataAgg)

PDataAgg$Label.Type <- factor(PDataAgg$Label.Type, c("Item", "Category"))

PDataAgg$Paper <- factor(PDataAgg$Paper, 
                       c("Lupyan et al. (2007)", "Kovic et al. (2010)", "Lupyan et al. (2014)", 
                           "Kovic et al. (2017)", "Nielsen et al. (2017)"))

PDataAgg$Congruency <- factor(PDataAgg$Congruency, c("Congruent", "Incongruent", "Conventional", "None"))


ggplot(data=PDataAgg, aes(x=Train.vs..Test, y=Performance, fill = Congruency)) +
  geom_bar(position= position_dodge(), stat = "identity") +
  geom_errorbar(position= position_dodge(1),
                aes(ymin = Performance - Performance.SD,
                     ymax = Performance + Performance.SD,
                     width = 0.5)) +
  labs(x="Trial Type", y="Performance") +
  ggtitle("Previous Studies - Training - Performance") +
  facet_grid(Paper ~ Label.Type) +
  theme(strip.text.y = element_text(angle = 0))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
```

![](Plotting_Previous_Results_files/figure-html/Performance - All Results-1.png)<!-- -->

So that's not perfect, mostly because it collapses bars into different sizes when you ask it to dodge, instead of just pretending there are all possible values- so we'll need to manipulate our data frame to put those back


```r
Papers <- c("Lupyan et al. (2007)", "Kovic et al. (2010)", "Lupyan et al. (2014)", 
                           "Kovic et al. (2017)", "Nielsen et al. (2017)")

Congruency <- c("Congruent", "Incongruent", "Conventional", "None")

Train.vs..Test <- c("Training", "Testing")

Label.Type <- c("Item", "Category")

PDataNew <- expand.grid(Papers, Congruency, Train.vs..Test, Label.Type)
colnames(PDataNew) <- c("Paper", "Congruency", "Train.vs..Test", "Label.Type")

PDataNew <- unite(PDataNew, "ID", c("Paper", "Congruency", "Train.vs..Test", "Label.Type"), 
                  sep = '-', remove= FALSE)

dataagg1 <- aggregate(Performance ~ Train.vs..Test + Congruency + Paper + Label.Type, data = PData, mean)
dataagg2 <- aggregate(Performance.SD ~ Train.vs..Test + Congruency + Paper + Label.Type, data = PData, mean)

PDataAgg <- dataagg1
PDataAgg$Performance.SD <- dataagg2$Performance.SD
PDataAgg <-droplevels(PDataAgg)

PDataAgg <- unite(PDataAgg, "ID", c("Paper", "Congruency", "Train.vs..Test", "Label.Type"), 
                  sep = '-', remove= FALSE)

PDataNew$Performance <- PDataAgg[match(PDataNew$ID, PDataAgg$ID),]$Performance
PDataNew$Performance.SD <- PDataAgg[match(PDataNew$ID, PDataAgg$ID),]$Performance.SD


ggplot(data=PDataNew, aes(x=Train.vs..Test, y=Performance, fill = Congruency)) +
  geom_bar(position= position_dodge(), stat = "identity", width = .75) +
  geom_errorbar(position= position_dodge(.75),
                aes(ymin = Performance - Performance.SD,
                     ymax = Performance + Performance.SD,
                     width = 0.5)) +
  labs(x="Trial Type", y="Performance") +
  ggtitle("Previous Studies - Performance") +
  facet_grid(Paper ~ Label.Type) +
  theme_alan() +
  scale_y_continuous(breaks = c(0,0.5,1), minor_breaks = c(0.25, 0.75)) +
  theme(strip.text.y = element_text(hjust= 0)) +
  theme(panel.spacing.y = unit(10, "pt"))
```

```
## Warning: Removed 60 rows containing missing values (geom_bar).
```

```
## Warning: Removed 60 rows containing missing values (geom_errorbar).
```

![](Plotting_Previous_Results_files/figure-html/New Performance Graph-1.png)<!-- -->


Now we just need the same for Response time


```r
RTData <- subset(PrevData, RT != 'NA')
RTData <- subset(RTData, Congruency != "Location")
RTData <- subset(RTData, Congruency != "Mixed")
RTData <- subset(RTData, SoundSym != "Real")
RTData <- subset(RTData, Train.vs..Test != "Verification")

dataagg3 <- aggregate(RT ~ Train.vs..Test + Congruency + Paper + Label.Type, data = RTData, mean)
dataagg4 <- aggregate(RT.SD ~ Train.vs..Test + Congruency + Paper + Label.Type, data = RTData, mean)

RTDataAgg <- dataagg3
RTDataAgg$RT.SD <- dataagg4$RT.SD

RTDataAgg <-droplevels(RTDataAgg)

RTDataAgg$Label.Type <- factor(RTDataAgg$Label.Type, c("Item", "Category"))

RTDataAgg$Paper <- factor(RTDataAgg$Paper, 
                       c("Lupyan et al. (2007)", "Kovic et al. (2010)", "Lupyan et al. (2014)", 
                           "Kovic et al. (2017)", "Nielsen et al. (2017)"))

RTDataAgg$Congruency <- factor(RTDataAgg$Congruency, c("Congruent", "Incongruent", "Conventional", "None"))

RTDataAgg <- unite(RTDataAgg, "ID", c("Paper", "Congruency", "Train.vs..Test", "Label.Type"), 
                  sep = '-', remove= FALSE)


Papers <- c("Lupyan et al. (2007)", "Kovic et al. (2010)", "Lupyan et al. (2014)", 
                           "Kovic et al. (2017)", "Nielsen et al. (2017)")

Congruency <- c("Congruent", "Incongruent", "Conventional", "None")

Train.vs..Test <- c("Training", "Testing")

Label.Type <- c("Item", "Category")

RTDataNew <- expand.grid(Papers, Congruency, Train.vs..Test, Label.Type)
colnames(RTDataNew) <- c("Paper", "Congruency", "Train.vs..Test", "Label.Type")

RTDataNew <- unite(RTDataNew, "ID", c("Paper", "Congruency", "Train.vs..Test", "Label.Type"), 
                  sep = '-', remove= FALSE)


RTDataNew$RT <- RTDataAgg[match(RTDataNew$ID, RTDataAgg$ID),]$RT
RTDataNew$RT.SD <- RTDataAgg[match(RTDataNew$ID, RTDataAgg$ID),]$RT.SD


ggplot(data=RTDataNew, aes(x=Train.vs..Test, y=RT, fill = Congruency)) +
  geom_bar(position= position_dodge(), stat = "identity", width = 0.75) +
  geom_errorbar(position= position_dodge(0.75),
                aes(ymin = RT - RT.SD,
                     ymax = RT + RT.SD,
                     width = 0.5)) +
  labs(x="Trial Type", y="ResponseTime") +
  ggtitle("Previous Studies - Response Times") +
  facet_grid(Paper ~ Label.Type, scales = "free_y") +
  theme_alan() +
  theme(strip.text.y = element_text(hjust= 0)) +
  theme(panel.spacing.y = unit(10, "pt")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) 
```

```
## Warning: Removed 60 rows containing missing values (geom_bar).
```

```
## Warning: Removed 60 rows containing missing values (geom_errorbar).
```

![](Plotting_Previous_Results_files/figure-html/Response Time Graphs-1.png)<!-- -->




