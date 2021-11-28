# Review_AI_CT_head
Here, you can find the code used for our review "Reproducibility of artificial intelligence models in computed tomography of the head – a systematic review"
DOI:

Code is written in R 4.1.1


**Note:** Critics, suggestions and comments are very well welcomed!

## Libraries
´´´
library(ggplot2)
library(psych)
library(ggthemes)
library(scales)
library(tidyverse)
library(dplyr)
library("qpcR")
´´´
## Statistical analysis
´´´
#get data and assign variables
data <- read.csv("Summary_papers.csv")
data <- data[1:83,]

#years of publication (freq & growth rate)
describe(data$Year)
year_freq <- data.frame(table(data$Year))
year_freq$Var1 <- as.numeric(year_freq$Var1)
growth_rate = year_freq %>%
  arrange(Var1) %>%
  mutate(Diff_year = Var1 - lag(Var1),
         Diff_growth = Freq - lag(Freq),
         Rate_percent = (Diff_growth / Diff_year)/Freq * 100)
Average_growth = mean(growth_rate$Rate_percent, na.rm = TRUE)

#classes of papers (freq)
Class.string <- paste(data$Class_of_paper , collapse = ",")
Class.vector <- strsplit(Class.string , ",")[[1]]
Class.vector.clean <- gsub( " " , "" , Class.vector)
Class.of.paper <- data.frame(table(Class.vector.clean))

#architecture illustrations (y/n)
architecture <- data.frame(table(data$Graphic.of.architecture..1.y..0.n., useNA = "ifany"))
Pct.architecture <- architecture[,2]/sum(architecture[,2])
#open source code (y/n)
open.source <- data.frame(table(data$Code.open.source..1.y..n.n., useNA = "ifany"))
Pct.open.source <- open.source[,2]/sum(open.source[,2])
open.source <- cbind(open.source, Pct.open.source)

#train/val split (y/n)
train.val.split <- data.frame(table(data$Train.Validation_split..1.y..NA.n., useNA = "ifany"))
Pct.train.val.split <- train.val.split[,2]/sum(train.val.split[,2])
train.val.split <- cbind(train.val.split, Pct.train.val.split)

#seperate test set (y/n)
test.set <- data.frame(table(data$Test_holdout..1.y..0.n., useNA = "ifany"))
Pct.test.set <- test.set[,2]/sum(test.set[,2])
test.set <- cbind(test.set, Pct.test.set)
  #external test set (y/n)
  external.test <- filter(data, Test_holdout..1.y..0.n. == 1)
  external.test <- data.frame(table(external.test$External_testset..1.y..0.n., useNA = "ifany"))
  Pct.external.test <- external.test[,2]/test.set[2,2]
  external.test <- cbind(external.test, Pct.external.test)
  
#preprocessing steps (y/n)
preproc <- data.frame(table(data$Preprocessing..1.y..0.n..NA.ND., useNA = "ifany"))
Pct.preproc <- preproc[,2]/sum(preproc[,2])
preproc <- cbind(preproc, Pct.preproc)

#hardware (CPU/GPU/both)
hardware <- data.frame(table(data$Hardware..0.GPU..1.CPU..2.both., useNA = "ifany"))
Pct.hardware <- hardware[,2]/sum(hardware[,2])
hardware <- cbind(hardware, Pct.hardware)

#algorithm types (freq)
Algo.string <- paste(data$Type, collapse = ",")
Algo.vector <- strsplit(Algo.string , ",")[[1]]
Algo.vector.clean <- gsub( " " , "" , Algo.vector)
algorithm <- data.frame(table(Algo.vector.clean, useNA = "ifany"))
Pct.algorithm <- algorithm[,2]/sum(algorithm[,2])
algorithm <- cbind(algorithm, Pct.algorithm)

#hyperparameters (min: learning rate, optimization, minibatch | max: dropout, epoch)
hyperparameters <- nrow(filter(data, Code.open.source..1.y..n.n. == 1 | Learning.rate == 1 & batch.size == 1 & Optimization.algorithm == 1))
Pct.hyperparameters <- hyperparameters/nrow(filter(data, Type == "CNN"))
hyperparameters <- cbind(hyperparameters, Pct.hyperparameters)

epoch <- nrow(filter(data, Code.open.source..1.y..n.n. == 1 |N.of.epochs..or.trees. == 1))
Pct.epoch <- epoch/nrow(filter(data, Type == "CNN"))
epoch <- cbind(epoch, Pct.epoch)

dropout <- nrow(filter(data, Code.open.source..1.y..n.n. == 1 | Dropout == 1))
Pct.dropout <- dropout/nrow(filter(data, Type == "CNN"))
dropout <- cbind(dropout, Pct.dropout)

#loss function (y/n)
Loss.string <- paste(data$Loss.of.function, collapse = ",")
Loss.vector <- strsplit(Loss.string, ",")[[1]]
Loss.vector.clean <- gsub( " " , "" , Loss.vector)
loss.function <- data.frame(table(Loss.vector.clean, useNA = "ifany"))
Pct.loss.function <- data.frame(loss.function[,2]/sum(loss.function[,2]))
open.loss.function <- nrow(filter(data, Code.open.source..1.y..n.n. == 1))
loss.function <- cbind(open.loss.function, loss.function,Pct.loss.function)

#Frameworks (freq)
Frame.string <- paste(data$Architektur, collapse = ",")
Frame.vector <- strsplit(Frame.string, ",")[[1]]
Frame.vector.clean <- gsub( " " , "" , Frame.vector)
Frame <- data.frame(table(Frame.vector.clean, useNA = "ifany"))

#ground truth (n,r,m)
Ground.truth <- data.frame(table(data$Ground.Truth, useNA = "ifany"))
Pct.Ground.truth <- Ground.truth[,2]/sum(Ground.truth[,2])
Ground.truth <- cbind(Ground.truth,Pct.Ground.truth)

#data source (single,multi,synth,NA)
Data.source <- data.frame(table(data$Source..0.single..1.multicentre..2.synthetisch., useNA = "ifany"))
Pct.Data.source <- Data.source[,2]/sum(Data.source[,2])
Data.source <- cbind(Data.source,Pct.Data.source)

#descriptive statistic data size
  Data.size.desc <- paste(data$Size_dataset, collapse = ",")
  Data.size.desc.vector <- strsplit(Data.size.desc, ",")[[1]]
  Data.size.desc.clean <- gsub( " " , "" , Data.size.desc.vector)
  Data.size.clean <- data.frame(as.numeric(Data.size.desc.clean))
  Description.datasize <- describe(Data.size.clean)
#augmentation (y/n)
  Augmentation <- data.frame(table(data$Augmentation..1.y..NA.n., useNA = "ifany"))
  Pct.Augmentation <- Augmentation[,2]/sum(Augmentation[,2])
  Augmentation <- cbind(Augmentation,Pct.Augmentation)
  
#resolution input (y/n)
  Resolution <- data.frame(table(data$Dimension.Resolution.Input..1.y..0.n., useNA = "ifany"))
  Pct.Resolution <- Resolution[,2]/sum(Resolution[,2])
  Resolution <- cbind(Resolution,Pct.Resolution)
#colour space input (y/n)
  Colour <- data.frame(table(data$Colour.space..1.y..NA.n., useNA = "ifany"))
  Pct.Colour <- Colour[,2]/sum(Colour[,2])
  Colour <- cbind(Colour,Pct.Colour)

#measurements of performance (freq by class of paper)
  Classification <- filter(data, Class_of_paper == "Classification")
  Detection <- filter(data, Class_of_paper == "Detection")
  Prediction <- filter(data, Class_of_paper == "Prediction")
  Segmentation <- filter(data, Class_of_paper == "Segmentation")
  Triage <- filter(data, Class_of_paper == "Triage")
  Reconstruction <- filter(data, Class_of_paper == "Reconstruction")
  Fusion <- filter(data, Class_of_paper == "Fusion")
  Generation <- filter(data, Class_of_paper == "Generation")
  Quantification <- filter(data, Class_of_paper == "Quantification")
  
  MOP.string.class <- paste(Classification$MOP, collapse = ",")
  MOP.string.det <- paste(Detection$MOP, collapse = ",")
  MOP.string.pred <- paste(Prediction$MOP, collapse = ",")
  MOP.string.seg <- paste(Segmentation$MOP, collapse = ",")
  MOP.string.tri <- paste(Triage$MOP, collapse = ",")
  MOP.string.rec <- paste(Reconstruction$MOP, collapse = ",")
  MOP.string.fus <- paste(Fusion$MOP, collapse = ",")
  MOP.string.gen <- paste(Generation$MOP, collapse = ",")
  MOP.string.qua <- paste(Quantification$MOP, collapse = ",")
  
  MOP.vector.class <- strsplit(MOP.string.class , ",")[[1]]
  MOP.vector.det <- strsplit(MOP.string.det , ",")[[1]]
  MOP.vector.pred <- strsplit(MOP.string.pred , ",")[[1]]
  MOP.vector.seg <- strsplit(MOP.string.seg , ",")[[1]]
  MOP.vector.tri <- strsplit(MOP.string.tri , ",")[[1]]
  MOP.vector.rec <- strsplit(MOP.string.rec , ",")[[1]]
  MOP.vector.fus <- strsplit(MOP.string.fus , ",")[[1]]
  MOP.vector.gen <- strsplit(MOP.string.gen , ",")[[1]]
  MOP.vector.qua <- strsplit(MOP.string.qua , ",")[[1]]
  
  MOP.vector.clean.class <- gsub( " " , "" , MOP.vector.class)
  MOP.vector.clean.det <- gsub( " " , "" , MOP.vector.det)
  MOP.vector.clean.pred <- gsub( " " , "" , MOP.vector.pred)
  MOP.vector.clean.seg <- gsub( " " , "" , MOP.vector.seg)
  MOP.vector.clean.tri <- gsub( " " , "" , MOP.vector.tri)
  MOP.vector.clean.rec <- gsub( " " , "" , MOP.vector.rec)
  MOP.vector.clean.fus <- gsub( " " , "" , MOP.vector.fus)
  MOP.vector.clean.gen <- gsub( " " , "" , MOP.vector.gen)
  MOP.vector.clean.qua <- gsub( " " , "" , MOP.vector.qua)
  
  MOP.all.class <- data.frame(table(MOP.vector.clean.class, useNA = "no"))
  MOP.all.det <- data.frame(table(MOP.vector.clean.det, useNA = "no"))
  MOP.all.pred <- data.frame(table(MOP.vector.clean.pred, useNA = "no"))
  MOP.all.seg <- data.frame(table(MOP.vector.clean.seg, useNA = "no"))
  MOP.all.tri <- data.frame(table(MOP.vector.clean.tri, useNA = "no"))
  MOP.all.rec <- data.frame(table(MOP.vector.clean.rec, useNA = "no"))
  MOP.all.fus <- data.frame(table(MOP.vector.clean.fus, useNA = "no"))
  MOP.all.gen <- data.frame(table(MOP.vector.clean.gen, useNA = "no"))
  MOP.all.qua <- data.frame(table(MOP.vector.clean.qua, useNA = "no"))
  
  Pct.MOP.class <- MOP.all.class[,2]/sum(MOP.all.class[,2])
  MOP.all.class <- cbind(MOP.all.class,Pct.MOP.class)
  Pct.MOP.det <- MOP.all.det[,2]/sum(MOP.all.det[,2])
  MOP.all.det <- cbind(MOP.all.det,Pct.MOP.det)
  Pct.MOP.pred <- MOP.all.pred[,2]/sum(MOP.all.pred[,2])
  MOP.all.pred <- cbind(MOP.all.pred,Pct.MOP.pred)
  Pct.MOP.seg <- MOP.all.seg[,2]/sum(MOP.all.seg[,2])
  MOP.all.seg <- cbind(MOP.all.seg,Pct.MOP.seg)
  Pct.MOP.tri <- MOP.all.tri[,2]/sum(MOP.all.tri[,2])
  MOP.all.tri <- cbind(MOP.all.tri,Pct.MOP.tri)
  Pct.MOP.rec <- MOP.all.rec[,2]/sum(MOP.all.rec[,2])
  MOP.all.rec <- cbind(MOP.all.rec,Pct.MOP.rec)
  Pct.MOP.fus <- MOP.all.fus[,2]/sum(MOP.all.fus[,2])
  MOP.all.fus <- cbind(MOP.all.fus,Pct.MOP.fus)
  Pct.MOP.gen <- MOP.all.gen[,2]/sum(MOP.all.gen[,2])
  MOP.all.gen <- cbind(MOP.all.gen,Pct.MOP.gen)
  Pct.MOP.qua <- MOP.all.qua[,2]/sum(MOP.all.qua[,2])
  MOP.all.qua <- cbind(MOP.all.qua,Pct.MOP.qua)
  
#comparisons
  Compare.string <- paste(data$Comparison.to, collapse = ",")
  Compare.vector <- strsplit(Compare.string , ",")[[1]]
  Compare.vector.clean <- gsub( " " , "" , Compare.vector)
  Compare <- data.frame(table(Compare.vector.clean, useNA = "ifany"))
  Pct.Compare <- Compare[,2]/sum(Compare[,2])
  Compare <- cbind(Compare,Pct.Compare)
  
#Prevalences
prevalence <- read.csv("Prevalences.csv")
prev.train.desc <- describe(prevalence$Prevalence_Training)
prev.test.desc <- describe(prevalence$Prevalence_Testset)
prev.real.desc <- describe(prevalence$Prevalence.Reality)
  #check for normality
  shapiro.test(prevalence$Prevalence_Training)
  shapiro.test(prevalence$Prevalence_Testset)
  shapiro.test(prevalence$Prevalence.Reality)
  hist(prevalence$Prevalence_Training, breaks = 10)
  hist(prevalence$Prevalence_Testset, breaks = 10)
  hist(prevalence$Prevalence.Reality, breaks = 10)
  #check for difference
  train.t.test <- t.test(prevalence$Prevalence_Training,prevalence$Prevalence_Testset)
  train.t.real <- t.test(prevalence$Prevalence_Training, prevalence$Prevalence.Reality)
  test.t.real <- t.test(prevalence$Prevalence_Testset, prevalence$Prevalence.Reality)
´´´
## Plots
´´´
ggplot(data %>% count(Year, Class_of_paper),
       aes(fill=Class_of_paper, y=n, x=Year)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_x_discrete(limits=c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) +
  scale_y_continuous(limits=c(0,30)) +
  theme(legend.title = element_text(size=9),
        legend.text = element_text(size=7),
        legend.key.size = unit(3, 'mm'),
        axis.text.x = element_text(angle = 90, size = 9, hjust=1,vjust=0.5), 
        text=element_text(size=12,  family="Lato Bold"),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white")) + 
  ylab("Number of publications") +
  guides(fill=guide_legend(title="Main function"))
´´´

