# Reading in necessary packages--------------------------------------------

library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(emmeans)
library(readxl)
library(multcompView)
library(multcomp)
library(car)


# Reading in flux contrib data -----------------------------------------------
thresh<-read_excel("/Users/emstuch/Desktop/Reinhart study/R scripts/Associated Excel Files/finalizeddata_all_data repository.xlsx", sheet = "contribcumflux_seasonsandyear")
View(thresh)
str(thresh)
thresh$Node<-as.factor(thresh$Node)
thresh$Chamber<-as.factor(thresh$Chamber)
thresh$Season<-as.factor(thresh$Season)
thresh$Strategy<-as.factor(thresh$Strategy)
str(thresh)
View(thresh)


# Early season flux contributions -------------------------------------------------

#####
#filter data to include only early season threshold values

early <- thresh %>%
  filter(Season %in% 
           "Early growing season") 
View(early)

SumStats_early <- ddply(early, c("Strategy"), summarise,
                  n = length(`Flux Contribution`),
                  mean = mean(`Flux Contribution`),
                  sd = sd(`Flux Contribution`),
                  se = sd / sqrt(n) )
SumStats_early

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
earlymodel<-lm(`Flux Contribution`~Strategy, data=early)
anova(earlymodel) 
summary(earlymodel)

#pairwise comparisons of thresholds among the different strategies for early season 
compearly<-emmeans(earlymodel,pairwise~Strategy)
compearly

#now setting up compact letter display
cld<-glht(earlymodel, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#####
#filter data to include only early season and node 1

early2 <- thresh %>%
  filter(Node %in% "Node1") %>%
  filter(Season %in% "Early growing season")
#View(early2)

#run one-way anova to compare thresholds among strategies (across only node 1)
earlymodel2<-lm(`Flux Contribution`~Strategy, data=early2)
anova(earlymodel2) 
summary(earlymodel2)

#pairwise comparisons of thresholds among the different strategies for early season 
compearly2<-emmeans(earlymodel2,pairwise~Strategy)
compearly2

#now setting up compact letter display
cldn1<-glht(earlymodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn1)

#####
#filter data to include only early season and node 2

early2 <- thresh %>%
  filter(Node %in% "Node2") %>%
  filter(Season %in% "Early growing season")
#View(early2)

#run one-way anova to compare thresholds among strategies (across only node 1)
earlymodel2<-lm(`Flux Contribution`~Strategy, data=early2)
anova(earlymodel2) 
summary(earlymodel2)

#pairwise comparisons of thresholds among the different strategies for early season 
compearly2<-emmeans(earlymodel2,pairwise~Strategy)
compearly2

#now setting up compact letter display
cldn2<-glht(earlymodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn2)

#####
#filter data to include only early season and node 3

early2 <- thresh %>%
  filter(Node %in% "Node3") %>%
  filter(Season %in% "Early growing season")
#View(early2)

#run one-way anova to compare thresholds among strategies (across only node 1)
earlymodel2<-lm(`Flux Contribution`~Strategy, data=early2)
anova(earlymodel2) 
summary(earlymodel2)

#pairwise comparisons of thresholds among the different strategies for early season 
compearly2<-emmeans(earlymodel2,pairwise~Strategy)
compearly2

#now setting up compact letter display
cldn3<-glht(earlymodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn3)

#####
#filter data to include only early season and node 4

early2 <- thresh %>%
  filter(Node %in% "Node4") %>%
  filter(Season %in% "Early growing season")
#View(early2)

#run one-way anova to compare thresholds among strategies (across only node 1)
earlymodel2<-lm(`Flux Contribution`~Strategy, data=early2)
anova(earlymodel2) 
summary(earlymodel2)

#pairwise comparisons of thresholds among the different strategies for early season 
compearly2<-emmeans(earlymodel2,pairwise~Strategy)
compearly2

#now setting up compact letter display
cldn4<-glht(earlymodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn4)

# Late season flux contributions -------------------------------------------------

#####
#filter data to include only late season threshold values

late <- thresh %>%
  filter(Season %in% 
           "Late growing season") 
View(late)

SumStats_late <- ddply(late, c("Strategy"), summarise,
                        n = length(`Flux Contribution`),
                        mean = mean(`Flux Contribution`),
                        sd = sd(`Flux Contribution`),
                        se = sd / sqrt(n) )
SumStats_late

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
latemodel<-lm(`Flux Contribution`~Strategy, data=late)
anova(latemodel) 
summary(latemodel)

#pairwise comparisons of thresholds among the different strategies for early season 
complate<-emmeans(latemodel,pairwise~Strategy)
complate

#now setting up compact letter display
cld<-glht(latemodel, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#####
#filter data to include only late season and node 1

late2 <- thresh %>%
  filter(Season %in% 
           "Late growing season") %>%
  filter(Node %in% "Node1")
#View(late2)

#run one-way anova to compare thresholds among strategies (across only node 1 chambers)
latemodel2<-lm(`Flux Contribution`~Strategy, data=late2)
anova(latemodel2) 
summary(latemodel2)

#pairwise comparisons of thresholds among the different strategies for late season 
complate2<-emmeans(latemodel2,pairwise~Strategy)
complate2

#now setting up compact letter display
cldn1<-glht(latemodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn1)

#####
#filter data to include only late season and node 2

late2 <- thresh %>%
  filter(Season %in% 
           "Late growing season") %>%
  filter(Node %in% "Node2")
#View(late2)

#run one-way anova to compare thresholds among strategies (across only node 2 chambers)
latemodel2<-lm(`Flux Contribution`~Strategy, data=late2)
anova(latemodel2) 
summary(latemodel2)

#pairwise comparisons of thresholds among the different strategies for late season 
complate2<-emmeans(latemodel2,pairwise~Strategy)
complate2

#now setting up compact letter display
cldn2<-glht(latemodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn2)

#####
#filter data to include only late season and node 3

late2 <- thresh %>%
  filter(Season %in% 
           "Late growing season") %>%
  filter(Node %in% "Node3")
#View(late2)

#run one-way anova to compare thresholds among strategies (across only node 3 chambers)
latemodel2<-lm(`Flux Contribution`~Strategy, data=late2)
anova(latemodel2) 
summary(latemodel2)

#pairwise comparisons of thresholds among the different strategies for late season 
complate2<-emmeans(latemodel2,pairwise~Strategy)
complate2

#now setting up compact letter display
cldn3<-glht(latemodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn3)

#####
#filter data to include only late season and node 4

late2 <- thresh %>%
  filter(Season %in% 
           "Late growing season") %>%
  filter(Node %in% "Node4")
#View(late2)

#run one-way anova to compare thresholds among strategies (across only node 4 chambers)
latemodel2<-lm(`Flux Contribution`~Strategy, data=late2)
anova(latemodel2) 
summary(latemodel2)

#pairwise comparisons of thresholds among the different strategies for late season 
complate2<-emmeans(latemodel2,pairwise~Strategy)
complate2

#now setting up compact letter display
cldn4<-glht(latemodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn4)

# Non-growing season flux contributions -------------------------------------------------

#####
#filter data to include only post harvest & winter season threshold values

post <- thresh %>%
  filter(Season %in% 
           "Post harvest & winter") 
View(post)

SumStats_post <- ddply(post, c("Strategy"), summarise,
                       n = length(`Flux Contribution`),
                       mean = mean(`Flux Contribution`),
                       sd = sd(`Flux Contribution`),
                       se = sd / sqrt(n) )
SumStats_post

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
postmodel<-lm(`Flux Contribution`~Strategy, data=post)
anova(postmodel) 
summary(postmodel)

#pairwise comparisons of thresholds among the different strategies for post harvest & winter seasons season 
comppost<-emmeans(postmodel,pairwise~Strategy)
comppost

#now setting up compact letter display
cld<-glht(postmodel, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#####
#filter data to include only post season and node 1

post2 <- thresh %>%
  filter(Season %in% 
           "Post harvest & winter") %>%
  filter(Node %in% "Node1")
#View(post2)

#run one-way anova to compare thresholds among strategies (across only node 1 chambers)
postmodel2<-lm(`Flux Contribution`~Strategy, data=post2)
anova(postmodel2) 
summary(postmodel2)

#pairwise comparisons of thresholds among the different strategies for post harvest & winter seasons 
comppost2<-emmeans(postmodel2,pairwise~Strategy)
comppost2

#now setting up compact letter display
cldn1<-glht(postmodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn1)

#####
#filter data to include only late season and node 2

post2 <- thresh %>%
  filter(Season %in% 
           "Post harvest & winter") %>%
  filter(Node %in% "Node2")
#View(post2)

#run one-way anova to compare thresholds among strategies (across only node 2 chambers)
postmodel2<-lm(`Flux Contribution`~Strategy, data=post2)
anova(postmodel2) 
summary(postmodel2)

#pairwise comparisons of thresholds among the different strategies for post harvest & winter seasons 
comppost2<-emmeans(postmodel2,pairwise~Strategy)
comppost2

#now setting up compact letter display
cldn2<-glht(postmodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn2)

#####
#filter data to include only late season and node 3

post2 <- thresh %>%
  filter(Season %in% 
           "Post harvest & winter") %>%
  filter(Node %in% "Node3")
#View(post2)

#run one-way anova to compare thresholds among strategies (across only node 3 chambers)
postmodel2<-lm(`Flux Contribution`~Strategy, data=post2)
anova(postmodel2) 
summary(postmodel2)

#pairwise comparisons of thresholds among the different strategies for post harvest & winter seasons 
comppost2<-emmeans(postmodel2,pairwise~Strategy)
comppost2

#now setting up compact letter display
cldn3<-glht(postmodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn3)

#####
#filter data to include only late season and node 4

post2 <- thresh %>%
  filter(Season %in% 
           "Post harvest & winter") %>%
  filter(Node %in% "Node4")
#View(post2)

#run one-way anova to compare thresholds among strategies (across only node 4 chambers)
postmodel2<-lm(`Flux Contribution`~Strategy, data=post2)
anova(postmodel2) 
summary(postmodel2)

#pairwise comparisons of thresholds among the different strategies for post harvest & winter seasons 
comppost2<-emmeans(postmodel2,pairwise~Strategy)
comppost2

#now setting up compact letter display
cldn4<-glht(postmodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn4)


# Reading in whole year flux contrib data ------------------------------------

#####
#filter data to include only whole year threshold values

wholeyear <- thresh %>%
  filter(Season %in% 
           "Whole Year") 
View(wholeyear)

#####
#comparing contributions to cumulative flux for whole year

SumStats_whole <- ddply(wholeyear, c("Strategy"), summarise,
                        n = length(`Flux Contribution`),
                        mean = mean(`Flux Contribution`),
                        sd = sd(`Flux Contribution`),
                        se = sd / sqrt(n) )
SumStats_whole

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
wholeyearmodel<-lm(`Flux Contribution`~Strategy, data=wholeyear)
anova(wholeyearmodel) 
summary(wholeyearmodel)

#pairwise comparisons of thresholds among the different strategies for early season 
compwholeyear<-emmeans(wholeyearmodel,pairwise~Strategy)
compwholeyear

#now setting up compact letter display
cld<-glht(wholeyearmodel, linfct = mcp(Strategy ="Tukey"))
cld(cld)

# Strategy by season flux contrib -------------------------------------------

#####
#now filtering by 1.5x IQR across all seasons

IQR <- thresh %>%
  filter(Strategy %in% 
           "IQR") #%>%
  #filter(!(Season %in% "Whole year"))
View(IQR)

SumStats_IQR <- ddply(IQR, c("Season"), summarise,
                      n = length(`Flux Contribution`),
                      mean = mean(`Flux Contribution`),
                      sd = sd(`Flux Contribution`),
                      se = sd / sqrt(n) )
SumStats_IQR

#run one-way anova to compare thresholds among the different seasons 
#for the 1.5x IQR method (across all nodes and all chambers)
IQRmodel<-lm(`Flux Contribution`~Season, data=IQR)
anova(IQRmodel) 
summary(IQRmodel)

#pairwise comparisons of thresholds among the different seasons for 
#the 1.5x IQR method 
compIQR<-emmeans(IQRmodel,pairwise~Season)
compIQR

#now setting up compact letter display
cld<-glht(IQRmodel, linfct = mcp(Season ="Tukey"))
cld(cld)

#####
#now filtering by IF across all seasons

IF <- thresh %>%
  filter(Strategy %in% 
           "IF") #%>%
  #filter(!(Season %in% "Whole year"))
View(IF)

SumStats_IF <- ddply(IF, c("Season"), summarise,
                     n = length(`Flux Contribution`),
                     mean = mean(`Flux Contribution`),
                     sd = sd(`Flux Contribution`),
                     se = sd / sqrt(n) )
SumStats_IF

#run one-way anova to compare thresholds among the different seasons 
#for the 1.5x IQR method (across all nodes and all chambers)
IFmodel<-lm(`Flux Contribution`~Season, data=IF)
anova(IFmodel) 
summary(IFmodel)

#pairwise comparisons of thresholds among the different seasons for 
#the 1.5x IQR method 
compIF<-emmeans(IFmodel,pairwise~Season)
compIF

#now setting up compact letter display
cld<-glht(IFmodel, linfct = mcp(Season ="Tukey"))
cld(cld)

#####
#now filtering by 4SD across all seasons

fourSD <- thresh %>%
  filter(Strategy %in% 
           "4SD") #%>%
  #filter(!(Season %in% "Whole year"))
View(fourSD)

SumStats_fourSD <- ddply(fourSD, c("Season"), summarise,
                         n = length(`Flux Contribution`),
                         mean = mean(`Flux Contribution`),
                         sd = sd(`Flux Contribution`),
                         se = sd / sqrt(n) )
SumStats_fourSD

#run one-way anova to compare thresholds among the different seasons 
#for the 1.5x IQR method (across all nodes and all chambers)
fourSDmodel<-lm(`Flux Contribution`~Season, data=fourSD)
anova(fourSDmodel) 
summary(fourSDmodel)

#pairwise comparisons of thresholds among the different seasons for 
#the 1.5x IQR method 
compfourSD<-emmeans(fourSDmodel,pairwise~Season)
compfourSD

#now setting up compact letter display
cld<-glht(fourSDmodel, linfct = mcp(Season ="Tukey"))
cld(cld)

#####
#now filtering by 2SD across all seasons

twoSD <- thresh %>%
  filter(Strategy %in% 
           "2SD") #%>%
#filter(!(Season %in% "Whole year"))
View(twoSD)

SumStats_twoSD <- ddply(twoSD, c("Season"), summarise,
                         n = length(`Flux Contribution`),
                         mean = mean(`Flux Contribution`),
                         sd = sd(`Flux Contribution`),
                         se = sd / sqrt(n) )
SumStats_twoSD

#run one-way anova to compare thresholds among the different seasons 
#for the 2 SD method (across all nodes and all chambers)
twoSDmodel<-lm(`Flux Contribution`~Season, data=twoSD)
anova(twoSDmodel) 
summary(twoSDmodel)

#pairwise comparisons of thresholds among the different seasons for 
#the 2 SD method 
comptwoSD<-emmeans(twoSDmodel,pairwise~Season)
comptwoSD

#now setting up compact letter display
cld<-glht(twoSDmodel, linfct = mcp(Season ="Tukey"))
cld(cld)


# Reading in time percentage data -----------------------------------------------
percent<-read_excel("/Users/emstuch/Desktop/Reinhart study/R scripts/Associated Excel Files/finalizeddata_all_data repository.xlsx", sheet = "amount of hot time")
View(percent)
str(percent)
percent$Node<-as.factor(percent$Node)
percent$Chamber<-as.factor(percent$Chamber)
percent$Season<-as.factor(percent$Season)
percent$Strategy<-as.factor(percent$Strategy)
str(percent)
View(percent)

#re-ordering the strategies so plots are in same order as Jiacheng's plots
percent$Strategy <- factor(percent$Strategy, levels = c("IQR", "iForest", "2SD", "4SD"),
                     labels = c("1.5 x IQR", "Isolation Forest", "2 SD", "4 SD"))
percent$Season <- factor(percent$Season, levels = c("Early-Growing", "Late-Growing", "Post-Harvest"),
                         labels = c("Early growing season", "Late growing season", "Non-growing season"))
View(percent)


# Time percentage graphs --------------------------------------------------

#Making plot to show percentages of time that are hot moments - STACKED BY CHAMBER BY SEASON
p <- ggplot(percent, aes(x = Strategy, y = Percentage, fill = Season)) +
  geom_bar(stat = "identity") +
  facet_grid(~Node~Chamber) +
  #facet_grid(~Node, switch = "x") +
  scale_fill_manual(values = c("#7570B3", "#D95F02", "#1B9E77")) +
  labs(x = "Strategy", y = "Percentages") +
  theme_classic2() + theme(legend.position = "right", plot.caption = element_text(size = 12L), 
                           axis.title.y = element_text(size = 15L, face = "bold", color = "black"), 
                           axis.title.x = element_text(size = 15L, face = "bold", color = "black"),
                           axis.text = element_text(size = 12L, color = "black")) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage, position = position_stack(vjust = 0.5), size = 4)))
p

#nicer looking plot - formatting that matches the python plot:
# Create the plot
p <- ggplot(percent, aes(x = Strategy, y = Percentage, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~Node~Chamber) +
  #facet_grid(~Node, switch = "x") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Strategy", y = "Percentages") +
  theme_classic2() + theme(legend.position = "right", plot.caption = element_text(size = 12L), 
                           axis.title.y = element_text(size = 15L, face = "bold", color = "black"), 
                           axis.title.x = element_text(size = 15L, face = "bold", color = "black"),
                           axis.text = element_text(size = 12L, color = "black"))
p

#FINALIZED PLOT - color matches seasons
#nicer looking plot - formatting that matches the python plot:
p <- ggplot(percent, aes(x = Strategy, y = Percentage, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_dodge(width = 0.9),  # Adjust the width as needed
            vjust = -0.5, size = 4) +
  facet_grid(~Node~Chamber) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Strategy", y = "Percentages") +
  theme_classic2() + 
  theme(legend.position = "right", 
        plot.caption = element_text(size = 12L), 
        axis.title.y = element_text(size = 15L, face = "bold", color = "black"), 
        axis.title.x = element_text(size = 15L, face = "bold", color = "black"),
        axis.text = element_text(size = 12L, color = "black"),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  guides(fill = guide_legend(title = "Season"))
p

#FINALIZED PLOT - color in grey scale (Wendy likes this option)
p <- ggplot(percent, aes(x = Season, y = Percentage, fill = Strategy)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = .9) +
  facet_grid(~Node~Chamber) +
  scale_fill_brewer(palette = "Greys") +
  labs(x = "Season", y = "Percentages") +
  theme_classic2() + 
  theme(legend.position = "top", 
        plot.caption = element_text(size = 12L), 
        axis.title.y = element_text(size = 15L, face = "bold", color = "black"), 
        axis.title.x = element_text(size = 15L, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12L, angle = 35, vjust = 1, hjust = .97, color = "black"),
        axis.text.y = element_text(size = 12L, color = "black")) +
  guides(fill = guide_legend(title = "Strategy"))
p


#####

#Summary stats of the percentages by season - for Tukey comparisons
SumStats <- ddply(percent, c("Strategy","Season"), summarise,
                  n = length(`Percentage`),
                  mean = mean(`Percentage`),
                  sum = sum(`Percentage`),
                  sd = sd(`Percentage`),
                  se = sd / sqrt(n) )
SumStats


# Early season time percentages -------------------------------------------

#filter data to include only early season percentage values

early <- percent %>%
  filter(Season %in% 
           "Early growing season") 
View(early)

SumStats_early <- ddply(early, c("Strategy"), summarise,
                        n = length(`Percentage`),
                        mean = mean(`Percentage`),
                        sd = sd(`Percentage`),
                        se = sd / sqrt(n) )
SumStats_early

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
earlymodel<-lm(`Percentage`~Strategy, data=early)
anova(earlymodel) 
summary(earlymodel)

#pairwise comparisons of thresholds among the different strategies for early season 
compearly<-emmeans(earlymodel,pairwise~Strategy)
compearly

#now setting up compact letter display
cld<-glht(earlymodel, linfct = mcp(Strategy ="Tukey"))
cld(cld)

# Late season time percentages -------------------------------------------------

#filter data to include only late season threshold values

late <- percent %>%
  filter(Season %in% 
           "Late growing season") 
View(late)

SumStats_late <- ddply(late, c("Strategy"), summarise,
                       n = length(`Percentage`),
                       mean = mean(`Percentage`),
                       sd = sd(`Percentage`),
                       se = sd / sqrt(n) )
SumStats_late

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
latemodel<-lm(`Percentage`~Strategy, data=late)
anova(latemodel) 
summary(latemodel)

#pairwise comparisons of thresholds among the different strategies for early season 
complate<-emmeans(latemodel,pairwise~Strategy)
complate

#now setting up compact letter display
cld<-glht(latemodel, linfct = mcp(Strategy ="Tukey"))
cld(cld)

# Post-harvest/winter season thresholds -------------------------------------------------

post <- percent %>%
  filter(Season %in% 
           "Non-growing season") 
View(post)

SumStats_post <- ddply(post, c("Strategy"), summarise,
                       n = length(`Percentage`),
                       mean = mean(`Percentage`),
                       sd = sd(`Percentage`),
                       se = sd / sqrt(n) )
SumStats_post

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
postmodel<-lm(`Percentage`~Strategy, data=post)
anova(postmodel) 
summary(postmodel)

#pairwise comparisons of thresholds among the different strategies for post harvest & winter seasons season 
comppost<-emmeans(postmodel,pairwise~Strategy)
comppost

#now setting up compact letter display
cld<-glht(postmodel, linfct = mcp(Strategy ="Tukey"))
cld(cld)

# Time percentage by season thresholds -------------------------------------------

#####
#now filtering by 1.5x IQR across all seasons

IQR <- percent %>%
  filter(Strategy %in% 
           "1.5 x IQR") #%>%
  #filter(!(Season %in% "Whole year"))
View(IQR)

SumStats_IQR <- ddply(IQR, c("Season"), summarise,
                      n = length(`Percentage`),
                      mean = mean(`Percentage`),
                      sd = sd(`Percentage`),
                      se = sd / sqrt(n) )
SumStats_IQR

#run one-way anova to compare thresholds among the different seasons 
#for the 1.5x IQR method (across all nodes and all chambers)
IQRmodel<-lm(`Percentage`~Season, data=IQR)
anova(IQRmodel) 
summary(IQRmodel)

#pairwise comparisons of thresholds among the different seasons for 
#the 1.5x IQR method 
compIQR<-emmeans(IQRmodel,pairwise~Season)
compIQR

#now setting up compact letter display
cld<-glht(IQRmodel, linfct = mcp(Season ="Tukey"))
cld(cld)

#####
#now filtering by IF across all seasons

IF <- percent %>%
  filter(Strategy %in% 
           "Isolation Forest") #%>%
  #filter(!(Season %in% "Whole year"))
View(IF)

SumStats_IF <- ddply(IF, c("Season"), summarise,
                     n = length(`Percentage`),
                     mean = mean(`Percentage`),
                     sd = sd(`Percentage`),
                     se = sd / sqrt(n) )
SumStats_IF

#run one-way anova to compare thresholds among the different seasons 
#for the 1.5x IQR method (across all nodes and all chambers)
IFmodel<-lm(`Percentage`~Season, data=IF)
anova(IFmodel) 
summary(IFmodel)

#pairwise comparisons of thresholds among the different seasons for 
#the 1.5x IQR method 
compIF<-emmeans(IFmodel,pairwise~Season)
compIF

#now setting up compact letter display
cld<-glht(IFmodel, linfct = mcp(Season ="Tukey"))
cld(cld)

#####
#now filtering by 4SD across all seasons

fourSD <- percent %>%
  filter(Strategy %in% 
           "4 SD")# %>%
  #filter(!(Season %in% "Whole year"))
View(fourSD)

SumStats_fourSD <- ddply(fourSD, c("Season"), summarise,
                         n = length(`Percentage`),
                         mean = mean(`Percentage`),
                         sd = sd(`Percentage`),
                         se = sd / sqrt(n) )
SumStats_fourSD

#run one-way anova to compare thresholds among the different seasons 
#for the 4 SD method (across all nodes and all chambers)
fourSDmodel<-lm(`Percentage`~Season, data=fourSD)
anova(fourSDmodel) 
summary(fourSDmodel)

#pairwise comparisons of thresholds among the different seasons for 
#the 4 SD method 
compfourSD<-emmeans(fourSDmodel,pairwise~Season)
compfourSD

#now setting up compact letter display
cld<-glht(fourSDmodel, linfct = mcp(Season ="Tukey"))
cld(cld)

#####
#now filtering by 2SD across all seasons

twoSD <- percent %>%
  filter(Strategy %in% 
           "2 SD")# %>%
#filter(!(Season %in% "Whole year"))
View(twoSD)

SumStats_twoSD <- ddply(twoSD, c("Season"), summarise,
                         n = length(`Percentage`),
                         mean = mean(`Percentage`),
                         sd = sd(`Percentage`),
                         se = sd / sqrt(n) )
SumStats_twoSD

#run one-way anova to compare thresholds among the different seasons 
#for the 2 SD method (across all nodes and all chambers)
twoSDmodel<-lm(`Percentage`~Season, data=twoSD)
anova(twoSDmodel) 
summary(twoSDmodel)

#pairwise comparisons of thresholds among the different seasons for 
#the 2 SD method 
comptwoSD<-emmeans(twoSDmodel,pairwise~Season)
comptwoSD

#now setting up compact letter display
cld<-glht(twoSDmodel, linfct = mcp(Season ="Tukey"))
cld(cld)

# Boxplot comparing amount of time that is hot moment by methods and seasons ---------------------

#re-ordering the strategies so plots are in same order as Jiacheng's plots
#percent$Strategy <- factor(percent$Strategy, levels = c("IQR", "iForest", "2SD", "4SD"),
                           #labels = c("1.5 x IQR", "Isolation Forest","2 SD","4 SD"))
#percent$Season <- factor(percent$Season, levels = c("Early-Growing", "Late-Growing", "Post-Harvest"),
                         #labels = c("Early growing season", "Late growing season", "Non-growing season"))
#View(percent)

#setting up the Dark2 palette so it accomodates monochromatic vision
# Extract the Dark2 palette colors
dark2_colors <- brewer.pal(n = 8, name = "Dark2")

# Manually reorder the colors
# Assuming that the lightest yellow-brown is the 6th color, the dark brown is the 7th, and the grey is the 8th.
# The pink can go next as the 4th color.
reordered_colors <- c(dark2_colors[6], dark2_colors[7], dark2_colors[8], 
                      dark2_colors[4])

# Select the last three colors
#last_three_colors <- tail(dark2_colors, 3)

# Use scale_fill_manual to apply these colors
#scale_fill_manual(values = last_three_colors)

percenthist <- ggplot(percent) +
  aes(x = Season, y = Percentage, fill = Season) +
  geom_boxplot() +
  scale_fill_manual(values = last_three_colors) +
  theme_classic() +
  labs( y = expression(bold(paste("Time in each season that \n   are hot moments (%)"))),
        x = expression(bold(paste("Season")))) +
  theme(
    plot.title = element_text(size = 16L, hjust = 0.5),
    plot.subtitle = element_text(size = 16L,hjust = 0.5, color = "black"),
    plot.caption = element_text(size = 16L, hjust = 0.5, color = "black"),
    axis.title.y = element_text(size = 20L,hjust = 0.5, face = "bold", color = "black"),
    axis.title.x = element_text(size = 20L,face = "bold", color = "black"),
    axis.text.y = element_text(size = 15L, color = "black"),
    axis.text.x = element_text(size = 15L, angle = 27L,hjust = 1L,vjust = 1.1, color = "black"),
    legend.text = element_text(size = 12L, color = "black"),
    legend.title = element_text(size = 12L, color = "black"),
    strip.text = element_text(size = 16, face = "bold", color = "black"),
    plot.margin = unit(c(1, 1, 1, 5), "cm")  # Adjust plot margins if needed
  ) +
  facet_wrap(vars(Strategy), nrow = 1) #got rid of free y scale so we could more easily see that 2 SD vs. 4 SD doesn't really solve the selectivity issue
percenthist

ggsave("time percent histogram_strategies&seasons.pdf",percenthist,device="pdf",
       path= "/Users/emstuch/Desktop/Reinhart study/Paper drafts/Results/Figures", 
       width=14, height=7, dpi=1000)


# Reading in contrib to cum. flux all seasons vs. whole year --------

#reading in data
seasonsvswhole<-read_excel("/Users/emstuch/Desktop/Reinhart study/R scripts/Associated Excel Files/finalizeddata_all_data repository.xlsx", sheet = "contribcumflux_wholevsseasonsum")
View(seasonsvswhole)
str(seasonsvswhole)
seasonsvswhole$Node<-as.factor(seasonsvswhole$Node)
seasonsvswhole$Chamber<-as.factor(seasonsvswhole$Chamber)
seasonsvswhole$Season<-as.factor(seasonsvswhole$Season)
seasonsvswhole$Strategy<-as.factor(seasonsvswhole$Strategy)
str(seasonsvswhole)
View(seasonsvswhole)

#####

#Sum Stats 
SumStats_seasonvswhole <- ddply(seasonsvswhole, c("Season"), summarise,
                        n = length(`Flux Contribution (%)`),
                        mean = mean(`Flux Contribution (%)`),
                        sd = sd(`Flux Contribution (%)`),
                        se = sd / sqrt(n) )
SumStats_seasonvswhole

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
seasonvswholemodel<-lm(`Flux Contribution (%)`~Season:Strategy, data=seasonsvswhole)
anova(seasonvswholemodel) 
summary(seasonvswholemodel)

#pairwise comparisons of thresholds among the different strategies for early season 
compseasonvswhole<-emmeans(seasonvswholemodel,pairwise~Strategy|Season)
compseasonvswhole


#now setting up compact letter display
cld<-glht(seasonvswholemodel, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#####
wholeyear <- seasonsvswhole %>%
  filter(Season %in% 
           "Whole year") 
View(wholeyear)

SumStats_wholeyear <- ddply(wholeyear, c("Strategy"), summarise,
                            n = length(`Flux Contribution (%)`),
                            mean = mean(`Flux Contribution (%)`),
                            sd = sd(`Flux Contribution (%)`),
                            se = sd / sqrt(n) )
SumStats_wholeyear

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
wholeyearmodel<-lm(`Flux Contribution (%)`~Strategy, data=wholeyear)
anova(wholeyearmodel) 
summary(wholeyearmodel)

#pairwise comparisons of thresholds among the different strategies for whole year 
compwholeyear<-emmeans(wholeyearmodel,pairwise~Strategy)
compwholeyear

#now setting up compact letter display
cld<-glht(wholeyearmodel, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#####
seasons <- seasonsvswhole %>%
  filter(Season %in% 
           "All seasons added") 
View(seasons)

SumStats_seasons <- ddply(seasons, c("Strategy"), summarise,
                            n = length(`Flux Contribution (%)`),
                            mean = mean(`Flux Contribution (%)`),
                            sd = sd(`Flux Contribution (%)`),
                            se = sd / sqrt(n) )
SumStats_seasons

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
seasonsmodel<-lm(`Flux Contribution (%)`~Strategy, data=seasons)
anova(seasonsmodel) 
summary(seasonsmodel)

#pairwise comparisons of thresholds among the different strategies for whole year 
compseasons<-emmeans(seasonsmodel,pairwise~Strategy)
compseasons

#now setting up compact letter display
cld<-glht(seasonsmodel, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#####
IQR <- seasonsvswhole %>%
  filter(Strategy %in% 
           "IQR") 
View(IQR)

SumStats_IQR <- ddply(IQR, c("Season"), summarise,
                       n = length(`Flux Contribution (%)`),
                       mean = mean(`Flux Contribution (%)`),
                       sd = sd(`Flux Contribution (%)`),
                       se = sd / sqrt(n) )
SumStats_IQR

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
IQRmodel<-lm(`Flux Contribution (%)`~Season, data=IQR)
anova(IQRmodel) 
summary(IQRmodel)

#pairwise comparisons of thresholds among the different strategies for early season 
compIQR<-emmeans(IQRmodel,pairwise~Season)
compIQR

#now setting up compact letter display
cld<-glht(IQRmodel, linfct = mcp(Season ="Tukey"))
cld(cld)

#####
IF <- seasonsvswhole %>%
  filter(Strategy %in% 
           "IF") 
View(IF)

SumStats_IF <- ddply(IF, c("Season"), summarise,
                      n = length(`Flux Contribution (%)`),
                      mean = mean(`Flux Contribution (%)`),
                      sd = sd(`Flux Contribution (%)`),
                      se = sd / sqrt(n) )
SumStats_IF

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
IFmodel<-lm(`Flux Contribution (%)`~Season, data=IF)
anova(IFmodel) 
summary(IFmodel)

#pairwise comparisons of thresholds among the different strategies for early season 
compIF<-emmeans(IFmodel,pairwise~Season)
compIF

#now setting up compact letter display
cld<-glht(IFmodel, linfct = mcp(Season ="Tukey"))
cld(cld)

#####
SD <- seasonsvswhole %>%
  filter(Strategy %in% 
           "4SD") 
View(SD)

SumStats_SD <- ddply(SD, c("Season"), summarise,
                     n = length(`Flux Contribution (%)`),
                     mean = mean(`Flux Contribution (%)`),
                     sd = sd(`Flux Contribution (%)`),
                     se = sd / sqrt(n) )
SumStats_SD

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
SDmodel<-lm(`Flux Contribution (%)`~Season, data=SD)
anova(SDmodel) 
summary(SDmodel)

#pairwise comparisons of thresholds among the different strategies for early season 
compSD<-emmeans(SDmodel,pairwise~Season)
compSD

#now setting up compact letter display
cld<-glht(SDmodel, linfct = mcp(Season ="Tukey"))
cld(cld)

#####
twoSD <- seasonsvswhole %>%
  filter(Strategy %in% 
           "2SD") 
View(twoSD)

SumStats_twoSD <- ddply(twoSD, c("Season"), summarise,
                     n = length(`Flux Contribution (%)`),
                     mean = mean(`Flux Contribution (%)`),
                     sd = sd(`Flux Contribution (%)`),
                     se = sd / sqrt(n) )
SumStats_twoSD

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
twoSDmodel<-lm(`Flux Contribution (%)`~Season, data=twoSD)
anova(twoSDmodel) 
summary(twoSDmodel)

#pairwise comparisons of thresholds among the different strategies for early season 
comptwoSD<-emmeans(twoSDmodel,pairwise~Season)
comptwoSD

#now setting up compact letter display
cld<-glht(twoSDmodel, linfct = mcp(Season ="Tukey"))
cld(cld)

# Boxplot comparing amount of time that is hot moment by methods and seasons ---------------------

#re-ordering the strategies so plots are in same order as Jiacheng's plots
thresh$Strategy <- factor(thresh$Strategy, levels = c("IQR", "IF","2SD" ,"4SD"),
                          labels = c("1.5 x IQR", "Isolation Forest","2 SD","4 SD"))
thresh$Season <- factor(thresh$Season, levels = c("Early growing season", "Late growing season", "Post harvest & winter", "Whole Year", "All seasons added"),
                        labels = c("Early growing season", "Late growing season", "Non-growing season", "Whole year", "All seasons summed"))
View(thresh)

#setting up the Dark2 palette so it accommodates monochromatic vision
# Extract the Dark2 palette colors
dark2_colors <- brewer.pal(n = 8, name = "Dark2")

# Manually reorder the colors
# Assuming that the lightest yellow-brown is the 6th color, the dark brown is the 8th, and the grey is the 1st.
# The green and pink can go next as the 5th and 7th colors respectively.
reordered_colors <- c(dark2_colors[6], dark2_colors[7], dark2_colors[8], 
                      dark2_colors[4], dark2_colors[5])


# Use scale_fill_manual to apply these colors
# e.g., scale_fill_manual(values = last_five_colors)

contribhist <- ggplot(thresh) +
  aes(x = Season, y = `Flux Contribution`, fill = Season) +
  geom_boxplot() +
  scale_fill_manual(values = reordered_colors) +
  theme_classic() +
  labs(y = expression(bold(paste("Flux Contribution (%)")))) +
  theme(
    plot.title = element_text(size = 16L, hjust = 0.5),
    plot.subtitle = element_text(size = 16L,hjust = 0.5, color = "black"),
    plot.caption = element_text(size = 16L, hjust = 0.5, color = "black"),
    axis.title.y = element_text(size = 20L,face = "bold", color = "black"),
    axis.title.x = element_text(size = 20L,face = "bold", color = "black", vjust = 1L),
    axis.text.y = element_text(size = 15L, color = "black"),
    axis.text.x = element_text(size = 15L, angle = 27L,hjust = 1L,vjust = 1L, color = "black"),
    legend.text = element_text(size = 12L, color = "black"),
    legend.title = element_text(size = 12L, color = "black"),
    strip.text = element_text(size = 16, face = "bold", color = "black"),
    plot.margin = unit(c(1, 1, 1, 2.3), "cm")  # Adjust plot margins if needed
  ) +
  facet_wrap(vars(Strategy), scales = "free_y", nrow = 1)
contribhist

ggsave("hot moment contrib histogram_strategies&seasons.pdf",contribhist,device="pdf",
       path= "/Users/emstuch/Desktop/Reinhart study/Paper drafts/Results/Figures", 
       width=14, height=7, dpi=1000)


