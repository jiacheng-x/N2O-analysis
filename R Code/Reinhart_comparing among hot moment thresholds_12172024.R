# Reading in necessary packages--------------------------------------------

library(plyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(emmeans)
library(readxl)
library(multcompView)
library(multcomp)
library(car)


# Reading in threshold data -----------------------------------------------
thresh<-read_excel("/Users/emstuch/Desktop/Reinhart study/R scripts/Associated Excel Files/finalizeddata_all_data repository.xlsx", sheet = "threshold comps_seasonsandyear")
View(thresh)
str(thresh)
thresh$Node<-as.factor(thresh$Node)
thresh$Chamber<-as.factor(thresh$Chamber)
thresh$Season<-as.factor(thresh$Season)
thresh$Strategy<-as.factor(thresh$Strategy)
str(thresh)
View(thresh)


# Early season thresholds -------------------------------------------------

#####
#filter data to include only early season threshold values

early <- thresh %>%
  filter(Season %in% 
           "Early growing season") 
View(early)

SumStats_early <- ddply(early, c("Strategy"), summarise,
                        n = length(`Threshold`),
                        mean = mean(`Threshold`),
                        sd = sd(`Threshold`),
                        se = sd / sqrt(n) )
SumStats_early

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
earlymodel<-lm(Threshold~Strategy, data=early)
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
earlymodel2<-lm(Threshold~Strategy, data=early2)
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
earlymodel2<-lm(Threshold~Strategy, data=early2)
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
earlymodel2<-lm(Threshold~Strategy, data=early2)
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
earlymodel2<-lm(Threshold~Strategy, data=early2)
anova(earlymodel2) 
summary(earlymodel2)

#pairwise comparisons of thresholds among the different strategies for early season 
compearly2<-emmeans(earlymodel2,pairwise~Strategy)
compearly2

#now setting up compact letter display
cldn4<-glht(earlymodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn4)

# Late season thresholds -------------------------------------------------

#####
#filter data to include only late season threshold values

late <- thresh %>%
  filter(Season %in% 
           "Late growing season") 
View(late)

SumStats_late <- ddply(late, c("Strategy"), summarise,
                        n = length(`Threshold`),
                        mean = mean(`Threshold`),
                        sd = sd(`Threshold`),
                        se = sd / sqrt(n) )
SumStats_late

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
latemodel<-lm(Threshold~Strategy, data=late)
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
latemodel2<-lm(Threshold~Strategy, data=late2)
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
latemodel2<-lm(Threshold~Strategy, data=late2)
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
latemodel2<-lm(Threshold~Strategy, data=late2)
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
latemodel2<-lm(Threshold~Strategy, data=late2)
anova(latemodel2) 
summary(latemodel2)

#pairwise comparisons of thresholds among the different strategies for late season 
complate2<-emmeans(latemodel2,pairwise~Strategy)
complate2

#now setting up compact letter display
cldn4<-glht(latemodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn4)

# Post-harvest/winter season thresholds -------------------------------------------------

#####
#filter data to include only post harvest & winter season threshold values

post <- thresh %>%
  filter(Season %in% 
           "Post harvest & winter") 
View(post)

SumStats_post <- ddply(post, c("Strategy"), summarise,
                       n = length(`Threshold`),
                       mean = mean(`Threshold`),
                       sd = sd(`Threshold`),
                       se = sd / sqrt(n) )
SumStats_post

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
postmodel<-lm(Threshold~Strategy, data=post)
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
postmodel2<-lm(Threshold~Strategy, data=post2)
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
postmodel2<-lm(Threshold~Strategy, data=post2)
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
postmodel2<-lm(Threshold~Strategy, data=post2)
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
postmodel2<-lm(Threshold~Strategy, data=post2)
anova(postmodel2) 
summary(postmodel2)

#pairwise comparisons of thresholds among the different strategies for post harvest & winter seasons 
comppost2<-emmeans(postmodel2,pairwise~Strategy)
comppost2

#now setting up compact letter display
cldn4<-glht(postmodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn4)

# Whole year thresholds -------------------------------------------------

#####
#filter data to include only whole year threshold values

wholeyear <- thresh %>%
  filter(Season %in% 
           "Whole Year") 
View(wholeyear)

SumStats_wholeyear <- ddply(wholeyear, c("Strategy"), summarise,
                        n = length(`Threshold`),
                        mean = mean(`Threshold`),
                        sd = sd(`Threshold`),
                        se = sd / sqrt(n) )
SumStats_wholeyear

#run one-way anova to compare thresholds among strategies (across all nodes and all chambers)
wholeyearmodel<-lm(Threshold~Strategy, data=wholeyear)
anova(wholeyearmodel) 
summary(wholeyearmodel)

#pairwise comparisons of thresholds among the different strategies for whole year 
compwholeyear<-emmeans(wholeyearmodel,pairwise~Strategy)
compwholeyear

#now setting up compact letter display
cld<-glht(wholeyearmodel, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#####
#filter data to include only whole year and node 1

wholeyear2 <- thresh %>%
  filter(Season %in% 
           "Whole year") %>%
  filter(Node %in% "Node1")
#View(wholeyear2)

#run one-way anova to compare thresholds among strategies (across only node 1 chambers)
wholeyearmodel2<-lm(Threshold~Strategy, data=wholeyear2)
anova(wholeyearmodel2) 
summary(wholeyearmodel2)

#pairwise comparisons of thresholds among the different strategies for whole year 
compwholeyear2<-emmeans(wholeyearmodel2,pairwise~Strategy)
compwholeyear2

#now setting up compact letter display
cldn1<-glht(wholeyearmodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn1)

#####
#filter data to include only whole year and node 2

wholeyear2 <- thresh %>%
  filter(Season %in% 
           "Whole year") %>%
  filter(Node %in% "Node2")
#View(wholeyear2)

#run one-way anova to compare thresholds among strategies (across only node 2 chambers)
wholeyearmodel2<-lm(Threshold~Strategy, data=wholeyear2)
anova(wholeyearmodel2) 
summary(wholeyearmodel2)

#pairwise comparisons of thresholds among the different strategies for whole year 
compwholeyear2<-emmeans(wholeyearmodel2,pairwise~Strategy)
compwholeyear2

#now setting up compact letter display
cldn2<-glht(wholeyearmodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn2)

#####
#filter data to include only whole year and node 3

wholeyear2 <- thresh %>%
  filter(Season %in% 
           "Whole year") %>%
  filter(Node %in% "Node3")
#View(wholeyear2)

#run one-way anova to compare thresholds among strategies (across only node 3 chambers)
wholeyearmodel2<-lm(Threshold~Strategy, data=wholeyear2)
anova(wholeyearmodel2) 
summary(wholeyearmodel2)

#pairwise comparisons of thresholds among the different strategies for whole year 
compwholeyear2<-emmeans(wholeyearmodel2,pairwise~Strategy)
compwholeyear2

#now setting up compact letter display
cldn3<-glht(wholeyearmodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn3)

#####
#filter data to include only whole year and node 4

wholeyear2 <- thresh %>%
  filter(Season %in% 
           "Whole year") %>%
  filter(Node %in% "Node4")
#View(wholeyear2)

#run one-way anova to compare thresholds among strategies (across only node 4 chambers)
wholeyearmodel2<-lm(Threshold~Strategy, data=wholeyear2)
anova(wholeyearmodel2) 
summary(wholeyearmodel2)

#pairwise comparisons of thresholds among the different strategies for whole year 
compwholeyear2<-emmeans(wholeyearmodel2,pairwise~Strategy)
compwholeyear2

#now setting up compact letter display
cldn4<-glht(wholeyearmodel2, linfct = mcp(Strategy ="Tukey"))
cld(cldn4)


# Strategy by season thresholds -------------------------------------------

#####
#now filtering by 1.5x IQR across all seasons

IQR <- thresh %>%
  filter(Strategy %in% 
           "IQR")# %>%
  #filter(!(Season %in% "Whole year"))
View(IQR)

SumStats_IQR <- ddply(IQR, c("Season"), summarise,
                            n = length(`Threshold`),
                            mean = mean(`Threshold`),
                            sd = sd(`Threshold`),
                            se = sd / sqrt(n) )
SumStats_IQR

#run one-way anova to compare thresholds among the different seasons 
#for the 1.5x IQR method (across all nodes and all chambers)
IQRmodel<-lm(Threshold~Season, data=IQR)
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
                      n = length(`Threshold`),
                      mean = mean(`Threshold`),
                      sd = sd(`Threshold`),
                      se = sd / sqrt(n) )
SumStats_IF

#run one-way anova to compare thresholds among the different seasons 
#for the 1.5x IQR method (across all nodes and all chambers)
IFmodel<-lm(Threshold~Season, data=IF)
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
           "4SD")# %>%
  #filter(!(Season %in% "Whole year"))
View(fourSD)

SumStats_fourSD <- ddply(fourSD, c("Season"), summarise,
                     n = length(`Threshold`),
                     mean = mean(`Threshold`),
                     sd = sd(`Threshold`),
                     se = sd / sqrt(n) )
SumStats_fourSD

#run one-way anova to compare thresholds among the different seasons 
#for the 1.5x IQR method (across all nodes and all chambers)
fourSDmodel<-lm(Threshold~Season, data=fourSD)
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
           "2SD")# %>%
#filter(!(Season %in% "Whole year"))
View(twoSD)

SumStats_twoSD <- ddply(twoSD, c("Season"), summarise,
                        n = length(`Threshold`),
                        mean = mean(`Threshold`),
                        sd = sd(`Threshold`),
                        se = sd / sqrt(n) )
SumStats_twoSD

#run one-way anova to compare thresholds among the different seasons 
#for the 2SD method (across all nodes and all chambers)
twoSDmodel<-lm(Threshold~Season, data=twoSD)
anova(twoSDmodel) 
summary(twoSDmodel)

#pairwise comparisons of thresholds among the different seasons for 
#the 2SD method 
comptwoSD<-emmeans(twoSDmodel,pairwise~Season)
comptwoSD

#now setting up compact letter display
cld<-glht(twoSDmodel, linfct = mcp(Season ="Tukey"))
cld(cld)

# Boxplot comparing thresholds by methods and seasons ---------------------

#re-ordering the strategies so plots are in same order as Jiacheng's plots
thresh$Strategy <- factor(thresh$Strategy, levels = c("IQR", "IF", "2SD" ,"4SD"),
                           labels = c("1.5 x IQR", "Isolation Forest", "2 SD", "4 SD"))
thresh$Season <- factor(thresh$Season, levels = c("Early growing season", "Late growing season", "Post harvest & winter", "Whole Year"),
                         labels = c("Early growing season", "Late growing season", "Non-growing season", "Whole year"))
View(thresh)

#setting up the Dark2 palette so it accommodates monochromatic vision
# Extract the Dark2 palette colors
dark2_colors <- brewer.pal(n = 8, name = "Dark2")

# Manually reorder the colors
# Assuming that the lightest yellow-brown is the 6th color, the dark brown is the 7th, and the grey is the 8th.
# The pink can go next as the 4th color.
reordered_colors <- c(dark2_colors[6], dark2_colors[7], dark2_colors[8], 
                      dark2_colors[4])

# Use scale_fill_manual to apply these colors
# e.g., scale_fill_manual(values = last_five_colors)

threshist <- ggplot(thresh) +
  aes(x = Season, y = Threshold, fill = Season) +
  geom_boxplot() +
  scale_fill_manual(values = reordered_colors) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, hjust = 0.5),
    plot.subtitle = element_text(size = 16L,hjust = 0.5, color = "black"),
    plot.caption = element_text(size = 16L, hjust = 0.5, color = "black"),
    axis.title.y = element_text(size = 20L,face = "bold", color = "black"),
    axis.title.x = element_text(size = 20L,face = "bold", color = "black"),
    axis.text.y = element_text(size = 15L, color = "black"),
    axis.text.x = element_text(size = 15L, angle = 27L,hjust = 1L,vjust = 1.1, color = "black"),
    legend.text = element_text(size = 12L, color = "black"),
    legend.title = element_text(size = 12L, color = "black"),
    strip.text = element_text(size = 16, face = "bold", color = "black"),
    plot.margin = unit(c(1, 1, 1, 2.3), "cm")  # Adjust plot margins if needed
  ) +
  facet_wrap(vars(Strategy), scales = "free_y", nrow = 1) 
threshist

ggsave("threshold histogram_strategies&seasons.pdf",threshist,device="pdf",
       path= "/Users/emstuch/Desktop/Reinhart study/Paper drafts/Results/Figures", 
       width=12, height=5, dpi=1000)







