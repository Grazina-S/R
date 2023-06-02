DMY$year <- as.factor(DMY$year)
DMY$rep <- as.factor(DMY$rep)
DMY$type <- as.factor(DMY$type)
library(agricolae)
ano_model <- aov(DMY ~ year + type, data = DMY)
anova_DMY <- summary(ano_model)
DMY_aov_result <- as.data.frame(anova_DMY[[1]])

ano_model2 <- aov(DMY ~ year * type, data = DMY)
summary.aov(ano_model2)
#kitas budas gauti rezultatus tekste
capture.output(anova_DMY, file = "anova_dmy.txt")

anova_dmy2 <- summary(ano_model2)
capture.output(anova_dmy2, file = "anova_dmy2.txt")

hsd_dmy <- HSD.test(ano_model2, c("year", "type"), group = TRUE, console = TRUE)
capture.output(hsd_dmy, file = "hsd_dmy.txt")

library(tidyverse)
ggplot(DMY, aes(year, DMY)) + geom_boxplot() + stat_summary(fun=mean, geom = "point", shape=16, size=3) + facet_grid(~ type)
ggplot(DMY, aes(type, DMY)) + geom_boxplot() + stat_summary(fun=mean, geom = "point", shape=16, size=3) + facet_grid(~ year)

install.packages("bestNormalize")
library(bestNormalize)

normal_dmy <- bestNormalize(DMY$DMY)
DMY$normalDMY <- normal_dmy$x.t
ggplot(DMY, aes(type, normalDMY)) + geom_boxplot() + stat_summary(fun=mean, geom = "point", shape=16, size=3) + facet_grid(~ year)

#perdelioju lentele

DMY2 <- DMY %>% pivot_wider(names_from = c(year, type), values_from = c(DMY, normalDMY))
shapiro.test(DMY2$DMY_2019_natural)
shapiro.test(DMY2$normalDMY_2019_natural) 
shapiro.test(DMY2$normalDMY_2019_cultivated)                            
shapiro.test(DMY2$normalDMY_2020_natural)
shapiro.test(DMY2$normalDMY_2020_cultivated)

#nu va dabar normalus duomenys, kazi ar pasikeis analizes rezultatas
anova_normal <- aov(normalDMY ~ year*type, data = DMY)
summary.aov(anova_normal)
#vistiek tik tarp tipu reiksmingi skirtumai
hsd_normal <- HSD.test(anova_normal, c("year", "type"), group = T, console = T)
#lygiai tas pats gavosi
t.test(DMY2$normalDMY_2019_natural, DMY2$normalDMY_2019_cultivated)
#nu tikrai 2019 tarp cultivated ir natural tipo nereiksmingas skirtumas. Bet p = 0.05273

ggplot(DMY, aes(x = year, y = DMY, fill = type)) + geom_col(position = "dodge")
library(esquisse)
esquisser(DMY)

library(ggplot2)

ggplot(DMY) +
 aes(x = year, fill = type, weight = DMY) +
 geom_bar(position = "dodge", color = "black") +
 scale_fill_manual(values = c(cultivated = "green", 
natural = "lightblue")) +
 labs(y = "DMY") +
 theme_minimal() +
 theme(legend.position = "top")
normal_stats <- hsd_normal$means
rm(nor)
stats_dmy <- hsd_dmy$means
stats_dmy$year <- c("2019", "2019", "2020", "2020")
stats_dmy$type <- c("cultivated", "natural", "cultivated", "natural")
ggplot(stats_dmy) +
  aes(x = year, fill = type, weight = DMY) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c(cultivated = "green", 
                               natural = "lightblue")) +
  labs(y = "DMY") +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_errorbar(aes(ymin = DMY - std, ymax = DMY + std), width = .2)


install.packages("rstatix")
library(rstatix)
dmy_stats <- DMY %>% group_by(year, type) %>% get_summary_stats(DMY, type = "mean_se")
DMY %>% group_by(year, type) %>% get_summary_stats(DMY, type = "mean_sd")

ggplot(dmy_stats, aes(x = year, fill = type, y = mean) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c(cultivated = "green", 
                               natural = "lightblue")) +
  labs(y = "DMY") +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2)
  colnames(dmy_stats)

  ggplot(dmy_stats, aes(x = year, y = mean, fill = type)) + geom_col(position = "dodge", color = "black") +
    scale_fill_manual(values = c(cultivated = "green", natural = "lightblue")) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  position = position_dodge(0.9), width = .2) +
    labs(y = "DMY", x = "Year") + theme_minimal() + theme(legend.position = "top")
  
  shapiro.test(FMY$FMY)
#kaip ir maniau duomenys ziauriai nenormalus
  normalFMY <- bestNormalize(FMY$FMY)
  FMY$normalFMY <- normalFMY$x.t
anova_fmy <- aov(normalFMY ~ year + type + base, data = FMY)  
summary.aov(anova_fmy)
anova_fmy2 <- aov(normalFMY ~ year * type * base, data = FMY)  
summary.aov(anova_fmy2)

