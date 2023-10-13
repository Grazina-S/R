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
fmy_hsd <- HSD.test(anova_fmy2, c("year", "type", "base"), group = T, console = T)
fmy_2019 <- FMY %>% subset(year == "2019")
fmy_2020 <- FMY %>% subset(year == "2020")
t.test(fmy_2019$normalFMY, fmy_2020$normalFMY)

data("ToothGrowth")
df <- ToothGrowth
df %>% t_test(len ~ supp)
FMY %>% t_test(normalFMY ~ year)
#bendrai paemus 2019 derlius nesiskiria nuo 2020

FMY %>% group_by(type) %>% t_test(normalFMY ~ year, p.adjust.method = "none")
#tas pats ir sugrupavus pagal tipa

FMY %>% group_by(type, base) %>% t_test(normalFMY ~ year, p.adjust.method = "none")
?citation
citation(package = "rstatix")
FMY %>% group_by(type, year) %>% t_test(normalFMY ~ base, p.adjust.method = "bonferroni")

FMY %>% group_by(year) %>% t_test(normalFMY ~ type)

fmy_stats <- FMY %>% group_by(year, type, base) %>% get_summary_stats(FMY, type = "mean_se")

ggplot(fmy_stats, aes(x= base, y = mean, fill = type)) + geom_col(position = "dodge", color = "black") +
  scale_fill_manual(values = c(cultivated = "green", natural = "lightblue")) + 
  labs(y = "FMY", x = NULL) + facet_wrap( ~ year) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9), width = .2) +
  theme_minimal() + theme(legend.position = "bottom", panel.background = element_rect(fill = "white"))
write.table(fmy_stats, file = "fmy_stats.txt", row.names = F)

anova_2019 <- aov(normalFMY ~ type * base, data = fmy_2019)
hsd_2019 <- HSD.test(anova_2019, c("type", "base"), group = T, console = T)

kokybe$year <- as.factor(kokybe$year)
kokybe$type <- as.factor(kokybe$type)

shapiro.test(kokybe$CP)
shapiro.test(kokybe$NDF)
#abu nenormalus
library(bestNormalize)

normalCP <- bestNormalize(kokybe$CP)
normalNDF <- bestNormalize(kokybe$NDF)

kokybe$normalCP <- normalCP$x.t
kokybe$normalNDF <- normalNDF$x.t

CP_anova <- aov(normalCP ~ year*type, data = kokybe)
summary.aov(CP_anova)
kokybe_stats <- kokybe %>% group_by(year, type) %>% get_summary_stats(show  = c("mean", "se", "sd"))

cp_stats <- kokybe_stats %>% subset(variable == "CP")
ggplot(cp_stats, aes(x = year, y = mean, fill = type)) + geom_col(color = "black", position = "dodge") +
  scale_fill_manual(values = c(cultivated = "green", natural = "lightblue")) + 
  labs(y = "Crude protein", x = "Year") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9), width = .2) +
  theme_minimal() + theme(legend.position = "bottom")

NDF_anova <- aov(normalNDF ~ year*type, data = kokybe)
summary.aov(NDF_anova)
ndfHSD <- HSD.test(NDF_anova, c("year", "type"), group = T, console = T, unbalanced = T)
ndf_stats <- kokybe_stats %>% subset(variable == "NDF")
ggplot(ndf_stats, aes(x = year, y = mean, fill = type)) + geom_col(color = "black", position = "dodge") +
  scale_fill_manual(values = c(cultivated = "green", natural = "lightblue")) + 
  labs(y = "NDF", x = "Year") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9), width = .2) +
  theme_minimal() + theme(legend.position = "bottom")

#biski neteisingai buvau supratus FMY lentele. Reik bandyt perdeliot
FMY$normalFMY <- NULL
newFMY <- FMY %>%  pivot_wider(names_from = base, values_from = FMY)
# gaidys gaunas, suplaka i lista visus FMY, nes nera jokio ID stulpelio ir traktuoja kaip duplikatus
FMY <- FMY %>% mutate(ID = row_number())
ewFMY <- FMY %>%  pivot_wider(names_from = base, values_from = FMY)
#ne ir taip negerai
rm(ewFMY)
FMY$ID <- NULL
derlius$year <- as.factor(derlius$year)
derlius$rep <- as.factor(derlius$rep)
derlius$type <- as.factor(derlius$type)

derlius %>% group_by(year, type) %>% cor_test(vars = "FMY", vars2 = c("legumes", "grasses", "herbs", "sedges"))
#rezultatai ne baisiai vertingi gavos
#bandau perskaiciuoti procentus
 # fmy 100 legumes x. x = legumes * 100 / fmy
derlius <- derlius %>% mutate(leg_perc = legumes*100/FMY, gras_perc = grasses*100/FMY, herb_perc = herbs*100/FMY, sedg_perc = sedges*100/FMY)
derlius %>% group_by(year, type) %>% cor_test(vars = "FMY", vars2 = c("leg_perc", "gras_perc", "herb_perc", "sedg_perc"))
ggplot(derlius, aes(x = DMY, y = FMY,  fill = type)) + 
  geom_point(size = 3, shape = 21) + facet_wrap( ~ year) + scale_fill_manual(values = c(cultivated = "green", natural = "lightblue")) + 
  theme_bw()

derlius %>% group_by(year, type) %>% cor_test(vars = "DMY", vars2 = c("leg_perc", "gras_perc", "herb_perc", "sedg_perc"))
#FMY kaip boxplot
 FMY <- FMY %>% unite("yeartype", year, type, sep = "_", remove = FALSE)
totFMY_fig <- ggplot(FMY, aes(x = yeartype, y = FMY)) + geom_boxplot() +
  stat_summary(fun=mean, geom = "point", shape=16, size=3)

anova_fmy <- aov(normalFMY ~ year * type , data = FMY)
summary.aov(anova_fmy)
hsd_fmy <- HSD.test(anova_fmy, c("year", "type"), console = T, group = T)

# NEVEIKIA totFMY_fig + theme_bw() + geom_text(aes(label = c("a", "b", "a", "b"), y = max(FMY) + 0.5),
           #                         position = position_dodge(width = 0.75),
            #                        vjust = -0.5, size = 4)
FMY %>% group_by(yeartype) %>% get_summary_stats(FMY, show = "max")
 labels_fig1 <- data.frame(yeartype = c("2019_cultivated", "2019_natural", "2020_cultivated", "2020_natural"),
                              max = c(3.02, 1.61, 1.6, 1.72),
                              labels = c("a", "b", "a", "b"))
 totFMY_fig <- totFMY_fig + theme_bw() + geom_text(data = labels_fig1, aes(label = labels, y = max + 0.5),
                                     position = position_dodge(width = 0.75),
                                     vjust = -0.5, size = 5)
 #sitaip pavyko uzdet patikimumus
totFMY_fig <- totFMY_fig + labs(x = NULL) + scale_x_discrete(labels = c("2019\ncultivated", "2019\nnatural", "2020\ncultivated", "2020\nnatural")) 
 

#____________________________________________________ ATNAUJINTI DUOMENYS NATURALIOS PIEVOS 2023-10-09

naturaliosv2$distr <- as.factor(naturaliosv2$distr)
naturaliosv2$year <- as.factor(naturaliosv2$year)
nat_anov <- aov(DMY1~  year  * distr, data = naturaliosv2)
summary.aov(nat_anov)

nat_hsd <- HSD.test(nat_anov, c("year", "distr"), group = TRUE, console = TRUE)
shapiro.test(naturaliosv2$DMY1)

normDMY <- bestNormalize(naturaliosv2$DMY1)
shapiro.test(normDMY$x.t)


hist(naturaliosv2$DMY1)
summary(normDMY)

# nieks cia nesinormalizuoja, is paziuros histograma atrodo kaip kupranugaris

naturalios19 <- naturaliosv2 %>% filter(year == "2019")
hist(naturalios19$DMY1)
shapiro.test(naturalios19$DMY1)
dmy19norm <- bestNormalize(naturalios19$DMY1)
shapiro.test(dmy19norm$x.t)
# susinormalizavo

naturalios20 <- naturaliosv2 %>% filter(year == "2020")
shapiro.test(naturalios20$DMY1)
hist(naturalios20$DMY1)
dmy20norm <- bestNormalize(naturalios20$DMY1)
shapiro.test(dmy20norm$x.t)
# dar pablogejo. Nenormalizuoju, bus gerai kaip yra

rm(dmy19norm, dmy20norm)
anov19 <- aov(DMY1 ~ distr, data = naturalios19)
summary.aov(anov19)
# nesiskiria

anov20 <- aov(DMY1 ~ distr, data = naturalios20)
summary.aov(anov20)

#skiriasi su p < 0.00001
nat_hsd$means

nat_statistics <- as.data.frame(nat_hsd$means) # metai:rajonas pasidare index. Negerai
row.names(nat_statistics)
nat_statistics <- cbind(year_distr = rownames(nat_statistics), nat_statistics)
row.names(nat_statistics) <- NULL
nat_hsd$groups
nat_groups <- nat_hsd$groups
nat_groups <- cbind(year_distr = rownames(nat_groups), nat_groups)
row.names(nat_groups) <- NULL
nat_groups <- nat_groups %>% arrange(year_distr) # surikiavau kaip reikia

nat_statistics$groups <- nat_groups$groups

nat_fig <- ggplot(nat_statistics, aes(x = year_distr, y = DMY1)) +
  geom_col(fill = c("lightblue", "lightblue", "lightblue", "lightblue", "springgreen", "springgreen", "springgreen", "springgreen"), colour = c("black")) +
             geom_errorbar(aes(ymin = DMY1 - se, ymax = DMY1 + se), width = .2) +
             theme_bw()
  
nat_fig
nat_fig <- nat_fig + geom_text(data = nat_statistics, aes(label = groups, y = DMY1 + 0.25),
                    vjust = -0.5,
                    size = 5)
nat_fig
nat_fig <- nat_fig + labs(x = NULL, y = bquote("Dry matter yield, t ha"^"-1")) + scale_x_discrete(labels = c("Kėdainiai", "Lazdijai", "Šilalė\n\n2019", "Zarasai",
                                                       "Kėdainiai", "Lazdijai", "Šilalė\n\n2020", "Zarasai"))
nat_fig
# Nu va gavos kaip norejau

distr_anov <- aov(DMY1 ~ distr, data = naturaliosv2)
summary.aov(distr_anov)
distr_HSD <- HSD.test(distr_anov, "distr", console = T)
distr_statistika <- as.data.frame(distr_HSD$means)
distr_statistika <- cbind(distr = rownames(distr_statistika), distr_statistika)
row.names(distr_statistika) <- NULL
distr_statistika <- distr_statistika %>% arrange(desc(DMY1))
distr_statistika$groups <- distr_HSD$groups$groups

# --------------------------------STRUKTURA
structure$distr <- as.factor(structure$distr)
structure$year <- as.factor(structure$year)
# blyn uzknisa. reik parasyt funkcija

convertToFactors <- function(df, columns_to_convert) {
  for (col_name in columns_to_convert) {
    if (col_name %in% names(df)) {
      df[[col_name]] <- as.factor(df[[col_name]])
    } else {
      cat(paste("Column", col_name, "not found in the data frame.\n"))
    }
  }
  return(df)
}

hsd_to_table <- function(hsd_output,factor_name) {
  df <- hsd_output$means
  df <- cbind(col1 = rownames(df), df)
  row.names(df) <- NULL
  colnames(df)[1] <- factor_name
  df <- df %>% arrange(desc(.[[2]]))
  df$group <- hsd_output$groups$groups
  return(df)
}

columns <- c("year", "rep", "distr", "test")
print(n = 114, convertToFactors(structure, columns))
convertToFactors(structure, columns_to_convert)
structure <- convertToFactors(structure, columns)

strukt_aov <- aov(grasses ~ year+ distr, data = structure)
summary.aov(strukt_aov)
gras_hsd <- HSD.test(strukt_aov, c("year", "distr"), group = TRUE, console = TRUE)

gras_stat <- hsd_to_table(gras_hsd, "year_distr")

strukt_aov <- aov(legumes ~ year+ distr, data = structure)
summary.aov(strukt_aov)
legume_hsd <- HSD.test(strukt_aov, c("year", "distr"), group = TRUE, console = TRUE)

legumes_stat <- hsd_to_table(legume_hsd, "year_distr")

strukt_aov <- aov(herbs ~ year+ distr, data = structure)
summary.aov(strukt_aov)
herb_hsd <- HSD.test(strukt_aov, c("year", "distr"), group = TRUE, console = TRUE)

herb_stat <- hsd_to_table(herb_hsd, "year_distr")

strukt_aov <- aov(sedges ~ year+ distr, data = structure)
summary.aov(strukt_aov)
sedges_hsd <- HSD.test(strukt_aov, c("year", "distr"), group = TRUE, console = TRUE)

sedges_stat <- hsd_to_table(sedges_hsd, "year_distr")
# Nu va viska pasidariau i lenteles

# perrikiuoju lenteles pagal metus_rajonus
gras_stat <- gras_stat %>% arrange(year_distr)
legumes_stat <- legumes_stat %>% arrange(year_distr)
herb_stat <- herb_stat %>% arrange(year_distr)
sedges_stat <- herb_stat %>% arrange(year_distr)

# Apjungiu

struktura_merged <- gras_stat[, 1:2]
struktura_merged$legumes <- round(legumes_stat$legumes, 3)
struktura_merged$herbs <- round(herb_stat$herbs, 3)
struktura_merged$sedges <- round(sedges_stat$sedges, 3)
struktura_merged$grasses <- round(struktura_merged$grasses, 3)


struk_merged_long <- struktura_merged %>% pivot_longer(!year_distr,
                                                       names_to = "type",
                                                       values_to = "DMY")
struk_merged_long$type <- as.factor(struk_merged_long$type)
struk_merged_long$type <- ordered(struk_merged_long$type, levels = c("sedges",
                                                                     "herbs",
                                                                     "grasses",
                                                                     "legumes"))

strukt_pav <- ggplot(struk_merged_long, aes(x = year_distr, y = DMY, fill = type)) +
  geom_col(color = "black", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = spalvos)




strukt_pav
spalvos <- c("gray46", "lightgoldenrod1", "green3", "deepskyblue3")


strukt_pav <- strukt_pav + labs(x = NULL, y = bquote("Dry matter yield, kg m"^"-2")) +
  scale_x_discrete(labels = c("Kėdainiai",
                              "Lazdijai",
                              "Šilalė\n\n2019",
                              "Zarasai",
                              "Kėdainiai",
                              "Lazdijai",
                              "Šilalė\n\n2020",
                              "Zarasai")) +
  theme_bw() + guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "top")
strukt_pav  

# noriu padaryt kitaip, fill = distr ir facet_wrap(year)

split_values <- str_split(struk_merged_long$year_distr, ":")
struk_merged_long$year <- sapply(split_values, function(x) x[1])
struk_merged_long$distr <- sapply(split_values, function(x) x[2])

strukt_pav2 <- ggplot(struk_merged_long, aes(x = distr, y = DMY, fill = type)) +
  geom_col(color = "black", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = spalvos) +
  facet_wrap(~year, strip.position = "bottom") + theme_metan_minimal() +
  theme(legend.position = "top", strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_discrete(labels = c("Kėdainiai",
                              "Lazdijai",
                              "Šilalė",
                              "Zarasai")) +
  labs(x  = "District", y = bquote("Dry matter yield, kg m"^"-2"))
# Lyg ir neblogai gavos

# - - - - - - - - - - - - - - JACCARD INDEX

jaccard <- function(M, user1, user2) {
  sums = rowSums(M[,c(user1, user2)])
  
  similarity = length(sums[sums==2])
  total = length(sums[sums==1]) + similarity
  
  similarity/total
}

Gineitai_Samaniai <- jaccard(jaccardui, "Gineitai", "Samaniai")
Gineitai_Samaniai

num_cols <- ncol(jaccardui)
result_matrix <- matrix(0, nrow = num_cols, ncol = num_cols)

# Loop through all pairs of columns
for (i in 1:num_cols) {
  for (j in 1:num_cols) {
    if (i != j) {
      similarity <- jaccard(jaccardui, i, j)
      result_matrix[i, j] <- similarity
    }
  }
}

# Create a dataframe from the result matrix
result_df <- as.data.frame(result_matrix)

# Rename the rows and columns with the column names from jaccardui
rownames(result_df) <- colnames(jaccardui)
colnames(result_df) <- colnames(jaccardui)

# Print or use the result_df as needed
print(result_df)

# - - - - - - - - Sejomainines pievos
num_cols <- ncol(jaccard_sejom)
result_matrix_sejom <- matrix(0, nrow = num_cols, ncol = num_cols)


for (i in 1:num_cols) {
  for (j in 1:num_cols) {
    if (i != j) {
      similarity <- jaccard(jaccard_sejom, i, j)
      result_matrix_sejom[i, j] <- similarity
    }
  }
}


result_df_sejom <- as.data.frame(result_matrix_sejom)


rownames(result_df_sejom) <- colnames(jaccard_sejom)
colnames(result_df_sejom) <- colnames(jaccard_sejom)

# Print or use the result_df as needed
print(result_df_sejom)
result_df_sejom <- round(result_df_sejom, 4)
result_df <- round(result_df, 4)
