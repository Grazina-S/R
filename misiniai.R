first_cut <- na.omit(first_cut)
merged_df <- merge(first_cut, to_add, by = c("var", "rep"), all.x = TRUE)
merged_df$fmy <- merged_df$ZMD + ifelse(is.na(merged_df$zmd_plus), 0, merged_df$zmd_plus / 1000)
merged_df$zmd_plus <- NULL
merged_df$fmy1_ha <- 10000*merged_df$fmy/8.25
merged_df$dmy <- merged_df$SMD*merged_df$fmy/0.5/1000
merged_df$dmy1_ha <- 10000*merged_df$dmy/8.25
merged_df$dmy1_ha <- round(merged_df$dmy1_ha)
first_cut_yield <- merged_df %>% select(c("var", "rep", "fmy1_ha", "dmy1_ha"))
first_cut_yield$var <- as.factor(first_cut_yield$var)
first_cut_yield$rep <- as.factor(first_cut_yield$rep)
write.csv(first_cut_yield, file = "first_cut_yield.csv", row.names = FALSE)
second_cut <- na.omit(second_cut)
second_cut$fmy2_ha <- 10000*second_cut$ZMD/8.25
second_cut$fmy2_ha <- round(second_cut$fmy2_ha)
second_cut$dmy2_ha <- second_cut$ZMD*second_cut$SMD*80/33 #cia viskas viename
#ir is g i kg, ir perskaiciavimas is 500 g ir paskui laukelio i ha
second_cut$dmy2_ha <- round(second_cut$dmy2_ha)
secong_cut_yield <- second_cut %>% select(c("var", "rep", "fmy2_ha", "dmy2_ha"))
write.csv(secong_cut_yield, file = "second_cut_yield.csv", row.names = FALSE)

third_cut <- na.omit(third_cut)
third_cut$fmy3_ha <- 10000*third_cut$ZMD/8.25
third_cut$fmy3_ha <- round(third_cut$fmy3_ha)
third_cut$dmy3_ha <- third_cut$ZMD*third_cut$SMD*80/33
third_cut$dmy3_ha <- round(third_cut$dmy3_ha)
third_cut_yield <- third_cut %>% select(c("var", "rep", "fmy3_ha", "dmy3_ha"))
write.csv(third_cut_yield, file = "third_cut_yield.csv", row.names = FALSE)

fourth_cut <- na.omit(fourth_cut)
fourth_cut$fmy4_ha <- 10000*fourth_cut$ZMD/8.25
fourth_cut$fmy4_ha <- round(fourth_cut$fmy4_ha)
fourth_cut$dmy4_ha <- fourth_cut$ZMD*fourth_cut$SMD*80/33
fourth_cut$dmy4_ha <- round(fourth_cut$dmy4_ha)
fourth_cut_yield <- fourth_cut %>% select(c("var", "rep", "fmy4_ha", "dmy4_ha"))
write.csv(fourth_cut_yield, file = "fourth_cut_yield.csv", row.names = FALSE)


#------------------------------------------------------------2024
#strukturine

strukt2024 <- strukt2024 %>% fill(var, .direction = "down") %>%
  fill(rep, .direction = "down")
strukt2024 <- na.omit(strukt2024)
strukt2024_proc <- strukt2024 %>% pivot_wider(names_from = type, values_from = weight)
strukt2024_proc$pup_proc <- strukt2024_proc$pup*100/strukt2024_proc$bendras
legumes2024 <- strukt2024_proc %>% select(c("var", "rep", "pup_proc"))
legumes2024$var <- as.factor(legumes2024$var)
legumes2024$rep <- as.factor(legumes2024$rep)
legumes2024_mean <- legumes2024 %>% group_by(var) %>% get_summary_stats(pup_proc, type = "mean_sd")
legumes2024_mean <- rename(legumes2024_mean, legume3.perc=mean)
legumes2024_mean$legume3.perc <- round(legumes2024_mean$legume3.perc, 1)
#----------------2023+2024

legume_perc$var <- as.factor(legume_perc$var)
legumes2023_mean <- legume_perc %>% group_by(var) %>% 
  summarise(across(c(legume0.perc, legume1.perc, legume2.perc), 
                   list(mean=mean, sd=sd)))
legumes23_24 <- merge(legumes2023_mean, legumes2024_mean, by="var")
legumes23_24 <- legumes23_24 %>% rename(
  legume0.perc=legume0.perc_mean,
  legume1.perc=legume1.perc_mean,
  legume2.perc=legume2.perc_mean)

legumes23_24 <- legumes23_24 %>%
  mutate(across(c(legume1.perc, legume2.perc), \(x) round(x, digits = 1)))
write.csv(legumes23_24, file = "legumes23_24.csv", row.names = FALSE)

legume_perc_presentation <- convertToFactors(legume_perc_presentation, 
                                             c("var", "pur_spec_prc", "pur_spec", "purpose", "species"))

pup24 <- strukt2024_proc %>% select("var", "rep", "pup_proc") %>%
  rename(legume3.perc = pup_proc)
pup24$legume3.perc <- round(pup24$legume3.perc, 1)
strukt_all <- merge(legume_perc, pup24, by=c("var", "rep"))
prijungimui <- all_yield_no0 %>% select(var, rep, purpose, species, legume)
strukt_all <- merge(prijungimui, strukt_all, by=c("var", "rep"))
anova_struk1 <- strukt_all %>% filter(legume == "60_prc") %>% aov(legume2.perc ~ species*purpose, data = .)
summary.aov(anova_struk1)
HSD.test(anova_struk1, c("species", "purpose"), console = TRUE)
HSD.test(anova_struk1, "species", console = TRUE)



#blyn pamirsau sudygima

germination_mean <- germination %>% group_by(var) %>%
  get_summary_stats(germination, type = "mean_sd")
germination_mean <- germination_mean %>% rename(legume.germ=mean,
                                                legume.germ_sd=sd)
germination_mean$variable <- NULL
germination_mean$n <- NULL

germination_mean$legume.germ <- round(germination_mean$legume.germ, 1)

legume_perc_presentation <- merge(legume_perc_presentation, germination_mean,
                                  by="var")
colnames(legume_perc_presentation)
legume_perc_presentation <- legume_perc_presentation[,c(1,2,3,4,5,6,14,7,8,9,10,15,11,12,13)]
legume_perc_presentation$legume0.perc_sd <- NULL
write.csv(legume_perc_presentation, file = "procentai_pristatymui.csv", row.names = FALSE)
#nesigavo pertvarkyti su pivot_longer, pasidariau rankom
#procentai_reshaped %>% filter(legume.start == 40) %>% ggplot(aes(time, legume.perc, fill=pur_spec)) +
#  geom_col(position = "dodge", color="black") + scale_fill_brewer(palette = "Paired")
procentai_reshaped %>% ggplot(aes(time, legume.perc, fill=species)) +
  geom_col(position = position_dodge(0.7), color="black", width = 0.6) + scale_fill_manual(values = c(clover="#FF6699", trefoil="#FFFF66")) +
  facet_grid(legume.start ~ purpose) +
  geom_errorbar(aes(ymin = legume.perc - legume.sd, ymax = legume.perc + legume.sd),
                width = 0.2, position = position_dodge(0.7)) +
  theme_bw()
#procentai_reshaped %>%filter(purpose == "forage") %>% ggplot(aes(time, legume.perc, fill=species)) +
#  geom_col(position = "dodge", color="black") + scale_fill_brewer(palette = "Paired") +
#  facet_wrap( ~ legume.start)
struktura_pic <- procentai_reshaped %>% filter(legume.start != 40) %>%  ggplot(aes(time, legume.perc, fill=species)) +
  geom_col(position = position_dodge(0.7), color="black", width = 0.6) + scale_fill_manual(values = c(clover="#FF6699", trefoil="#FFFF66")) +
  facet_grid(legume.start ~ purpose) +
  geom_errorbar(aes(ymin = legume.perc - legume.sd, ymax = legume.perc + legume.sd),
                width = 0.2, position = position_dodge(0.7)) +
  theme_bw() + ylab("Legume, %")

tiff(filename = "struktura.tiff", width = 20, height = 12, units = "cm", res = 300)
struktura_pic
dev.off()

#---------------------------------- DMY--------------------------------------------------------------------------------

df_list <- list(first_cut_yield, second_cut_yield, third_cut_yield, fourth_cut_yield) 
all_yield <- reduce(df_list, full_join, by= c("var", "rep"))
all_yield$fmy_total <- all_yield$fmy1_ha+all_yield$fmy2_ha+all_yield$fmy3_ha+all_yield$fmy4_ha
all_yield$dmy_total <- all_yield$dmy1_ha+all_yield$dmy2_ha+all_yield$dmy3_ha+all_yield$dmy4_ha
#all_yield <- na.omit(all_yield)
dmy_means <- all_yield %>%
  group_by(var) %>%
  summarise(across(c(dmy1_ha, dmy2_ha, dmy3_ha, dmy4_ha, dmy_total),
                   list(mean = ~ mean(.x, na.rm = TRUE))))

dmy_paveikslui <- legume_perc_presentation %>% select(var:legume0.perc)            
dmy_paveikslui <- merge(dmy_paveikslui, dmy_means, by="var")
dmy_paveikslui$dmy_total_mean <- NULL
dmy_long <- dmy_paveikslui %>% pivot_longer(cols = starts_with("dmy"), 
                                            names_to = "cut",
                                            values_to = "DMY")
dmy_long$DMY <- round(dmy_long$DMY, 1)
dmy_pic <- dmy_long %>% filter(legume0.perc != 40) %>% ggplot(aes(species, DMY, fill = cut)) +
  geom_col(position = position_stack(reverse = TRUE), colour = "black") +
  facet_grid(legume0.perc ~ purpose) +
  scale_fill_manual(values = c("springgreen4", "limegreen", "lawngreen", "lightgreen"))+
  theme_bw()
tiff(filename = "dmy_didelis.tiff", width = 18, height = 15, units =  "cm", res = 300)
dmy_pic  
dev.off()


all_yield <- all_yield %>%
  mutate(species = case_when(
    var %in% c(4, 5, 6, 13, 14, 15, 22, 23, 24) ~ "clover",
    var %in% c(7, 8, 9, 16, 17, 18, 25, 26, 27) ~ "trefoil",
    TRUE ~ as.character(NA)  # For other cases, set to NA or any other value
  ))

all_yield <- all_yield %>%
  mutate(purpose = case_when(
    var %in% c(4, 5, 6, 7, 8, 9) ~ "forage",
    var %in% c(22, 23, 24, 25, 26, 27) ~ "grazing",
    var %in% c(13, 14, 15, 16, 17, 18) ~ "universal",
    TRUE ~ as.character(NA)
  ))

all_yield <- all_yield %>%
  mutate(legume = case_when(
    var %in% c(4, 7, 13, 16, 22, 25) ~ "40_prc",
    var %in% c(5, 8, 14, 17, 23, 26) ~ "50_prc",
    var %in% c(6, 9, 15, 18, 24, 27) ~ "60_prc",
    TRUE ~ as.character(NA)
  ))
all_yield_no0 <- na.omit(all_yield)
anova_dmy <- aov(dmy_total ~ species*purpose*legume, data = all_yield_no0)
summary.aov(anova_dmy)
HSD.test(anova_dmy, "purpose", group = TRUE, console = TRUE)
all_yield_no0 %>% filter(purpose == "grazing", legume == "50_prc") %>%
  t_test(dmy_total ~ species)

#------------------------------------------METEO--------------------------

meteo22_07_01_23_06_30 <- meteo22_07_01_23_06_30 %>% select(obs_time_utc, air_temperature, precipitation)
meteo23_07_01_24_05_31 <- meteo23_07_01_24_05_31 %>% select(obs_time_utc, air_temperature, precipitation)
meteo <- bind_rows(meteo22_07_01_23_06_30, meteo23_07_01_24_05_31)
meteo_summary <- meteo %>%
  mutate(
    date = as_date(obs_time_utc),
    year_month = format(obs_time_utc, "%Y-%m")
  ) %>%
  group_by(date) %>%
  summarise(
    mean_air_temperature = mean(air_temperature, na.rm = TRUE),
    total_precipitation = sum(precipitation, na.rm = TRUE),
    year_month = first(year_month)
  )

meteo_summary$mean_air_temperature <- round(meteo_summary$mean_air_temperature, 1)
meteo_summary %>% ggplot(aes(date, mean_air_temperature)) +
  geom_line() + facet_wrap(~year_month, scales = "free_x")
meteo_monthly <- meteo_summary %>%
  group_by(year_month) %>%
  summarise(
    temperature = mean(mean_air_temperature),
    precipitation = sum(total_precipitation)
  )
meteo_monthly$temperature <- round(meteo_monthly$temperature, 1)
meteo_monthly %>%
  ggplot(aes(year_month, temperature)) +
  geom_col()

#PAVEIKSLAI
# Daugiameciai paimti is wikipedijos https://lt.wikipedia.org/wiki/Lietuvos_klimatas
# tas pats yra https://www.meteo.lt/klimatas/lietuvos-klimatas/skn/
# metai 1991-2020


#precip_fig <- ggplot(meteo_decad) + geom_col( aes(x = eile2, y = precip), fill = "skyblue", color = "black") + geom_line(aes(x = eile2, y = precip_longterm), color = "gray30", size = 1) +
#  facet_wrap(~year, scales = "free_x", ncol = 3, strip.position = "bottom") +
#  scale_x_continuous(breaks = meteo_decad$eile2[seq(1, nrow(meteo_decad), by = 3)],
#                     labels = meteo_decad$month[seq(1, nrow(meteo_decad), by = 3)]) +
#  theme_metan_minimal() + xlab("Date") + ylab("Precipitation, mm")  
  
temper <- ggplot(meteo_monthly, aes(eile, temperature, color = kind)) +
  geom_line(data = subset(meteo_monthly, kind == "mean"), linewidth = 1) +  # Thicker blue line
  geom_line(data = subset(meteo_monthly, kind == "long_term"), size = 0.5) +  # Thinner black line
  scale_color_manual(values = c("mean" = "blue", "long_term" = "gray30")) +
  xlab("Date") +
  ylab("Temperature, \u00b0C") +
  scale_x_continuous(breaks = meteo_monthly$eile, labels = meteo_monthly$year_month) +
  geom_hline(yintercept = 0, color = 'black') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), legend.position = "top")


precip <- ggplot() +
  geom_col(data = subset(meteo_monthly, kind == "mean"), aes(eile, precipitation), fill = "skyblue", color = "black") +
  geom_line(data = subset(meteo_monthly, kind == "long_term"), aes(eile, precipitation), color = "gray30", linewidth=1) +
  scale_x_continuous(breaks = meteo_monthly$eile, labels = meteo_monthly$year_month) +
  xlab("Date") + ylab("Precipitation, mm") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), legend.position = "top")

tiff(filename = "temper.tiff", width = 28, height = 10, units =  "cm", res = 300)              
temper
dev.off()
tiff(filename = "precip.tiff", width = 28, height = 10, units =  "cm", res = 300)              
precip
dev.off()
