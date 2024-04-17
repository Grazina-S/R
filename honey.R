# Stacked faceted barplot kai facetu y skales skirtingos
# daryta tam kad gauti paveiksla su gapped y axis

## LAIBRARIAI
library(tidyverse) # arba tiesiog ggplot2
library(ggh4x) # sita reikalinga kad padaryt skirtingas skales
library(RColorBrewer) # spalvu paletes

## PAVEIKSLO PAISYMAS

h <- ggplot(honey_stacked2, aes(x = samples, y = q, fill = type)) +
  geom_col(position = position_stack(reverse = TRUE), color = "black", width = 0.5) + 
  #reverse = TRUE reikejo kad sukrautu atvirkscia tvarka nei default
  facet_wrap(~place, scales = "free_y", nrow = 2) +
  #"free_y" tam kad nebutu vienodos skales tarp facetu, nrow=2 kad sudetu viena po kitu
  theme_minimal() + theme(strip.text = element_blank()) +
  # nuemiau strip uzrasus visiskai
  scale_fill_brewer(palette = "Paired", labels = c("Red clover proteins identified in honey",
                                                   "Red clover proteins not identified in honey",
                                                   "Other proteins")) 
h

## Y ASIES SKALIU NUSTATYMAS

# pirma uzduoti virsutines eiles skale, tada apatines
position_scales <- list(
  scale_y_continuous(limits = c(550, 610), breaks = seq(550, 610, 10)),
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10))
  )

h <- h + facetted_pos_scales(y = position_scales)
h

## KITI PAGRAZINIMAI

h <- h + xlab("Honey samples") + ylab("Number of proteins") +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))
# pridejau asiu pavadinimus, apverciau legendos ikonu eile ir nuemiau legendos pavadinima

h
h <- h +
  theme(plot.margin=unit(c(3,1.5,1,0.5),"cm")) +
  theme(legend.position=c(0.6,1.15))
h
# pakeiciau legendos vieta.
# kad ji tilptu, reikejo praplesti parastes su plot.margin
# tada su legend.position nurodziau kur turi but (virs grafiko)
# tiesiog nurodyt legend.position = "top" netiko, nes tada
# sudetu ikonas eilute, o man reikejo viena po kita

h <- h + theme(axis.text.x = element_text(colour = "black"),
          axis.text.y = element_text(colour = "black"))
# asiu uzrasai buvo pilki, padariau juodus

tiff(filename = "honey.tif", width = 10, height = 15, units =  "cm", res = 300)
h
dev.off()
