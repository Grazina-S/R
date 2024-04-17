# Kaip padaryti stacked faceted barplot kur kiekvienas facetas turi skirtingas Y asies reiksmes
## Duomenys
zdown yra tam kad pagal abecele down detu i apatini facet. Neieskojau kaip ta padaryt gudriau
```
place	samples	type	q
zdown	C3	not_iden	9
zdown	S5	not_iden	15
zdown	S15	not_iden	16
zdown	S22	not_iden	12
zdown	S23	not_iden	8
zdown	C3	iden	39
zdown	S5	iden	33
zdown	S15	iden	32
zdown	S22	iden	36
zdown	S23	iden	40
zdown	C3	remaining	50
zdown	S5	remaining	50
zdown	S15	remaining	50
zdown	S22	remaining	50
zdown	S23	remaining	50
up	C3	not_iden	
up	S5	not_iden	
up	S15	not_iden	
up	S22	not_iden	
up	S23	not_iden	
up	C3	iden	
up	S5	iden	
up	S15	iden	
up	S22	iden	
up	S23	iden	
up	C3	remaining	606
up	S5	remaining	558
up	S15	remaining	590
up	S22	remaining	585
up	S23	remaining	578
```

## Reikalingi laibrariai
``` r
library(tidyverse)
library(ggh4x)
library(RColorBrewer)
```
## Paveikslo paisymas
- reverse = TRUE reikejo kad sukrautu atvirkscia tvarka nei default
- "free_y" tam kad nebutu vienodos skales tarp facetu, nrow=2 kad sudetu viena po kitu
- strip.text = element_blank() nuima faceto juosta
``` r
h <- ggplot(honey_stacked2, aes(x = samples, y = q, fill = type)) +
  geom_col(position = position_stack(reverse = TRUE), color = "black", width = 0.5) +  
  facet_wrap(~place, scales = "free_y", nrow = 2) + 
  theme_minimal() + theme(strip.text = element_blank()) +  
  scale_fill_brewer(palette = "Paired", labels = c("Red clover proteins identified in honey",
                                                   "Red clover proteins not identified in honey",
                                                   "Other proteins")) 
h
```
## Y asies skaliu nustatymas
Skales reikia nurodyti is eiles. Siuo atveju - nuo virsaus
``` r
position_scales <- list(
  scale_y_continuous(limits = c(550, 610), breaks = seq(550, 610, 10)),
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10))
  )
h <- h + facetted_pos_scales(y = position_scales)
```
## Kiti pagrazinimai
- pakeisti asiu pavadinimai, apverstas legendos ikonu eiliskumas ir nuimtas legendos pavadinimas
- pakeista legendos vieta. Kad ji tilptu, su plot.margin praplestos parastes, labiausiai i virsu. Tada su legend.position nurodyta vieta virsuje uz grafiko ribu. Tiesiog "top" netiko nes tada isdelioja eilute, o reikia kad ikonos eitu viena po kita.
- asiu uzrasai buvo pilki, padaryta juodi
  ``` r
  h <- h + xlab("Honey samples") + ylab("Number of proteins") +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))
  h <- h +
  theme(plot.margin=unit(c(3,1.5,1,0.5),"cm")) +
  theme(legend.position=c(0.6,1.15))
  h <- h + theme(axis.text.x = element_text(colour = "black"),
          axis.text.y = element_text(colour = "black"))
  ```
  ## Paveikslo issaugojimas
  ``` r
  tiff(filename = "honey.tif", width = 10, height = 15, units =  "cm", res = 300)
  h
  dev.off()
  ```

