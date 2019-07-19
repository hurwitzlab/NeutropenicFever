library(ggplot2)
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggrepel)
library(Cairo)
library(stringr)
library(extrafont)
font_import()
loadfonts()
fonts()

## normal blood samples
normal <- readr::read_csv("normal_blood.csv")

filter_pct <- normal %>% filter(pct >= 0.01)

#plot the pct of reads
bplot_all = ggplot(filter_pct, aes(week, tax_name)) + 
  geom_point(aes(size = pct*100)) +
  labs(size="percent reads\nclassified", y = "Organisms detected", x = "Time of sampling") + 
  geom_text(label=round(filter_pct$pct*100,1), nudge_y = -0.3)+
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  theme_classic(base_size = 20)+
  theme(text=element_text(family="Times New Roman"),
        axis.text.y = element_text(face = "italic",size = 15), 
        axis.text.x = element_text(angle = 50, size = 15, hjust = 1),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 15),
        strip.background.x = element_rect(fill = "grey"),
        panel.spacing.x = unit(0,"line"))

bplot_all

panel <- bplot_all + 
  facet_grid(. ~ Donor,scales="free_y",space="free")
panel

out_file="raw_Fig3_part1.png"
ggsave(out_file, width = 25, height = 10, units = "cm")

filter_abundance <- normal %>% filter(abundance>=0.01)

#plot the centrifuge abundance
bplot_all = ggplot(filter_abundance, aes(week, tax_name)) + 
  geom_point(aes(size = abundance*100)) +
  labs(size="relative\nabundance", y = "Organisms detected", x = "Time of sampling") + 
  geom_text(label=round(filter_abundance$abundance*100,1), nudge_y = -0.4)+
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  theme_classic(base_size = 20)+
  theme(text=element_text(family="Times New Roman"),
        axis.text.y = element_text(face = "italic",size = 15), 
        axis.text.x = element_text(angle = 50, size = 15, hjust = 1),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 15),
        strip.background.x = element_rect(fill = "grey"),
        panel.spacing.x = unit(0,"line"))

bplot_all

panel <- bplot_all + 
  facet_grid(. ~ Donor,scales="free_y",space="free")
panel

out_file="raw_Fig3_part2.png"
ggsave(out_file, width = 25, height = 12, units = "cm")

