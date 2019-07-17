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


###### VAP dataset
file_vap="vap.csv"
vap <- readr::read_csv(file_vap)
fp <- c("Taylorella equigenitalis","synthetic construct")
percent_vap<- vap%>%mutate(proportion=abundance*100) %>% filter(!tax_name %in% fp)

#wrap organisms names
percent_vap$Organisms_wrap = str_wrap(percent_vap$tax_name, width = 0)

#Patient1
pat1 <- percent_vap %>% filter(Patient=="Patient 1")

level_order <- c("Klebsiella aerogenes",
                 "Rothia mucilaginosa",
                 "Streptococcus constellatus",
                 "Streptococcus oralis",
                 "Propionibacterium acnes",
                 "Staphylococcus aureus")

bplot_vap1 = ggplot(pat1, aes(x=Day, y=factor(tax_name, levels=level_order), color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_color_manual(values=c("black", "red"))+
  scale_x_continuous(limits=c(0, 4), breaks=c(1, 3)) +
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  labs(y = "Organism", x = "Day", color="detection\nby culture", size="relative\nabundance") + 
  geom_text(label=round(pat1$proportion), nudge_y = -0.4)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"),
        text=element_text(family="Times New Roman"),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 15),
        strip.background.x = element_rect(fill = "grey"))
bplot_vap1

panel <- bplot_vap1 + 
  facet_grid(. ~ Patient,scales="free_y",space="free")
panel

out_file="Fig4_part1.png"
ggsave(out_file, width = 15, height = 17, units = "cm")

#Patient2
pat2 <- percent_vap %>% filter(Patient=="Patient 2")

level_order <- c("Prevotella denticola",
                 "Prevotella intermedia",
                 "Streptococcus intermedius",
                 "Tannerella forsythia",
                 "Klebsiella aerogenes",
                 "Staphylococcus aureus")

bplot_vap2 = ggplot(pat2, aes(x=Day, y=factor(tax_name, levels=level_order), color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_color_manual(values=c("black", "red"))+
  scale_x_continuous(limits=c(0, 4), breaks=c(1, 3)) +
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  labs(y = "Organism", x = "Day", color="detection\nby culture", size="relative\nabundance") + 
  geom_text(label=round(pat2$proportion), nudge_y = -0.4)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"),
        text=element_text(family="Times New Roman"),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 15),
        strip.background.x = element_rect(fill = "grey"))
bplot_vap2

panel <- bplot_vap2 + 
  facet_grid(. ~ Patient,scales="free_y",space="free")
panel

out_file="Fig4_part2.png"
ggsave(out_file, width = 15, height = 17, units = "cm")
