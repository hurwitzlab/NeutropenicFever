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
file_vap="bubble.csv"
vap <- readr::read_csv(file_vap)
fp <- c("Taylorella equigenitalis","synthetic construct",
        "Streptococcus thermophilus","Lactococcus lactis",
        "Propionibacterium acnes")

percent_vap<- vap %>% filter(abundance >= 0.01 & pct>=0.0001)%>%mutate(proportion=abundance*100) %>% filter(!tax_name %in% fp)
percent_vap$detection<-c("no","yes","no","no","no","yes","yes","no")


#wrap organisms names
percent_vap$Organisms_wrap = str_wrap(percent_vap$tax_name, width = 0)

#Patient1
pat1 <- percent_vap %>% filter(Patient=="Patient1")

bplot_vap1 = ggplot(pat1, aes(x=Day, y=tax_name, color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_color_manual(values=c("black", "red"))+
  scale_x_continuous(limits=c(0, 4), breaks=c(1, 3)) +
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  labs(y = "Organism", x = "Day", color="detection\nby culture", size="relative\nabundance") + 
  geom_text(label=round(pat1$proportion), nudge_y = -0.2)+
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

out_file="raw_Fig4_part1.png"
ggsave(out_file, width = 15, height = 15, units = "cm")

#Patient2
pat2 <- percent_vap %>% filter(Patient=="Patient2")

bplot_vap2 = ggplot(pat2, aes(x=Day, y=tax_name, color=detection)) + 
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

out_file="raw_Fig4_part2.png"
ggsave(out_file, width = 15, height = 15, units = "cm")

#patients together
bplot_vap = ggplot(percent_vap, aes(x=Day, y=tax_name, color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_color_manual(values=c("black", "red"))+
  scale_x_continuous(limits=c(0, 4), breaks=c(1, 3)) +
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  labs(y = "Organism", x = "Day", color="detection\nby culture", size="relative\nabundance") + 
  geom_text(label=round(percent_vap$proportion), nudge_y = -0.2)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"),
        text=element_text(family="Times New Roman"),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 15),
        strip.background.x = element_rect(fill = "grey"))
bplot_vap

panel <- bplot_vap + 
  facet_grid(. ~ Patient,scales="free_y",space="free")
panel

out_file="raw_Fig4.png"
ggsave(out_file, width = 20, height = 15, units = "cm")

