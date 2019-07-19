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


### DFU dataset
file_dfu="bubble.csv"
patient <- readr::read_csv(file_dfu)
fp <- c("Taylorella equigenitalis","synthetic construct",
        "Streptococcus thermophilus","Lactococcus lactis",
        "Propionibacterium acnes")

percent_dfu<- patient %>% filter(abundance >= 0.01 & pct>=0.0001)%>%mutate(proportion=abundance*100) %>% filter(!tax_name %in% fp)
percent_dfu <- percent_dfu %>% filter(!grepl("phage", tax_name, fixed = TRUE)) 

percent_dfu$detection<-c("no","yes","yes","no","no","no","no","no",
                         "no","yes","no","no","no","no","no",
                         "no","no","no","no","no","yes","no","no","no")



#plot Patient 1
patient1 <- percent_dfu%>% filter(Patient=="Patient1")
level_order <- c("Homo sapiens",
                 "Enterobacter hormaechei",
                 "Corynebacterium simulans")
patient1$tax_name_wrap <- str_wrap(patient1$tax_name, width = 0)
level_order_wrap <- str_wrap(level_order, width = 0)

bplot1 = ggplot(patient1, aes(x=Time, y=factor(patient1$tax_name_wrap, levels=level_order_wrap), color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_color_manual(values=c("black", "red"))+
  scale_x_continuous(limits=c(0, 4), breaks=c(1,2,3)) +
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  labs(y = "Organism", x = "Time point", color="detection\nby culture", size="relative\nabundance") + 
  geom_text(label=round(patient1$proportion), nudge_y = -0.2)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"),
        text=element_text(family="Times New Roman"),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 15),
        strip.background.x = element_rect(fill = "grey"))
bplot1

panel <- bplot1 + 
  facet_grid(. ~ Patient,scales="free_y",space="free")
panel

out_file="raw_Fig5_part1.png"
ggsave(out_file, width = 17, height = 17, units = "cm")

#Plot patient 2
patient2 <- percent_dfu %>% filter(Patient=="Patient2")
level_order <- c("Homo sapiens","Staphylococcus aureus",
                 "Enterococcus faecium","Enterococcus faecalis",
                 "Klebsiella pneumoniae",
                 "Pseudomonas aeruginosa")


patient2$tax_name_wrap <- str_wrap(patient2$tax_name, width = 0)
level_order_wrap <- str_wrap(level_order, width = 0)

bplot2 = ggplot(patient2, aes(x=Time, y=factor(patient2$tax_name_wrap, levels=level_order_wrap), color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_color_manual(values=c("black", "red"))+
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  scale_x_continuous(limits=c(0, 6), breaks=c(1, 2, 3, 4, 5, 6)) +
  labs(y = "Organism", x = "Time point", color="detection\nby culture", size="relative\nabundance") + 
  geom_text(label=round(patient2$proportion), nudge_y = -0.4)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"),
        text=element_text(family="Times New Roman"),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 15),
        strip.background.x = element_rect(fill = "grey"))
bplot2

panel <- bplot2 + 
  facet_grid(. ~ Patient,scales="free_y",space="free")
panel

out_file="raw_Fig5_part2.png"
ggsave(out_file, width = 19, height = 17, units = "cm")

