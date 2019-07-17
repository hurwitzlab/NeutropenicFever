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
file_dfu="/Users/aponsero/Desktop/jimmy_figures/5_dfu/dfu_patient.csv"
patient <- readr::read_csv(file_dfu)


#plot Patient 1
patient1 <- patient %>% filter(Patient=="Patient 1")
level_order <- c("Corynebacterium simulans",
                 "Enterobacter hormaechei",
                 "Homo sapiens")
patient1$Organisms_wrap <- str_wrap(patient1$Organisms, width = 0)
level_order_wrap <- str_wrap(level_order, width = 0)

bplot1 = ggplot(patient1, aes(x=Time, y=factor(Organisms_wrap, levels=level_order_wrap), color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_color_manual(values=c("black", "red"))+
  scale_x_continuous(limits=c(0, 4), breaks=c(1,2,3)) +
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  labs(y = "Organism", x = "Time point", color="detection\nby culture", size="relative\nabundance") + 
  geom_text(label=round(patient1$proportion), nudge_y = -0.4)+
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

out_file="/Users/aponsero/Desktop/jimmy_figures/figures/raw_Fig5_part1.png"
ggsave(out_file, width = 17, height = 17, units = "cm")

#Plot patient 2
patient2 <- patient %>% filter(Patient=="Patient 2")
level_order <- c("Staphylococcus aureus",
                 "Enterococcus faecium",
                 "Enterococcus faecalis",
                 "Klebsiella pneumoniae",
                 "Pseudomonas aeruginosa",
                 "Homo sapiens")
patient2$Organisms_wrap <- str_wrap(patient2$Organisms, width = 0)
level_order_wrap <- str_wrap(level_order, width = 0)

bplot2 = ggplot(patient2, aes(x=Time, y=factor(patient2$Organisms_wrap, levels=level_order_wrap), color=detection)) + 
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

out_file="/Users/aponsero/Desktop/jimmy_figures/figures/raw_Fig5_part2.png"
ggsave(out_file, width = 19, height = 17, units = "cm")

