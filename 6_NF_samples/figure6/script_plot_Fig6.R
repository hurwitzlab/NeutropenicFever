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

#### NF samples
file_nf="NF.csv"
nf <- readr::read_csv(file_nf)
fp <- c("Taylorella equigenitalis","synthetic construct")
percent_nf<- nf%>%mutate(proportion=abundance*100) %>% filter(!tax_name %in% fp)

level_order <- c("Torque teno",
                 "Pseudomonas sp.",
                 "Propionibacterium acnes",
                 "Human parvovirus B19",
                 "Homo sapiens")

bplot_nf = ggplot(percent_nf, aes(x=patient, y=factor(tax_name, levels=level_order), color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_color_manual(values=c("black", "red"))+
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  labs(y = "Organism", x = "Patient", color="detection\nby PCR", size="relative\nabundance") + 
  geom_text(label=round(percent_nf$proportion), nudge_y = -0.4)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"),
        text=element_text(family="Times New Roman"),
        axis.text.x = element_text(angle = 50, size = 15, hjust = 1),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 15),
        strip.background.x = element_rect(fill = "grey"))
bplot_nf


out_file="Fig6.png"
ggsave(out_file, width = 17, height = 17, units = "cm")
