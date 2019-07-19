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

torque_data <- nf %>% filter(grepl("Torque teno virus", tax_name, fixed = TRUE)) %>% group_by(sample) %>% summarise(abundance = sum(abundance), pct=sum(pct))
torque_data$tax_name <- 'Torque teno virus'
torque_data

Pseudo_data <- nf %>% filter(grepl("Pseudomonas", tax_name, fixed = TRUE) & 
                                      !grepl("Pseudomonas phage", tax_name, fixed = TRUE)) %>% group_by(sample) %>% summarise(abundance = sum(abundance), pct=sum(pct))
Pseudo_data$tax_name <- 'Pseudomonas sp.'
Pseudo_data
other_hits<- nf %>% filter(!grepl("Pseudomonas", tax_name, fixed = TRUE) & 
                                    !grepl("Torque teno", tax_name, fixed = TRUE)) %>% select(sample,abundance,pct,tax_name,sample)

other_hits
some_data<-bind_rows(Pseudo_data, torque_data)
plot_data<-bind_rows(other_hits, some_data)

fp <- c("Taylorella equigenitalis","synthetic construct",
        "Streptococcus thermophilus","Lactococcus lactis",
        "Propionibacterium acnes")

filter_nf <-plot_data %>% filter(abundance >= 0.01 & pct >=0.0001)%>%mutate(proportion=abundance*100) %>% filter(!tax_name %in% fp)

filter_nf$detection<-c("yes","no","no","no","no")

level_order <- c("Homo sapiens",
                 "Torque teno virus",
                 "Staphylococcus warneri",
                 "Pseudomonas sp.",
                 "Human parvovirus B19")

bplot_nf = ggplot(filter_nf, aes(x=sample, y=factor(filter_nf$tax_name, levels=level_order), color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_color_manual(values=c("black", "red"))+
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  labs(y = "Organism", x = "Patient", color="detection\nby PCR", size="relative\nabundance") + 
  geom_text(label=round(filter_nf$proportion), nudge_y = -0.4)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"),
        text=element_text(family="Times New Roman"),
        axis.text.x = element_text(angle = 50, size = 15, hjust = 1),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 15),
        strip.background.x = element_rect(fill = "grey"))
bplot_nf


out_file="raw_Fig6.png"
ggsave(out_file, width = 17, height = 15, units = "cm")

