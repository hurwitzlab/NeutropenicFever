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

NF_qc="qc_no_qc_nf.csv"
data_host <- readr::read_csv(NF_qc)

torque_data <- data_host %>% filter(grepl("Torque teno virus", tax_name, fixed = TRUE)) %>% group_by(sample, category, Patient) %>% summarise(abundance = sum(abundance), pct=sum(pct))
torque_data$tax_name <- 'Torque teno virus'
torque_data

Pseudo_data <- data_host %>% filter(grepl("Pseudomonas", tax_name, fixed = TRUE) & 
                                      !grepl("Pseudomonas phage", tax_name, fixed = TRUE)) %>% group_by(sample, category, Patient) %>% summarise(abundance = sum(abundance), pct=sum(pct))
Pseudo_data$tax_name <- 'Pseudomonas sp.'

other_hits<- data_host %>% filter(!grepl("Pseudomonas", tax_name, fixed = TRUE) & 
                                    !grepl("Torque teno", tax_name, fixed = TRUE)) %>% select(sample,category, Patient,
                                                                                              abundance, tax_name, pct)


some_data<-bind_rows(Pseudo_data, torque_data)
plot_data<-bind_rows(other_hits, some_data)
fp <- c("Taylorella equigenitalis","synthetic construct",
        "Streptococcus thermophilus","Lactococcus lactis",
        "Propionibacterium acnes")
filter_nf <-plot_data %>% filter(abundance >= 0.01 & pct >=0.0001)%>%mutate(proportion=abundance*100) %>% filter(!tax_name %in% fp)


bplot_host = ggplot(filter_nf, aes(x=Patient, y=tax_name)) + 
  geom_point(aes(size = abundance*100)) + 
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  labs(y = "Organism", x = "Patient", size="relative\nabundance") + 
  geom_text(label=round(filter_nf$abundance*100), nudge_y = -0.4)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"),
        text=element_text(family="Times New Roman"),
        axis.text.x = element_text(angle = 50, size = 15, hjust = 1),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        strip.background.x = element_rect(fill = "grey"))
bplot_host

panel <- bplot_host + 
  facet_grid(. ~ category, scales="free_y",space="free")
panel

out_file="S2_File_Fig2.png"
ggsave(out_file, width = 20, height = 15, units = "cm")
