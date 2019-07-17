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

#QC-no QC plot
qc_noqc="QC_noQC_result.csv"
qc <- readr::read_csv(qc_noqc)

data_plot <- qc %>% gather(type, abundance, -name)

#data_plot$name <- factor(data_plot$name, 
#                         levels = qc$name[order(qc$`Expected value (%)`)])

level_order <- c("Bacillus thuringiensis",
                 "Bacteroides vulgatus",
                 "Deinococcus radiodurans",
                 "Enterococcus faecalis",
                 "Streptococcus pneumoniae",
                 "Actinomyces odontolyticus",
                 "Listeria monocytogenes",
                 "Lactobacillus gasseri",
                 "Acinetobacter baumannii",
                 "Neisseria meningitidis",
                 "Propionibacterium acnes",
                 "Helicobacter pylori",
                 "Clostridium beijerinckii",
                 "Bacillus cereus",
                 "Streptococcus agalactiae",
                 "Staphylococcus aureus",
                 "Pseudomonas aeruginosa",
                 "Escherichia coli",
                 "Streptococcus mutans",
                 "Staphylococcus epidermidis",
                 "Rhodobacter sphaeroides")

bplot_host = ggplot(data_plot, aes(x=type, y=factor(name, levels=level_order))) + 
  geom_point(aes(size = abundance)) + 
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  labs(y = "Organism", x="",size="relative\nabundance") + 
  geom_text(label=round(data_plot$abundance, 3), nudge_x = 0.2)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"),
        text=element_text(family="Times New Roman"),
        axis.text.x = element_text(angle = 50, size = 15, hjust = 1),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        strip.background.x = element_rect(fill = "grey"))
bplot_host

out_file="S2File_Fig1.png"
ggsave(out_file, width = 25, height = 17, units = "cm")