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


### mock communities R2 plot
data_mock <-  readr::read_csv("all_tools.csv")
bugs <- c("Rhodobacter sphaeroides","Staphylococcus epidermidis",
          "Streptococcus mutans","Escherichia coli","Pseudomonas aeruginosa",
          "Staphylococcus aureus","Streptococcus agalactiae","Bacillus cereus",
          "Clostridium beijerinckii","Helicobacter pylori","Propionibacterium acnes",
          "Neisseria meningitidis","Acinetobacter baumannii","Listeria monocytogenes",
          "Lactobacillus gasseri","Deinococcus radiodurans",
          "Enterococcus faecalis","Streptococcus pneumoniae","Bacteroides vulgatus")

data_plot <- data_mock %>% filter(name %in% bugs) %>% gather(classifier, abundance, Centrifuge:KrakenUniq)

#calculate the R2 coeficients
R_label<-"R-squared = "

cent <- data_plot %>% filter(classifier=="Centrifuge")
cent.lm = lm(abundance ~ Expected, data=cent)
centR2<-summary(cent.lm)$r.squared
cent_label<-paste(R_label, as.character(round(centR2,3)))
cent_label

clark <- data_plot %>% filter(classifier=="CLARK")
clark.lm = lm(abundance ~ Expected, data=clark)
clarkR2<-summary(clark.lm)$r.squared
clark_label<-paste(R_label, as.character(round(clarkR2,3)))
clark_label

kraken <- data_plot %>% filter(classifier=="KrakenUniq")
kraken.lm = lm(abundance ~ Expected, data=kraken)
krakenR2<-summary(kraken.lm)$r.squared
kraken_label<-paste(R_label, as.character(round(krakenR2,3)))
kraken_label


bplot_all = ggplot(data_plot, aes(abundance, Expected, color=classifier)) + 
  geom_point() + 
  labs(y = "Expected abundance (Log10 %)", x = "Measured abundance (Log10 %)") + 
  geom_smooth(method = lm, se = FALSE)+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))+
  annotate("text", x = 1.00, y = 35.00, label = cent_label, color = "#F8766D", size = 4)+
  annotate("text", x = 1.00, y = 23.00, label = clark_label, color = "#00BA38", size = 4)+
  annotate("text", x = 1.00, y = 15.00, label = kraken_label, color = "#619CFF", size = 4)
bplot_all

out_file="Fig2.pdf"
ggsave(out_file, width = 17, height = 12, units = "cm")

out_file="Fig2.png"
ggsave(out_file, width = 17, height = 12, units = "cm")

out_file="Fig2.tiff"
ggsave(out_file, width = 17, height = 12, units = "cm")