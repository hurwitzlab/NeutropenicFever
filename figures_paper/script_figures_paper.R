library(ggplot2)
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggrepel)
library(Cairo)
library(stringr)

#####################################################
### DFU dataset
file_dfu="/Users/aponsero/Desktop/jimmy_figures/dfu/dfu_patient.csv"
patient <- readr::read_csv(file_dfu)
patient$proportion %<>% as.numeric()

#wrap organisms names
patient$Organisms_wrap = str_wrap(patient$Organisms, width = 0)

#plot separately
patient1 <- patient %>% filter(Patient=="Patient 1")
patient2 <- patient %>% filter(Patient=="Patient 2")

bplot1 = ggplot(patient1, aes(Time, Organisms_wrap, color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_color_manual(values=c("black", "red"))+
  scale_x_continuous(limits=c(0, 4), breaks=c(1, 2, 3))+
  labs(y = "Organism", x = "Time point", color = "detection by culture") + 
  ggtitle("Patient 1")+
  geom_text(label=round(patient1$proportion), nudge_y = -0.18)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"))
bplot1

out_file="/Users/aponsero/Desktop/jimmy_figures/dfu/ggsave_Patient1_dfu_TIF.tiff"
ggsave(out_file, width = 20, height = 20, units = "cm")

bplot2 = ggplot(patient2, aes(Time, Organisms_wrap, color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_color_manual(values=c("black", "red"))+
  scale_x_continuous(limits=c(0, 7), breaks=c(1, 2, 3, 4, 5, 6))+
  labs(y = "Organism", x = "Time point", color = "detection by culture") + 
  ggtitle("Patient 2")+
  geom_text(label=round(patient2$proportion), nudge_y = -0.15)+
  theme_classic(base_size = 20)+
  labs(fill = "Dose (mg)")+
  theme(axis.text.y = element_text(face = "italic"))
bplot2

out_file="/Users/aponsero/Desktop/jimmy_figures/dfu/ggsave_Patient2_dfu_TIFF.tiff"
ggsave(out_file, width = 20, height = 20, units = "cm")

#####################################################
#####################################################
###### VAP dataset

file_vap="/Users/aponsero/Desktop/jimmy_figures/vap/vap.csv"
vap <- readr::read_csv(file_vap)
percent_vap<- vap%>%mutate(proportion=abundance*100)

#wrap organisms names
percent_vap$Organisms_wrap = str_wrap(percent_vap$tax_name, width = 0)

bplot_vap = ggplot(percent_vap, aes(Day, tax_name, color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_color_manual(values=c("black", "red"))+
  scale_x_continuous(limits=c(0, 4), breaks=c(1, 3)) +
  labs(y = "Organism", x = "Day", color="detection by culture") + 
  geom_text(label=round(percent_vap$proportion), nudge_y = -0.4)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"))
bplot_vap

panel <- bplot_vap + facet_grid(Patient ~ .,scales="free_y",space="free")
panel

out_file="/Users/aponsero/Desktop/jimmy_figures/vap/ggsave_vapfacet_TIFF.pdf"
ggsave(out_file, width = 25, height = 17, units = "cm")

out_file="/Users/aponsero/Desktop/jimmy_figures/vap/ggsave_vapfacet.pdf"
ggsave(out_file, width = 25, height = 17, units = "cm")

vap1 <- percent_vap %>% filter(Patient=="Patient 1")
vap2 <- percent_vap %>% filter(sample=="Patient 2")

bplot3 = ggplot(vap1, aes(day, Organisms, color=detection)) + 
  geom_point(aes(size = proportion)) + 
  scale_x_continuous(limits=c(0, 4), breaks=c(1, 3)) +
  labs(y = "Organism", x = "Day") + 
  geom_text(label=round(vap1$proportion), nudge_y = -0.23)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"))
bplot3

bplot4 = ggplot(vap2, aes(day, Organisms, color=detection)) + 
  geom_point(aes(size = proportion)) + 
  labs(y = "Organism", x = "Day") + 
  scale_x_continuous(limits=c(0, 4), breaks=c(1, 3))+
  guides(color = FALSE) +
  geom_text(label=round(vap2$proportion), nudge_y = -0.2)+
  theme_classic(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"))
bplot4

#####################################################
#####################################################
# mock community with or without QC
file_QC="/Users/aponsero/Desktop/NeutropenicFever/mockcommunity/mockcommunity_centrifuge_report_with-QC.tsv"
file_noQC="/Users/aponsero/Desktop/NeutropenicFever/mockcommunity/mockcommunity_centrifuge_report_no-QC.tsv"
community_profile="/Users/aponsero/Desktop/NeutropenicFever/mockcommunity/profile.csv"
qc <- readr::read_tsv(file_QC)
nqc <- readr::read_tsv(file_noQC)
profile <- readr::read_csv(community_profile)

qc_species <- qc %>% filter(abundance > 0.00005) %>% select(name, abundance) %>% rename(qc=abundance)
qc_species
nqc_species <- nqc %>% filter(abundance > 0.00005) %>% select(name, abundance) %>% rename(no_qc=abundance)
nqc_species

data_cent <- full_join(qc_species,nqc_species, by="name") 
data_all <-  full_join(data_cent, profile, by="name")
data_all


#####################################################
#####################################################
### bugmixture
bug_all <- readr::read_csv("/Users/aponsero/Desktop/jimmy_figures/bugmix/all_bug_mix.csv")

filter_bug <- bug_all %>% filter(pct >= 0.001 & reads >=0.001)
my_bug=c('Escherichia coli', 'Staphylococcus saprophyticus', 'Shigella flexneri','Streptococcus pyogenes')


#calculate the false positives
false_pos <- filter_bug %>% filter(!tax_name %in% my_bug) %>% group_by(Experiment,mixture,tool) %>% summarize(pct=sum(pct))
tax_name <- rep("false positive", nrow(false_pos))
false_pos <- false_pos %>% add_column(tax_name)

#get only the true positives
true_pos <- filter_bug %>% filter(tax_name %in% my_bug)
# issue : streptococcus phage T12 to Streptococcus pyogenes https://www.ncbi.nlm.nih.gov/pubmed/9157243

# avengers assemble (true and false positives)!
data_plot <- bind_rows(true_pos,false_pos) %>% mutate(tax_name = fct_reorder(tax_name, pct))
data_plot <- data_plot %>% mutate(abundance=pct*100)

#fix order of the facets
names_experiments=c('E. coli - S. flexneri', 'S. saprophyticus - S. pyogenes', 'E. coli - S. saprophyticus')
data_plot$Experiment_f = factor(data_plot$Experiment, levels=names_experiments)

bplot_all = ggplot(data_plot, aes(mixture, tax_name)) + 
  geom_point(aes(size = abundance)) +
  labs("abundance", y = "Organism", x = "expected ratios") + 
  geom_text(label=round(data_plot$abundance,1), nudge_y = -0.3)+
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), labels=c("0.1","1","5","10","25","50","75","100"))+
  theme_classic(base_size = 15)+
  theme(axis.text.y = element_text(face = "italic",size = 15), 
        axis.text.x = element_text(angle = 50, size = 15, hjust = 1),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 8))
bplot_all

panel <- bplot_all + facet_grid(data_plot$Experiment_f ~ tool,scales="free_y",space="free")
panel

out_file="/Users/aponsero/Desktop/jimmy_figures/all_bug_mix.pdf"
ggsave(out_file, width = 40, height = 18, units = "cm")

### get R2 for Centrifuge
R2_cent <-  readr::read_csv("/Users/aponsero/Desktop/jimmy_figures/bugmix/R2_cent.csv")

coli_flex <- R2_cent %>% filter(Experiment=="E. coli - S. flexneri")
coli_flex.lm = lm(proportion ~ expected, data=coli_flex)
summary(coli_flex.lm)$r.squared

coli_sapro <- R2_cent %>% filter(Experiment=="E. coli - S. saprophyticus")
coli_sapro.lm = lm(proportion ~ expected, data=coli_sapro)
summary(coli_sapro.lm)$r.squared

sapro_pyo <- R2_cent %>% filter(Experiment=="S. saprophyticus - S. pyogenes")
sapro_pyo.lm = lm(proportion ~ expected, data=sapro_pyo)
summary(sapro_pyo.lm)$r.squared


#####################################################
#####################################################
### mock communities R2 plot
data_mock <-  readr::read_csv("/Users/aponsero/Desktop/jimmy_figures/mock_community/R2_plot/all_tools.csv")
bugs <- c("Rhodobacter sphaeroides","Staphylococcus epidermidis",
          "Streptococcus mutans","Escherichia coli","Pseudomonas aeruginosa",
          "Staphylococcus aureus","Streptococcus agalactiae","Bacillus cereus",
          "Clostridium beijerinckii","Helicobacter pylori","Propionibacterium acnes",
          "Neisseria meningitidis","Acinetobacter baumannii","Listeria monocytogenes",
          "Lactobacillus gasseri","Deinococcus radiodurans",
          "Enterococcus faecalis","Streptococcus pneumoniae","Bacteroides vulgatus")

data_plot <- data_mock %>% select(-Centrifuge_std) %>% filter(name %in% bugs) %>% gather(tool, abundance, Centrifuge:KrakenUnique)

#calculate the R2 coeficients
R_label<-"R-squared = "

cent <- data_plot %>% filter(tool=="Centrifuge")
cent.lm = lm(abundance ~ Expected, data=cent)
centR2<-summary(cent.lm)$r.squared
cent_label<-paste(R_label, as.character(round(centR2,3)))
cent_label

clark <- data_plot %>% filter(tool=="Clark")
clark.lm = lm(abundance ~ Expected, data=clark)
clarkR2<-summary(clark.lm)$r.squared
clark_label<-paste(R_label, as.character(round(clarkR2,3)))
clark_label

kraken <- data_plot %>% filter(tool=="KrakenUnique")
kraken.lm = lm(abundance ~ Expected, data=kraken)
krakenR2<-summary(kraken.lm)$r.squared
kraken_label<-paste(R_label, as.character(round(krakenR2,3)))
kraken_label


bplot_all = ggplot(data_plot, aes(abundance, Expected, color=tool)) + 
  geom_point() + 
  labs(y = "Expected abundance (Log10 %)", x = "measured abundance (Log10 %)") + 
  geom_smooth(method = lm, se = FALSE)+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  theme_classic(base_size = 10)+
  annotate("text", x = 1.00, y = 35.00, label = cent_label, color = "#F8766D", size = 4)+
  annotate("text", x = 1.00, y = 23.00, label = clark_label, color = "#00BA38", size = 4)+
  annotate("text", x = 1.00, y = 15.00, label = kraken_label, color = "#619CFF", size = 4)
bplot_all

out_file="/Users/aponsero/Desktop/jimmy_figures/R2.pdf"
ggsave(out_file, width = 17, height = 12, units = "cm")
