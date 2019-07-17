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


### bugmixture
bug_all <- readr::read_csv("all_bug_mix.csv")

filter_bug <- bug_all %>% filter(pct >= 0.001 & reads >=0.001)
my_bug=c('Escherichia coli', 'Staphylococcus saprophyticus', 'Shigella flexneri','Streptococcus pyogenes')


#calculate the false positives
false_pos <- filter_bug %>% filter(!tax_name %in% my_bug) %>% group_by(Experiment,mixture,tool) %>% summarize(pct=sum(pct))
tax_name <- rep("false positive", nrow(false_pos))
false_pos <- false_pos %>% add_column(tax_name)

#get only the true positives
true_pos <- filter_bug %>% filter(tax_name %in% my_bug)

# avengers assemble (true and false positives)!
data_plot <- bind_rows(true_pos,false_pos) %>% mutate(tax_name = fct_reorder(tax_name, pct))
data_plot <- data_plot %>% mutate(abundance=pct*100)

#fix order of the facets
names_experiments=c('E.coli - S.flexneri', 'S.saprophyticus - S.pyogenes', 'E.coli - S.saprophyticus')
data_plot$Experiment_f = factor(data_plot$Experiment, levels=names_experiments)

bplot_all = ggplot(data_plot, aes(mixture, tax_name)) + 
  geom_point(aes(size = abundance)) +
  labs("abundance", y = "Organisms detected", x = "Expected ratios") + 
  geom_text(label=round(data_plot$abundance,1), nudge_y = -0.3)+
  scale_size(limits=c(0,100),breaks=c(0.1,1,5,10,25,50,75,100), 
             labels=c("0.1","1","5","10","25","50","75","100"))+
  theme_classic(base_size = 20)+
  theme(text=element_text(family="Times New Roman"),
        axis.text.y = element_text(face = "italic",size = 15), 
        axis.text.x = element_text(angle = 50, size = 15, hjust = 1),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(face = "italic", size = 15),
        strip.background.y = element_rect(fill = "grey"),
        strip.background.x = element_rect(colour = NA),
        panel.spacing.x = unit(0,"line"))

bplot_all

panel <- bplot_all + 
  facet_grid(data_plot$Experiment_f ~ tool,scales="free_y",space="free", labeller = label_wrap_gen(10))
panel

out_file="Fig1.png"
ggsave(out_file, width = 40, height = 18, units = "cm")

### get calculation of R2 for Centrifuge
R2_cent <-  readr::read_csv("R2_cent.csv")

coli_flex <- R2_cent %>% filter(Experiment=="E. coli - S. flexneri")
coli_flex.lm = lm(proportion ~ expected, data=coli_flex)
summary(coli_flex.lm)$r.squared

coli_sapro <- R2_cent %>% filter(Experiment=="E. coli - S. saprophyticus")
coli_sapro.lm = lm(proportion ~ expected, data=coli_sapro)
summary(coli_sapro.lm)$r.squared

sapro_pyo <- R2_cent %>% filter(Experiment=="S. saprophyticus - S. pyogenes")
sapro_pyo.lm = lm(proportion ~ expected, data=sapro_pyo)
summary(sapro_pyo.lm)$r.squared


### get calculation of R2 for CLARK
R2_clark <-  readr::read_csv("R2_CLARK.csv")

coli_flex <- R2_clark %>% filter(Experiment=="E.coli - S.flexneri")
coli_flex.lm = lm(proportion ~ expected, data=coli_flex)
summary(coli_flex.lm)$r.squared

coli_sapro <- R2_clark %>% filter(Experiment=="E.coli - S.saprophyticus")
coli_sapro.lm = lm(proportion ~ expected, data=coli_sapro)
summary(coli_sapro.lm)$r.squared

sapro_pyo <- R2_clark %>% filter(Experiment=="S.saprophyticus - S.pyogenes")
sapro_pyo.lm = lm(proportion ~ expected, data=sapro_pyo)
summary(sapro_pyo.lm)$r.squared

### get calculation of R2 for KU
R2_KU <-  readr::read_csv("R2_KU.csv")

coli_flex <- R2_KU %>% filter(Experiment=="E.coli - S.flexneri")
coli_flex.lm = lm(proportion ~ expected, data=coli_flex)
summary(coli_flex.lm)$r.squared

coli_sapro <- R2_KU %>% filter(Experiment=="E.coli - S.saprophyticus")
coli_sapro.lm = lm(proportion ~ expected, data=coli_sapro)
summary(coli_sapro.lm)$r.squared

sapro_pyo <- R2_KU %>% filter(Experiment=="S.saprophyticus - S.pyogenes")
sapro_pyo.lm = lm(proportion ~ expected, data=sapro_pyo)
summary(sapro_pyo.lm)$r.squared


