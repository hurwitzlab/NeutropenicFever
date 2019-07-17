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


#coverage plot
coverage="meca_reads_aligned_sorted.coverage.tab"
data_cov <- readr::read_tsv(coverage)

p1 <- ggplot(data_cov, aes(x=Position, y=Depth)) + 
  geom_area()+
  theme_classic()+
  labs(y = "Coverage", x = "MecA gene position (bp)")
p1

out_file="S1_Fig_mecA.pdf"
ggsave(out_file, width = 17, height = 10, units = "cm")
