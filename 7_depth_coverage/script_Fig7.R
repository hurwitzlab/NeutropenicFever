#!/usr/bin/env Rscript

library(ggplot2)
library(gridExtra)

setwd("/rsgrps/bhurwitz/jetjr/neutropenicfever/coverage/")

df <- read.delim("sbw25.coverage", header=F)
names(df) <- c("name", "Position", "Depth")
df$name <- "SBW25"

ttv8 <- read.delim("ttv_new.coverage", header=F)
names(ttv8) <- c("name", "Position", "Depth")

parvo <- read.delim("parvovirus.coverage", header=F)
names(parvo) <- c("name", "Position", "Depth")

p1 <- ggplot(df, aes(x=Position, y=Depth, fill = name)) + geom_area()
p1 <- p1 + ggtitle("NF001: Pseudomonas fluorescens") + theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill="white", colour="gray53"))
p1 <- p1 + scale_fill_manual(values="black") + guides(fill=FALSE) + theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill="white", colour="gray53"), text = element_text(size=14))
p1 <- p1 + scale_y_continuous(limits = c(0, 70))

p2 <- ggplot(parvo, aes(x=Position, y=Depth, fill = name)) + geom_area()
p2 <- p2 + ggtitle("NF002: Human Parvovirus B19") + theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill="white", colour="gray53"))
p2 <- p2 + scale_fill_manual(values="black") + guides(fill=FALSE) + theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill="white", colour="gray53"), text = element_text(size=14))

p3 <- ggplot(ttv8, aes(x=Position, y=Depth, fill = name)) + geom_area()
p3 <- p3 + ggtitle("NF003: Torque Teno Virus") + theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill="white", colour="gray53"))
p3 <- p3 + scale_fill_manual(values="black") + guides(fill=FALSE) + theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill="white", colour="gray53"), text = element_text(size=14))

#png(filename="coverage.png", width = 800, height = 1000)
p4 <- grid.arrange(p1, p2, p3, ncol = 1)
#print(p4)
#dev.off()


svg(filename = "coverage.svg", width = 7, height = 8)
p4 <- grid.arrange(p1, p2, p3, ncol = 1)
print(p4)
dev.off()
