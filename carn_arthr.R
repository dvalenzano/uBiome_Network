library(ggplot2)
tab <- read.csv("../DValenzano/uBiome/data/carn_arthr.csv", head=T)

pdf("../DValenzano/uBiome/Jun2016/Carnobacterium.pdf", width=5, height=4)
ggplot(tab, aes(factor(Taxon), carn)) + 
  geom_boxplot(outlier.shape=NA, fill="grey80") + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.2, height=0), size=2, colour="red") +
  theme(axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title.x= element_text(size=20),
        axis.title.y= element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))
dev.off()

pdf("../DValenzano/uBiome/Jun2016/Arthrobacter.pdf", width=5, height=4)
ggplot(tab, aes(factor(Taxon), arthr)) + 
  geom_boxplot(outlier.shape=NA, fill="grey80") + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.2, height=0), size=2, colour="red") +
  theme(axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title.x= element_text(size=20),
        axis.title.y= element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))
dev.off()