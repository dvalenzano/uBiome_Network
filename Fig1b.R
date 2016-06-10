yvso <- read.csv("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/Figure1/y-o_otus.csv", header=T)

yvso$depth[yvso$age == '6wk'] <- 'early'
yvso$depth[yvso$age == '16wk'] <- 'late'

yvso_os <- subset(yvso, yvso$measure == 'os')

yo_stat <- aggregate(yvso$OTUs,
                     by = list(measure = yvso$measure, age = yvso$depth),
                     FUN = function(x) c(mean = mean(x), sd = sd(x),
                                         n = length(x)))

yvso_pd <- subset(yvso, yvso$measure == 'pd')
yo_stat_pd <- aggregate(yvso_pd$OTUs,
                        by = list(age = yvso_pd$dept),
                        FUN = function(x) c(mean = mean(x), sd = sd(x),
                                            n = length(x)))

yvso_sh <- subset(yvso, yvso$measure == 'sh')
yo_stat_sh <- aggregate(yvso_sh$OTUs,
                        by = list(age = yvso_sh$dept),
                        FUN = function(x) c(mean = mean(x), sd = sd(x),
                                            n = length(x)))

# Shannon Diversity
yo_stat_sh <- subset(yo_stat, yo_stat$measure == "sh")

limits_yo_sh <- aes(ymax = yo_stat_sh$x[,1] + yo_stat_sh$x[,2],
                    ymin = yo_stat_sh$x[,1] - yo_stat_sh$x[,2])

time1 <- c("early", "late")
yo_stat_sh$time <- time1

p_sh <- ggplot( data = yo_stat_sh, aes(x = factor(time), y = x[,1]))

pdf("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/nature_2/Figures/Figure1B_sh.pdf", width=5, height=4)

ggplot(yvso_sh, aes(factor(depth), OTUs)) + 
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

pdf("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/nature_2/Figures/Figure1B_os.pdf", width=5, height=4)

ggplot(yvso_os, aes(factor(depth), OTUs)) + 
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

####10.06.2016###NOW I MODIFY THE SCRIPT TO DISPLAY STANDARD ERROR INSTEAD OF STANDARD DEVIATION##MinMeanSEMMax 

MinMeanSEMMax <- function(x) {
  v <- c(min(x), mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)), max(x))
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}

pdf("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/nature_2/Figures/Figure1B_sh_se.pdf", width=5, height=4)

ggplot(yvso_sh, aes(factor(depth), OTUs)) + 
  stat_summary(fun.data=MinMeanSEMMax, geom="boxplot", fill="grey80") + 
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

pdf("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/nature_2/Figures/Figure1B_os_se.pdf", width=5, height=4)

ggplot(yvso_os, aes(factor(depth), OTUs)) + 
  stat_summary(fun.data=MinMeanSEMMax, geom="boxplot", fill="grey80") + 
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
