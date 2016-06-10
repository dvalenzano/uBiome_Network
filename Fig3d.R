library(ggplot2)

rar_sh <- read.csv("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/data/rarefaction_transfer/shannon_5500_all.csv", header=T)
rar_sh_sm <- read.csv("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/data/rarefaction_transfer/shannon_5500_small.csv", header=T)
rar_os <- read.csv("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/data/rarefaction_transfer/observed_5500_all.csv", header=T)
rar_os_sm <- read.csv("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/data/rarefaction_transfer/observed_5500_small.csv", header=T)

# rar_sh_stat <- aggregate(rar_sh$abundance,
#                          by=list(age=rar_sh$group0),
#                          FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)),
#                                              n = length(x)))

rar_sh_stat <- aggregate(rar_sh$abundance,
                         by=list(age=rar_sh$group0),
                         FUN = function(x) c(mean = mean(x), sd = sd(x),
                                          n = length(x)))

rar_sh_sm_stat <- aggregate(rar_sh_sm$abundance,
                         by=list(age=rar_sh_sm$group0),
                         FUN = function(x) c(mean = mean(x), sd = sd(x),
                                             n = length(x)))

rar_os_stat <- aggregate(rar_os$abundance,
                         by=list(age=rar_os$group0),
                         FUN = function(x) c(mean = mean(x), sd = sd(x),
                                             n = length(x)))

rar_os_sm_stat <- aggregate(rar_os_sm$abundance,
                            by=list(age=rar_os_sm$group0),
                            FUN = function(x) c(mean = mean(x), sd = sd(x),
                                                n = length(x)))

pdf("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/nature_2/Figures/rar_sh_all.pdf", width=5, height=4)

ggplot(rar_sh, aes(factor(group0), abundance)) + 
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

pdf("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/nature_2/Figures/rar_sh_small.pdf", width=5, height=4)

ggplot(rar_sh_sm, aes(factor(group0), abundance)) + 
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

pdf("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/nature_2/Figures/rar_os_all.pdf", width=5, height=4)

ggplot(rar_os, aes(factor(group0), abundance)) + 
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

pdf("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/nature_2/Figures/rar_os_small.pdf", width=5, height=4)

ggplot(rar_os_sm, aes(factor(group0), abundance)) + 
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

# Now I will try to display the stanard error instead of the standard deviation:

library(gridExtra)

MinMeanSEMMax <- function(x) {
  v <- c(min(x), mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)), max(x))
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}

g1 <- ggplot(rar_os_sm, aes(factor(group0), abundance)) + geom_boxplot() +
  ggtitle("Regular Boxplot")

g2 <- ggplot(rar_os_sm, aes(factor(group0), abundance)) +
  stat_summary(fun.data=MinMeanSEMMax, geom="boxplot", colour="red") + 
  ggtitle("Boxplot: Min, Mean-1SEM, Mean, Mean+1SEM, Max")

grid.arrange(g1, g2, ncol=2)

# IT WORKS GREAT!!
# NOW AGAIN, WITH THE ACTUAL DATA (STANDARD ERROR ONLY FOR THE SMALL DATASET)

pdf("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/nature_2/Figures/rar_sh_small_se.pdf", width=5, height=4)
ggplot(rar_sh_sm, aes(factor(group0), abundance)) + 
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

pdf("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/nature_2/Figures/rar_os_small_se.pdf", width=5, height=4)
ggplot(rar_os_sm, aes(factor(group0), abundance)) + 
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