library(FSA)
library(dunn.test)
tab <- read.csv("../DValenzano/uBiome/data/carn_arthr.csv", head=T)
kruskal.test(tab$carn, tab$Taxon)

dunn.test(tab$carn, tab$Taxon,
         method="bh")

# Kruskal-Wallis rank sum test
# 
# data: x and group
# Kruskal-Wallis chi-squared = 19.8847, df = 4, p-value = 0
# 
# 
# Comparison of x by group                            
# (Benjamini-Hochberg)                              
# Col Mean-|
#   Row Mean |      ABX16       SM16       WT16        WT6
#   ---------+--------------------------------------------
#       SM16 |  -1.597581
#            |     0.0787
#            |
#       WT16 |  -1.697518   0.088791
#            |     0.0747     0.4646
#            |
#        WT6 |  -3.485790  -1.486431  -1.834331
#            |    0.0012*     0.0857     0.0666
#            |
#       YM16 |  -3.957404  -2.052068  -2.440956  -0.792272
#            |    0.0004*     0.0502    0.0244*     0.2379

pairwise.wilcox.test(tab$carn, tab$Taxon,
                     method="BH")

# 
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  tab$carn and tab$Taxon 
# 
#        ABX16  SM16   WT16   WT6   
#   SM16 0.0606 -      -      -     
#   WT16 0.1933 1.0000 -      -     
#   WT6  0.0065 0.3448 0.2025 -     
#   YM16 0.0065 0.0653 0.0559 0.8843
# 
# P value adjustment method: holm 
# Warning message:
#   In wilcox.test.default(xi, xj, paired = paired, ...) :
#   cannot compute exact p-value with ties

dunn.test(tab$arthr, tab$Taxon,
          method="bh")

# Kruskal-Wallis rank sum test
# 
# data: x and group
# Kruskal-Wallis chi-squared = 21.7594, df = 4, p-value = 0
# 
# 
# Comparison of x by group                            
# (Benjamini-Hochberg)                              
# Col Mean-|
#   Row Mean |      ABX16       SM16       WT16        WT6
#   ---------+--------------------------------------------
#       SM16 |   0.191074
#            |     0.4242
#            |
#       WT16 |   1.558092   1.273074
#            |     0.0852     0.1269
#            |
#        WT6 |  -1.672801  -1.788565  -3.638023
#            |     0.0786     0.0737    0.0007*
#            |
#       YM16 |  -2.239757  -2.324745  -4.102146  -0.776111
#            |     0.0314     0.0335    0.0002*     0.2432

pairwise.wilcox.test(tab$arthr, tab$Taxon,
                     method="BH")

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  tab$arthr and tab$Taxon 
# 
#        ABX16  SM16   WT16   WT6   
#   SM16 1.0000 -      -      -     
#   WT16 0.1762 0.1762 -      -     
#   WT6  0.2620 0.2070 0.0143 -     
#   YM16 0.0326 0.0140 0.0086 1.0000
# 
# P value adjustment method: holm 

