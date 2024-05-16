library(tidyverse)
library(ggtree)
library(ggtreeExtra)
library(treeio)
library(ggnewscale)
library(phangorn)

#读取树文件
tree <- read.tree('data/example2.nwk')
tree <- midpoint(tree)

#读取注释信息

#热图数据
eco_tab <- read.csv('data/example2_anno_heatmap.csv', header = T, check.names = F, row.names = 1)

#柱形图数据
coef_tab <- read.csv('data/example2_anno_bar.csv', header = T, check.names = F, row.names = 1)
coef_tab_pos_only <- coef_tab
coef_tab_pos_only[coef_tab_pos_only < 0] = 0

#自定义颜色
# phylum: c('Acidobacteriota','Actinobacteriota','Bacteroidota','Chloroflexi','Cyanobacteria','Firmicutes','Others,'Patescibacteria','Planctomycetota','Proteobacteria','Verrucomicrobiota')
# colors: c('#B09C85FF','#F39B7FFF','#DC0000FF','#91D1C2FF','#00A087FF','#7E6148FF','grey','dimgrey','#4DBBD5FF','#3C5488FF','#8491B4FF')
mycol <- c('#B09C85FF','#F39B7FFF','#DC0000FF','#91D1C2FF',
           '#00A087FF','#7E6148FF','grey','dimgrey',
           '#4DBBD5FF','#3C5488FF','#8491B4FF')

#绘图
#1.树的主体: 树枝太多把size调小，圆形布局，并打开一个小角度
p <- ggtree(tree, layout="fan", size=0.15, open.angle=10)

#p


#2.外圈注释1: 热图形式，颜色代表Phylum，透明度代表环境中有无
p1 <- p + 
  geom_fruit(data=eco_tab,
             geom=geom_tile,
             mapping=aes(y=ASV, x=Ecosystem, fill=Phylum, alpha=Presence),
             offset = 0.04, size = 0.02,
             axis.params = list(axis = "x", 
                                text.angle = -90, text.size = 2,
                                hjust=0, vjust = 1)) +
  scale_fill_manual(values = mycol) +
  scale_alpha_discrete()

#p1


#3.外圈注释2：柱形图形式，颜色代表Phylum，高度代表SVM系数
p2 <- p1 + 
  new_scale_fill() +
  geom_fruit(data=coef_tab_pos_only, geom=geom_bar,
             mapping=aes(y=ASV, x=Coefficient, fill=Phylum, colour=Phylum),
             pwidth=0.38, 
             orientation="y", 
             stat="identity", 
             axis.params=list(axis = "x",text.size = 2, 
                              hjust = 1,vjust = 1, nbreak = 3), 
             grid.params=list()) + 
  scale_fill_manual(values = mycol) +
  scale_color_manual(values = mycol)


ggsave('figure/example2.pdf', width = 10, height = 8)


