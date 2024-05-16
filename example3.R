library(tidyverse)
library(ggtree)
library(ggtreeExtra)
library(treeio)
library(ggnewscale)

#读取树文件
tree <- read.tree("data/example3.nwk")

#读取tip的丰度数据等信息
anno <- read.csv("data/example3_anno.csv", header = T, check.names = F, row.names = 1)

#基于node，添加背景颜色
#ggtree(tree)+geom_text2(aes(label=node), size = 3, color = "red") +  #用来查看node编号
in_anno <- read.csv("data/example3_anno_highlight.csv", header = T, check.names = F, row.names = 1)


#1.树的主体：layout为树的布局，size为枝的粗细
p <- ggtree(tree,
            layout = "fan", 
            branch.length = "none", 
            size = 1)
p


#2.添加背景颜色：in_anno记录了填充信息
p1 <- p+geom_highlight(data = in_anno,
                    mapping = aes(node=node,fill=Phylum),
                    to.bottom = T,
                    alpha = 0.4)+
  scale_fill_manual(values = c("#FEA443","#B8DFD8","#FF616D","#D5C455",
                               "#957DAD","#F8C3AF"),
                    guide=guide_legend(ncol = 1,
                                       title = "Phylum",
                                       order = 1,
                                       override.aes = list(alpha = 1, color="#000000")))
p1


#3.添加分支：用点的大小表示丰度
p2 <- p1 + 
  new_scale_fill()+
  geom_fruit(
    data=anno,
    geom=geom_point,
    mapping=aes(y=ID, size=`ASV size`)) + 
  scale_size_continuous(
    range=c(1, 4), 
    guide=guide_legend(keywidth=0.5,keyheight=0.5))

p2

#4. 映射差异值：用颜色深浅表示
p3 <- p2 +
  new_scale_fill()+
  geom_fruit(data=anno,
             geom=geom_tile,
             mapping=aes(y=ID, fill=`Niche breadths`),
             pwidth=2.5,
             offset = 0.25) + 
  scale_fill_gradient(low = "#D3D3D3", high = "#F08080",
                      name = "\u0394 Niche breadths")
p3

#4.添加分支名字，修改legend主题
p3 + geom_tiplab(size=5, fontface=2, offset=4.5) +
  theme(legend.text = element_text(colour="black", size = 20),
        legend.title = element_text(color = "black", size = 20, face = "bold"))

ggsave("figure/example3.png", width = 10, height = 8, dpi = 600)
ggsave("figure/example3.pdf", width = 10, height = 8)

