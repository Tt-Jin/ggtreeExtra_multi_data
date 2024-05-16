library(tidyverse)
library(ggtree)
library(ggtreeExtra)
library(treeio)
library(ggnewscale)

#读取树文件
tree <- read.tree("data/example1.nhx")

#读取注释信息
anno <- readxl::read_xlsx("data/example1_anno.xlsx")

#自定义颜色
mycol = c("#e6550d","#fd8d3c","#fdae6b","#fdd0a2","#31a354",
          "#74c476","#a1d99b","#c7e9c0","#756bb1","#9e9ac8",
          "#bcbddc","#dadaeb","#636363","#969696","#bdbdbd",
          "#d9d9d9","#3182bd","#6baed6","#9ecae1","#c6dbef")

mylevel = c("Proteobacteria (N=2474)","Bacteroidota (N=822)","Actinobacteriota (N=383)",
            "Cyanobacteria (N=281)","Firmicutes (N=187)","Planctomycetota (N=123)",
            "Chloroflexota (N=117)","Verrucomicrobiota (N=107)","Desulfobacterota (N=78)",
            "Marinisomatota (N=76)","Campylobacterota (N=59)","Firmicutes_A (N=51)",
            "Myxococcota (N=41)","Bdellovibrionota (N=31)","Spirochaetota (N=30)",
            "Desulfobacterota_A (N=26)","Halobacterota (N=132)","Thermoplasmatota (N=119)",
            "Crenarchaeota (N=109)","Euryarchaeota (N=52)")

#树枝添加颜色：将 Phylum 信息和进化树组合到一起
df_phylum <- subset(anno, select = c(ID, `Phylum (Top 20)`))
list_phylum <- split(df_phylum$ID, df_phylum$`Phylum (Top 20)`)

tree_phylum <- groupOTU(tree, list_phylum)

#1.树的主体：layout为树的布局，size为枝的粗细
p <- ggtree(tree_phylum,
            layout = "fan",
            open.angle = 10,
            aes(color = factor(group, levels = mylevel)),
            size = 0.7) +
  scale_color_manual(values = mycol, na.value = "#000000",
                     guide="none")
p

#2. 外圈注释1：Genome Type
p1 <- p + new_scale_fill() +
  geom_fruit(
    data = anno,
    geom = geom_tile,
    mapping = aes(y = ID, fill = factor(`Genome type`, 
                                        levels = c("WGS", "MAG", "SAG"))),
    pwidth = 0.1,offset = 0.05) +
  scale_fill_manual(values = c("#000000", "#ff7f0e", "#1f77b4"),
                    guide=guide_legend(keywidth=0.8, keyheight=0.8,
                                       ncol = 1,
                                       title = "Genome Type",
                                       title.position = "top",
                                       override.aes = list(color="#000000",size=4),
                                       order = 1))

p1

#3. 外圈注释2：Phylum
p2 <- p1 + new_scale_fill() +
  geom_fruit(
    data = anno,
    geom = geom_tile,
    mapping = aes(y = ID, fill = factor(`Phylum (Top 20)`, levels = mylevel)),
    pwidth = 0.1,offset = 0.08) +
  scale_fill_manual(values = mycol, na.translate=FALSE, 
                    guide=guide_legend(keywidth=0.8, keyheight=0.8,
                                       ncol = 2,
                                       title = "Phylum (Top 20)",
                                       title.position = "top",
                                       override.aes = list(color="#000000",size=4),
                                       order = 3)) 

p2

#4. 外圈注释3：Presence
p3 <- p2 + new_scale_fill() +
  geom_fruit(
    data = anno,
    geom = geom_tile,
    mapping = aes(y = ID, fill = factor(`Occurrence in samples`, 
                                        levels = c("Present in ≥10 photic samples", 
                                                   "Present in at least 1 photic sample", 
                                                   "Absent from photic samples"))),
    pwidth = 0.1, offset = 0.09) +
  scale_fill_manual(values = c("#00711d", "#74c989", "#ffffff"), 
                    guide=guide_legend(keywidth=0.8, keyheight=0.8,
                                       ncol = 1,
                                       title = "Occurrence in samples",
                                       title.position = "top",
                                       override.aes = list(color="#000000",size=4),
                                       order = 2))

p3

#最后再加上几个标签
p3 + geom_text(data = data.frame(x=c(1.85,2,2.15),y=c(10),
                              label=c("Type   ","Phylum ","Presence")),
            aes(x,y,label=label), inherit.aes = FALSE, angle=90,hjust=1,size=2.8)

#ggsave("figure/example1.png", width = 11, height = 8, dpi = 600)
ggsave("figure/example1.pdf", width = 11, height = 8)
