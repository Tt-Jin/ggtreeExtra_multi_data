library(tidyverse)
library(ggsci)
library(forcats)
library(ggtree)
library(ggtext)


### 1. Read in data files

### This data file contains summary of the two datasets; Gentry et al, and Lucas et al
bacteria_producers<- read.csv("Rimal.2024.Nature.figure1d/producer_gene.csv") %>% 
  select(-X) %>% 
  distinct(Accession, .keep_all = TRUE) %>%
  rename(fileName=label) %>%
  mutate(Accession=as.numeric(gsub("GC[F|A]_","",Accession))) %>% select(-gene,-n)

### This data file contains gene presence absence table from the Prokka analysis of the all the genomes of the bacteria involved in above studies. The table is filtered for the genes involved in secondary bile acid metabolism. Only bsh will be plotted. 
prokka_gene_presence_absence <- read.csv("Rimal.2024.Nature.figure1d/prokka_merged_output.csv",header=TRUE) %>%
  rename(Accession="locus_tag") %>%
  mutate(Accession=gsub("GC[A|F]_","",Accession),
         Accession=as.numeric(Accession)) %>%
  select(-X)

### Taxonomy table for each bacteria
bacteria_taxonomy<- read.csv("Rimal.2024.Nature.figure1d/merged_metadata.csv")


### Tree file for the taxa involved in the study; used to make the circle plot
tree<-ggtree::read.tree("Rimal.2024.Nature.figure1d/phyliptree.ph")

### Tree has non-standard labels; this file helps to get taxid as label and use that to merge with our bacteria taxaid
tree_taxid <- read.table("Rimal.2024.Nature.figure1d/read_tree_taxid.txt",sep="\t", header=TRUE) %>% 
  select(name,taxid)



###2. Prep

#prokka_gene_presence_absence[-1][prokka_gene_presence_absence[-1] != 0] <- 1
bacteria_producers<-bacteria_producers %>% 
  merge(prokka_gene_presence_absence,by="Accession",all=TRUE)

## Root the tree at midpoint
tree_rooted <- phangorn::midpoint(tree, node.labels="support")

## Tree as df; helpful to change labels
tree_df <-tree %>% as.tibble %>% mutate(label=gsub("'","",label)) #%>% write.csv("tree_df.csv") 

### changing labels
tree_df_taxid <- merge(tree_df,tree_taxid, by.x="label", by.y="name") %>%
  merge(bacteria_taxonomy,by="taxid") %>% select(-c(X,label.y)) %>%
  rename(label=label.x) %>%
  mutate(organism_name=gsub("","",organism_name)) %>%
  mutate(Accession=as.numeric(gsub("GC[F|A]_","",Accession))) 



####3. Base tree

tree_rooted$node.label = gsub("'","",tree_rooted$node.label)


p<-ggtree::ggtree(tree_rooted, branch.length = "none", layout="circular", size=.3, open.angle = 5) %<+% 
   tree_df_taxid + 
   geom_tiplab(size=2,aes(label=label), offset=0.1) + 
   scale_color_discrete(name="genus", na.value="grey40")

p <- ggtree(tree_rooted, 
            layout="circular", 
            size=0.5, 
            open.angle=5,
            branch.length = "none",
            aes(color=phylum)) %<+%
  tree_df_taxid + 
  scale_color_npg(palette=c("nrc"),na.value="grey40")

p


p1 <- p %>% collapse(node=240)  +
  geom_point2(aes(subset=(node==240)),shape=16,size=3,color='red') 

p1 <- p1 %>% collapse(node=236) +
  geom_point2(aes(subset=(node==236)),shape=18,size=3,color='red') 

p1 <- p1 %>% collapse(node=278) +
  geom_point2(aes(subset=(node==278)),shape=18,size=3,color='blue') 

p1$data<-p1$data %>% mutate(label=gsub("'","",label))

p1


### Plot in MCBAs data
tree_df_producer_gene<-tree_df_taxid %>%
  merge(bacteria_producers, by=c("Accession","organism_name"), all.x=TRUE) %>%
  # mutate(
  #   gene=as.factor(gene)) %>%
  mutate(production = ifelse(
    nMCB>0,1,0))

proportions_df <- tree_df_producer_gene %>% 
  select(label,proportion) %>%
  column_to_rownames("label") 

p2<-gheatmap(p1, 
             proportions_df, 
             offset=1, width=0.1,
             color = 'grey40',
             colnames=FALSE) +
  scale_fill_gradient2(
    low = "white", 
    mid="#ED7F7F",
    high = "#DC0000FF", 
    midpoint = 0.3)  +
  labs(fill="Fraction of total <br /> BBAAs detected") +
  theme(legend.title = element_markdown())

p2

### BSH
bsh_df <- tree_df_producer_gene %>% 
  select(label,`Choloylglycine.hydrolase`) %>%
  column_to_rownames("label") %>% 
  rename(bsh="Choloylglycine.hydrolase") %>%
  mutate(bsh=case_when(
    bsh==1 ~ "Present",
    bsh ==0 ~ "Absent")) %>%
  mutate(bsh = as.factor(bsh))

p3 <- p2 + ggnewscale::new_scale_fill()

p4 <- gheatmap(p3, 
               bsh_df, 
               offset=1.75, width=0.1,
               color = 'grey40',
               colnames=FALSE) + 
  scale_fill_manual(values=c("#EBF7FA","#4DBBD5FF"),na.translate=FALSE,
                    name="Presence of *bsh* gene",
                    guide=guide_legend(order = 2)) +
  theme(legend.title = element_markdown())

p4

###
nodeids <- nodeid(tree, tree_rooted$node.label[tree_rooted$node.label %in% tree_df_taxid$genus])
nodelab <- tree_rooted$node.label[tree_rooted$node.label %in% tree_df_taxid$genus]
nodedf <- data.frame(id=nodeids, label=nodelab)

genus_to_highlight=c("Bifidobacterium","Enterococcus","Bacteroides",
                     "Fusobacterium","Collinsella","Clostridium")

nodedf <- nodedf%>% filter(nodelab %in% genus_to_highlight) 
#nodedf$type <- as.factor(genus_to_highlight)

poslist <- c(0.3,0.3,0.3,0.3,0.3,0.3)
nodedf$pos <- poslist

p5 <- p4 +
  geom_hilight(data=nodedf, 
                       mapping=aes(node=id),
                       fill = c('red','red','#009f86','#009f86','#3c5487','#4dbad5'),
                       extendto=8, alpha=0.3,
                       size=0.03) +
  geom_cladelab(data=nodedf, 
                mapping=aes(node=id, 
                            label=label,
                            offset.text=pos),
                hjust=0.5,
                angle="auto",
                barsize=NA,
                horizontal=FALSE, 
                fontface="bold",
                fontsize=3)
  

p5

ggsave("Rimal.2024.Nature.figure1d/BSH.pdf", width = 8, height = 6)

