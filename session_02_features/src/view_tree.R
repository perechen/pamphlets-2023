

view_tree <- function(stylo_res,
                       k=2,
                       p=0.05,
                       color_leaves=F,
                       output=T,
                       label_size=6,
                       leaf_size=1) {

  
## check for required packages
pcks <- c("dplyr", "stringr", "stylo", "FactoMineR", "dendextend", "ggplot2", "paletteer", "tidyr")
required_packages <- setdiff(pcks, rownames(installed.packages()))
  
if(length(required_packages) > 0) {
    stop(c('\nSorry, you need additional packages to run this function:\n\n,',paste(required_packages, collapse=", ")))
  }
  
  
## data
library(dplyr)
library(stringr)
## analysis
library(stylo)
library(FactoMineR)
## plotting
library(dendextend)
library(ggplot2)
library(paletteer)
library(tidyr)

  

if(class(stylo_res) != "stylo.results") {
  stop("\nWrong input! Data should come from `stylo()` results!")
}



## get labels
lbls <- rownames(stylo_res$table.with.all.freqs)
## get classes
classes <- str_extract(lbls,"^.*?(?=_)")


## get palette 

if(color_leaves) {
clrs <- paletteer_d("basetheme::minimal")

## expand palette if needed
if(length(classes) > length(clrs)) {
  multiplier <- ceiling(length(classes)/length(clrs))
    clrs <- rep(clrs, multiplier)}

} else {
  clrs <- "black"
} ## end of if color statement

## get cluster palette
clust_clrs <- paletteer_d("Polychrome::dark")

## expand cluster palette if needed
if(length(k) > length(clust_clrs)) {
  multiplier <- ceiling(length(k)/length(clust_clrs)) 
    clust_clrs <- rep(clust_clrs, multiplier)
}


# s <- palettes_d_names %>%
#   filter(type != "sequential") %>% sample_n(1)
# paletteer_d(paste0(s$package,"::",s$palette))
# v <- paletteer_d(paste0(s$package,"::",s$palette))

## assign colors to classes
tdf <- tibble(class=unique(classes)) %>%
  mutate(class_id=row_number(),
         colors=clrs[1:length(unique(classes))])

## hierarchical clustering with Ward's linkage (the method that stylo uses)
tr <- hclust(as.dist(stylo_res$distance.table),method = "ward.D2") %>% 
  as.dendrogram() 

## cut tree
tr_cut <- cutree(tr,k=k)
k_df <- tibble(k_id = tr_cut,label=names(tr_cut))

## match dendro labels with colors
meta <- tibble(label=labels(tr))  %>%
  mutate(class=str_extract(label,"^.*?(?=_)")) %>% 
  left_join(tdf,by="class") %>% 
  left_join(k_df,by="label")

## color labels
# v <- rep(NA,9)
# v[c(4,3,8,6,9)] <- 19
# vs <- v
# vs[c(4,3,8,6,9)] <- 3

tr <- tr %>%
  set("labels_col", value=meta$colors) %>% # label colors
  set("labels_cex", leaf_size)
#  set("nodes_pch",v) %>% # draw needed nodes
#  set("nodes_cex",vs)# size of nodes


## plotting
par(mfrow=c(1,1),mar = c(2,2,2,22))
tr %>% plot(horiz=T,main=paste("Hierarchical clustering,"," cut at k=",k))
tr %>% rect.dendrogram(horiz = TRUE, border = clust_clrs,lty=5,lwd=3,k=k)

## subset frequencies table by features used
t <- stylo_res$table.with.all.freqs[,stylo_res$features.actually.used] 

k_sort <- meta %>% arrange(label) %>% pull(k_id)
t <- bind_cols(tibble(key1k_=as.character(k_sort)),as_tibble(t))

corr <- FactoMineR::catdes(t, num.var = 1,proba = p)

k_names <- names(corr$quanti)

dfs <- lapply(corr$quanti, function(x) {
  f <- rownames(x)
  tb <- as_tibble(x) %>% 
    mutate(feature=f)
  
  if(length(tb) == 0) {
    tb = tibble(v.test=1,
                `Mean in category`=NA,
                `Overall mean`=NA,
                `sd in category`=NA,
                `Overall sd`=NA,
                p.value=NA,
                feature="NO_FEATURE_AT_P_THRESHOLD")
  }
  return(tb)
  
})
n <- sapply(dfs,nrow)

## reverse names because (???) dendextend inexplicably reverses clusters (???) this should be prosecuted in court
f_df <- bind_rows(dfs) %>%mutate(k_id=rep(rev(k_names),n)) %>% 
  group_by(k_id) %>% 
  mutate(rank=row_number()) %>% 
  ungroup()

suppressWarnings(
fplot <- f_df %>% filter(v.test>0) %>%
  ggplot(aes(k_id, -rank)) + 
  geom_text(aes(label=feature,color=k_id),size=label_size,hjust=0) + 
  scale_size(range=c(8,15)) + 
  scale_x_discrete(position="top",labels=paste("Cluster", rev(k_names), "\n")) + 
  scale_color_manual(values=clust_clrs) + 
  theme_void() + 
  guides(size="none", color="none") + 
  theme(axis.text.x = element_text(size=24,color = clust_clrs))
)

print(fplot)


## prepare collapsed feature-per-vector representation
output_df <- bind_rows(dfs) %>%mutate(k_id=rep(k_names,n)) %>% 
  group_by(k_id) %>% 
  mutate(rank=row_number(),
         k_id=as.integer(k_id)) %>% 
  ungroup() %>% filter(v.test>0) %>%
  select(k_id,v.test,rank,feature) %>% 
  group_by(k_id) %>% 
  summarize(feature=paste(feature,collapse = " "),.groups="keep")

report <- meta %>%
  left_join(output_df,by="k_id")  %>%
  group_by(feature,k_id) %>% 
  summarize(label=paste(label,collapse="\n"),.groups="keep") %>%
  arrange(k_id)

if(output) {

filename <- paste0("words_behind_trees_k",k,"_p",str_remove(as.character(p),"\\."), ".txt")
file.create(filename,overwrite=T)
for(i in 1:nrow(report)) {
  
  txt <- paste0("CLUSTER ",report$k_id[i], "\n==============\nTEXTS\n",report$label[i], "\n==============\nFEATURES associated (p<",p,")\n\n", report$feature[i], "\n\n\n\n")
  
  write(txt,file = filename,append = T)
  
}

}

}
# 
# 
# ## checking names for the future
# name_check <- paste(names(dfs),collapse=" ")
# str_detect(name_check, "^1 2")