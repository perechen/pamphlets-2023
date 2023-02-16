## load library
library(stylo)

## load tree cutting function
source("src/view_tree.R")

## run stylo() , save results
res1 <- stylo(corpus.dir="../session_01_samples/corpus_carre/",
              gui=F,
              distance.measure="wurzburg")

## feed stylo() results into a tree cutting function, handle settings

view_tree(res1, ## saved results variable from stylo() call
          k=2, ## to how many groups you want to cut a dendrogram
          p=0.05,  ## p-value threshold for word-to-group correlations
          color_leaves=F, ## should it color leaves by classes (string before the first underscore in the label)
          output=T, ## should it save output in a .txt file
          label_size=6, ## text size on cluster~words plots
          leaf_size=1 ## leaves size on dendrograms
          ) 

## view_tree() works only with stylo results for now, but what it uses really, is distance table + frequency tables + "features actually used". It will yell at you if the input is something other than 'stylo.results' class
## view_trees() relies on a number of packages, it will yell at you if you miss something, and print missing packages
## better not use with high *k*
## colors-to-cluster correspondence between plot 1 & plot 2 might not be reliable (because of how `dendextend` behaves with k-cutting). The most reliable source of features and corresponding texts is .txt output!

