# start with loading stylo
library(stylo)

# load texts from the corpus folder
tokenized.texts = load.corpus.and.parse(files = "all", corpus.dir = "corpus", markup.type = "plain", encoding = "UTF-8")

# get the word frequencies
features = make.frequency.list(tokenized.texts, head = 2000)

# create the table of frequencies
data = make.table.of.frequencies(tokenized.texts, features, relative = TRUE)

# prepare another version of the table of frequencies so we can compare to it
culled_data = perform.culling(data, culling.level = 0)

# run imposters
imposters(reference.set = data[-c(1),], test = culled_data[1,], distance="wurzburg") 

# you also want to find the ranges in which the results are most reliable - any results below the first value and over the second can be with a high degree of confidence, translated into binary answers of “no” and “yes”, respectively.
imposters.optimize(data)