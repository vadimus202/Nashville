
# Normalize ratings
nash <- mutate(nash,
               yelp_qrt = cut(rating, 
                              breaks = quantile(rating, seq(0,1,length.out = 5)), 
                              include.lowest = T,
                              labels = 1:4))


##############################################################
# Analyze Category Frequencies


# summarize by category
# Frequency of Yelp categories



categ <-  data.frame(
  cat = unlist(lapply(nash$categories, strsplit, split = ':')),
  cat_cd = unlist(lapply(nash$categories_cd, strsplit, split = ':')),
  stringsAsFactors = F) %>% 
  unique() %>% 
  arrange(rank(cat_cd))


# Extract full name lookup for Yelp categories
categ.unlist <- function(x) {
  paste(unlist(lapply(x$categories_cd, strsplit, split = ':')),collapse = ' ')
  }

q1 <- categ.unlist(filter(nash, yelp_qrt==1))
q2 <- categ.unlist(filter(nash, yelp_qrt==2))
q3 <- categ.unlist(filter(nash, yelp_qrt==3))
q4 <- categ.unlist(filter(nash, yelp_qrt==4))

all <- c(q1,q2,q3,q4)


# Frequency of Yelp categories
require(tm)
require(wordcloud)
corpus <- Corpus(VectorSource(all))
m <- as.matrix(TermDocumentMatrix(corpus))
colnames(m) <- c('Worst 25%','25-50%','50-75%','Best 25%')
rownames(m) <- categ$cat

# print wordcloud:
png('img/wordcloud.png',width = 900, height = 900, res = 72*3)
set.seed(999)
comparison.cloud(m, scale=c(2, 0.5), title.size=1,
          random.order=FALSE, rot.per=0,  
          use.r.layout=FALSE, colors=c('red','darksalmon','palegreen4','green4'))
dev.off()
shell.exec('img/wordcloud.png')






