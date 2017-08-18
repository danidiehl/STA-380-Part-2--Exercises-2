library(tm) 
library(magrittr)
library(class)


readerPlain = function(fname){
				readPlain(elem=list(content=readLines(fname)), 
							id=fname, language='en') }

train = "/Users/daniellediehl/Documents/MSBA/Predictive_Modeling/STA380/data/ReutersC50/C50train/"
test = "/Users/daniellediehl/Documents/MSBA/Predictive_Modeling/STA380/data/ReutersC50/C50test/"
file_list = Sys.glob(paste0(train,'/*/*.txt'))
authornames = list.dirs("/Users/daniellediehl/Documents/MSBA/Predictive_Modeling/STA380/data/ReutersC50/C50train", full.names = FALSE)[-1]
classificationnames = rep(authornames, each=50)
authors = lapply(file_list, readerPlain) 

mynames = file_list %>%
	{ strsplit(., '/', fixed=TRUE) } %>%
	{ lapply(., tail, n=2) } %>%
	{ lapply(., paste0, collapse = '') } %>%
	unlist
	
names(authors) = mynames
my_documents = Corpus(VectorSource(authors))

## Some pre-processing/tokenization steps.
## tm_map just maps some function to every document in the corpus

my_documents = tm_map(my_documents, content_transformer(tolower)) # make everything lowercase
my_documents = tm_map(my_documents, content_transformer(removeNumbers)) # remove numbers
my_documents = tm_map(my_documents, content_transformer(removePunctuation)) # remove punctuation
my_documents = tm_map(my_documents, content_transformer(stripWhitespace)) ## remove excess white-space

my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en"))


## create a doc-term-matrix
DTM_authors = DocumentTermMatrix(my_documents)
DTM_authors # some basic summary statistics

DTM_authors = removeSparseTerms(DTM_authors, 0.95)


# Now PCA on term frequencies
X = as.matrix(DTM_authors)
X = X/rowSums(X)  # term-frequency weighting
X=cbind(X,classificationnames)

Z=X[,1:801] #taking out author
Z<-mapply(Z,FUN=as.numeric)
Z<-matrix(data=Z, ncol=801, nrow=2500)

pc = prcomp(Z, scale.=TRUE)
names(pc)
summary(pc) #801 pc's
plot(pc)
biplot(pc)
loadings = pc$rotation
scores = pc$x
qplot(scores[,1], scores[,2], color=X[,802], xlab='Component 1', ylab='Component 2')
library(ggplot2)


#maybe try regression on the first 10 pc's? 

library(pls)
library()





# Split data by rownumber into two equal portions
train <- sample(nrow(mat.df), ceiling(nrow(mat.df) * .50))
test <- (1:nrow(mat.df))[- train]

# Isolate classifier
cl <- mat.df[, "category"]

# Create model data and remove "category"
modeldata <- mat.df[,!colnames(mat.df) %in% "category"]

# Create model: training set, test set, training set classifier
knn.pred <- knn(modeldata[train, ], modeldata[test, ], cl[train])

# Confusion matrix
conf.mat <- table("Predictions" = knn.pred, Actual = cl[test])
conf.mat

# Accuracy
(accuracy <- sum(diag(conf.mat))/length(test) * 100)

# Create data frame with test data and predicted category
df.pred <- cbind(knn.pred, modeldata[test, ])
write.table(df.pred, file="output.csv", sep=";")
