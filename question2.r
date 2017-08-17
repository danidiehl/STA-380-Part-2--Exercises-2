library(tm) 
library(magrittr)
library(class)


readerPlain = function(fname){
				readPlain(elem=list(content=readLines(fname)), 
							id=fname, language='en') }

train = "C:/users/jessc/documents/STA-380-Part-2--Exercises-2/data/ReutersC50/C50train"
test = "C:/users/jessc/documents/STA-380-Part-2--Exercises-2/data/ReutersC50/C50test"
file_list = Sys.glob(paste0(train,'/*/*.txt'))
authornames = list.dirs("C:/users/jessc/documents/STA-380-Part-2--Exercises-2/data/ReutersC50/C50train", full.names = FALSE)[-1]
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


# Transform dtm to matrix to data frame - df is easier to work with
mat.df <- as.data.frame(data.matrix(DTM_authors), stringsAsfactors = FALSE)

# Column bind category (known classification)
mat.df$category <- classificationnames

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
