#importing libraries
library(tm)
library(tmap)
library(keras)
gcloud_init()
library(cloudml)
cloudml_train("Assignment2.R")


# Import data
data = read.csv(file = 'C:\\Users\\lbhar\\OneDrive\\spring 2019\\R\\train.csv')
dim(data)
data$Sentiment <- factor(data$Sentiment)

#Splitting data into train and test
train_index <- sample(1:10000, 0.8 * 10000)
test_index <- setdiff(1:10000, train_index)
train <- data[train_index,]
test <- data[test_index,]


#Preprocessing Train Data
train_corpus <- Corpus(VectorSource(train$SentimentText))
train_corpus = tm_map(train_corpus, content_transformer(tolower))
train_corpus = tm_map(train_corpus, removePunctuation)
train_corpus = tm_map(train_corpus, removeWords, stopwords("english"))

#Preprocessing Test Data
test_corpus <- Corpus(VectorSource(test$SentimentText))
test_corpus = tm_map(test_corpus, content_transformer(tolower))
test_corpus = tm_map(test_corpus, removePunctuation)
test_corpus = tm_map(test_corpus, removeWords, stopwords("english"))

#####################################
#---- eval=FALSE, message=FALSE------------------------------------------
tokenizer <- text_tokenizer(num_words = 15000, lower = TRUE) %>% 
  fit_text_tokenizer(train_corpus$content)
  
# Turns strings into lists of integer indices
X1t <- texts_to_sequences(tokenizer, train_corpus$content)
X2t <- texts_to_sequences(tokenizer, test_corpus$content)

#---- eval=FALSE, message=FALSE------------------------------------------
vectorize_sequences <- function(sequences,
                                dimension = 10000) {
  # Create an all-zero matrix of shape
  # (len(sequences), dimension)
  results <- matrix(0, nrow = length(sequences),
                    ncol = dimension)
  for (i in 1:length(sequences))
    # Sets specific indices of results[i] to 1s
    if (length(sequences[[i]])>0) {
      for (j in sequences[[i]])
        results[i][j] <- 1
    }
  else {next}  
  results
}


# Our vectorized training data
xtrain <- vectorize_sequences(X1t)
# Our vectorized test data
xtest <- vectorize_sequences(X2t)

# Vectorize Labels
y_train <- as.numeric(train$Sentiment)
y_test <- as.numeric(test$Sentiment)

#---- eval=FALSE, message=FALSE------------------------------------------
library(keras)
model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = "relu",
              input_shape = c(10000)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

#---- eval=FALSE, message=FALSE------------------------------------------
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
model %>% fit(xtrain, y_train, epochs = 10,
              batch_size = 512)
results <- model %>% evaluate(xtest, y_test)
results
