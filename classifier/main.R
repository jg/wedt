# main.R
library(tm)
library(Snowball)
library(SnowballC)
library(hash)
library(e1071)
library(FSelector)
options(width=140)

prepCorpus <- function(dirname) {
  # Reads in a newsgroup directory, cleans the data, loads it into a Corpus
  #
  # Args:
  #   dirname: Directory name (string)
  # Returns:
  #   The Corpus
  removeStopWords <- function (doc) {
    return(removeWords(doc, stopwords("english")))
  }
  stemDoc <- function (doc) {
    return(stemDocument(doc, language = "english"))
  }
  convert <- function (t) {
    return(iconv(t, "ISO-8859-1", "UTF-8"))
  }
  corpus <- Corpus( DirSource (dirname) )
  data <- corpus
  data <- tm_map(data, convert)
  data <- tm_map(data, tolower)
  data <- tm_map(data, removeStopWords)
  data <- tm_map(data, stripWhitespace)
  data <- tm_map(data, removeNumbers)
  data <- tm_map(data, removePunctuation)
  data <- tm_map(data, stemDoc)
  names(data) <- dirname
  return(data)
}

kFoldIndices <- function(Nobs,K=5) {
  # Computes array indices that can be used for k-fold cross validation
  #
  # Args:
  #  Nobs: Number of rows in the array
  #  K: Number of folds
  #
  # Returns:
  #  A list of k elements, each of which has a $train and a $test component.
  rs <- runif(Nobs)
  id <- seq(Nobs)[order(rs)]
  k <- as.integer(Nobs*seq(1,K-1)/K)
  k <- matrix(c(0,rep(k,each=2),Nobs),ncol=2,byrow=TRUE)
  k[,1] <- k[,1]+1
  l <- lapply(seq.int(K),function(x,k,d)
              list(train=d[!(seq(d) %in% seq(k[x,1],k[x,2]))],
                   test=d[seq(k[x,1],k[x,2])]),k=k,d=id)
  return(l)
}

selectDtmColumns <- function (dtm, filterFn) {
  return (dtm[,!(sapply(dimnames(dtm)$Terms, filterFn))])
}

selectDfColumns <- function (df, filterFn) {
  return (df[,!(sapply(dimnames(df)[[2]], filterFn))])
}

createDocumentTermMatrix <- function (corpus, maxAllowedSparseness) {
  dtm <- removeSparseTerms(DocumentTermMatrix(corpus), maxAllowedSparseness)
  # get rid of row names
  rownames(dtm) = c()
  return (dtm)
}

err <- function (y.true, y.pred) {
  sum(y.pred!=y.true)/length(y.true)
}

prepCorpora <- function (dirs) {
  # Runs prepCorpus for each of the directory names in dirs
  #
  # Args:
  #   dirs: list of directory names
  #
  # Returns:
  #   The list of Corpora
  corpora <- lapply(dirs, function (dirName) {
    return(prepCorpus(dirName))
  })
  names(corpora) = dirs
  return(corpora)
}

createDtm <- function (corpus, maxSparseness) {
  m = createDocumentTermMatrix(corpus, maxSparseness)
  rownames(m) = rep(names(corpus)[1], dim(m)[1])
  return(m)
}

createDtms <- function (corpora, maxAllowedSparseness) {
  data <- lapply(corpora, function (corpus) {
    return (createDtm(corpus, maxAllowedSparseness))
  })
  names(data) = names(corpora)

  return(data)
}

createMainDtm <- function(corpora, sparsenessThreshold) {
  dtms = createDtms(corpora, sparsenessThreshold)
  # merge dtms
  dtm = do.call(c, dtms)
  return (dtm)
}

removeCols <- function(data, cols) {
  # Remove columns specifcied in cols from data
  return(data[,!names(data) %in% cols])
}

filterOutSillyColumns <- function(df) {
  # Removes columns with names that contain non-ASCII characters from
  # dataframe
  # Returns dataframe
  colnames(df) <- iconv(colnames(df), "UTF-8", "ASCII", sub=NA)
  return(removeCols(df, NA))
}

computePerformanceMeasuresForAlgorithm <- function (corpora,
                                                    algorithm,
                                                    sparsenessThreshold,
                                                    attributeSelectionFn,
                                                    k) {
  # attribute selection
  dtm <- createMainDtm(corpora, sparsenessThreshold)
  df <- filterOutSillyColumns(dataFrameFromDocumentTermMatrix(dtm))
  attributes <- attributeSelectionFn(df)

  # filter out terms which did not get to be attributes
  df <- selectDfColumns(df, function (colname) { colname %in% attributes })

  folds <- kFoldIndices(dim(df)[1], k)
  measures <- sapply(folds, function (fold) {
    return (computeClassificationMeasuresOnFold(df, algorithm, fold))
  })
  # add means column to the results
  measures <- data.frame(means=rowMeans(apply(measures, 2, as.numeric)), measures)

  # return only means over folds
  means <- t(measures[1])

  # add attribute count
  attributeString <- paste(attributes, collapse='|')
  measuresWithAttrInfo <- cbind(means,
                                attr_count=as.numeric(length(attributes)),
                                attrs=attributeString
                                )

  # remove rownames
  rownames(measuresWithAttrInfo) <- c()

  return (measuresWithAttrInfo)
}

# Ref: http://rali.iro.umontreal.ca/rali/sites/default/files/publis/SokolovaLapalme-JIPM09.pdf
computeClassificationMeasuresOnFold <- function (df, algorithm, fold) {
  train <- df[fold$train,]
  test <- df[fold$test,]
  model <- algorithm(train)

  predictions <- predict(model, test[, -1])
  allClasses <- levels(test$class)
  tp <- tn <- fn <- fp <- c()
  allTp <- allTn <- allFn <- allFp <- c()
  accuracy <- errorRate <- precision <- recall <- c()
  for (class in allClasses) {
    # select predictions corresponding to test rows with class 'class'
    classPredictions <- predictions[test[, 1] == class]
    # predictions corresponding to test rows with other classes
    otherPredictions <- predictions[test[, 1] != class]
    allTp[[class]] <- tp <- sum(classPredictions == class)
    allTn[[class]] <- tn <- sum(otherPredictions != class)
    allFn[[class]] <- fn <- sum(classPredictions != class)
    allFp[[class]] <- fp <- sum(otherPredictions == class)


    accuracy[[class]] <- (tp + tn) / (tp + fn + fp + tn)
    errorRate[[class]] <- (fp + fn) / (tp + fn + fp + tn)

    precision[[class]] <- tp / ( tp + fp )
    recall[[class]] <- tp / (tp + fn)

  }
  l <- length(allClasses)
  measures <- c()

  # The average per-class effectiveness of a classiﬁer
  measures$avgAccuracy <- sum(accuracy) / l
  # The average per-class classiﬁcation error
  measures$avgError <- sum(errorRate) / l

  # Agreement of the data class labels with those of a classiﬁers if calculated from sums of per-text decisions
  # measures$avgPrecision <- sum(allTp) / sum(allTp) + sum(allFp) # micro
  # Effectiveness of a classiﬁer to identify class labels if calculated from sums of per-text decisions
  # measures$avgRecall <- sum(allTp) / sum(allTp) + sum(allFp) # micro

  # An average per-class agreement of the data class labels with those of a classiﬁers
  avgPrecision <- measures$avgPrecision <- sum(precision) / l

  # An average per-class effectiveness of a classiﬁer to identify class labels
  avgRecall <- measures$avgRecall <- sum(recall) / l

  measures$avgFscore <- 2 * avgPrecision * avgRecall / ( avgPrecision + avgRecall )
  measures$oldError <- err(test$class, predict(model, test[,-1]))

  return(measures)
}

dataFrameFromDocumentTermMatrix <- function (dtm) {
  class <- rownames(dtm)
  rownames(dtm) <- c()
  df <- as.data.frame(as.matrix(dtm))
  df <- cbind.data.frame(class, df)
  return(df)
}

selectAttributesChiSquared <- function (df, cutoff) {
  lst <- chi.squared(df)
  # sort by attr_importance, keep rownames
  sorted <- lst[order(lst[,"attr_importance"]), , drop=FALSE]
  # keep only those bigger than cutoff
  attrs <- sorted[sorted$attr_importance > cutoff, , drop=FALSE]
  return(rownames(attrs))
}

testChiSquaredAttributeSelection <- function (corpora, algorithm) {
  chiSquaredCutOffs <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  sparsenessThreshold <- 0.8

  measureMeans <- lapply(chiSquaredCutOffs, function (cutoff) {
    attributeSelectionFn <- function (df) {
      return (selectAttributesChiSquared(df, cutoff))
    }

    results <- computePerformanceMeasuresForAlgorithm(corpora,
                                                      algorithm,
                                                      0.8,
                                                      attributeSelectionFn,
                                                      3)
    data <- cbind.data.frame(chiSquaredCutOff=cutoff, results)
    rownames(data) <- c()
    return(data)
  })

  results <- do.call(rbind, measureMeans)
  return (results)
}



# dirs <- c('alt.atheism', 'comp.graphics', 'comp.os.ms-windows.misc', 'comp.sys.ibm.pc.hardware', 'comp.sys.mac.hardware', 'comp.windows.x', 'misc.forsale', 'rec.autos', 'rec.motorcycles', 'rec.sport.baseball', 'rec.sport.hockey', 'sci.crypt', 'sci.electronics', 'sci.med', 'sci.space', 'soc.religion.christian', 'talk.politics.guns', 'talk.politics.mideast', 'talk.politics.misc', 'talk.religion.misc')
dirs <- c('sport', 'tech', 'business', 'environment')

corpora <- prepCorpora(dirs)

naiveBayesAlgorithm <- function (trainingSet) {
  return (naiveBayes(class ~ ., data=trainingSet, laplace = laplace))
}
resultsNB <- testChiSquaredAttributeSelection(corpora, naiveBayesAlgorithm)

SVMAlgorithm <- function (trainingSet) {
  return (svm(class ~ ., data=trainingSet, laplace = laplace))
}
resultsSVM <- testChiSquaredAttributeSelection(corpora, SVMAlgorithm)
## attributeSelectionFn = function (df) {
##   return (selectAttributesChiSquared(df, chiSquaredCutOff))
## }
# results <- runNaiveBayes(corpora, 0.8, attributeSelectionFn, 3)
# results <-  testChiSquaredAttributeSelection(corpora)
# sparsenessThreshold = 0.8
# chiSquaredCutOff = 0.3
# attributeSelectionFn = function (df) {
#   return (selectAttributesChiSquared(df, chiSquaredCutOff))
# }
# error <- meanClassificationError(corpora, sparsenessThreshold, attributeSelectionFn, 3)

# maxSparsenessThresholds <- list(0.7, 0.6, 0.5, 0.4)
# meanClassificationError(corpora, 0.7, 2)
# results = lapply(maxSparsenessThresholds, function(threshold) {
  # return(meanClassificationError(corpora, threshold, 3)
# })

# model$apriori
# model$tables
# predict(model, hv.test[,-1], type="raw")
