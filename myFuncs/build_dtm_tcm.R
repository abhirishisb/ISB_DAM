build_dtm_tcm <- function(x){   # x is cleaned corpus
  require(text2vec)
  x1=x[[1]]
  x1test=x[[2]]
  tok_fun = word_tokenizer  # using word & not space tokenizers
  it_0 = itoken( x1,
                 #preprocessor = text.clean,
                 tokenizer = tok_fun,
                 ids = x1test$id,
                 progressbar = T)
  
  vocab = create_vocabulary(it_0,    #  func collects unique terms & corresponding statistics
                            ngram = c(2L, 2L))
  
  pruned_vocab = prune_vocabulary(vocab,  # filters input vocab & throws out v frequent & v infrequent terms
                                  term_count_min = 1)
  
  vectorizer = vocab_vectorizer(pruned_vocab) #  creates a text vectorizer func used in constructing a dtm/tcm/corpus
  
  dtm_0  = create_dtm(it_0, vectorizer) # high-level function for creating a document-term matrix
  
  # Sort bi-gram with decreasing order of freq
  tsum = as.matrix(t(slam::rollup(dtm_0, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
  tsum = tsum[order(tsum, decreasing = T),]       # terms in decreasing order of freq
  
  #-------------------------------------------------------
  # Code bi-grams as unigram in clean text corpus
  #-------------------------------------------------------
  
  text2 = x1
  text2 = paste("",text2,"")
  
  pb <- txtProgressBar(min = 1, max = (length(tsum)), style = 3) ; 
  
  i = 0
  for (term in names(tsum)){
    i = i + 1
    focal.term = gsub("_", " ",term)        # in case dot was word-separator
    replacement.term = term
    text2 = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2)
    setTxtProgressBar(pb, i)
  }
  
  
  it_m = itoken(text2,     # function creates iterators over input objects to vocabularies, corpora, DTM & TCM matrices
                # preprocessor = text.clean,
                tokenizer = tok_fun,
                ids = x1test$id,
                progressbar = T)
  
  vocab = create_vocabulary(it_m)     # vocab func collects unique terms and corresponding statistics
  pruned_vocab = prune_vocabulary(vocab,
                                  term_count_min = 1)
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm_m  = create_dtm(it_m, vectorizer)
  # dim(dtm_m)
  
  dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
  a0 = (apply(dtm, 1, sum) > 0)   # build vector to identify non-empty docs
  dtm = dtm[a0,]                  # drop empty docs
  
  vectorizer = vocab_vectorizer(pruned_vocab)    # start with the pruned vocab
  #grow_dtm = FALSE,    # doesn;t play well in R due to memory & over-writing issues
  #skip_grams_window = 5L)   # window size = no. of terms to left & right of focal term
  
  tcm = create_tcm(it_m, vectorizer) # create_tcm() func to build a TCM
  
  out = list(dtm = dtm, tcm = tcm, dtm_sparse = dtm_m)
  
  dtm = out[[1]]    # first element of above function's output is the dtm
  print(dim(dtm))
  
  dtm = dtm[,order(apply(dtm, 2, sum), decreasing = T)]     # sorting dtm's columns in decreasing order of column sums
  print(inspect(dtm[1:5, 1:5]))
  tcm = out[[2]]
  dim(tcm)
  print(dim(tcm))
  return(dtm)  # output is list of length 3 containing dtm, tcm and a sparse dtm representation.
  
  }
