library(ROCR)
library(dplyr)

if (!exists('test_sm') | !exists('train_sm')) {
    source('dataSource.R')
}

softMax <- function(df, alpha, max_cycles, lambda) {
    n <- ncol(df)
    m <- nrow(df)
    ctg_n <- length(unique(df[, n]))
    
    label <- matrix(0, nrow = m, ncol = ctg_n)
    label[cbind(1:nrow(df), df[, n])] <- 1
    train <- df[, -n] %>% as.matrix()
    
    weight <- matrix(rnorm((n - 1) * ctg_n), nrow = n - 1, ncol = ctg_n)
    
    for (i in 1:max_cycles) {
        
        h <- train %*% weight
        h <- h - apply(h, 1, max)
        h <- exp(h)
        h <- h / rowSums(h)
        
        error <- label - h
        
        weight <- weight + alpha * (t(train) %*% error - lambda * weight)
    }
    return(weight)
}

multiLrPredict <- function(test, label, weight) {
    result <- as.matrix(test[, -ncol(test)]) %*% weight %>% 
        apply(1, which.max) %>% 
        `[`(label, .)
    return(result)
}

modelByVote <- function(df, test, alpha, max_cycles, lambda, 
                   max_iter, voter_n, thr) {
    voters <- list()
    
    for (i in 1:max_iter) {
        weight <- softMax(df, alpha, max_cycles, lambda)
        pre <- multiLrPredict(test, unique(df[, ncol(df)]), weight)
        
        if (mean(pre == test[, ncol(test)]) >= thr) {
            voters <- append(voters, list(weight))
            
            if (length(voters) == voter_n) {
                return(voters)
            }
        }
    }
    return(voters)
}

predictByVote <- function(test, label, model_by_vote) {
    result <- sapply(model_by_vote, function(weight){
        as.matrix(test[, -ncol(test)]) %*% weight %>% 
            apply(1, which.max) %>% 
            `[`(label, .)
    }) %>% 
        apply(1, function(votes){
            temp <- table(votes)
            names(temp)[which.max(temp)] %>% as.numeric()
        })
    return(result)
}

softmaxCompare <- function(df, test, alpha, max_cycles, lambda) {
    result <- list()
    weight <- softMax(df, alpha, max_cycles, lambda)
    pre <- multiLrPredict(test, unique(df[, ncol(df)]), weight)
    
    result[['accuracy']] <- mean(pre == test[, ncol(test)])
    result[['table']] <- table(pre, test[, ncol(test)])
    return(result)
}

result <- vector(length = 100)
for (i in 1:100) {
    result[i] <- softmaxCompare(train_sm, test_sm, 
                                alpha_sm, max_cycles_sm, lambda_sm) %>% 
        `[[`('accuracy')
}



