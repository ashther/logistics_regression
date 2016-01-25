require(magrittr)
require(ROCR)
require(dplyr)

if (!exists('test_lr') | !exists('train_lr')) {
    source('dataSource.R')
}

sigmoid <- function(x) {
    return(1 / (1 + exp(-x)))
}

gradAscent <- function(train, alpha, max_cycles, lambda) {
    n <- dim(train)[2] - 1
    m <- dim(train)[1]
    
    # 获取分类结果，重新定义训练集
    label <- train[, n + 1]
    train <- train[, 1:n]
    
    weight <- rep(1, n)
    #weight <- rnorm(n)
    
    for (i in 1:max_cycles) {
        h <- train %*% weight %>% 
            sigmoid()
        error <- label - h
        weight <- weight + alpha * (t(train) %*% error - lambda * weight)
    }
    
    return(weight)
}

lrPredict <- function(test, weight) {
    result <- apply(test, 1, function(t){
        temp <- ifelse(sigmoid(t %*% weight) > 0.5, 1, 0)
        return(temp)
    })
    return(result)
} 

lrCompare <- function(train, verify, alpha, max_cycles, lambda) {
    result <- list()
    
    pre <- gradAscent(train, alpha, max_cycles, lambda) %>%
        lrPredict(verify[, 1:(ncol(verify) - 1)], .)
    
    result[['accuracy']] <- mean(pre == verify[, ncol(verify)])
    result[['auc']] <- tryCatch({prediction(pre, verify[, ncol(verify)]) %>%
            performance('auc') %>% 
            `@`(y.values) %>% 
            `[[`(1)}, error = function(e)return(e))
    
    return(result)
}

lrCompare(train_lr, test_lr, alpha_lr, max_cycles_lr, lambda_lr)

