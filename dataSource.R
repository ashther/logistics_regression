
train_lr <- NULL
for (i in list.files('train/')) {
    ctg <- substr(i, 1, 1) %>% as.numeric()
    con <- file(paste0('train/', i), 'r')
    temp <- readLines(con) %>% 
        paste0(collapse = '') %>% 
        strsplit('') %>% 
        unlist() %>% 
        as.numeric()
    train_lr <- rbind(train_lr, c(temp, ctg))
    close(con)
}

test_lr <- NULL
for (i in list.files('test/')) {
    ctg <- substr(i, 1, 1) %>% as.numeric()
    con <- file(paste0('test/', i), 'r')
    temp <- readLines(con) %>% 
        paste0(collapse = '') %>% 
        strsplit('') %>% 
        unlist() %>% 
        as.numeric()
    test_lr <- rbind(test_lr, c(temp, ctg))
    close(con)
}

alpha_lr <- 0.1
max_cycles_lr <- 50
lambda_lr <- 0.01

rm(list = c('con', 'ctg', 'i', 'temp'))

set.seed(1)
train_sm <- iris %>% 
    mutate(cls = as.integer(Species)) %>% 
    select(-Species) %>% 
    as.matrix() %>% 
    `[`(sample(nrow(iris), round(0.67 * nrow(iris)), replace = FALSE), )

test_sm <- iris %>% 
    mutate(cls = as.integer(Species)) %>% 
    select(-Species) %>% 
    as.matrix() %>% 
    `[`(sample(nrow(iris), round(0.33 * nrow(iris)), replace = FALSE), )

alpha_sm <- 0.01
max_cycles_sm <- 200
lambda_sm <- 0.001