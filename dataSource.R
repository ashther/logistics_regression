
train <- NULL
for (i in list.files('train/')) {
    ctg <- substr(i, 1, 1) %>% as.numeric()
    con <- file(paste0('train/', i), 'r')
    temp <- readLines(con) %>% 
        paste0(collapse = '') %>% 
        strsplit('') %>% 
        unlist() %>% 
        as.numeric()
    train <- rbind(train, c(temp, ctg))
    close(con)
}

verify <- NULL
for (i in list.files('test/')) {
    ctg <- substr(i, 1, 1) %>% as.numeric()
    con <- file(paste0('test/', i), 'r')
    temp <- readLines(con) %>% 
        paste0(collapse = '') %>% 
        strsplit('') %>% 
        unlist() %>% 
        as.numeric()
    verify <- rbind(verify, c(temp, ctg))
    close(con)
}

rm(list = c('con', 'ctg', 'i', 'temp'))

alpha <- 0.01
max_cycles <- 50
