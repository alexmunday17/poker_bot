a <- read.delim("data/hand_histories/50NLH (1).txt", header = FALSE)
b <- head(a, 100)


clean_hand_history <- function(file) {
    file$new_hand <- as.numeric(grepl("Stage.*:", file$V1))
    file$new_hand <- cumsum(file$new_hand)
    return(split(file, file$new_hand))
}