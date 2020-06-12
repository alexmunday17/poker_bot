ranks <- factor(c("A", "K", "Q", "J", "T", as.character(seq(9, 2))))
ranks <- factor(ranks, levels = rev(as.character(ranks)), ordered = TRUE)
suits <- factor(c("C", "D", "H", "S"))
hands <- paste0(rep(ranks, each = length(suits)), suits)

hand_ranks <- readRDS("data/hand_ranks.Rds")
hand_rank_functions <- c("rf_c", "sf_c", "xk_c", "fh_c", "fl_c", "st_c",
                         "xk_c", "tp_c", "xk_c", "hc_c")