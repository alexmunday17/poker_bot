simplify_cards <- function(cards) {
    values <- substr(cards, 1, 1)
    suits <- substr(cards, 2, 2)
    values <- as.character(ranks[ranks %in% values])
    if (length(values) == 1) {
        return(paste0(values, values))
    }
    if (suits[1] == suits[2]) {
        s_or_o <- "s"
    } else {
        s_or_o <- "o"
    }
    return(paste0(values[1], values[2], s_or_o))
}

sim_hand <- function() {
    all_cards <- deal_hands(2)
    p1_cards <- all_cards[1:2]
    p2_cards <- all_cards[3:4]
    table_cards <- all_cards[5:9]
    p1_hand <- c(p1_cards, table_cards)
    p2_hand <- c(p2_cards, table_cards)
    return(c(simplify_cards(p1_cards),
             simplify_cards(p2_cards),
             compare_hands(p1_hand, p2_hand)))
}

h2h_sim <- function(n) {
    all_hands <- replicate(n, sim_hand())
    hand_stats1 <- table(all_hands[1, ], all_hands[3, ])
    hand_stats2 <- table(all_hands[2, ], all_hands[3, ])
    hand_stats1 <- cbind(hand_stats1[, 2],
                         hand_stats1[, 1] + hand_stats1[, 2] + hand_stats1[, 3])
    hand_stats2 <- cbind(hand_stats2[, 3],
                         hand_stats2[, 1] + hand_stats2[, 2] + hand_stats2[, 3])
    hand_stats <- hand_stats1 + hand_stats2
    return(cbind(sort(hand_stats[, 1] / hand_stats[, 2], decreasing = TRUE)))
}

deal_hand <- function() {
    return(sample(hands, 7))
}

hand_probs <- function(n) {
    all_hands <- replicate(n, deal_hand())
    eval_hands <- apply(all_hands, 2, what_hand)
    return(sort(table(eval_hands)) / n)
}

player_sim <- function(player_types, n, verbose = FALSE) {
    player_info <- data.frame(name = paste0("p", seq(1, length(player_types))),
                              player_type = player_types,
                              stringsAsFactors = FALSE)
    game_log <- replicate(n, run_game(player_info, 50, 10, 0, verbose))
    return(table(game_log))
}
