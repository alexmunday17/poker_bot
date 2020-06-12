ranks <- factor(c("A", "K", "Q", "J", "T", as.character(seq(9, 2))))
ranks <- factor(ranks, levels = rev(as.character(ranks)), ordered = TRUE)
suits <- factor(c("C", "D", "H", "S"))
hands <- paste0(rep(ranks, each = length(suits)), suits)

hand_ranks <- readRDS("data/hand_ranks.Rds")
hand_rank_functions <- c("rf_c", "sf_c", "xk_c", "fh_c", "fl_c", "st_c",
                         "xk_c", "tp_c", "xk_c", "hc_c")

is_straight <- function(x) {
    if (identical(x, rep(1, 4))) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

ace_high_straight <- function(x, y) {
    if (is_straight(x)) {
        if (y == "A") {
            return(TRUE)
        }
    }
    return(FALSE)
}

straight <- function(values, type = FALSE) {
    ranks <- readRDS("data/ranks.Rds")
    values <- ranks[ranks %in% values]
    values_num <- sort(as.numeric(values))
    val_diff <- diff(values_num)
    val_diffs <- paste(val_diff, collapse = " ")
    if (grepl("1 1 1", val_diffs)) {
        if (all(c(13,1,2,3,4) %in% values_num)) {
            high_card <- "5"
        } else if (!grepl("1 1 1 1", val_diffs)) {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
    if (type) {
        which_hc <- which(val_diff == 1)[1]
        high_card <- values[which_hc]
        return(high_card)
    } else {
        return(TRUE)
    }
}

straight_fast <- function(values, type = FALSE) {
    values_num <- sort(unique(values))
    val_diff <- diff(values_num)
    val_diffs <- paste(val_diff, collapse = " ")
    if (grepl("1 1 1", val_diffs)) {
        if (all(c(14,2,3,4,5) %in% values)) {
            high_card <- "5"
        } else if (!grepl("1 1 1 1", val_diffs)) {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
    if (!type) {
        return(TRUE)
    } else {
        which_hc <- tail(which(val_diff == 1), 1) + 1
        high_card <- values_num[which_hc]
        return(high_card)
    }
}

flush <- function(cards) {
    suits <- substr(cards, 2, 2)
    suit_max <- max(table(suits))
    return(suit_max >= 5)
}

pair <- function(cards) {
    values <- substr(cards, 1, 1)
    res <- identical(as.numeric(sort(table(values), decreasing = TRUE)[1:2]), c(2, 1))
    return(res)
}

three_of_a_kind <- function(cards) {
    values <- substr(cards, 1, 1)
    res <- ifelse(identical(as.numeric(sort(table(values),
                                            decreasing = TRUE)[1:2]),
                            c(3, 1)), TRUE, FALSE)
    return(res)
}

four_of_a_kind <- function(cards) {
    values <- substr(cards, 1, 1)
    res <- ifelse(4 %in% table(values), TRUE, FALSE)
    return(res)
}

two_pair <- function(cards) {
    values <- substr(cards, 1, 1)
    res <- ifelse(identical(as.numeric(sort(table(values),
                                            decreasing = TRUE)[1:2]),
                            c(2, 2)), TRUE, FALSE)
    return(res)
}

full_house <- function(cards, type = FALSE) {
    values <- substr(cards, 1, 1)
    values <- sort(table(values), decreasing = TRUE)
    if (type) {
        fh_vals <- values[values > 1]
        fh_cards <- names(fh_vals)
        if (length(fh_vals) == 2) {
            if (all(fh_vals == c(3, 2))) {
                return(fh_cards)
            } else {
                return(as.character(ranks[ranks %in% fh_cards]))
            }
        } else {
            return(c(fh_cards[1], as.character(ranks[ranks %in% fh_cards[2:3]][1])))
        }
    }
    res1 <- ifelse(sum(match(values, 3), na.rm = TRUE) == 2, TRUE, FALSE)
    res2 <- ifelse(all(c(2, 3) %in% values), TRUE, FALSE)
    return(any(res1, res2))
}

is_straight_flush <- function(cards) {
    return(all(straight(cards), flush(cards)))
}

straight_flush <- function(cards, type = FALSE) {
    all_combos <- combn(cards, 5)
    is_sf <- apply(all_combos, 2, is_straight_flush)
    res <- any(is_sf)
    if (!res) {
        return(FALSE)
    }
    sf_values <- unlist(substr(all_combos[, is_sf], 1, 1), use.names = FALSE)
    ranks <- readRDS("data/ranks.Rds")
    high_card <- max(ranks[ranks %in% sf_values])
    if (type) {
        return(high_card)
    }
    if (high_card == "A") {
        return(c(TRUE, TRUE))
    } else {
        return(c(TRUE, FALSE))
    }
}

Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

straight_flush_fast <- function(cards, type = FALSE) {

    len <- length(cards)
    mid <- len / 2

    ranks <- cards[1:mid]
    suits <- cards[(mid + 1):len]
    flush_suit <- Mode(suits)
    ranks <- ranks[suits == flush_suit]
    return(straight_fast(ranks, type))
}

what_hand <- function(cards) {

    values <- substr(cards, 1, 1)
    val_table <- sort(table(values), decreasing = TRUE)
    max_val <- val_table[1]

    suits <- substr(cards, 2, 2)
    fl <- max(table(suits)) > 4

    st <- straight(values)

    if (!(st | fl)) {
        if (max_val == 1) {
            return("High card")
        }
        first_two_sum <- sum(val_table[1:2])
        if (first_two_sum == 3) {
            return("Pair")
        } else if (first_two_sum == 4) {
            return(ifelse(max_val == 3, "Three of a kind", "Two pair"))
        } else {
            return(ifelse(max_val == 4, "Four of a kind", "Full house"))
        }
    }

    if (fl & st) {
        sf <- straight_flush(cards)
        if (sum(sf) == 2) {
            return("Royal flush")
        } else if (sum(sf) == 1) {
            return("Straight flush")
        }
    }

    if (fl) {
        return("Flush")
    }

    if (st) {
        return("Straight")
    }
}

what_hand_fast <- function(cards) {

    len <- length(cards)
    mid <- len / 2

    values <- cards[1:mid]
    val_table <- sort(tabulate(values), decreasing = TRUE)
    max_val <- val_table[1]

    suits <- cards[(mid + 1):len]
    fl <- max(table(suits)) > 4

    st <- straight_fast(values)

    if (!(st | fl)) {
        if (max_val == 1) {
            return(1)
        }
        first_two_sum <- sum(val_table[1:2])
        if (first_two_sum == 3) {
            return(2)
        } else if (first_two_sum == 4) {
            return(ifelse(max_val == 3, 4, 3))
        } else {
            return(ifelse(max_val == 4, 8, 7))
        }
    }

    if (fl & st) {
        sf <- straight_flush_fast(cards)
        if (sf) {
            return(9)
        } else {
            return(6)
        }
    }

    if (fl) {
        return(6)
    }

    if (st) {
        return(5)
    }
}

what_hand_vals <- function(values) {

    val_table <- sort(tabulate(values), decreasing = TRUE)
    max_val <- val_table[1]

    st <- straight_fast(values)

    if (!st) {
        if (max_val == 1) {
            return(1)
        }
        first_two_sum <- sum(val_table[1:2])
        if (first_two_sum == 3) {
            return(2)
        } else if (first_two_sum == 4) {
            return(ifelse(max_val == 3, 4, 3))
        } else {
            return(ifelse(max_val == 4, 8, 7))
        }
    } else {
        return(5)
    }
}

compare_hands <- function(cards1, cards2, hand1 = NULL, table_cards = NULL) {
    # hand_ranks <- readRDS("data/hand_ranks.Rds")
    if (is.null(hand1)) {
        hand1 <- what_hand(cards1)
    }
    if (!is.null(table_cards)) {
        cards2 <- c(cards2, table_cards)
    }
    hand2 <- what_hand(cards2)
    hand_rank1 <- hand_ranks[hand_ranks == hand1]
    hand_rank2 <- hand_ranks[hand_ranks == hand2]
    if (hand_rank1 > hand_rank2) {
        return(1)
    } else if (hand_rank2 > hand_rank1) {
        return(2)
    } else {
        hand_rank_pos <- hand_ranks == hand1
        if (hand_ranks[hand_rank_pos] == "Royal flush") {
            return(0)
        }
        # hand_rank_functions <- c("rf_c", "sf_c", "xk_c", "fh_c", "fl_c", "st_c",
        #                          "xk_c", "tp_c", "xk_c", "hc_c")
        hand_rank_fun <- get(hand_rank_functions[hand_rank_pos])
        return(hand_rank_fun(cards1, cards2))
    }
}

compare_hands_fast <- function(cards1, cards2, hand1 = NULL, table_cards = NULL) {
    if (is.null(hand1)) {
        hand1 <- what_hand_fast(cards1)
    }
    if (!is.null(table_cards)) {
        cards2 <- c(cards2, table_cards)
    }
    hand2 <- what_hand_fast(cards2)
    if (hand1 > hand2) {
        return(1)
    } else if (hand2 > hand1) {
        return(2)
    } else {
        if (hand1 == 1) {
            return(hc_c(cards1, cards2))
        } else if (hand1 %in% c(2, 4, 8)) {
            return(xk_c(cards1, cards2))
        } else if (hand1 == 3) {
            return(tp_c(cards1, cards2))
        } else if (hand1 == 5) {
            return(st_c(cards1, cards2))
        } else if (hand1 == 6) {
            return(fl_c(cards1, cards2))
        }
        # hand_rank_functions <- c("rf_c", "sf_c", "xk_c", "fh_c", "fl_c", "st_c",
        #                          "xk_c", "tp_c", "xk_c", "hc_c")
        hand_rank_fun <- get(hand_rank_functions[hand_rank_pos])
        return(hand_rank_fun(cards1, cards2))
    }
}

who_won <- function(player_cards, table_cards) {
    best_hand <- paste(player_cards[1], paste(table_cards, collapse = " "))
    is_split <- FALSE
    split_winners <- substr(best_hand, 1, 5)
    i <- 2
    while (i <= length(player_cards)) {
        best_cards <- strsplit(best_hand, " ")[[1]]
        next_hand <- paste(player_cards[i], paste(table_cards, collapse = " "))
        next_cards <- strsplit(next_hand, " ")[[1]]
        winner <- compare_hands(best_cards, next_cards)
        if (winner == 0) {
            is_split <- TRUE
            split_winners <- c(split_winners, substr(next_hand, 1, 5))
        }
        if (winner == 2) {
            best_hand <- next_hand
        }
        i <- i + 1
    }
    if (is_split) {
        if (substr(best_hand, 1, 5) %in% split_winners) {
            win_pos <- which(player_cards %in% split_winners)
            return(win_pos)
        }
    }
    win_pos <- which(player_cards == substr(best_hand, 1, 5))
    return(win_pos)
}

get_card_num <- function(cards1, cards2, sort = TRUE) {
    ranks1 <- ranks[match(substr(cards1, 1, 1), ranks)]
    ranks2 <- ranks[match(substr(cards2, 1, 1), ranks)]
    if (sort) {
        ranks1 <- sort(ranks1, decreasing = TRUE)
        ranks2 <- sort(ranks2, decreasing = TRUE)
    }
    ranks1 <- as.numeric(ranks1)
    ranks2 <- as.numeric(ranks2)
    return(list(ranks1, ranks2))
}

hc_c <- function(cards1, cards2, num_cards = 5, is_char = TRUE, sort = TRUE) {
    if (is_char) {
        ranks12 <- get_card_num(cards1, cards2, sort)
        cards1 <- ranks12[[1]]
        cards2 <- ranks12[[2]]
    }

    if (num_cards == 5) {
        cards1 <- cards1[1:5]
        cards2 <- cards2[1:5]
    }
    rank_diff <- cards1 - cards2
    for (i in 1:num_cards) {
        if (rank_diff[i] > 0) {
            return(1)
        } else if (rank_diff[i] < 0) {
            return(2)
        }
    }
    return(0)
}

tp_c <- function(cards1, cards2) {
    ranks12 <- get_card_num(cards1, cards2)
    ranks1 <- ranks12[[1]]
    ranks2 <- ranks12[[2]]
    card1_table <- sort(table(ranks1), decreasing = TRUE)
    card2_table <- sort(table(ranks2), decreasing = TRUE)
    tp_names1 <- names(card1_table)[1:2]
    tp_names2 <- names(card2_table)[1:2]
    tp_ranks1 <- unique(ranks1[ranks1 %in% tp_names1])
    tp_ranks2 <- unique(ranks2[ranks2 %in% tp_names2])
    rank_diff <- tp_ranks1 - tp_ranks2
    if (any(is.na(rank_diff))) {browser()}
    for (i in 1:2) {
        if (rank_diff[i] > 0) {
            return(1)
        } else if (rank_diff[i] < 0) {
            return(2)
        }
    }
    hc_ranks1 <- ranks1[!ranks1 %in% tp_names1][1]
    hc_ranks2 <- ranks2[!ranks2 %in% tp_names2][1]
    return(hc_c(hc_ranks1, hc_ranks2, 1, FALSE))
}

xk_c <- function(cards1, cards2) {
    ranks12 <- get_card_num(cards1, cards2)
    ranks1 <- ranks12[[1]]
    ranks2 <- ranks12[[2]]
    x1 <- sort(table(ranks1), decreasing = TRUE)[1]
    x2 <- sort(table(ranks2), decreasing = TRUE)[1]
    xk_card1 <- names(x1)
    xk_card2 <- names(x2)
    xk_card1_c <- unique(ranks1[ranks1 == xk_card1])
    xk_card2_c <- unique(ranks2[ranks2 == xk_card2])
    if (xk_card1_c > xk_card2_c) {
        return(1)
    } else if (xk_card1_c < xk_card2_c) {
        return(2)
    }
    xk_card1_c <- ranks1[ranks1 != xk_card1]
    xk_card2_c <- ranks2[ranks2 != xk_card2]
    return(hc_c(xk_card1_c, xk_card2_c, 5 - x1, FALSE))
}

st_c <- function(cards1, cards2) {
    high_card1 <- straight(cards1, TRUE)
    high_card2 <- straight(cards2, TRUE)
    hc1 <- ranks[ranks == high_card1]
    hc2 <- ranks[ranks == high_card2]
    if (hc1 == hc2) {
        return(0)
    } else if (hc1 > hc2) {
        return(1)
    } else {
        return(2)
    }
}

fl_c <- function(cards1, cards2) {
    values1 <- substr(cards1, 1, 1)
    values2 <- substr(cards2, 1, 1)
    suits1 <- substr(cards1, 2, 2)
    suits2 <- substr(cards2, 2, 2)
    flush_suit <- names(sort(table(suits1), decreasing = TRUE)[1])
    fl_vals1 <- cards1[suits1 == flush_suit]
    fl_vals2 <- cards2[suits2 == flush_suit]
    return(hc_c(fl_vals1, fl_vals2))
}

sf_c <- function(cards1, cards2) {
    hc1 <- straight_flush(cards1, TRUE)
    hc2 <- straight_flush(cards2, TRUE)
    if (hc1 == hc2) {
        return(0)
    } else if (hc1 > hc2) {
        return(1)
    } else {
        return(2)
    }
}

fh_c <- function(cards1, cards2) {
    fh1 <- full_house(cards1, TRUE)
    fh2 <- full_house(cards2, TRUE)
    return(hc_c(fh1, fh2, 2, sort = FALSE))
}

hand_strength <- function(player_cards, table_cards, stage) {
    if (stage == "PF") {
        pf_hs <- readRDS("data/pf_hs.Rds")
        player_cards_str <- 1 -
            pf_hs[rownames(pf_hs) == simplify_cards(player_cards)]
        return(player_cards_str)
    }
    other_cards <- hands[!hands %in% c(player_cards, table_cards)]
    cards1 <- c(player_cards, table_cards)
    hand1 <- what_hand(cards1)
    other_card_combos <- combn(other_cards, 2)
    better <- rep(0, ncol(other_card_combos))
    for (i in 1:ncol(other_card_combos)) {
        cards2 <- c(other_card_combos[, i], table_cards)
        better[i] <- compare_hands(cards1, cards2, hand1)
    }
    # which_better <- better == 2
    # which_same <- better == 0
    return(better)
}

hand_strength_fast <- function(player_cards, table_cards, stage) {
    if (stage == "PF") {
        pf_hs <- readRDS("data/pf_hs.Rds")
        player_cards_str <- 1 -
            pf_hs[rownames(pf_hs) == simplify_cards(player_cards)]
        return(player_cards_str)
    }
    other_cards <- hands[!hands %in% c(player_cards, table_cards)]
    cards1 <- c(player_cards, table_cards)
    hand1 <- what_hand(cards1)
    better <- combn(other_cards, 2, compare_hands, cards1 = cards1, hand1 = hand1,
                    table_cards = table_cards)
    # better[i] <- compare_hands(cards1, cards2, hand1, table_cards)
    # which_better <- better == 2
    # which_same <- better == 0
    return(better)
}

rank_ints <- 2:14
suit_ints <- 1:4
rank_strings <- c(2:9, "T", "J", "Q", "K", "A")
suit_strings <- c("S", "C", "H", "D")

IntToString <- function(x) {
    len <- length(x)
    mid <- len / 2
    ranks <- x[1:mid]
    suits <- x[(mid + 1):len]
    ranks <- rank_strings[ranks - 1]
    suits <- suit_strings[suits]
    return(paste0(ranks, suits))
}

StringToInt <- function(x) {
    ranks <- substr(x, 1, 1)
    suits <- substr(x, 2, 2)

    ranks <- rank_ints[match(ranks, rank_strings)]
    suits <- suit_ints[match(suits, suit_strings)]

    return(c(ranks, suits))
}

get_prime_prod <- function(values) {
    primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41)
    pos <- match(values, 2:14)
    return(prod(primes[pos]))
}

get_original_values <- function(prod) {
    prime_fac <- factors(prod)
    primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41)
    pos <- match(prime_fac, primes)
    return(pos + 1)
}

what_hand_lookup <- function(cards, hand_table = NULL, fl_hand_table = NULL) {
    len <- length(cards)
    mid <- len / 2
    if (is.null(hand_table)) {
        table_loc <- paste0("data/hand_tables/hand_table", mid, ".Rds")
        hand_table <- readRDS(table_loc)
    }
    values <- cards[1:mid]
    suits <- cards[(mid + 1):len]
    suit_tab <- tabulate(suits)
    ranks <- hand_table$rank
    p_prods <- hand_table$prod
    if (max(suit_tab) > 4) {
        fl_suit <- which.max(suit_tab)
        fl_vals <- values[suits == fl_suit]
        fl_hand <- get_prime_prod(fl_vals) * 2
        nfl_hand <- get_prime_prod(values)
        if (is.null(fl_hand_table)) {
            table_loc <- paste0("data/hand_tables/hand_table",
                                length(fl_vals), ".Rds")
            fl_hand_table <- readRDS(table_loc)
        }
        fl_ranks <- fl_hand_table$rank
        fl_p_prods <- fl_hand_table$prod
        fl_pos <- match(fl_hand, fl_p_prods)
        nfl_pos <- match(nfl_hand, p_prods)
        fl_out <- fl_ranks[fl_pos]
        nfl_out <- ranks[nfl_pos]
        out <- min(fl_out, nfl_out)
    } else {
        hand <- get_prime_prod(values)
        pos <- match(hand, p_prods)
        out <- ranks[pos]
    }
    return(out)
}

# need hand_strength function to read hand_table then do lookups - at end

# what_hand_lookup and compare_hands need updating
# then move onto hand_strength

