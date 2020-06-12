hand_order <- c()
# straight flush
for (i in 14:6) {
    hand_order <- c(hand_order, i - 0:4)
}
hand_order <- c(hand_order, 5:2, 14)
sfl_end <- length(hand_order) / 5
# four of a kind
for (i in 14:2) {
    for (j in 14:2) {
        if (i == j) {
            next
        }
        hand_order <- c(hand_order, rep(i, 4), j)
    }
}
# full house
for (i in 14:2) {
    for (j in 14:2) {
        if (i == j) {
            next
        }
        hand_order <- c(hand_order, rep(i, 3), rep(j, 2))
    }
}
# flush
fl_start <- 1 + length(hand_order) / 5
for (i in 14:2) {
    for (j in 14:2) {
        if (i <= j) {
            next
        }
        for (k in 14:2) {
            if (j <= k) {
                next
            }
            for (l in 14:2) {
                if (k <= l) {
                    next
                }
                for (m in 14:2) {
                    if (l <= m) {
                        next
                    }
                    hand_order <- c(hand_order, i, j, k, l, m)
                }
            }
        }
    }
}
fl_end <- length(hand_order) / 5
# straight
for (i in 14:6) {
    hand_order <- c(hand_order, i - 0:4)
}
hand_order <- c(hand_order, 5:2, 14)
# three of a kind
for (i in 14:2) {
    for (j in 14:2) {
        if (i == j) {
            next
        }
        for (k in 14:2) {
            if (i == k | j <= k) {
                next
            }
            hand_order <- c(hand_order, rep(i, 3), j, k)
        }
    }
}
# two pair
for (i in 14:2) {
    for (j in 14:2) {
        if (i <= j) {
            next
        }
        for (k in 14:2) {
            if (i == k | j == k) {
                next
            }
            hand_order <- c(hand_order, rep(i, 2), rep(j, 2), k)
        }
    }
}
# pair
for (i in 14:2) {
    for (j in 14:2) {
        if (i == j) {
            next
        }
        for (k in 14:2) {
            if (i == k | j <= k) {
                next
            }
            for (l in 14:2) {
                if (i == l | j <= l | k <= l) {
                    next
                }
                hand_order <- c(hand_order, rep(i, 2), j, k, l)
            }
        }
    }
}
# high card
for (i in 14:2) {
    for (j in 14:2) {
        if (i <= j) {
            next
        }
        for (k in 14:2) {
            if (i <= k | j <= k) {
                next
            }
            for (l in 14:2) {
                if (i <= l | j <= l | k <= l) {
                    next
                }
                for (m in 14:2) {
                    if (i <= m | j <= m | k <= m | l <= m) {
                        next
                    }
                    hand_order <- c(hand_order, i, j, k, l, m)
                }
            }
        }
    }
}

hand_table <- matrix(hand_order, nrow = 5)
prime_prods <- apply(hand_table, 2, get_prime_prod)
prime_prods[1:sfl_end] <- prime_prods[1:sfl_end] * 2
prime_prods[fl_start:fl_end] <- prime_prods[fl_start:fl_end] * 2
prime_prods <- prime_prods[!duplicated(prime_prods)]
hand_table5 <- data.frame(prod = prime_prods, rank = 1:length(prime_prods))
saveRDS(hand_table5, "data/hand_tables/hand_table5.Rds")

# can just add dead cards for 6 and 7 card hands

# have tables for each hand type (hc, p, tp etc) with prime multiplication lookup
# need this for 5, 6, 7 card hands
# order appropriately
# for flush and st flush, just find flush values and input
