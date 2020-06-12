library(dplyr)
hand_order <- c()
prod5 <- 0
# straight flush
for (i in 14:6) {
    best5 <- get_prime_prod(i - 0:4) * 2
    for (j in 14:2) {
        if (j - i == 1) {
            next
        }
        hand_order <- c(hand_order, i - 0:4, j)
        prod5 <- c(prod5, best5)
    }
}
for (i in 14:2) {
    if (i == 6) {
        next
    }
    hand_order <- c(hand_order, 5:2, 14, i)
    prod5 <- c(prod5, get_prime_prod(c(14, 2:5)) * 2)
}
sfl_end <- length(hand_order) / 6
# four of a kind: 13 * (choose(12,2) + 12)
for (i in 14:2) {
    for (j in 14:2) {
        if (i == j) {
            next
        }
        best5 <- get_prime_prod(c(rep(i, 4), j))
        for (k in 14:2) {
            if (i == k | j < k) {
                next
            }
            hand_order <- c(hand_order, rep(i, 4), j, k)
            prod5 <- c(prod5, best5)
        }
    }
}
# full house: 13 * 12 * 11 + sum(1:12)
for (i in 14:2) {
    for (j in 14:2) {
        if (i == j) {
            next
        }
        best5 <- get_prime_prod(c(rep(i, 3), rep(j, 2)))
        for (k in 14:2) {
            if (i == k | (j == k & j > i)) {
                next
            }
            hand_order <- c(hand_order, rep(i, 3), rep(j, 2), k)
            prod5 <- c(prod5, best5)
        }
    }
}
# flush
fl_start <- 1 + length(hand_order) / 6
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
                    best5 <- get_prime_prod(c(i, j, k, l, m)) * 2
                    for (n in 14:2) {
                        if (m <= n) {
                            next
                        }
                        hand_order <- c(hand_order, i, j, k, l, m, n)
                        prod5 <- c(prod5, best5)
                    }
                }
            }
        }
    }
}
fl_end <- length(hand_order) / 6
# straight: 13 + 12 * 8 + 12 (ace high, others, ace low)
for (i in 14:6) {
    best5 <- get_prime_prod(i - 0:4)
    for (j in 14:2) {
        if (j - i == 1) {
            next
        }
        hand_order <- c(hand_order, i - 0:4, j)
        prod5 <- c(prod5, best5)
    }
}
for (i in 14:2) {
    if (i == 6) {
        next
    }
    hand_order <- c(hand_order, 5:2, 14, i)
    prod5 <- c(prod5, get_prime_prod(c(14, 2:5)))
}
# three of a kind: 13 * choose(12, 3)
for (i in 14:2) {
    for (j in 14:2) {
        if (i == j) {
            next
        }
        for (k in 14:2) {
            if (i == k | j <= k) {
                next
            }
            best5 <- get_prime_prod(c(rep(i, 3), j, k))
            for (l in 14:2) {
                if (i == l | j <= l | k <= l) {
                    next
                }
                hand_order <- c(hand_order, rep(i, 3), j, k, l)
                prod5 <- c(prod5, best5)
            }
        }
    }
}
# two pair: choose(13, 2) * choose(11, 2) + sum(1:11) + sum(1:10) + ... + sum(1)
# count <- 0
# for (i in 1:11) {count <- count + (12 - i) * i}
# choose(13, 2) * choose(11, 2) + count
for (i in 14:2) {
    for (j in 14:2) {
        if (i <= j) {
            next
        }
        for (k in 14:2) {
            if (i == k | j == k) {
                next
            }
            best5 <- get_prime_prod(c(rep(i, 2), rep(j, 2), k))
            for (l in 14:2) {
                if (i == l | j == l | k < l) {
                    next
                }
                if (k == l & k > j) {
                    next
                }
                hand_order <- c(hand_order, rep(i, 2), rep(j, 2), k, l)
                prod5 <- c(prod5, best5)
            }
        }
    }
}
# pair: 13 * choose(12, 4)
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
                best5 <- get_prime_prod(c(rep(i, 2), j, k, l))
                for (m in 14:2) {
                    if (i == m | j <= m | k <= m | l <= m) {
                        next
                    }
                    hand_order <- c(hand_order, rep(i, 2), j, k, l, m)
                    prod5 <- c(prod5, best5)
                }
            }
        }
    }
}
# high card: choose(13, 6)
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
                    best5 <- get_prime_prod(c(i, j, k, l, m))
                    for (n in 14:2) {
                        if (i <= n | j <= n | k <= n | l <= n | m <= n) {
                            next
                        }
                        hand_order <- c(hand_order, i, j, k, l, m, n)
                        prod5 <- c(prod5, best5)
                    }
                }
            }
        }
    }
}

hand_table <- matrix(hand_order, nrow = 6)
prod5 <- prod5[-1]
prime_prods <- apply(hand_table, 2, get_prime_prod)
prime_prods[1:sfl_end] <- prime_prods[1:sfl_end] * 2
prime_prods[fl_start:fl_end] <- prime_prods[fl_start:fl_end] * 2
prod5 <- prod5[!duplicated(prime_prods)]
prime_prods <- prime_prods[!duplicated(prime_prods)]
hand_table6 <- data.frame(prod = prime_prods, prod5 = prod5)
hand_table5 <- readRDS("data/hand_tables/hand_table5.Rds")
hand_table6 <- left_join(hand_table6, hand_table5, by = c("prod5" = "prod"))
hand_table6 <- select(hand_table6, -prod5)
saveRDS(hand_table6, "data/hand_tables/hand_table6.Rds")

# have tables for each hand type (hc, p, tp etc) with prime multiplication lookup
# need this for 5, 6, 7 card hands
# order appropriately
# for flush and st flush, just find flush values and input
