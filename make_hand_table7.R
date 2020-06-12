hand_order <- c()
prod5 <- 0
# straight flush
for (i in 14:6) {
    best5 <- get_prime_prod(i - 0:4) * 2
    for (j in 14:2) {
        if (j - i == 1) {
            next
        }
        for (k in 14:2) {
            if (k - i == 1 | k < j) {
                next
            }
            hand_order <- c(hand_order, i - 0:4, j, k)
            prod5 <- c(prod5, best5)
        }
    }
}
for (i in 14:2) {
    if (i == 6) {
        next
    }
    for (j in 14:2) {
        if (j == 6 | j < i) {
            next
        }
        hand_order <- c(hand_order, 5:2, 14, i, j)
        prod5 <- c(prod5, 2 * get_prime_prod(c(14, 2:5)))
    }
}
sfl_end <- length(hand_order) / 7
# four of a kind: 13 * (choose(12,3) + choose(12,2) + choose(12,2) + 12)
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
            for (l in 14:2) {
                if (i == l | j < l | k < l) {
                    next
                }
                hand_order <- c(hand_order, rep(i, 4), j, k, l)
                prod5 <- c(prod5, best5)
            }
        }
    }
}
# full house:
# 331: sum(1:12) * 11
# 322:
# count <- 0
# for (i in 1:11) {count <- count + (12 - i) * i}
# 3211: 13 * 12 * choose(11,2)
# total: sum(1:12) * 11 + count + 13 * 12 * choose(11,2)
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
            for (l in 14:2) {
                if (i == l | j == l | (k == l & k > j) | k < l) {
                    next
                }
                hand_order <- c(hand_order, rep(i, 3), rep(j, 2), k, l)
                prod5 <- c(prod5, best5)
            }
        }
    }
}
# flush
fl_start <- 1 + length(hand_order) / 7
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
                        for (o in 14:2) {
                            if (m <= o) {
                                next
                            }
                            hand_order <- c(hand_order, i, j, k, l, m, n, o)
                            prod5 <- c(prod5, best5)
                        }
                    }
                }
            }
        }
    }
}
fl_end <- length(hand_order) / 7
# straight: sum(13:1) + sum(12:1) * 8 + sum(12:1) (ace high, others, ace low)
# (choose(13,2) + 13) + (choose(12,2) + 12) * 9
for (i in 14:6) {
    best5 <- get_prime_prod(i - 0:4)
    for (j in 14:2) {
        if (j - i == 1) {
            next
        }
        for (k in 14:2) {
            if (k - i == 1 | k < j) {
                next
            }
            hand_order <- c(hand_order, i - 0:4, j, k)
            prod5 <- c(prod5, best5)
        }
    }
}
for (i in 14:2) {
    if (i == 6) {
        next
    }
    for (j in 14:2) {
        if (j == 6 | j < i) {
            next
        }
        hand_order <- c(hand_order, 5:2, 14, i, j)
        prod5 <- c(prod5, get_prime_prod(c(14, 2:5)))
    }
}
# three of a kind: 13 * choose(12, 4)
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
                for (m in 14:2) {
                    if (i == m | j <= m | k <= m | l <= m) {
                        next
                    }
                    hand_order <- c(hand_order, rep(i, 3), j, k, l, m)
                    prod5 <- c(prod5, best5)
                }
            }
        }
    }
}
# two pair: choose(13, 2) * choose(11, 2) + sum(1:11) + sum(1:10) + ... + sum(1)
# 22111: choose(13, 2) * choose (11, 3)
# 2221: choose(13, 3) * 10
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
                if (i == l | j == l | k < l | (k == l & k > j)) {
                    next
                }
                for (m in 14:2) {
                    if (i == m | j == m | k == m | (l == m & (k < l | j < l)) |
                        l < m) {
                        next
                    }
                    hand_order <- c(hand_order, rep(i, 2), rep(j, 2), k, l, m)
                    prod5 <- c(prod5, best5)
                }
            }
        }
    }
}
# pair: 13 * choose(12, 5)
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
                    for (n in 14:2) {
                        if (i == n | j <= n | k <= n | l <= n | m <= n) {
                            next
                        }
                        hand_order <- c(hand_order, rep(i, 2), j, k, l, m, n)
                        prod5 <- c(prod5, best5)
                    }
                }
            }
        }
    }
}
# high card: choose(13, 7)
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
                        for (o in 14:2) {
                            if (i <= o | j <= o | k <= o | l <= o | m <= o | n <= o) {
                                next
                            }
                            hand_order <- c(hand_order, i, j, k, l, m, n, o)
                            prod5 <- c(prod5, best5)
                        }
                    }
                }
            }
        }
    }
}

hand_table <- matrix(hand_order, nrow = 7)
prod5 <- prod5[-1]
prime_prods <- apply(hand_table, 2, get_prime_prod)
prime_prods[1:sfl_end] <- prime_prods[1:sfl_end] * 2
prime_prods[fl_start:fl_end] <- prime_prods[fl_start:fl_end] * 2
prod5 <- prod5[!duplicated(prime_prods)]
prime_prods <- prime_prods[!duplicated(prime_prods)]
hand_table7 <- data.frame(prod = prime_prods, prod5 = prod5)
hand_table5 <- readRDS("data/hand_tables/hand_table5.Rds")
hand_table7 <- left_join(hand_table7, hand_table5, by = c("prod5" = "prod"))
hand_table7 <- select(hand_table7, -prod5)
saveRDS(hand_table7, "data/hand_tables/hand_table7.Rds")

# have tables for each hand type (hc, p, tp etc) with prime multiplication lookup
# need this for 5, 6, 7 card hands
# order appropriately
# for flush and st flush, just find flush values and input
