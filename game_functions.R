# change bet player to call when no option to raise
# test scenarios with certain types of player

library(dplyr)
library(crayon)

deal_hands <- function(x) {
    return(sample(hands, 5 + 2 * x))
}

init_hand <- function(player_info, start_chips = 100, big_blind = 10) {
    ## initial chips
    player_info$chips <- start_chips

    ## first dealer
    n_players <- nrow(player_info)
    first_dealer_pos <- sample(1:n_players, 1)
    dealer <- rep(0, n_players)
    dealer[first_dealer_pos] <- 1
    player_info$dealer <- dealer

    ## return player info
    return(player_info)
}

next_dealer <- function(current_dealer) {
    n_players <- length(current_dealer)
    new_dealer <- current_dealer[c(n_players, 1:(n_players - 1))]
    return(new_dealer)
}

check_dealer <- function(player_info, current_dealer) {
    if (any(is.na(player_info$chips))) {
        if (1 %in% filter(player_info, is.na(chips))$dealer) {
            player_info$dealer <- next_dealer(current_dealer)
            current_dealer <- player_info$dealer
            player_info <- check_dealer(player_info, current_dealer)
        }
    }
    return(player_info)
}

run_game <- function(player_info, start_chips = 100, big_blind = 10, ante = 0,
                     verbose = TRUE) {

    if (length(player_info$chips) == 0) {
        player_info <- init_hand(player_info, start_chips, big_blind)
    } else {
        ## next dealer
        current_dealer <- player_info$dealer
        player_info$dealer <- next_dealer(current_dealer)
        player_info <- check_dealer(player_info, current_dealer)
        player_info <- filter(player_info, !is.na(chips))
    }

    if (nrow(player_info) == 1) {
        cat(blue(paste0("\n", player_info$name, " wins!\n")))
        return(player_info$player_type)
    }

    player_info <- apply_blinds(player_info, big_blind)
    new_chips <- run_betting(player_info, verbose)
    player_info$chips <- NULL
    player_info <- left_join(player_info, new_chips, by = "name")

    if (verbose) {
        print(player_info)
    }

    run_game(player_info, start_chips, big_blind, ante, verbose)
}

run_betting <- function(player_info, verbose = TRUE) {

    cat(blue("\nNew hand.\n\n"))

    for (round in c("PF", "F", "T", "R")) {

        if (round == "PF") {

            ## initialise variables
            player_info <- arrange(player_info, order1)
            n_players <- nrow(player_info)
            player_names <- player_info$name
            folded <- all_in <- acted <- rep(FALSE, n_players)
            names(folded) <- names(all_in) <- names(acted) <- player_names
            all_cards <- deal_hands(n_players)
            p_cards <- head(all_cards, 2 * n_players)
            player_cards <- NULL
            for (i in 1:length(p_cards)) {
                if (i %% 2 == 0) {
                    player_cards <- c(player_cards, paste(p_cards[i - 1], p_cards[i],
                                                          collapse = " "))
                }
            }
            names(player_cards) <- player_names
            table_cards <- tail(all_cards, 5)
            action_log <- data.frame(name = player_names,
                                     cards = player_cards,
                                     dealer = player_info$dealer,
                                     chips = player_info$chips,
                                     stage = "PF", action = NA,
                                     bet = player_info$bet,
                                     stage_bet = player_info$stage_bet,
                                     total_bet = player_info$total_bet,
                                     increment = 0,
                                     stringsAsFactors = FALSE)
            big_blind <- max(action_log$stage_bet)
            i <- 1

            ## print
            print(select(action_log, name, dealer, chips, stage_bet))
            cat(blue("\nPre Flop:\n\n"))

        } else {
            if (round == "F") {
                ## print
                cat(blue("\nFlop:\n\n"))
            } else if (round == "T") {
                ## print
                cat(blue("\nTurn:\n\n"))
            } else {
                ## print
                cat(blue("\nRiver:\n\n"))
            }
            i <- match(1, player_info$order2)
            acted[!(folded | all_in)] <- FALSE
        }

        stop_loop <- FALSE
        break_loop <- FALSE

        ## start loop
        while (!stop_loop) {
            j <- ((i - 1) %% n_players) + 1

            ## is all in from blinds?
            if (round == "PF" & action_log[j, "chips"] <= 0 & i <= n_players) {
                all_in[j] <- TRUE
                action_log[j, "total_bet"] <- action_log[j, "chips"] +
                    action_log[j, "stage_bet"]
                action_log[j, "chips"] <- 0
                i <- i + 1
                next
            }

            ## current game situation
            current_sit <- filter(action_log, stage == round)

            ## actions of player j in this stage
            p_name <- player_names[j]
            last_action_stage <- tail(filter(current_sit, name == p_name), 1)

            ## current player info
            max_bet <- max(pull(current_sit, stage_bet), 0)
            current_bet <- max(pull(last_action_stage, stage_bet), 0)
            to_call <- max_bet - current_bet

            ## stop if all folded or all all in
            if (all(folded[-j]) | all(all_in[-j][!folded[-j]], to_call == 0)) {
                break_loop <- TRUE
                break
            }

            ## next player if folded or all in
            if (any(folded[j], all_in[j])) {
                i <- i + 1
                next
            }

            ## last action of player j
            last_action <- tail(filter(action_log, name == p_name), 1)

            ## current player info
            player_type <- get(player_info[j, "player_type"])
            current_total_bet <- max(pull(last_action, total_bet), 0)
            current_chips <- last_action$chips
            increments <- pull(current_sit, increment)
            increments <- increments[!is.na(increments)]
            last_increment <- tail(increments[increments > 0], 1)
            if (length(last_increment) == 0) {
                min_inc <- big_blind
            } else {
                min_inc <- last_increment
            }
            last_actions <- action_log[!duplicated(action_log$name, fromLast = TRUE),
                                       c("name", "chips", "total_bet")]
            pot_size <- sum(last_actions$total_bet)

            ## player action based on game situation
            p_cards_split <- strsplit(player_cards[j], " ")[[1]]
            action <- player_type(to_call = to_call, player_cards = p_cards_split,
                                  table_cards = table_cards, stage = round,
                                  min_inc = min_inc, pot_size = pot_size,
                                  current_bet = current_bet, max_bet = max_bet,
                                  current_chips = current_chips)

            ## check action is okay
            if (!action[1] %in% c("f", "k", "c")) {
                action <- action_check(action, max_bet, min_inc, big_blind)
            }
            new_bet <- as.numeric(action[2])

            ## make new row or input in current row (pre flop first action)
            input_row <- ifelse(i <= n_players & round == "PF",
                                j, nrow(action_log) + 1)
            action_log[input_row, "name"] <- p_name
            action_log[input_row, "cards"] <- player_cards[j]
            action_log[input_row, "stage"] <- round
            action_log[input_row, "action"] <- action[1]

            ## if player is all in from action
            all_in_condition1 <- (new_bet >= current_chips) & (action[1] == "c")
            all_in_condition2 <- (new_bet - current_bet >= current_chips)
            if (all_in_condition1 | all_in_condition2) {
                action_log[input_row, c("bet", "stage_bet", "chips")] <-
                    c(current_chips, current_chips + current_bet, 0)
                action_log[input_row, "increment"] <-
                    max(action_log[input_row, "stage_bet"] - max_bet, 0)
                action_log[input_row, "total_bet"] <-
                    current_total_bet + current_chips
                if (action[1] != "c") {
                    acted[!(folded | all_in)] <- FALSE
                }
                acted[j] <- TRUE
                all_in[j] <- TRUE
                i <- i + 1
                if (verbose) {
                    print(action_log)
                } else {
                    print(select(action_log, -cards))
                }
                next
            }

            ## fill in new game situation following player action
            if (action[1] == "f") {
                folded[j] <- TRUE
            }
            if (action[1] != "r") {
                action_log[input_row, "bet"] <- new_bet
                action_log[input_row, "stage_bet"] <- new_bet + current_bet
                action_log[input_row, "chips"] <- current_chips - new_bet
                action_log[input_row, "total_bet"] <-
                    current_total_bet + new_bet
            } else {
                acted[!(folded | all_in)] <- FALSE
                action_log[input_row, "bet"] <- new_bet - current_bet
                action_log[input_row, "stage_bet"] <- new_bet
                action_log[input_row, "chips"] <-
                    current_chips - new_bet + current_bet
                action_log[input_row, "total_bet"] <-
                    current_total_bet + new_bet - current_bet
            }
            action_log[input_row, "increment"] <-
                max(action_log[input_row, "stage_bet"] - max_bet, 0)
            acted[j] <- TRUE

            if (all(folded | all_in)) {
                stop_loop <- TRUE
            }

            current_names <- player_names[!(folded | all_in)]
            name_pos <- player_info$name %in% current_names
            current_players <- filter(action_log, name %in% current_names)
            last_action_pos <- !duplicated(current_players$name, fromLast = TRUE)
            last_actions <- current_players[last_action_pos, ]
            if (length(unique(pull(last_actions, stage_bet))) == 1 &
                all(acted[name_pos]) & all(pull(last_actions, stage) == round)) {
                stop_loop <- TRUE
            }
            i <- i + 1
            if (verbose) {
                print(action_log)
            } else {
                print(select(action_log, -cards))
            }
        }
        if (round == "R" | break_loop) {
            cat(blue("\nBetting finished.\n"))
            break
        }
    }

    final_actions <- action_log[!duplicated(action_log$name, fromLast = TRUE),
                                c("name", "chips", "total_bet")]
    main_pot <- sum(final_actions$total_bet)

    player_cards <- player_cards[!folded]
    cat(blue("\nPlayer Cards:\n\n"))
    print(player_cards)
    cat(blue("\nTable Cards:\n\n"))
    last_stage <- tail(action_log$stage, 1)
    if (last_stage == "F") {
        table_cards <- table_cards[1:3]
    } else if (last_stage == "T") {
        table_cards <- table_cards[1:4]
    }
    if (last_stage != "PF" | any(all_in)) {
        print(table_cards)
    }

    if (sum(!folded) == 1) {
        winner <- player_names[which(!folded)]
        final_actions[final_actions$name == winner, "chips"] <-
            final_actions[final_actions$name == winner, "chips"] + main_pot
        cat(green("\n", winner, "wins", main_pot, "chips.\n"))
        return(select(final_actions, -total_bet))
    }

    winner <- names(player_cards[who_won(player_cards, table_cards)])

    if (any(all_in)) {
        while (sum(final_actions$total_bet) > 0) {
            final_actions <- get_winners(final_actions, winner)
            player_cards <- player_cards[!names(player_cards) %in% winner]
            winner <- names(player_cards[who_won(player_cards, table_cards)])
        }
        final_actions <- filter(final_actions, chips > 0)
        cat(red("\nALL IN SETTLEMENT\n\n"))
        return(select(final_actions, -total_bet))
    }

    if (length(winner) > 1) {
        for (w in winner) {
            final_actions[final_actions$name == w, "chips"] <-
                final_actions[final_actions$name == w, "chips"] +
                floor(main_pot / length(winner))
        }
        cat(green("\n", winner, "wins", main_pot, "chips.\n"))
    } else {
        final_actions[final_actions$name == winner, "chips"] <-
            final_actions[final_actions$name == winner, "chips"] + main_pot
        cat(green("\n", paste(winner, collaspe = " and "), "split",
                  main_pot, "chips.\n"))
    }

    final_actions <- filter(final_actions, chips > 0)
    return(select(final_actions, -total_bet))
}

get_winners <- function(final_actions, winner) {
    if (length(winner) > 1) {
        entitled <- rep(0, length(winner))
        names(entitled) <- winner
        for (w in winner) {
            final_actions$winner <- FALSE
            final_actions[final_actions$name == w, "winner"] <- TRUE
            total_bets <- final_actions$total_bet
            win_pos <- final_actions$winner
            leftover <- sapply(total_bets - total_bets[win_pos], max, 0)
            win_chips <- sum(total_bets) - sum(leftover)
            entitled[names(entitled) == w] <- win_chips
        }
        total_pot <- sum(total_bets)
        entitled <- sort(entitled)
        p1_win <- entitled[1] / length(entitled)
        final_actions[final_actions$name == names(entitled)[1], "chips"] <-
            final_actions[final_actions$name == names(entitled)[1], "chips"] +
            floor(p1_win)
        entitled <- entitled - p1_win
        p1_bet <- final_actions[final_actions$name == names(entitled)[1], "total_bet"]
        leftover <- sapply(total_bets - p1_bet, max, 0)
        final_actions$total_bet <- leftover
        for (i in 2:length(entitled)) {
            w <- names(entitled)[i]
            win_chips <- entitled[i] / (length(entitled) - i + 1)
            final_actions[final_actions$name == w, "chips"] <-
                final_actions[final_actions$name == w, "chips"] + floor(win_chips)
            entitled <- entitled - win_chips
            pi_bet <- final_actions[final_actions$name == w, "total_bet"]
            total_bets <- final_actions$total_bet
            leftover <- sapply(total_bets - pi_bet, max, 0)
            final_actions$total_bet <- leftover
        }
        final_actions$winner <- NULL
        return(final_actions)
    }
    final_actions$winner <- FALSE
    final_actions[final_actions$name == winner, "winner"] <- TRUE
    total_bets <- final_actions$total_bet
    win_pos <- final_actions$winner
    leftover <- sapply(total_bets - total_bets[win_pos], max, 0)
    win_chips <- sum(total_bets) - sum(leftover)
    final_actions[final_actions$name == winner, "chips"] <-
        final_actions[final_actions$name == winner, "chips"] + win_chips
    final_actions$total_bet <- leftover
    final_actions$winner <- NULL
    return(final_actions)
}

apply_blinds <- function(player_info, big_blind) {
    player_info$bet <- 0
    player_info$stage_bet <- 0
    player_info$total_bet <- 0
    n_players <- nrow(player_info)
    dealer_pos <- which(player_info$dealer == 1)
    play_order1 <- ((dealer_pos + 1 + 1:n_players) %% n_players) + 1
    play_order2 <- ((dealer_pos - 1 + 1:n_players) %% n_players) + 1
    player_info[play_order1, "order1"] <- 1:n_players
    player_info[play_order2, "order2"] <- 1:n_players
    player_info[play_order2[1:2], "stage_bet"] <-
        player_info[play_order2[1:2], "total_bet"] <- c(big_blind / 2, big_blind)
    player_info$chips <- player_info$chips - player_info$stage_bet
    return(player_info)
}

action_check <- function(action, max_bet, min_inc, big_blind) {
    bet <- as.numeric(action[2])
    if (action[1] == "b") {
        action[2] <- max(big_blind, bet)
    } else {
        action[2] <- max(min_inc + max_bet, bet)
    }
    if (as.numeric(action[2]) > bet) {
        cat(blue("\nBet/raise size too small, min bet/raise made.\n\n"))
    }
    return(action)
}

player_info <- data.frame(name = c("console1", "console2", "console3", "console4"),
                          player_type = "console_player",
                          chips = 100,
                          dealer = c(1,0,0,0),
                          stringsAsFactors = FALSE)

player_info <- data.frame(name = paste0("p", seq(1,4)),
                          player_type = c("fold_player", "bet_player",
                                          "fold_player", "call_player"),
                          stringsAsFactors = FALSE)

player_info <- data.frame(name = paste0("p", seq(1,4)),
                          player_type = c("fold_player", "call_player",
                                          "console_player", "bet_player"),
                          stringsAsFactors = FALSE)

player_info <- data.frame(name = paste0("p", seq(1,2)),
                          player_type = c("console_player", "console_player"),
                          chips = 100,
                          dealer = c(0,1),
                          stringsAsFactors = FALSE)

player_info1 <- apply_blinds(player_info, 10)


player_info <- data.frame(name = c("bot", "alex"),
                          player_type = c("bot_v0.1", "console_player"),
                          stringsAsFactors = FALSE)

