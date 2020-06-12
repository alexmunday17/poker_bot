console_player <- function(to_call, player_cards, table_cards, stage, ...) {
    print(player_cards)
    if (stage == "F") {
        table_cards <- table_cards[1:3]
    } else if (stage == "T") {
        table_cards <- table_cards[1:4]
    }
    if (stage != "PF") {
        print(table_cards)
        cat("\n")
    }
    if (to_call == 0) {
        action <- readline("Check (k), Bet (b): ")
    } else {
        action <- readline(paste("Fold (f), Call", to_call, "(c), Raise (r): "))
    }
    if (!action %in% c("f", "k", "c", "b", "r")) {
        warning("Invalid input.")
        console_player(to_call)
    }
    if (action %in% c("f", "k")) {
        return(c(action, 0))
    } else if (action == "c") {
        return(c("c", to_call))
    } else {
        value <- readline("Bet/Raise amount: ")
        return(c(action, value))
    }
}

fold_player <- function(to_call, ...) {
    if (to_call == 0) {
        return(c("k", 0))
    } else {
        return(c("f", 0))
    }
}

call_player <- function(to_call, ...) {
    if (to_call == 0) {
        return(c("k", 0))
    } else {
        return(c("c", to_call))
    }
}

bet_player <- function(to_call, ...) {
    if (to_call == 0) {
        return(c("b", 0))
    } else {
        return(c("r", 0))
    }
}

random_player <- function(to_call, ...) {
    if (to_call == 0) {
        possible_actions <- c("k", "b")
    } else {
        possible_actions <- c("f", "c", "r")
    }
    action <- sample(possible_actions, 1)
    if (action != "c") {
        return(c(action, 0))
    } else {
        return(c("c", to_call))
    }
}

bot_v0.1 <- function(to_call, player_cards, table_cards, stage, min_inc, pot_size,
                     current_bet, max_bet, current_chips) {
    if (stage == "F") {
        table_cards <- table_cards[1:3]
    } else if (stage == "T") {
        table_cards <- table_cards[1:4]
    }
    h_str <- hand_strength(player_cards, table_cards, stage)
    if (stage == "PF") {
        action <- bot_action(h_str, 0.42, to_call, min_inc, max_bet)
    } else {
        action <- bot_action(h_str, 0.3, to_call, min_inc, max_bet)
    }
}

bot_action <- function(h_str, threshold, to_call, min_inc, max_bet) {
    if (h_str < threshold) {
        if (to_call == 0) {
            action <- c("b", min_inc * 3)
        } else {
            action <- c("r", max_bet + min_inc * 2)
        }
    } else {
        if (to_call == 0) {
            action <- c("k", 0)
        } else {
            action <- c("f", 0)
        }
    }
}

pf_strategy <- function(to_call, player_cards, table_cards, stage, min_inc, pot_size,
                        current_bet, current_chips) {

}

# add bluffing (start with random (20%))
#   read up on post flop strategy
#   need to then use ranges to bluff at correct percentages

# function to check prob of improving hand
# pot odds when calling

# initial hand strength of better hands
# pre flop ranges - use this with get_hand_rank
# potential hand strength?

# start bot using get_hand_rank as benchmark
# need to make functions for is_straight_poss etc on flop and turn for player and opp

# hand_strength needs to refer to pre flop hand table
# analytic formula for pre flop odds?

# use number of hands in range for percentage of bluff
