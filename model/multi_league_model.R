# Multi-League Soccer Betting Model
# Poisson Model for Premier League, La Liga, Bundesliga, Serie A, Ligue 1
# Reads from: All Leagues Clean.xlsx
# Odds template: odds_template.xlsx
# Results output: value_results.xlsx

library(readxl)
library(writexl)
library(tidyverse)

# ============================================================
# STEP 1: READ AND PREPARE DATA FOR ALL LEAGUES
# ============================================================

pl_clean <- read_excel("All Leagues Clean.xlsx", sheet = "Premier League") %>%
  filter(!is.na(HomeGoals))

la_liga_clean <- read_excel("All Leagues Clean.xlsx", sheet = "La Liga") %>%
  filter(!is.na(HomeGoals))

bundesliga_clean <- read_excel("All Leagues Clean.xlsx", sheet = "Bundesliga") %>%
  filter(!is.na(HomeGoals))

serie_a_clean <- read_excel("All Leagues Clean.xlsx", sheet = "Serie A") %>%
  filter(!is.na(HomeGoals))

ligue_1_clean <- read_excel("All Leagues Clean.xlsx", sheet = "Ligue 1") %>%
  filter(!is.na(HomeGoals))

cl_clean <- read_excel("All Leagues Clean.xlsx", sheet = "Champions League") %>%
  filter(!is.na(HomeGoals))

all_leagues <- list(
  "Premier League" = pl_clean,
  "La Liga" = la_liga_clean,
  "Bundesliga" = bundesliga_clean,
  "Serie A" = serie_a_clean,
  "Ligue 1" = ligue_1_clean,
  "Champions League" = cl_clean
)

# ============================================================
# STEP 2: BUILD MODELS FOR EACH LEAGUE (Bayesian Weighted)
# ============================================================
# Games from current season count more than older seasons.
# Weights: Current season = 1.0, Last season = 0.6, Two seasons ago = 0.3

models <- list()

current_season_start <- as.Date("2025-07-01")
last_season_start <- as.Date("2024-07-01")

for (league_name in names(all_leagues)) {
  data <- all_leagues[[league_name]]
  
  data <- data %>%
    mutate(
      game_date = as.Date(Date),
      weight = case_when(
        game_date >= current_season_start ~ 1.0,
        game_date >= last_season_start    ~ 0.6,
        TRUE                              ~ 0.3
      )
    )
  
  league_avg <- data %>%
    summarize(
      HomeGoalsAvg = weighted.mean(HomeGoals, weight),
      AwayGoalsAvg = weighted.mean(AwayGoals, weight)
    )
  
  home_stats <- data %>%
    group_by(Home) %>%
    summarize(
      HomeGamesPlayed = n(),
      HomeGoalsScored = weighted.mean(HomeGoals, weight),
      HomeGoalsConceded = weighted.mean(AwayGoals, weight)
    ) %>%
    mutate(
      HomeAttack = HomeGoalsScored / league_avg$HomeGoalsAvg,
      HomeDefense = HomeGoalsConceded / league_avg$AwayGoalsAvg
    )
  
  away_stats <- data %>%
    group_by(Away) %>%
    summarize(
      AwayGamesPlayed = n(),
      AwayGoalsScored = weighted.mean(AwayGoals, weight),
      AwayGoalsConceded = weighted.mean(HomeGoals, weight)
    ) %>%
    mutate(
      AwayAttack = AwayGoalsScored / league_avg$AwayGoalsAvg,
      AwayDefense = AwayGoalsConceded / league_avg$HomeGoalsAvg
    )
  
  team_stats <- home_stats %>%
    rename(Team = Home) %>%
    inner_join(away_stats %>% rename(Team = Away), by = "Team")
  
  models[[league_name]] <- list(league_avg = league_avg, team_stats = team_stats)
  cat(league_name, ":", nrow(data), "games,", nrow(team_stats), "teams\n")
}

# ============================================================
# STEP 3: FIND WHICH LEAGUE A TEAM IS IN
# ============================================================

find_team_league <- function(team_name) {
  for (league_name in names(models)) {
    if (team_name %in% models[[league_name]]$team_stats$Team) return(league_name)
  }
  return(NA)
}

# ============================================================
# STEP 4: PREDICT MATCH
# ============================================================

predict_match <- function(home_team, away_team) {
  home_league <- find_team_league(home_team)
  away_league <- find_team_league(away_team)
  if (is.na(home_league)) stop(paste("Team not found:", home_team))
  if (is.na(away_league)) stop(paste("Team not found:", away_team))
  home <- models[[home_league]]$team_stats %>% filter(Team == home_team)
  away <- models[[away_league]]$team_stats %>% filter(Team == away_team)
  league_avg <- models[[home_league]]$league_avg
  home_xg <- league_avg$HomeGoalsAvg * home$HomeAttack * away$AwayDefense
  away_xg <- league_avg$AwayGoalsAvg * away$AwayAttack * home$HomeDefense
  scores <- expand.grid(HomeG = 0:6, AwayG = 0:6)
  scores$Prob <- dpois(scores$HomeG, home_xg) * dpois(scores$AwayG, away_xg)
  home_win <- sum(scores$Prob[scores$HomeG > scores$AwayG])
  draw <- sum(scores$Prob[scores$HomeG == scores$AwayG])
  away_win <- sum(scores$Prob[scores$HomeG < scores$AwayG])
  over_25 <- sum(scores$Prob[(scores$HomeG + scores$AwayG) > 2.5])
  under_25 <- sum(scores$Prob[(scores$HomeG + scores$AwayG) < 2.5])
  over_15 <- sum(scores$Prob[(scores$HomeG + scores$AwayG) > 1.5])
  under_15 <- sum(scores$Prob[(scores$HomeG + scores$AwayG) < 1.5])
  btts_yes <- sum(scores$Prob[scores$HomeG > 0 & scores$AwayG > 0])
  btts_no <- sum(scores$Prob[scores$HomeG == 0 | scores$AwayG == 0])
  if (home_league == away_league) {
    cat("Match:", home_team, "vs", away_team, "(", home_league, ")\n")
  } else {
    cat("Match:", home_team, "(", home_league, ") vs", away_team, "(", away_league, ")\n")
  }
  cat("Expected Goals:", round(home_xg, 2), "-", round(away_xg, 2), "\n\n")
  cat("Home Win:", round(home_win * 100, 1), "%\n")
  cat("Draw:", round(draw * 100, 1), "%\n")
  cat("Away Win:", round(away_win * 100, 1), "%\n\n")
  cat("Over 2.5:", round(over_25 * 100, 1), "%\n")
  cat("Under 2.5:", round(under_25 * 100, 1), "%\n\n")
  cat("Over 1.5:", round(over_15 * 100, 1), "%\n")
  cat("Under 1.5:", round(under_15 * 100, 1), "%\n\n")
  cat("BTTS Yes:", round(btts_yes * 100, 1), "%\n")
  cat("BTTS No:", round(btts_no * 100, 1), "%\n")
}

# ============================================================
# STEP 5: CONVERT AMERICAN ODDS TO DECIMAL
# ============================================================

american_to_decimal <- function(odds) {
  if (odds < 0) { return(1 + (100 / abs(odds))) } else { return(1 + (odds / 100)) }
}

# ============================================================
# STEP 6: PROCESS ALL MATCHES FROM XLSX TEMPLATE
# ============================================================

process_all_matches <- function(file = "odds_template.xlsx") {
  odds <- read_excel(file)
  odds$Home <- trimws(odds$Home)
  odds$Away <- trimws(odds$Away)
  all_results <- list()
  for (i in 1:nrow(odds)) {
    row <- odds[i, ]
    home_league <- find_team_league(row$Home)
    away_league <- find_team_league(row$Away)
    if (is.na(home_league)) { cat("WARNING: Team not found:", row$Home, "- skipping\n"); next }
    if (is.na(away_league)) { cat("WARNING: Team not found:", row$Away, "- skipping\n"); next }
    home <- models[[home_league]]$team_stats %>% filter(Team == row$Home)
    away <- models[[away_league]]$team_stats %>% filter(Team == row$Away)
    league_avg <- models[[home_league]]$league_avg
    home_xg <- league_avg$HomeGoalsAvg * home$HomeAttack * away$AwayDefense
    away_xg <- league_avg$AwayGoalsAvg * away$AwayAttack * home$HomeDefense
    scores <- expand.grid(HomeG = 0:6, AwayG = 0:6)
    scores$Prob <- dpois(scores$HomeG, home_xg) * dpois(scores$AwayG, away_xg)
    model_home <- sum(scores$Prob[scores$HomeG > scores$AwayG])
    model_draw <- sum(scores$Prob[scores$HomeG == scores$AwayG])
    model_away <- sum(scores$Prob[scores$HomeG < scores$AwayG])
    model_over25 <- sum(scores$Prob[(scores$HomeG + scores$AwayG) > 2.5])
    model_under25 <- sum(scores$Prob[(scores$HomeG + scores$AwayG) < 2.5])
    model_over15 <- sum(scores$Prob[(scores$HomeG + scores$AwayG) > 1.5])
    model_under15 <- sum(scores$Prob[(scores$HomeG + scores$AwayG) < 1.5])
    model_btts_y <- sum(scores$Prob[scores$HomeG > 0 & scores$AwayG > 0])
    model_btts_n <- sum(scores$Prob[scores$HomeG == 0 | scores$AwayG == 0])
    h_dec <- american_to_decimal(row$Home_Odds)
    d_dec <- american_to_decimal(row$Draw_Odds)
    a_dec <- american_to_decimal(row$Away_Odds)
    o25_dec <- american_to_decimal(row$Over25_Odds)
    u25_dec <- american_to_decimal(row$Under25_Odds)
    o15_dec <- american_to_decimal(row$Over15_Odds)
    u15_dec <- american_to_decimal(row$Under15_Odds)
    by_dec <- american_to_decimal(row$BTTS_Yes_Odds)
    bn_dec <- american_to_decimal(row$BTTS_No_Odds)
    match_results <- data.frame(
      Match = paste(row$Home, "vs", row$Away),
      Bet = c("Home Win", "Draw", "Away Win", "Over 2.5", "Under 2.5", "Over 1.5", "Under 1.5", "BTTS Yes", "BTTS No"),
      Model = round(c(model_home, model_draw, model_away, model_over25, model_under25, model_over15, model_under15, model_btts_y, model_btts_n) * 100, 1),
      Bookmaker = round(c(1/h_dec, 1/d_dec, 1/a_dec, 1/o25_dec, 1/u25_dec, 1/o15_dec, 1/u15_dec, 1/by_dec, 1/bn_dec) * 100, 1),
      Odds = round(c(h_dec, d_dec, a_dec, o25_dec, u25_dec, o15_dec, u15_dec, by_dec, bn_dec), 2)
    )
    match_results$Edge <- match_results$Model - match_results$Bookmaker
    match_results$Value <- ifelse(match_results$Edge > 0, "VALUE", "")
    match_results$Rating <- case_when(
      match_results$Edge >= 5 & match_results$Model >= 60 ~ "A",
      match_results$Edge >= 5 & match_results$Model >= 40 ~ "B",
      match_results$Edge > 0 & match_results$Model >= 60 ~ "C",
      match_results$Edge > 0 ~ "D",
      TRUE ~ ""
    )
    all_results[[i]] <- match_results
  }
  final <- bind_rows(all_results)
  print(final)
  cat("\n=== VALUE BETS ONLY ===\n\n")
  value_only <- final %>% filter(Value == "VALUE") %>% arrange(Rating)
  print(value_only)
  write_xlsx(final, "value_results.xlsx")
  cat("\nResults saved to value_results.xlsx\n")
}

# ============================================================
# STEP 7: CREATE THE ODDS TEMPLATE (run once, then comment out)
# ============================================================

# template <- data.frame(
#  Home = c("Arsenal", "Wolves"), Away = c("Sunderland", "Chelsea"),
#  Home_Odds = c(-350, 375), Draw_Odds = c(450, 310), Away_Odds = c(1000, -150),
#  Over25_Odds = c(-125, -150), Under25_Odds = c(100, 120),
#  Over15_Odds = c(-300, -450), Under15_Odds = c(200, 300),
#  BTTS_Yes_Odds = c(137, -143), BTTS_No_Odds = c(-188, 105)
# )
# write_xlsx(template, "odds_template.xlsx")

# ============================================================
# STEP 8: LIST ALL TEAMS
# ============================================================

list_teams <- function(league = NULL) {
  if (is.null(league)) {
    for (league_name in names(models)) {
      cat("\n===", league_name, "===\n")
      cat(sort(models[[league_name]]$team_stats$Team), sep = "\n")
    }
  } else {
    cat(sort(models[[league]]$team_stats$Team), sep = "\n")
  }
}

# ============================================================
# STEP 9: LOAD STATS (xG from FotMob, shots from FBref)
# ============================================================

load_fbref_stats <- function(file = "FBref_Stats.xlsx") {
  sheets <- excel_sheets(file)
  all_fbref <- data.frame()
  for (sheet in sheets) {
    if (sheet == "Instructions") next
    data <- read_excel(file, sheet = sheet, skip = 3) %>%
      filter(!is.na(Team), Team != "")
    if (nrow(data) > 0) {
      data$League <- sheet
      all_fbref <- bind_rows(all_fbref, data)
    }
  }
  colnames(all_fbref) <- c("Team", "xG_For", "xG_Against", "BigChances", "BigChancesMissed",
                           "Corners", "Shots", "SoT", "Shots_Against", "SoT_Against", "GP", "League")
  all_fbref$xG_For <- as.numeric(all_fbref$xG_For)
  all_fbref$xG_Against <- as.numeric(all_fbref$xG_Against)
  all_fbref$BigChances <- as.numeric(all_fbref$BigChances)
  all_fbref$BigChancesMissed <- as.numeric(all_fbref$BigChancesMissed)
  all_fbref$Corners <- as.numeric(all_fbref$Corners)
  all_fbref$Shots <- as.numeric(all_fbref$Shots)
  all_fbref$SoT <- as.numeric(all_fbref$SoT)
  all_fbref$Shots_Against <- as.numeric(all_fbref$Shots_Against)
  all_fbref$SoT_Against <- as.numeric(all_fbref$SoT_Against)
  all_fbref$GP <- as.numeric(all_fbref$GP)
  return(all_fbref)
}

# ============================================================
# STEP 10: GET RECENT FORM FROM MATCH DATA
# ============================================================

get_recent_form <- function(team, n = 5) {
  league <- find_team_league(team)
  if (is.na(league)) { cat("Team not found:", team, "\n"); return(NULL) }
  data <- all_leagues[[league]]
  team_games <- data %>% filter(Home == team | Away == team) %>% arrange(desc(Date)) %>% head(n)
  if (nrow(team_games) == 0) { cat("No games found for", team, "\n"); return(NULL) }
  form <- team_games %>%
    mutate(
      GF = ifelse(Home == team, HomeGoals, AwayGoals),
      GA = ifelse(Home == team, AwayGoals, HomeGoals),
      Opponent = ifelse(Home == team, Away, Home),
      Venue = ifelse(Home == team, "H", "A"),
      Result = case_when(GF > GA ~ "W", GF == GA ~ "D", GF < GA ~ "L")
    )
  return(form)
}

# ============================================================
# STEP 11: GET HEAD TO HEAD HISTORY
# ============================================================

get_h2h <- function(home_team, away_team, n = 5) {
  all_meetings <- data.frame()
  for (league_name in names(all_leagues)) {
    data <- all_leagues[[league_name]]
    meetings <- data %>% filter((Home == home_team & Away == away_team) | (Home == away_team & Away == home_team))
    if (nrow(meetings) > 0) { meetings$Competition <- league_name; all_meetings <- bind_rows(all_meetings, meetings) }
  }
  if (nrow(all_meetings) == 0) return(NULL)
  all_meetings %>% arrange(desc(Date)) %>% head(n)
}

# ============================================================
# STEP 12: GET SEASON STATS FROM MATCH DATA
# ============================================================

get_season_stats <- function(team) {
  league <- find_team_league(team)
  if (is.na(league)) return(NULL)
  data <- all_leagues[[league]]
  home_games <- data %>% filter(Home == team)
  away_games <- data %>% filter(Away == team)
  total_games <- nrow(home_games) + nrow(away_games)
  goals_scored <- sum(home_games$HomeGoals) + sum(away_games$AwayGoals)
  goals_conceded <- sum(home_games$AwayGoals) + sum(away_games$HomeGoals)
  clean_sheets <- sum(home_games$AwayGoals == 0) + sum(away_games$HomeGoals == 0)
  home_scored <- sum(home_games$HomeGoals)
  home_conceded <- sum(home_games$AwayGoals)
  away_scored <- sum(away_games$AwayGoals)
  away_conceded <- sum(away_games$HomeGoals)
  list(
    team = team, league = league, total_games = total_games,
    home_games = nrow(home_games), away_games = nrow(away_games),
    goals_scored = goals_scored, goals_conceded = goals_conceded,
    goals_per_game = round(goals_scored / total_games, 2),
    conceded_per_game = round(goals_conceded / total_games, 2),
    clean_sheets = clean_sheets,
    clean_sheet_pct = round(clean_sheets / total_games * 100, 1),
    home_scored_pg = round(home_scored / nrow(home_games), 2),
    home_conceded_pg = round(home_conceded / nrow(home_games), 2),
    away_scored_pg = round(away_scored / nrow(away_games), 2),
    away_conceded_pg = round(away_conceded / nrow(away_games), 2)
  )
}

# ============================================================
# STEP 13: THE DEEP DIVE FUNCTION
# ============================================================

deep_dive <- function(home_team, away_team) {
  
  cat("\n")
  cat("==============================================================\n")
  cat("  DEEP DIVE:", home_team, "vs", away_team, "\n")
  cat("==============================================================\n\n")
  
  cat("--- POISSON MODEL PREDICTION ---\n\n")
  predict_match(home_team, away_team)
  
  cat("\n\n--- SEASON STATS ---\n\n")
  home_stats <- get_season_stats(home_team)
  away_stats <- get_season_stats(away_team)
  
  if (!is.null(home_stats) & !is.null(away_stats)) {
    cat(sprintf("%-30s %-15s %-15s\n", "", home_team, away_team))
    cat(sprintf("%-30s %-15s %-15s\n", "League", home_stats$league, away_stats$league))
    cat(sprintf("%-30s %-15s %-15s\n", "Total games", home_stats$total_games, away_stats$total_games))
    cat(sprintf("%-30s %-15s %-15s\n", "Goals scored (total)", home_stats$goals_scored, away_stats$goals_scored))
    cat(sprintf("%-30s %-15s %-15s\n", "Goals conceded (total)", home_stats$goals_conceded, away_stats$goals_conceded))
    cat(sprintf("%-30s %-15s %-15s\n", "Goals per game", home_stats$goals_per_game, away_stats$goals_per_game))
    cat(sprintf("%-30s %-15s %-15s\n", "Conceded per game", home_stats$conceded_per_game, away_stats$conceded_per_game))
    cat(sprintf("%-30s %-15s %-15s\n", "Clean sheets", home_stats$clean_sheets, away_stats$clean_sheets))
    cat(sprintf("%-30s %-15s %-15s\n", "Clean sheet %", paste0(home_stats$clean_sheet_pct, "%"), paste0(away_stats$clean_sheet_pct, "%")))
    cat(sprintf("%-30s %-15s %-15s\n", "Home scoring avg", home_stats$home_scored_pg, ""))
    cat(sprintf("%-30s %-15s %-15s\n", "Home conceding avg", home_stats$home_conceded_pg, ""))
    cat(sprintf("%-30s %-15s %-15s\n", "Away scoring avg", "", away_stats$away_scored_pg))
    cat(sprintf("%-30s %-15s %-15s\n", "Away conceding avg", "", away_stats$away_conceded_pg))
  }
  
  cat("\n--- xG, SHOOTING & BIG CHANCES ---\n\n")
  
  tryCatch({
    fbref <- load_fbref_stats()
    home_fb <- fbref %>% filter(Team == home_team)
    away_fb <- fbref %>% filter(Team == away_team)
    
    if (nrow(home_fb) > 0 & nrow(away_fb) > 0) {
      h_gp <- home_fb$GP
      a_gp <- away_fb$GP
      
      cat(sprintf("%-30s %-15s %-15s\n", "", home_team, away_team))
      cat(sprintf("%-30s %-15s %-15s\n", "xG For (per game)", round(home_fb$xG_For / h_gp, 2), round(away_fb$xG_For / a_gp, 2)))
      cat(sprintf("%-30s %-15s %-15s\n", "xG Against (per game)", round(home_fb$xG_Against / h_gp, 2), round(away_fb$xG_Against / a_gp, 2)))
      cat(sprintf("%-30s %-15s %-15s\n", "xG Difference", round((home_fb$xG_For - home_fb$xG_Against) / h_gp, 2), round((away_fb$xG_For - away_fb$xG_Against) / a_gp, 2)))
      
      cat("\n")
      cat(sprintf("%-30s %-15s %-15s\n", "Shots per game", round(home_fb$Shots / h_gp, 1), round(away_fb$Shots / a_gp, 1)))
      cat(sprintf("%-30s %-15s %-15s\n", "Shots on target per game", round(home_fb$SoT / h_gp, 1), round(away_fb$SoT / a_gp, 1)))
      cat(sprintf("%-30s %-15s %-15s\n", "Shot accuracy %", paste0(round(home_fb$SoT / home_fb$Shots * 100, 1), "%"), paste0(round(away_fb$SoT / away_fb$Shots * 100, 1), "%")))
      cat(sprintf("%-30s %-15s %-15s\n", "Shots conceded per game", round(home_fb$Shots_Against / h_gp, 1), round(away_fb$Shots_Against / a_gp, 1)))
      cat(sprintf("%-30s %-15s %-15s\n", "SoT conceded per game", round(home_fb$SoT_Against / h_gp, 1), round(away_fb$SoT_Against / a_gp, 1)))
      
      home_conversion <- round((home_stats$goals_per_game * h_gp) / home_fb$SoT * 100, 1)
      away_conversion <- round((away_stats$goals_per_game * a_gp) / away_fb$SoT * 100, 1)
      cat(sprintf("%-30s %-15s %-15s\n", "Conversion rate (G/SoT)", paste0(home_conversion, "%"), paste0(away_conversion, "%")))
      
      cat("\n")
      cat(sprintf("%-30s %-15s %-15s\n", "Big chances created", home_fb$BigChances, away_fb$BigChances))
      cat(sprintf("%-30s %-15s %-15s\n", "Big chances missed", home_fb$BigChancesMissed, away_fb$BigChancesMissed))
      
      home_bc_conv <- round((home_fb$BigChances - home_fb$BigChancesMissed) / home_fb$BigChances * 100, 1)
      away_bc_conv <- round((away_fb$BigChances - away_fb$BigChancesMissed) / away_fb$BigChances * 100, 1)
      cat(sprintf("%-30s %-15s %-15s\n", "Big chance conversion %", paste0(home_bc_conv, "%"), paste0(away_bc_conv, "%")))
      
      cat("\n")
      cat(sprintf("%-30s %-15s %-15s\n", "Corners per game", round(home_fb$Corners / h_gp, 1), round(away_fb$Corners / a_gp, 1)))
      
      home_actual_gpg <- home_stats$goals_per_game
      home_xg_gpg <- round(home_fb$xG_For / h_gp, 2)
      away_actual_gpg <- away_stats$goals_per_game
      away_xg_gpg <- round(away_fb$xG_For / a_gp, 2)
      
      cat("\n--- PERFORMANCE FLAGS ---\n\n")
      
      if (home_actual_gpg > home_xg_gpg + 0.15) {
        cat("  WARNING:", home_team, "is OVERPERFORMING xG (scoring", home_actual_gpg, "vs xG of", home_xg_gpg, ") - goals may drop\n")
      } else if (home_actual_gpg < home_xg_gpg - 0.15) {
        cat("  NOTE:", home_team, "is UNDERPERFORMING xG (scoring", home_actual_gpg, "vs xG of", home_xg_gpg, ") - goals may increase\n")
      } else {
        cat("  ", home_team, "is scoring in line with xG\n")
      }
      
      if (away_actual_gpg > away_xg_gpg + 0.15) {
        cat("  WARNING:", away_team, "is OVERPERFORMING xG (scoring", away_actual_gpg, "vs xG of", away_xg_gpg, ") - goals may drop\n")
      } else if (away_actual_gpg < away_xg_gpg - 0.15) {
        cat("  NOTE:", away_team, "is UNDERPERFORMING xG (scoring", away_actual_gpg, "vs xG of", away_xg_gpg, ") - goals may increase\n")
      } else {
        cat("  ", away_team, "is scoring in line with xG\n")
      }
      
      if (home_fb$BigChancesMissed > home_fb$BigChances * 0.6) {
        cat("  WARNING:", home_team, "is wasting big chances (missed", home_fb$BigChancesMissed, "of", home_fb$BigChances, ") - could start converting\n")
      }
      if (away_fb$BigChancesMissed > away_fb$BigChances * 0.6) {
        cat("  WARNING:", away_team, "is wasting big chances (missed", away_fb$BigChancesMissed, "of", away_fb$BigChances, ") - could start converting\n")
      }
      
      if (home_conversion > 35) {
        cat("  WARNING:", home_team, "has HIGH conversion rate (", home_conversion, "%) - may regress\n")
      } else if (home_conversion < 20) {
        cat("  NOTE:", home_team, "has LOW conversion rate (", home_conversion, "%) - may improve\n")
      }
      if (away_conversion > 35) {
        cat("  WARNING:", away_team, "has HIGH conversion rate (", away_conversion, "%) - may regress\n")
      } else if (away_conversion < 20) {
        cat("  NOTE:", away_team, "has LOW conversion rate (", away_conversion, "%) - may improve\n")
      }
      
    } else {
      if (nrow(home_fb) == 0) cat("  No stats found for", home_team, "\n")
      if (nrow(away_fb) == 0) cat("  No stats found for", away_team, "\n")
      cat("  Update FBref_Stats.xlsx with these teams to see full stats\n")
    }
  }, error = function(e) {
    cat("  FBref_Stats.xlsx not found or could not be read.\n")
    cat("  Create it to see xG, shooting, and big chances data here.\n")
  })
  
  cat("\n--- RECENT FORM (Last 5 Games) ---\n\n")
  home_form <- get_recent_form(home_team, 5)
  away_form <- get_recent_form(away_team, 5)
  
  if (!is.null(home_form)) {
    cat(home_team, ":\n")
    for (i in 1:nrow(home_form)) {
      r <- home_form[i, ]
      cat(sprintf("  %s %s %s %d-%d %s (%s)\n", r$Result, r$Venue, r$Opponent, r$GF, r$GA, format(r$Date, "%b %d"), r$Venue))
    }
    form_gpg <- round(sum(home_form$GF) / nrow(home_form), 2)
    form_gapg <- round(sum(home_form$GA) / nrow(home_form), 2)
    wins <- sum(home_form$Result == "W"); draws <- sum(home_form$Result == "D"); losses <- sum(home_form$Result == "L")
    cat(sprintf("  Form: %dW %dD %dL | Scoring: %.1f/game | Conceding: %.1f/game\n", wins, draws, losses, form_gpg, form_gapg))
    if (form_gpg > home_stats$goals_per_game + 0.3) { cat("  FORM: HOT (scoring above season average)\n")
    } else if (form_gpg < home_stats$goals_per_game - 0.3) { cat("  FORM: COLD (scoring below season average)\n")
    } else { cat("  FORM: Average (in line with season)\n") }
  }
  
  cat("\n")
  
  if (!is.null(away_form)) {
    cat(away_team, ":\n")
    for (i in 1:nrow(away_form)) {
      r <- away_form[i, ]
      cat(sprintf("  %s %s %s %d-%d %s (%s)\n", r$Result, r$Venue, r$Opponent, r$GF, r$GA, format(r$Date, "%b %d"), r$Venue))
    }
    form_gpg <- round(sum(away_form$GF) / nrow(away_form), 2)
    form_gapg <- round(sum(away_form$GA) / nrow(away_form), 2)
    wins <- sum(away_form$Result == "W"); draws <- sum(away_form$Result == "D"); losses <- sum(away_form$Result == "L")
    cat(sprintf("  Form: %dW %dD %dL | Scoring: %.1f/game | Conceding: %.1f/game\n", wins, draws, losses, form_gpg, form_gapg))
    if (form_gpg > away_stats$goals_per_game + 0.3) { cat("  FORM: HOT (scoring above season average)\n")
    } else if (form_gpg < away_stats$goals_per_game - 0.3) { cat("  FORM: COLD (scoring below season average)\n")
    } else { cat("  FORM: Average (in line with season)\n") }
  }
  
  cat("\n--- HEAD TO HEAD ---\n\n")
  h2h <- get_h2h(home_team, away_team, 5)
  
  if (!is.null(h2h) && nrow(h2h) > 0) {
    for (i in 1:nrow(h2h)) {
      r <- h2h[i, ]
      cat(sprintf("  %s | %s %d-%d %s\n", format(r$Date, "%Y-%m-%d"), r$Home, r$HomeGoals, r$AwayGoals, r$Away))
    }
    home_wins <- sum((h2h$Home == home_team & h2h$HomeGoals > h2h$AwayGoals) | (h2h$Away == home_team & h2h$AwayGoals > h2h$HomeGoals))
    away_wins <- sum((h2h$Home == away_team & h2h$HomeGoals > h2h$AwayGoals) | (h2h$Away == away_team & h2h$AwayGoals > h2h$HomeGoals))
    h2h_draws <- nrow(h2h) - home_wins - away_wins
    cat(sprintf("\n  H2H Record: %s %dW %dD %dL %s\n", home_team, home_wins, h2h_draws, away_wins, away_team))
    avg_goals <- round(mean(h2h$HomeGoals + h2h$AwayGoals), 1)
    cat(sprintf("  Avg total goals in H2H: %.1f\n", avg_goals))
    btts_count <- sum(h2h$HomeGoals > 0 & h2h$AwayGoals > 0)
    cat(sprintf("  BTTS in H2H: %d out of %d (%.0f%%)\n", btts_count, nrow(h2h), btts_count/nrow(h2h)*100))
  } else {
    cat("  No head to head matches found in the data\n")
  }
  
  cat("\n--- QUICK CHECKLIST ---\n\n")
  cat("  [ ] Does the xG data support the model prediction?\n")
  cat("  [ ] Are both teams in good recent form?\n")
  cat("  [ ] Any key injuries or suspensions? (check manually)\n")
  cat("  [ ] Does H2H history support the bet?\n")
  cat("  [ ] DECISION: BET / SKIP\n")
  cat("\n==============================================================\n\n")
}

# ============================================================
# STEP 14: UPDATE DATA FROM RAW FBREF PASTE
# ============================================================

update_data <- function(raw_file = "Raw_Data.xlsx",
                        clean_file = "All Leagues Clean.xlsx",
                        current_season_start = "2025-07-01") {
  leagues <- c("Premier League", "La Liga", "Bundesliga", "Serie A", "Ligue 1")
  cat("=== UPDATING DATA ===\n\n")
  all_clean <- list()
  for (league in leagues) {
    tryCatch({ data <- read_excel(clean_file, sheet = league); all_clean[[league]] <- data },
             error = function(e) { cat("Could not read", league, "from clean file\n") })
  }
  cl_data <- NULL
  tryCatch({ cl_data <- read_excel(clean_file, sheet = "Champions League"); cat("Champions League data found (not updated from raw, kept as is)\n\n") },
           error = function(e) { cat("No Champions League sheet found\n\n") })
  updated_leagues <- list()
  for (league in leagues) {
    cat("Processing", league, "...\n")
    raw <- tryCatch({ read_excel(raw_file, sheet = league, skip = 2) },
                    error = function(e) { cat("  No raw data found for", league, "- keeping existing data\n"); return(NULL) })
    if (is.null(raw) || nrow(raw) == 0) {
      updated_leagues[[league]] <- all_clean[[league]]
      cat("  No new data, kept existing:", nrow(all_clean[[league]]), "games\n"); next
    }
    raw_clean <- raw %>%
      select(Date, Home, Score, Away) %>%
      filter(!is.na(Score)) %>% filter(Score != "Score") %>% filter(Home != "Home") %>%
      mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
      separate(Score, into = c("HomeGoals", "AwayGoals"), sep = "\u2013") %>%
      mutate(HomeGoals = as.numeric(HomeGoals), AwayGoals = as.numeric(AwayGoals)) %>%
      filter(!is.na(HomeGoals), !is.na(AwayGoals))
    cat("  Cleaned raw data:", nrow(raw_clean), "new games\n")
    historical <- all_clean[[league]] %>% filter(Date < as.Date(current_season_start))
    cat("  Historical data:", nrow(historical), "games\n")
    combined <- bind_rows(historical, raw_clean) %>% arrange(Date)
    updated_leagues[[league]] <- combined
    cat("  Total after update:", nrow(combined), "games\n\n")
  }
  sheets_to_save <- updated_leagues
  if (!is.null(cl_data)) { sheets_to_save[["Champions League"]] <- cl_data }
  write_xlsx(sheets_to_save, clean_file)
  cat("Saved updated data to", clean_file, "\n\n")
  cat("Reloading models...\n\n")
  for (league in leagues) {
    data <- updated_leagues[[league]]
    assign_name <- switch(league, "Premier League" = "pl_clean", "La Liga" = "la_liga_clean",
                          "Bundesliga" = "bundesliga_clean", "Serie A" = "serie_a_clean", "Ligue 1" = "ligue_1_clean")
    assign(assign_name, data, envir = .GlobalEnv)
  }
  if (!is.null(cl_data)) { assign("cl_clean", cl_data, envir = .GlobalEnv) }
  new_all_leagues <- updated_leagues
  if (!is.null(cl_data)) { new_all_leagues[["Champions League"]] <- cl_data }
  assign("all_leagues", new_all_leagues, envir = .GlobalEnv)
  new_models <- list()
  for (league_name in names(new_all_leagues)) {
    data <- new_all_leagues[[league_name]]
    data <- data %>%
      mutate(
        game_date = as.Date(Date),
        weight = case_when(
          game_date >= as.Date(current_season_start) ~ 1.0,
          game_date >= as.Date(current_season_start) - 365 ~ 0.6,
          TRUE ~ 0.3
        )
      )
    league_avg <- data %>%
      summarize(
        HomeGoalsAvg = weighted.mean(HomeGoals, weight),
        AwayGoalsAvg = weighted.mean(AwayGoals, weight)
      )
    home_stats <- data %>% group_by(Home) %>%
      summarize(
        HomeGamesPlayed = n(),
        HomeGoalsScored = weighted.mean(HomeGoals, weight),
        HomeGoalsConceded = weighted.mean(AwayGoals, weight)
      ) %>%
      mutate(
        HomeAttack = HomeGoalsScored / league_avg$HomeGoalsAvg,
        HomeDefense = HomeGoalsConceded / league_avg$AwayGoalsAvg
      )
    away_stats <- data %>% group_by(Away) %>%
      summarize(
        AwayGamesPlayed = n(),
        AwayGoalsScored = weighted.mean(AwayGoals, weight),
        AwayGoalsConceded = weighted.mean(HomeGoals, weight)
      ) %>%
      mutate(
        AwayAttack = AwayGoalsScored / league_avg$AwayGoalsAvg,
        AwayDefense = AwayGoalsConceded / league_avg$HomeGoalsAvg
      )
    team_stats <- home_stats %>% rename(Team = Home) %>%
      inner_join(away_stats %>% rename(Team = Away), by = "Team")
    new_models[[league_name]] <- list(league_avg = league_avg, team_stats = team_stats)
    cat(league_name, ":", nrow(data), "games,", nrow(team_stats), "teams\n")
  }
  assign("models", new_models, envir = .GlobalEnv)
  cat("\n=== UPDATE COMPLETE ===\n")
  cat("Models rebuilt with fresh data. Ready to run process_all_matches() or deep_dive()\n")
}

# ============================================================
# HOW TO USE
# ============================================================
#
# 1. Source this script: it loads data, builds all models
#
# 2. Quick prediction:
#    predict_match("Arsenal", "Chelsea")
#    predict_match("Liverpool", "Bayern Munich")
#
# 3. See all valid team names:
#    list_teams()
#    list_teams("Premier League")
#
# 4. Full value bet workflow:
#    a. Open odds_template.xlsx, enter matches and odds
#    b. Run: process_all_matches()
#    c. Check value_results.xlsx
#
# 5. Deep dive on specific matches:
#    deep_dive("Arsenal", "Chelsea")
#    (fill FBref_Stats.xlsx for xG/shooting/big chances data)
#
# 6. Update match data weekly:
#    a. Paste raw FBref data into Raw_Data.xlsx
#    b. Run: update_data()
#
# 7. Ratings guide:
#    A = Edge 5%+ AND Model 60%+ (best bets)
#    B = Edge 5%+ AND Model 40%+ (good value, less certain)
#    C = Edge 0%+ AND Model 60%+ (likely but small edge)
#    D = Edge 0%+ (risky)
