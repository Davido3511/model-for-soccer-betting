# Multi-League Soccer Match Prediction & Value Detection Model

## Overview

This project builds a probabilistic soccer forecasting system using Poisson distributions and weighted historical match data. The model predicts match outcomes across six major European leagues and identifies potential value betting opportunities by comparing model probabilities with bookmaker implied probabilities.

The system simulates score distributions for each match and evaluates nine betting markets including win probabilities, over/under goals, and both teams to score.

## Competitions Modeled

- English Premier League
- La Liga
- Serie A
- Bundesliga
- Ligue 1
- UEFA Champions League

## Model Methodology

The model estimates expected goals using team attack and defense strengths derived from historical match results.

Expected Goals Formula:

Expected Goals = League Average × Team Attack Strength × Opponent Defense Strength

Match scorelines are simulated from 0–0 to 6–6 using Poisson distributions. Probabilities for different betting markets are then calculated by summing the relevant scoreline probabilities.

## Value Bet Identification

A value opportunity is identified when the model’s probability exceeds the bookmaker's implied probability.

Edge = Model Probability − Bookmaker Implied Probability

Positive edges are flagged and rated using a tier system (A–D).

## Key Features

- Multi-league modeling across six competitions
- Bayesian-weighted historical match data
- Poisson scoreline simulation
- Automated value detection against bookmaker odds
- Deep-dive statistical analysis including expected goals and recent form

## Technologies Used

- R
- tidyverse
- readxl
- Poisson probability modeling

## Project Structure
