# Bayesian Hierarchical Modelling for Projecting MLB Batter's Future Exit Velocities

## Overview
- **Goal**: Estimate MLB batter's mean exit velocity for future seasons. 
- **Approach**: Use Bayesian hierarchical modelling to create predictive distributions for a batter's future exit velocity. From those distributions, estimate the batter's mean exit velocity for the following season.
- Several key advantages:
  1. **Better predictions**: Parameter shrinkage from hierarchical modelling greatly improves predictions.
  2. **Better player understanding**: Predictive distributions give a holistic perspective to a player's batting style.
  3. **Uncertainty quantification**: Added uncertainty in player's future performance gives even more info for decision making.
  4. **Able to simulate new players**: International or minor league players' future performance can still be projected.

## Data
- Used the [baseballr](https://billpetti.github.io/baseballr/) package for batted ball info and the [Sean Lahman Baseball Database](https://cran.r-project.org/web/packages/Lahman/index.html) for player height and weight info.
- Collected batted ball info for 394 players across the 2024, 2025 MLB seasons.
- Over 250,000 batted ball observations.
- Train model on 2024 season, evaluate on 2025 season.

## Exploratory Data Analysis

## Modelling Process

## Results

## Conclusions

## Future Work
