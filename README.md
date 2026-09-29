# 🏎️ Formula 1 Data Analysis

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Language](https://img.shields.io/badge/language-R-red.svg)](https://www.r-project.org/)
[![Status](https://img.shields.io/badge/status-work%20in%20progress-orange.svg)]()

## 📋 Project Overview

This repository contains an ongoing exploratory data analysis (EDA) of Formula 1 World Championship data from 1950 to present, with the ultimate goal of identifying the greatest driver of all time (GOAT) using statistical methods and machine learning.

### Research Question
> *"Who is or was the best Formula 1 driver of all time?"*

This project takes a mathematical approach to evaluate driver performance across different eras, accounting for evolving rules, car technology, and competitive landscapes.

---

## 📊 Dataset

**⚠️ Note: The current dataset contains known issues and is actively being fixed.** Please refer to the companion ETL project for the latest version.

### Data Source

The raw data originates from [Jolpica](https://jolpica.com/f1/), a community-maintained F1 database. To ensure data quality and reproducibility, I've created a dedicated ETL pipeline in a companion repository:

- **ETL Pipeline:** [jmr-lab/f1-etl-pipeline](https://github.com/jmr-lab/f1-etl-pipeline)
- **Current Status:** Data transformation and validation in progress

| File | Description | Expected Records |
|------|-------------|------------------|
| `circuits.csv` | Circuit information | ~78 circuits |
| `drivers.csv` | Driver demographics | 864+ drivers |
| `races.csv` | Race events | 1,149+ races |
| `results.csv` | Race results per driver | Full race history |
| `driver_standings.csv` | Championship standings | Season-by-season |
| `constructors.csv` | Team information | 211 constructors |
| `constructor_standings.csv` | Team standings | Full history |
| `status.csv` | Finishing status codes | Status categories |

**Key Statistics (as of 2026):**
- 76 seasons analyzed
- 1,149 total races
- 864 drivers participated
- 115 race winners
- 35 world champions

---

## 🔍 Preliminary Findings

### Driver Performance Leaders

| Driver | Titles | Wins | Races | Seasons | Win/Race Ratio |
|--------|--------|------|-------|---------|----------------|
| Lewis Hamilton | 7 | 105 | 380 | 19 | 0.276 |
| Michael Schumacher | 7 | 91 | 308 | 19 | 0.295 |
| Juan Manuel Fangio | 5 | 24 | 58 | 8 | **0.414** |
| Max Verstappen | 4 | 71 | 233 | 11 | 0.305 |
| Sebastian Vettel | 4 | 53 | 300 | 16 | 0.177 |
| **Alain Prost** | **4** | **51** | **202** | **13** | **0.252** |
| Ayrton Senna | 3 | 41 | 162 | 11 | 0.253 |
| Jackie Stewart | 3 | 27 | 100 | 9 | 0.270 |
| Niki Lauda | 3 | 25 | 174 | 13 | 0.144 |

### Key Observations

**📈 Era Comparisons**
- **Early Era (1950-1968)**: Fewer races per season, shorter careers, higher risk
- **Middle Era (1969-1993)**: Transitional period with rule changes
- **Modern Era (1994-present)**: More races, longer careers, advanced technology

**🎯 Efficiency Metrics**
- Fangio dominates in efficiency: 5 titles in just 8 seasons (62.5% title rate)
- Modern drivers accumulate more points due to expanded scoring systems
- Some champions won with surprisingly low win ratios (e.g., Keke Rosberg: 1/15 wins in 1982)

**⚙️ Scoring Evolution**
- 1950s: 8 points for winner
- 2010+: 25 points for winner
- Only best results counted until 1990, creating championship anomalies (e.g., Senna 1988 vs Prost)

**👥 Constructors**
| Team | Titles | Wins | First Entry |
|------|--------|------|-------------|
| Ferrari | 22 | 249 | 1950 |
| McLaren | 11 | 199 | 1968 |
| Mercedes | 9 | 131 | 1954 |
| Williams | 9 | 114 | 1975 |
| Red Bull | 6 | 130 | 2005 |

---

## 🛠️ Methods & Pipeline
