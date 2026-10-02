# TOC:
# 1. Introduction
# 2. Dataset Overview
# 2.1 Source Files
# 2.2 Entity Relationship Model
# 2.3 Tables & Columns
# 3. Data Quality Assessment
# 3.1 Disqualification Anomalies
# 3.2 Shared Race Wins
# 3.3 Fatal Accident Classification
# 4. Dataset Transformation
# 4.1 Column Pruning
# 4.2 Table Joins
# 4.3 Feature Engineering (age, names, etc.)
# 4.4 Final Flat Dataset
# 5. Exploratory Data Analysis
# 5.1 Driver Performance Metrics
# 5.2 Era Comparisons
# 5.3 Constructor Dominance
# 6. Conclusion

# Load the libraries required for data manipulation, database management,
# data visualization, plot arrangement, colour palettes, and formatted tables.

library(cowplot)
library(dplyr)
library(tidyr)
library(dm)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(kableExtra)
library(DBI)
library(RSQLite)
