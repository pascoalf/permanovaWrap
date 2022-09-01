source("./full_permanova_summary.R")
library(vegan)
library(dplyr)
# Load data from vegan package
data(dune)
data("dune.env")
# Standard example that is equivalent to examples from help(adonis2)
full_permanova_summary(y = dune, 
                       env = dune.env,
                       a = "Management", 
                       b = "A1",
                       alpha = 0.01,
                       model = "y ~ Management*A1")
