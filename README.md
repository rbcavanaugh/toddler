# toddler 🧸

> *"Why is all this data so clean? Let me help!"* - Every toddler, probably

<!-- badges: start -->
 [![R-CMD-check](https://github.com/rbcavanaugh/toddler/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rbcavanaugh/toddler/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
 
## Overview

The `toddler` package contains a collection of functions that systematically mess up clean data in all the wonderful ways that real-world data comes pre-messed-up. Think of it as Murphy's Law in package form, or a toddler with access to your pristine dataset.

Perfect for:
- 🎓 **Teaching data cleaning** - Give students realistic messy data to wrangle
- 🧪 **Testing data pipelines** - See how robust your code really is
- 🎭 **Simulating real-world chaos** - Because actual data never comes clean
- 😈 **Having fun** - Sometimes you just want to watch the world burn (in a statistical sense)

## Installation

You can install the development version of toddler from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rbcavanaugh/toddler")
```
## The Chaos Toolkit

### 🏗️ Structural Vandalism
- `toddler_wide()` - Spreads your tidy data wide like toys across a living room
- `toddler_unjoin()` - Splits data into pieces because why keep things together?
- `toddler_extra()` - Adds random empty rows (because R doesn't like Excel-style headers in data frames)

### 🎨 Cosmetic Chaos  
- `toddler_names()` - Messes Up Column Names Like This Or LIKE THIS or like this (handles snake_case, camelCase, dot.notation)
- `toddler_whitespace()` - Adds   random   spaces   everywhere
- `toddler_inconsistent()` - Makes "Yes" also "YES", "yes", "Y", and "True"

### 🔢 Data Type Anarchy
- `toddler_types()` - Mixes text into numeric columns ("N/A", "missing", ".")
- `toddler_units()` - Adds random units: 25 becomes "25kg" or "25sec"
- `toddler_missing()` - Strategically places NAs like hidden landmines

### 📚 Quantity Mayhem
- `toddler_duplicate()` - Creates duplicate rows because more is always better

## Quick Start

```r
library(toddler)

# Start with some pristine data (various naming conventions)
clean_data <- data.frame(
  participant_id = 1:10,        # snake_case
  ageGroup = rep(c("young", "old"), 5),  # camelCase  
  test.score = runif(10, 50, 100),       # dot.notation
  completed = rep(c("yes", "no"), 5)
)

# Apply the full toddler treatment
messy_data <- clean_data |>
  toddler_missing(prop = 0.1) |>                                    # Add some NAs
  toddler_missing(add_missing = FALSE, modify_missing = TRUE) |>   # Make NAs messy  
  toddler_inconsistent(cols = "completed", prop = 0.2) |>
  toddler_whitespace(cols = "ageGroup") |>
  toddler_types(cols = "test.score", prop = 0.05) |>
  toddler_duplicate(prop = 0.2) |>
  toddler_names(style = "mixed") |>  # Could become "Participant Id", "AGEGROUP", "test score", etc.
  toddler_extra(add_random = TRUE, add_end = 5)                   # Scattered + 5 at end

# Congratulations! Your data now looks like it came from the real world
```

## Advanced Chaos Scenarios

### The "Missing Data Reality Check"
```r
# The toddler_missing() function is your Swiss Army knife of missing data chaos
clean_data <- data.frame(x = 1:10, y = letters[1:10], z = runif(10))

# Just add NAs (classic behavior)
with_nas <- toddler_missing(clean_data, prop = 0.2)

# Just mess up existing NAs  
data_with_nas <- data.frame(x = c(1, NA, 3), y = c("a", NA, "c"))
messy_nas <- toddler_missing(data_with_nas, add_missing = FALSE, modify_missing = TRUE)
# Result: x becomes c("1", "N/A", "3"), y becomes c("a", "missing", "c")

# Do both: add NAs then mess them up
double_trouble <- toddler_missing(clean_data, prop = 0.1, modify_missing = TRUE)

# Keep numeric columns clean with -999 (for analysis)
analyst_friendly <- toddler_missing(data_with_nas, add_missing = FALSE, 
                                   modify_missing = TRUE, extra_tricky = TRUE)
# Result: x becomes c(1, -999, 3) [stays numeric!]
```

### The "Excel Export" Special
```r
# Simulate what happens when someone exports from Excel (minus the headers)
excel_nightmare <- clean_data |>
  toddler_extra(add_end = TRUE) |>                    # 3-30 empty rows at end
  toddler_names(style = "title") |>
  toddler_whitespace(prop = 0.3) |>
  toddler_types(prop = 0.1)
```

### The "Survey Data" Classic
```r
# Typical survey response chaos
survey_mess <- clean_data |>
  toddler_inconsistent(cols = "completed", prop = 0.3) |>
  toddler_missing(cols = c("ageGroup", "test.score"), prop = c(0.05, 0.15)) |>
  toddler_missing(add_missing = FALSE, modify_missing = TRUE, extra_tricky = TRUE) |>  # -999 in numeric
  toddler_whitespace(cols = "ageGroup", prop = 0.2)
```

### The "Multiple Systems" Merger
```r
# When data comes from different systems with different naming conventions
# Start with mixed good formats, then mess them up
mixed_data <- data.frame(
  participant_id = 1:10,      # snake_case
  firstName = letters[1:10],  # camelCase
  test.score = runif(10)      # dot.notation
)

messy_data <- mixed_data |>
  toddler_names(style = "title") |>  # "Participant Id", "First Name", "Test Score"
  toddler_missing(prop = 0.1, modify_missing = TRUE, replacement = "UNKNOWN") |>  # Add NAs then make them "UNKNOWN"
  toddler_unjoin(by_rows = "Participant Id", prop = 0.7)  # Note: name changed!

# Now you have two datasets with consistent (but terrible) naming!
```

### The "Real-World Missing Data Nightmare"
```r
# Simulate the chaos of missing data from multiple sources
missing_chaos <- clean_data |>
  toddler_missing(prop = 0.15) |>                                          # Add some NAs
  toddler_missing(add_missing = FALSE, modify_missing = TRUE, 
                 replacement = c("", "NULL", "n/a", "#N/A", "999")) |>     # Replace with various messy values
  toddler_missing(cols = "test.score", add_missing = FALSE, 
                 modify_missing = TRUE, extra_tricky = TRUE)               # Keep numeric columns numeric with -999
```



## Contributing

Found a new way that data can be wonderfully broken? Contributions are welcome! 

