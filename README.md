# toddler 🧸

> *"Why is all this data so clean? Let me help!"* - Every toddler, probably

[![R-CMD-check](https://github.com/yourusername/toddler/workflows/R-CMD-check/badge.svg)](https://github.com/yourusername/toddler/actions)

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
devtools::install_github("yourusername/toddler")
```

## The Chaos Toolkit

### 🏗️ Structural Vandalism
- `toddler_wide()` - Spreads your tidy data wide like toys across a living room
- `toddler_unjoin()` - Splits data into pieces because why keep things together?
- `toddler_extra()` - Adds bonus rows nobody asked for

### 🎨 Cosmetic Chaos  
- `toddler_names()` - Mixes Up Column Names Like This Or LIKE THIS or like this
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

# Start with some pristine data
clean_data <- data.frame(
  participant_id = 1:10,
  age_group = rep(c("young", "old"), 5),
  test_score = runif(10, 50, 100),
  completed = rep(c("yes", "no"), 5)
)

# Apply the full toddler treatment
messy_data <- clean_data |>
  toddler_missing(prop = 0.1) |>
  toddler_inconsistent(cols = "completed") |>
  toddler_whitespace(cols = "age_group") |>
  toddler_types(cols = "test_score", prop = 0.05) |>
  toddler_duplicate(prop = 0.2) |>
  toddler_names(style = "mixed") |>
  toddler_extra()

# Congratulations! Your data now looks like it came from the real world
```

## Advanced Chaos Scenarios

### The "Excel Export" Special
```r
# Simulate what happens when someone exports from Excel
excel_nightmare <- clean_data |>
  toddler_extra(add_header = TRUE, add_totals = TRUE) |>
  toddler_names(style = "title") |>
  toddler_whitespace(prop = 0.3) |>
  toddler_types(prop = 0.1)
```

### The "Survey Data" Classic
```r
# Typical survey response chaos
survey_mess <- clean_data |>
  toddler_inconsistent(cols = "completed") |>
  toddler_missing(cols = c("age_group", "test_score"), prop = c(0.05, 0.15)) |>
  toddler_whitespace(cols = "age_group", prop = 0.2)
```

### The "Multiple Systems" Merger
```r
# When data comes from different systems
split_data <- clean_data |>
  toddler_names(style = "mixed") |>
  toddler_unjoin(by_rows = "participant_id", prop = 0.7)

# Now you have two datasets with slightly different problems!
```

## Educational Philosophy

This package embraces the educational principle that students learn data cleaning best when they encounter realistic problems. Rather than working with artificially clean datasets, `toddler` helps create the beautiful chaos that mirrors real-world data challenges.

Each function is designed to introduce specific, common data quality issues:
- **Encoding problems** (mixed case, whitespace)
- **Structural issues** (wide format, split tables)  
- **Type confusion** (mixed data types)
- **Completeness problems** (missing values, duplicates)
- **Consistency issues** (multiple representations of the same value)

## Contributing

Found a new way that data can be wonderfully broken? Contributions are welcome! Please read our contributing guidelines and remember: the goal is educational chaos, not malicious destruction.

## License

MIT License - Because even chaos should be free and open source.

## Acknowledgments

- Inspired by every real dataset that ever existed
- Dedicated to all the data scientists who have muttered "Why is it like this?" 
- Special thanks to toddlers everywhere for showing us that creativity knows no bounds

---
