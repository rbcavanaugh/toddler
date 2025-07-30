# Toddler Package: Base R functions to mess up clean data for teaching
# All functions work with pipe |> and have minimal dependencies

#' Helper function to split camelCase into words (internal)
#' @param x A character string that might be camelCase
#' @return A string with spaces between words
#' @keywords internal
.split_camel_case <- function(x) {
    # Insert space before uppercase letters (except at start)
    # This handles camelCase like "firstName" -> "first Name"
    result <- gsub("([a-z])([A-Z])", "\\1 \\2", x)
    # Also handle acronyms like "XMLHttpRequest" -> "XML Http Request"
    result <- gsub("([A-Z])([A-Z][a-z])", "\\1 \\2", result)
    return(result)
}

#' Helper function for title case (internal)
#' @param x A character string
#' @return A title case string
#' @keywords internal
.to_title_case <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
}

#' Make data wider (untidy) like a toddler spreading toys everywhere
#'
#' Takes your beautifully tidy data and spreads it wide like a toddler with
#' a bag of toys. Perfect for teaching students why we love tidy data.
#'
#' @param data A data frame (preferably a nice, tidy one)
#' @param names_from Column name (character) to get names from
#' @param values_from Column name (character) to get values from
#' @param id_cols Character vector of ID columns (optional). If NULL, uses all other columns
#'
#' @return A wider, messier data frame that would make Hadley Wickham weep
#' @export
#'
#' @examples
#' # Create some tidy data
#' tidy_data <- data.frame(
#'   id = rep(1:3, each = 2),
#'   measure = rep(c("height", "weight"), 3),
#'   value = c(170, 70, 165, 65, 180, 80)
#' )
#'
#' # Now make it wide and unwieldy
#' toddler_wide(tidy_data, "measure", "value", "id")
toddler_wide <- function(data, names_from, values_from, id_cols = NULL) {

    # If no id_cols specified, use all other columns
    if (is.null(id_cols)) {
        id_cols <- setdiff(names(data), c(names_from, values_from))
    }

    # Use reshape function
    reshape(data,
            direction = "wide",
            idvar = id_cols,
            timevar = names_from,
            v.names = values_from,
            sep = "_")
}

#' Create and replace missing values with messy representations
#'
#' Takes clean data and systematically ruins it for educational purposes by first
#' adding missing values, then replacing all NA values with the chaotic mess
#' of missing value representations found in real-world data, like a toddler
#' who thinks there are many ways to say "I don't know."
#'
#' @param data A data frame to mess up with missing values
#' @param cols Column names (character vector) or column indices to target.
#'   Default is all columns because missing data chaos should be democratic.
#' @param add_missing Logical. If TRUE, adds missing values to data (step A).
#'   If FALSE, skips adding new missing values. Default TRUE.
#' @param modify_missing Logical. If TRUE, converts existing NA values to messy
#'   representations (step B). If FALSE, leaves existing NAs as proper NAs. Default FALSE.
#' @param prop Numeric vector of proportions (0-1) of missing data to add to each column.
#'   Can be single value (applied to all columns) or vector matching length of cols.
#'   Set to 0 to skip adding missing data and only mess up existing NAs. Default 0.05 (5%).
#' @param replacement Character vector of messy missing values to use, or a single
#'   specific value. Default uses common messy representations.
#' @param random Logical. If TRUE, randomly selects from replacement values.
#'   If FALSE, uses the first replacement value for all NAs. Default TRUE.
#' @param extra_tricky Logical. If TRUE, replaces NAs in numeric columns with
#'   -999 and keeps columns numeric, regardless of other settings. Default FALSE.
#' @param seed Random seed for reproducible mess
#'
#' @return A data frame where missing values have been added and all NAs replaced
#'   with messy alternatives. Warns if fewer than 3 total missing values exist.
#' @export
#'
#' @examples
#' # Add missing data with default 5% proportion
#' df <- data.frame(x = 1:10, y = letters[1:10])
#' toddler_missing(df)
#'
#' # Add missing data and mess it up (both steps)
#' toddler_missing(df, add_missing = TRUE, modify_missing = TRUE, prop = 0.2)
#'
#' # Only mess up existing NAs without adding new ones
#' df_with_nas <- data.frame(x = c(1, 2, NA, 4), y = c("a", NA, "c", NA))
#' toddler_missing(df_with_nas, add_missing = FALSE, modify_missing = TRUE)
#'
#' # Only add missing data, keep as proper NAs
#' toddler_missing(df, add_missing = TRUE, modify_missing = FALSE, prop = 0.2)
#'
#' # Add different proportions per column and mess up
#' toddler_missing(df, add_missing = TRUE, prop = c(0.1, 0.3))
#'
#' # Keep numeric columns as numeric with -999
#' toddler_missing(df_with_nas, extra_tricky = TRUE)

toddler_missing <- function(data, cols = names(data), add_missing = TRUE,
                            modify_missing = FALSE, prop = 0.05,
                            replacement = c("n/a", "na", "NA", "N/A", "-999", "999", "missing"),
                            random = TRUE, extra_tricky = FALSE, seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

    # Early return for empty data frames
    if (nrow(data) == 0 || ncol(data) == 0) {
        return(data)
    }

    # Handle different types of column specification
    if (is.numeric(cols)) {
        selected_cols <- names(data)[cols]
    } else if (is.character(cols)) {
        selected_cols <- cols
    } else {
        selected_cols <- names(data)
    }

    # Ensure columns exist
    selected_cols <- intersect(selected_cols, names(data))

    # Early return if no valid columns selected
    if (length(selected_cols) == 0) {
        if (modify_missing) {
            warning("No valid columns selected for modification.")
        }
        return(data)
    }

    # Validate prop values
    if (any(prop < 0) || any(prop > 1)) {
        stop("All prop values must be between 0 and 1 (0% to 100%)")
    }

    # Handle proportion specification
    if (length(prop) == 1) {
        prop <- rep(prop, length(selected_cols))
    } else if (length(prop) != length(selected_cols)) {
        stop("Length of prop must be 1 or match number of selected columns")
    }

    result <- data

    # Step (a): Add missing data based on proportions
    if (add_missing) {
        for (i in seq_along(selected_cols)) {
            col_name <- selected_cols[i]
            if (prop[i] > 0) {
                n_na <- round(nrow(data) * prop[i])
                # Only add NAs to non-missing values
                non_na_positions <- which(!is.na(result[[col_name]]))
                if (length(non_na_positions) >= n_na) {
                    na_positions <- sample(non_na_positions, n_na)
                    result[[col_name]][na_positions] <- NA
                }
            }
        }
    }

    # Check for sufficient missing data after step (a) - only if we have data to check
    if (modify_missing && length(selected_cols) > 0) {
        # Safely calculate total NAs using vapply for type safety
        total_nas <- sum(vapply(result[selected_cols], function(x) sum(is.na(x)), integer(1)))

        if (total_nas == 0) {
            warning("No missing values found in selected columns. Nothing to modify.")
            return(result)
        } else if (total_nas < 3) {
            warning("Less than 3 missing values found across selected columns. Consider increasing prop values or adding missing data first.")
        }
    }

    # Step (b): Replace all existing NAs with messy representations
    if (modify_missing) {
        for (col_name in selected_cols) {
            if (col_name %in% names(result)) {
                col_data <- result[[col_name]]
                na_positions <- which(is.na(col_data))

                if (length(na_positions) > 0) {
                    # Handle extra_tricky for numeric columns
                    if (extra_tricky && is.numeric(col_data)) {
                        col_data[na_positions] <- -999
                        result[[col_name]] <- col_data
                    } else {
                        # Convert to character for messy missing values
                        col_data <- as.character(col_data)

                        if (random && length(replacement) > 1) {
                            # Randomly sample replacement values for each NA
                            messy_values <- sample(replacement, length(na_positions), replace = TRUE)
                        } else {
                            # Use first replacement value for all NAs, handle empty replacement
                            if (length(replacement) > 0) {
                                messy_values <- rep(replacement[1], length(na_positions))
                            } else {
                                messy_values <- rep("", length(na_positions))
                            }
                        }

                        col_data[na_positions] <- messy_values
                        result[[col_name]] <- col_data
                    }
                }
            }
        }
    }

    result
}

#' Mess up column names like a toddler with a label maker
#'
#' Takes your sensible naming conventions (snake_case, camelCase, etc.) and turns
#' them into a typographic disaster that looks like a toddler got hold of the Shift key.
#'
#' @param data A data frame with perfectly reasonable column names
#' @param style Type of messiness: "mixed" (random chaos), "title" (Title Case With Spaces),
#'   "upper" (SHOUTY), or "lower" (whisper quiet)
#' @param seed Random seed for reproducible mess
#'
#' @return A data frame where column names have lost all sense of consistency
#' @export
#'
#' @examples
#' df <- data.frame(first_name = "Alice", lastName = "Smith", age_years = 25)
#' toddler_names(df, style = "mixed")  # Maximum chaos
#' toddler_names(df, style = "title")  # "First Name", "Last Name", "Age Years"
toddler_names <- function(data, style = "mixed", seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

    old_names <- names(data)
    new_names <- old_names

    for (i in seq_along(new_names)) {
        name <- new_names[i]

        if (style == "mixed") {
            # Sample with replacement from all options for each name
            choice <- sample(c("title", "upper", "lower"), 1)
        } else {
            choice <- style
        }

        new_names[i] <- switch(choice,
                               "title" = {
                                   # First split camelCase, then replace dots/underscores with spaces, then title case
                                   camel_split <- .split_camel_case(name)
                                   name_with_spaces <- gsub("[._]", " ", camel_split)
                                   .to_title_case(name_with_spaces)
                               },
                               "upper" = toupper(name),
                               "lower" = tolower(name),
                               name  # fallback
        )
    }

    names(data) <- new_names
    data
}

#' Split data like a toddler "sharing" their toys
#'
#' Takes a perfectly good data frame and splits it into pieces like a toddler
#' dividing up their snacks (spoiler: the portions won't be equal).
#'
#' @param data A data frame that's about to be dismantled
#' @param by_rows Column name (character) to split rows by
#' @param by_cols Logical. Split columns instead of rows (horizontal chaos)
#' @param by_groups Column name (character) to split into groups
#' @param id_cols Character vector of ID columns to keep when splitting columns
#' @param cols1 Character vector of columns for first dataframe when splitting columns
#' @param prop Proportion for first group when splitting rows (default 0.5)
#' @param seed Random seed for reproducible splitting tantrums
#'
#' @return A list of data frames, because who needs everything in one place?
#' @export
#'
#' @examples
#' df <- data.frame(id = 1:10, name = letters[1:10], value = runif(10))
#'
#' # Split rows randomly
#' toddler_unjoin(df, by_rows = "id", prop = 0.7)
#'
#' # Split columns (keeping id in both)
#' toddler_unjoin(df, by_cols = TRUE, id_cols = "id", cols1 = "name")
toddler_unjoin <- function(data, by_rows = NULL, by_cols = FALSE, by_groups = NULL,
                           id_cols = NULL, cols1 = NULL, prop = 0.5, seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

    # Split by rows
    if (!is.null(by_rows)) {
        unique_ids <- unique(data[[by_rows]])
        n_first <- round(length(unique_ids) * prop)
        first_ids <- sample(unique_ids, n_first)

        data1 <- data[data[[by_rows]] %in% first_ids, ]
        data2 <- data[!data[[by_rows]] %in% first_ids, ]

        return(list(data1 = data1, data2 = data2))
    }

    # Split by columns
    if (by_cols) {
        if (is.null(id_cols) || is.null(cols1)) {
            stop("Must specify id_cols and cols1 when splitting by columns")
        }

        data1 <- data[, c(id_cols, cols1)]

        # Get remaining columns for data2
        cols2_names <- setdiff(names(data), c(id_cols, cols1))
        data2 <- data[, c(id_cols, cols2_names)]

        return(list(data1 = data1, data2 = data2))
    }

    # Split by groups
    if (!is.null(by_groups)) {
        groups <- unique(data[[by_groups]])

        group_data <- list()
        for (i in seq_along(groups)) {
            group_data[[i]] <- data[data[[by_groups]] == groups[i], ]
        }
        names(group_data) <- paste0("group_", groups)

        return(group_data)
    }

    # If no splitting specified, return original data
    data
}

#' Add duplicate rows like a toddler's "more please" demands
#'
#' Because toddlers always want more of everything, this function duplicates
#' rows with the persistence of a three-year-old asking for another cookie.
#'
#' @param data A data frame that's about to get some uninvited guests
#' @param prop Proportion of rows to duplicate (0-1). Higher values = more chaos
#' @param seed Random seed for reproducible duplication drama
#'
#' @return A data frame with some rows that just couldn't take a hint and leave
#' @export
#'
#' @examples
#' df <- data.frame(id = 1:5, value = letters[1:5])
#' toddler_duplicate(df, prop = 0.4)  # 40% chance of "again! again!"
toddler_duplicate <- function(data, prop = 0.05, seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

    n_dups <- round(nrow(data) * prop)

    if (n_dups == 0 || nrow(data) == 0) {
        return(data)
    }

    dup_rows <- sample(nrow(data), n_dups, replace = TRUE)

    # Use drop = FALSE to ensure we always get a data frame, not a vector
    rbind(data, data[dup_rows, , drop = FALSE])
}

#' Make categorical values inconsistent like toddler pronunciation
#'
#' Takes perfectly consistent categorical data and makes it as inconsistent
#' as a toddler trying to say "spaghetti" (is it "pasketti"? "sketty"? who knows!).
#'
#' @param data A data frame with boringly consistent categories
#' @param cols Column names (character vector) to make inconsistent.
#'   If NULL, targets all character columns because chaos should be democratic.
#' @param prop Proportion of values to make inconsistent (0-1). Default 0.2.
#' @param seed Random seed for reproducible linguistic disasters
#'
#' @return A data frame where "Yes" might also be "YES", "y", or "True"
#' @export
#'
#' @examples
#' df <- data.frame(
#'   answer = rep(c("yes", "no"), 5),
#'   gender = rep(c("male", "female"), 5)
#' )
#' toddler_inconsistent(df, prop = 0.3)
toddler_inconsistent <- function(data, cols = NULL, prop = 0.2, seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

    # If cols not specified, find character columns
    if (is.null(cols)) {
        if (ncol(data) == 0) {
            cols <- character(0)
        } else {
            cols <- names(data)[sapply(data, is.character)]
        }
    }

    result <- data
    for (col_name in cols) {
        if (col_name %in% names(result)) {
            x <- result[[col_name]]
            # Apply inconsistency to specified proportion of values
            change_indices <- runif(length(x)) < prop

            for (i in which(change_indices)) {
                val <- tolower(x[i])
                if (val %in% c("yes", "y", "true")) {
                    x[i] <- sample(c("YES", "y", "True", "yes"), 1)
                } else if (val %in% c("no", "n", "false")) {
                    x[i] <- sample(c("NO", "n", "False", "no"), 1)
                } else if (val == "male") {
                    x[i] <- sample(c("Male", "MALE", "M", "male"), 1)
                } else if (val == "female") {
                    x[i] <- sample(c("Female", "FEMALE", "F", "female"), 1)
                } else {
                    # Random case change for other values
                    x[i] <- sample(c(toupper(x[i]), tolower(x[i]), .to_title_case(x[i])), 1)
                }
            }
            result[[col_name]] <- x
        }
    }

    result
}

#' Add whitespace problems like toddler finger painting
#'
#' Adds random spaces to your text data with the artistic flair of a toddler
#' who thinks "more space" always makes things better.
#'
#' @param data A data frame with sensibly spaced text
#' @param cols Column names (character vector) to add whitespace to.
#'   If NULL, targets all character columns because spaces are for everyone.
#' @param prop Proportion of values to affect (0-1)
#' @param seed Random seed for reproducible spatial chaos
#'
#' @return A data frame where "hello" might become "  hello", "hello  ", or " hello "
#' @export
#'
#' @examples
#' df <- data.frame(name = c("Alice", "Bob", "Charlie"))
#' toddler_whitespace(df, prop = 0.5)
toddler_whitespace <- function(data, cols = NULL, prop = 0.15, seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

    # If cols not specified, find character columns
    if (is.null(cols)) {
        if (ncol(data) == 0) {
            cols <- character(0)
        } else {
            cols <- names(data)[sapply(data, is.character)]
        }
    }

    result <- data
    for (col_name in cols) {
        if (col_name %in% names(result)) {
            x <- result[[col_name]]
            change_indices <- runif(length(x)) < prop

            for (i in which(change_indices)) {
                rand_choice <- runif(1)
                if (rand_choice < 0.4) {
                    x[i] <- paste0("  ", x[i])           # leading spaces
                } else if (rand_choice < 0.7) {
                    x[i] <- paste0(x[i], "  ")           # trailing spaces
                } else {
                    x[i] <- paste0(" ", x[i], " ")       # both
                }
            }
            result[[col_name]] <- x
        }
    }

    result
}

#' Mix data types like a toddler mixing food groups
#'
#' Takes your nice numeric columns and sprinkles in some text values like
#' a toddler putting ketchup on ice cream - technically possible but oh so wrong.
#'
#' @param data A data frame with well-behaved numeric columns
#' @param cols Column names (character vector) to mess up types.
#'   If NULL, targets numeric columns because numbers are too orderly.
#' @param prop Proportion of values to affect (0-1)
#' @param seed Random seed for reproducible type confusion
#'
#' @return A data frame where numbers get mixed with "N/A", "missing", and "NULL"
#' @export
#'
#' @examples
#' df <- data.frame(age = c(25, 30, 35), weight = c(70, 80, 75))
#' toddler_types(df, prop = 0.2)
toddler_types <- function(data, cols = NULL, prop = 0.1, seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

    # If cols not specified, find numeric columns
    if (is.null(cols)) {
        if (ncol(data) == 0) {
            cols <- character(0)
        } else {
            cols <- names(data)[sapply(data, is.numeric)]
        }
    }

    result <- data
    for (col_name in cols) {
        if (col_name %in% names(result)) {
            col_data <- as.character(result[[col_name]])
            n_mess <- round(length(col_data) * prop)
            mess_positions <- sample(length(col_data), n_mess)

            mess_values <- sample(c("N/A", "missing", ".", "-", "NULL"), n_mess, replace = TRUE)
            col_data[mess_positions] <- mess_values

            result[[col_name]] <- col_data
        }
    }

    result
}

#' Add units to numeric values like a toddler's helpful "corrections"
#'
#' Because toddlers think every number needs a friend, this function adds
#' units to your numeric data whether they make sense or not.
#'
#' @param data A data frame with naked numbers
#' @param cols Column names (character vector) to add units to.
#'   If NULL, targets all numeric columns because every number deserves a buddy.
#' @param units Vector of possible units to add. Mix and match for maximum confusion!
#' @param prop Proportion of values to affect (0-1)
#' @param seed Random seed for reproducible unit chaos
#'
#' @return A data frame where 25 might become "25kg" or "25sec" regardless of context
#' @export
#'
#' @examples
#' df <- data.frame(height = c(170, 165, 180), age = c(25, 30, 35))
#' toddler_units(df, units = c("cm", "kg", "years"), prop = 0.5)
toddler_units <- function(data, cols = NULL,
                          units = c("kg", "lbs", "cm", "in", "sec", "min"),
                          prop = 0.3, seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

    # If cols not specified, find numeric columns
    if (is.null(cols)) {
        if (ncol(data) == 0) {
            cols <- character(0)
        } else {
            cols <- names(data)[sapply(data, is.numeric)]
        }
    }

    result <- data
    for (col_name in cols) {
        if (col_name %in% names(result)) {
            x <- result[[col_name]]
            change_indices <- runif(length(x)) < prop

            # Only convert to character if we're actually making changes
            if (any(change_indices)) {
                new_values <- as.character(x)
                for (i in which(change_indices)) {
                    new_values[i] <- paste0(x[i], sample(units, 1))
                }
                result[[col_name]] <- new_values
            }
        }
    }

    result
}

#' Add extra rows like a toddler's "helpful" additions
#'
#' Because toddlers love to add their own special touches, this function
#' adds empty rows that make about as much sense as a toddler's art project.
#' (Note: Excel-style headers and totals removed to maintain R's rectangular data structure)
#'
#' @param data A data frame that's about to get some uninvited additions
#' @param add_random Logical. Add empty rows randomly scattered throughout the data (default FALSE)
#' @param add_end Logical or numeric. If TRUE, adds 3-30 random empty rows at the end.
#'   If numeric, adds that many empty rows at the end. (default FALSE)
#' @param seed Random seed for reproducible "improvements"
#'
#' @return A data frame with bonus empty rows that nobody asked for
#' @export
#'
#' @examples
#' df <- data.frame(name = c("Alice", "Bob"), score = c(85, 92))
#' toddler_extra(df, add_random = TRUE)        # Random empty rows scattered
#' toddler_extra(df, add_end = TRUE)          # 3-30 empty rows at end
#' toddler_extra(df, add_end = 5)             # Exactly 5 empty rows at end
#' toddler_extra(df, add_random = TRUE, add_end = 10)  # Both types
toddler_extra <- function(data, add_random = FALSE, add_end = FALSE, seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

    result <- data

    # Add empty rows randomly scattered throughout the data
    if (add_random && nrow(result) > 0) {
        n_empty <- sample(1:3, 1)
        # Don't try to sample more positions than rows available
        max_positions <- min(n_empty, nrow(result))
        if (max_positions > 0) {
            empty_positions <- sample(1:nrow(result), max_positions)

            for (pos in sort(empty_positions, decreasing = TRUE)) {
                empty_row <- result[1, ]
                empty_row[1, ] <- NA

                # Insert empty row at position
                if (pos == 1) {
                    result <- rbind(empty_row, result)
                } else if (pos >= nrow(result)) {
                    result <- rbind(result, empty_row)
                } else {
                    result <- rbind(result[1:pos, ], empty_row, result[(pos+1):nrow(result), ])
                }
            }
        }
    }

    # Add empty rows at the end
    if (add_end != FALSE) {
        if (isTRUE(add_end)) {
            # Add random number between 3 and 30
            n_end_rows <- sample(3:30, 1)
        } else if (is.numeric(add_end) && add_end > 0) {
            # Add specific number
            n_end_rows <- round(add_end)
        } else {
            n_end_rows <- 0
        }

        if (n_end_rows > 0 && ncol(result) > 0) {
            # Create empty rows to add at end
            for (i in 1:n_end_rows) {
                empty_row <- result[1, ]
                empty_row[1, ] <- NA
                result <- rbind(result, empty_row)
            }
        }
    }

    result
}
