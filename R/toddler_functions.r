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

#' Add missing values like a toddler hiding their vegetables
#'
#' Introduces NAs into your data with the randomness of a toddler deciding
#' what they will and won't eat today.
#'
#' @param data A data frame
#' @param cols Column names (character vector) or column indices to target.
#'   Default is all columns because toddlers are indiscriminate in their chaos.
#' @param prop Proportion of missing values (0-1), single value or vector.
#'   Can be different for each column, just like toddler preferences.
#' @param seed Random seed for reproducibility (because sometimes you need
#'   the same mess twice)
#'
#' @return A data frame with strategically placed NAs
#' @export
#'
#' @examples
#' df <- data.frame(x = 1:10, y = letters[1:10])
#' toddler_missing(df, prop = 0.2)  # 20% chaos
#' toddler_missing(df, cols = "x", prop = 0.5)  # Target specific columns
toddler_missing <- function(data, cols = names(data), prop = 0.1, seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

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

    if (length(prop) == 1) {
        prop <- rep(prop, length(selected_cols))
    } else if (length(prop) != length(selected_cols)) {
        stop("Length of prop must be 1 or match number of selected columns")
    }

    result <- data
    for (i in seq_along(selected_cols)) {
        col_name <- selected_cols[i]
        n_na <- round(nrow(data) * prop[i])
        na_positions <- sample(nrow(data), n_na)
        result[[col_name]][na_positions] <- NA
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
#' @param add_empty Add empty rows (strategic breathing room)
#' @param seed Random seed for reproducible "improvements"
#'
#' @return A data frame with bonus empty rows that nobody asked for
#' @export
#'
#' @examples
#' df <- data.frame(name = c("Alice", "Bob"), score = c(85, 92))
#' toddler_extra(df, add_empty = TRUE)
toddler_extra <- function(data, add_empty = TRUE, seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

    result <- data

    # Add empty rows randomly (the only sensible option for R's rectangular data)
    if (add_empty && nrow(result) > 0) {
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

    result
}
