#' Create table with strings containing means and significance labels
#' 
#' Copyright (c) 2022 Vincent Talen.
#' Licensed under GPLv3. See LICENSE file.
#'
#' For each algorithm specified by `displayed_columns`, perform t-test for each metric 
#'   in `comparison_fields` against the algorithm selected with `test_base_selection`.
#' Creates data table where the rows are the algorithms and the columns have strings with
#'   the means for metrics and significance label compared to the test base.
#' The following paper was used to implement the same correction for the t-test as Weka:
#'   - https://www.cs.waikato.ac.nz/~eibe/pubs/bouckaert_and_frank.pdf
#'
#' @param performance_df `data.table` created by using `loadPerformanceData()` on .arff file from Weka
#' @param test_base_selection Integer that specifies test base algorithm. `default = 1` (TIP: use `showAllKeys()` to see all available algorithms in data table)
#' @param comparison_fields Character vector with metric field names that should be compared `default = c("Percent_correct")`
#' @param displayed_columns Integer vector with algorithms that should be shown (defaults to all algorithms)
#' @param alpha Numeric value between 0 and 1 to use for t-test significance testing `default = 0.05`
#'
#' @return final data table with algorithms as rows and metric field strings in columns
#' @export
#'
#' @examples
#' # Load performance data from file (same for each example)
#' file_path <- "output/algorithm_performances/performance_data_file.arff"
#' performance_data <- loadPerformanceData(file_path)
#' 
#' # Example 1 - Fully custom by specify everything
#' comparison_fields <- c("Percent_correct", "True_positive_rate", "Area_under_ROC")
#' createTableWithSignificances(performance_df = performance_data,
#'                              test_base_selection = 4,
#'                              comparison_fields = comparison_fields, 
#'                              displayed_columns = 2:7,
#'                              alpha = 0.025)
#' 
#' # Example 2 - Compare all algorithms to the first one for only Percent_correct
#' createTableWithSignificances(performance_df = performance_data)
#' 
#' # Example 3 - Use different test base algorithm and select other comparison fields
#' comparison_fields <- c("Percent_correct", "True_positive_rate", "Area_under_ROC")
#' createTableWithSignificances(performance_df = performance_data,
#'                              test_base_selection = 2,
#'                              comparison_fields = comparison_fields)
createTableWithSignificances <- function(performance_df, 
                                         test_base_selection = 1, 
                                         comparison_fields = c("Percent_correct"),
                                         displayed_columns = NULL,
                                         alpha = 0.05) {
  createStringsPerAlgorithm <- function(cur_data_table) {
    createStringsForMetricColumn <- function(metric_name, metric_data) {
      # Get current metric data from test base
      metric_test_base_data <- test_base_data[[metric_name]]
      
      # Perform t-test
      ttest_res <- corrected_dependant_ttest(
        data1 = metric_test_base_data, data2 = metric_data,
        test_training_ratio = test_training_ratio, alpha = alpha)
      
      # Define significance marker from p-value
      if (is.na(ttest_res$p.value)) {
        blub <- ""
      } else if (ttest_res$p.value <= alpha) {
        blub <- "v"
      } else if (ttest_res$p.value >= 1 - alpha) {
        blub <- "*"
      } else {
        blub <- ""
      }
      # Return as string with mean and significance label e.g. `96.69 v`
      return(list(sprintf("%.2f %s", mean(metric_data), blub)))
    }
    # Use above function to get string with mean and significance label for each metric
    return(cur_data_table[j = as.list(unlist(mapply(createStringsForMetricColumn, comparison_fields, .SD))), .SDcols = comparison_fields])
  }
  
  # Compute ratio of amounts of testing- divided by training instances
  test_training_ratio <- 
    sum(performance_df$Number_of_testing_instances) / 
    sum(performance_df$Number_of_training_instances)
  
  # Select the wanted test base
  test_base_name <- levels(performance_df$Key_Scheme_S)[test_base_selection]
  test_base_data <- performance_df[i = Key_Scheme_S == test_base_name]
  
  # Select only the algorithm columns desired to be displayed
  if (is.null(displayed_columns)) {
    # If all are wanted only remove test base
    performance_df <- performance_df[i = !(Key_Scheme_S == test_base_name)]
  } else {
    # Remove test base from displayed columns
    displayed_columns <- displayed_columns[-test_base_selection]
    wanted_column_names <- levels(performance_df$Key_Scheme_S)[displayed_columns]
    performance_df <- performance_df[i = (Key_Scheme_S %in% wanted_column_names)]
  }
  
  # Get strings with mean and significance label for all comparison fields/metrics
  comparison_fields_final <- performance_df[j = as.list(createStringsPerAlgorithm(.SD)), 
                                            by = Key_Scheme_S, .SDcols = comparison_fields]
  # Test base mean metrics
  test_field_final <- getMeansForFields(test_base_data, comparison_fields)
  # Final data frame
  final_df <- data.table::rbindlist(list(test_field_final, comparison_fields_final))
  return(final_df)
}
# ===========================================
printNiceLatexTable <- function(algo_comp_data, new_metric_names, caption) {
  colnames(algo_comp_data) <- c("Algorithm", new_metric_names)
  nice_kable <- knitr::kable(algo_comp_data, booktabs = T, caption = caption, linesep = "") %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"), 
                              stripe_index = seq(3, nrow(algo_comp_data), 2)) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::row_spec(1, hline_after = TRUE) %>%
    kableExtra::footnote(general = "'*' = significantly less; 'v' = significantly more",
                         general_title = "")
  print(nice_kable)
}
# ===========================================
loadPerformanceData <- function(file_path) {
  getShortNamesWithIndices <- function(performance_data) {
    # Unique combinations of Scheme and Scheme_options
    present_algorithms <- performance_data[j = unique(.SD), .SDcols = c("Key_Scheme", "Key_Scheme_options")]
    rows_per_algorithm <- nrow(performance_data) / nrow(present_algorithms)
    
    # Get the short name with index per algorithm
    getNamesVectorPerAlgorithm <- function(i) {
      short_name <- stringr::str_split(present_algorithms[i, Key_Scheme], "\\.")[[1]][4]
      short_name_with_index <- sprintf("(%d) %s", i, short_name)
      rep(short_name_with_index, rows_per_algorithm)
    }
    short_names_with_indices <- as.vector(sapply(1:nrow(present_algorithms), 
                                                 getNamesVectorPerAlgorithm))
    # Return as factor
    return( factor(short_names_with_indices, levels = unique(short_names_with_indices)) )
  }
  
  # Load the performance data
  performance_data <- data.table::as.data.table( RWeka::read.arff(file_path) )
  # Create new column with indices and short names of algorithms
  performance_data$Key_Scheme_S <- getShortNamesWithIndices(performance_data)
  return(performance_data)
}
# ===========================================
showAllKeys <- function(performance_data) {
  keys <- performance_data[j = unique(.SD), .SDcols = c("Key_Scheme_S", "Key_Scheme_options")]
  cat("**All present algorithms with their settings:**  \n")
  for (i in 1:nrow(keys)) {
    if (keys[i, Key_Scheme_options] == "") {
      cat(sprintf("**%s** ''  \n", keys[i, Key_Scheme_S]))
    } else {
      cat(sprintf("**%s** '_%s_'  \n", keys[i, Key_Scheme_S], keys[i, Key_Scheme_options]))
    }
  }
}
# ===========================================
getMeansForFields <- function(performance_data, field_names) {
  performance_data[j = lapply(.SD, function(x) round(mean(x), 2)), 
                   .SDcols = field_names, by = .(Key_Scheme_S)]
}
# ===========================================
corrected_dependant_ttest <- function(data1, data2, test_training_ratio, alpha) {
  n <- length(data1); df <- n - 1
  # compute differences with mapply (faster for bigger data frames)
  differences <- mapply(function(value1, value2){ c(value1 - value2) }, data1, data2)
  
  # compute estimated mean (a.k.a. numerator)
  estimated_mean <- 1 / n * sum(differences)
  # compute estimated variance
  estimated_variance <- 1 / df * sum( (differences - estimated_mean)^2 )
  # compute denominator
  denominator <- sqrt( (1 / n + test_training_ratio) * estimated_variance )
  
  # compute t-score
  t_score <- estimated_mean / denominator
  # compute confidence interval
  confidence_interval <- c( qt(p = 0.5*alpha, df = df), qt(p = 1 - 0.5*alpha, df = df))
  # compute p-value
  pval <- pt(q = t_score, df = df)
  
  # return as list
  result <- list(t.score = t_score, 
                degrees.of.freedom = df, 
                conf.interval = confidence_interval, 
                p.value = pval)
  return(result)
}
