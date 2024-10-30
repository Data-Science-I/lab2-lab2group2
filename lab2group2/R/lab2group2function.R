#' Load DRG Data from CSV
#'
#' This function loads the DRG_data.csv file included in the package.
#' @return A data frame containing the DRG data.
#' @export
load_DRG_data <- function() {
  # Your code to load the data
  data <- read.csv(system.file("extdata", "DRG_data.csv", package = "lab2group2"))
  return(data)
}

#' Generate Boxplot by DRG Code
#'
#' This function generates a boxplot of payments by DRG code.
#' Users can choose between average Medicare payments, average total payments, or average covered charges.
#'
#' @param data Data frame containing DRG data.
#' @param payment_type A string indicating the type of payment to plot: "medicare", "total", or "covered".
#' @return A ggplot object of the boxplot.
#' @examples
#' boxplot_function(drg_data, "medicare")
#' @export
boxplot_function <- function(data, payment_type) {
  payment_column=case_when(
    payment_type=="medicare" ~ "Average.Medicare.Payments",
    payment_type=="total" ~ "Average.Total.Payments",
    payment_type=="covered" ~ "Average.Covered.Charges"
  )

  # Check if the chosen column exists in the data
  if (!payment_column %in% names(data)) {
    stop("The specified payment type does not exist in the data.")
  }

  #add column to data that has just the code numbers
  new_data <- data |>
    mutate(DRG_code=substr(DRG.Definition, 1, 3))

  # Create the boxplot
  ggplot(new_data, aes_string(x = "DRG_code", y = payment_column)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", payment_type, "Payments by DRG Code"),
         x = "DRG Code",
         y = paste("Average", payment_type, "Payments")) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, size=6))

}

#' Calculate Summary Statistics for DRG Codes
#'
#' This function calculates statistics for average Medicare payments across all DRG codes.
#' Users can select to compute either the mean, median, or standard deviation.
#'
#' @param data Data frame containing DRG data.
#' @param stat A string specifying the statistic to calculate: "mean", "median", or "sd".
#' @return A numeric value representing the computed statistic.
#' @examples
#' summary_function(drg_data, "mean")
#' @export
# Define the summary function
summary_function <- function(data, stat = "mean") {
  if (!(stat %in% c("mean", "median", "sd"))) {
    stop("Invalid stat type. Choose 'mean', 'median', or 'sd'")
  }

  # Calculate the specified statistic by 'DRG.Definition' group
  result <- data %>%
    group_by(DRG.Definition) %>%
    summarise(
      stat_value = switch(stat,
                          mean = mean(Average.Medicare.Payments, na.rm = TRUE),
                          median = median(Average.Medicare.Payments, na.rm = TRUE),
                          sd = sd(Average.Medicare.Payments, na.rm = TRUE))
    )

  return(result)
}
