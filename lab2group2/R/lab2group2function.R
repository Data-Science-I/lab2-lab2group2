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
    payment_type=="covered" ~ "Average.Covered.Payments"
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
    labs(title = paste("Boxplot of", payment_type, "payments by DRG Code"),
         x = "DRG Code",
         y = paste("Average", payment_type, "payments")) +
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
summary_function <- function(data, stat = "mean") {
  if (!(stat %in% c("mean", "median", "sd"))) {
    stop("Invalid stat type. Choose 'mean', 'median', or 'sd'")
  }

  # Calculate the specified statistic
  result <- switch(stat,
                   mean = mean(data$Average.Medicare.Payment, na.rm = TRUE),
                   median = median(data$Average.Medicare.Payment, na.rm = TRUE),
                   sd = sd(data$Average.Medicare.Payment, na.rm = TRUE))

  return(result)
}
