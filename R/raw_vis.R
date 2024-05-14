

#' Load necessary packages
#' 
#' @import ggplot2
#' @import ggridges
#' @import viridis
#' @import hrbrthemes
#' @import dplyr
NULL

#' Plot Total Hours Distribution by Month
#' 
#' @param data The data frame containing the data.
#' @param period_end_col The name of the column with the period end date.
#' @param total_hours_col The name of the column with total hours.
#' @return A ggplot object.
#' @export
plot_total_hours_by_month <- function(data, period_end_col, total_hours_col) {
  period_end_col <- rlang::ensym(period_end_col)
  total_hours_col <- rlang::ensym(total_hours_col)
  
  data <- data %>%
    dplyr::mutate(month = format(!!period_end_col, "%m"))
  
  ggplot(data, aes(x = !!total_hours_col, y = month, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, fill = "#69b3a2") +
    labs(title = "Total Hours distribution by month") +
    theme_ipsum() +
    theme(
      legend.position = "none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 10))
}

#' Plot Gross Pay Distribution by Month
#' 
#' @param data The data frame containing the data.
#' @param period_end_col The name of the column with the period end date.
#' @param gross_pay_col The name of the column with gross pay.
#' @return A ggplot object.
#' @export
plot_gross_pay_by_month <- function(data, period_end_col, gross_pay_col) {
  period_end_col <- rlang::ensym(period_end_col)
  gross_pay_col <- rlang::ensym(gross_pay_col)
  
  data <- data %>%
    dplyr::mutate(month = format(!!period_end_col, "%m"))
  
  ggplot(data, aes(x = !!gross_pay_col, y = month, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, fill = "#69b3b2") +
    labs(title = "Gross Pay distribution by month") +
    theme_ipsum() +
    theme(
      legend.position = "none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    scale_x_continuous(limits = c(0, 20000), breaks = seq(0, 20001, 1000))
}

#' Plot Full Time / Part Time Distribution by Company
#' 
#' @param data The data frame containing the data.
#' @param company_name_col The name of the column with company names.
#' @param worker_category_col The name of the column with worker categories.
#' @return A ggplot object.
#' @export
plot_ft_pt_distribution <- function(data, company_name_col, worker_category_col) {
  company_name_col <- rlang::ensym(company_name_col)
  worker_category_col <- rlang::ensym(worker_category_col)
  
  ggplot(data, aes(x = !!company_name_col, fill = !!worker_category_col)) +
    geom_bar(position = "dodge") +
    labs(title = "Full Time / Part Time distribution by company") +
    theme(
      legend.position = "bottom",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    facet_wrap(~ !!company_name_col, scales = "free")
}

#' Plot Payroll Observations by Month
#' 
#' @param data The data frame containing the data.
#' @param period_end_col The name of the column with the period end date.
#' @param company_name_col The name of the column with company names.
#' @param associate_id_col The name of the column with associate IDs.
#' @return A ggplot object.
#' @export
plot_payroll_observations <- function(data, period_end_col, company_name_col, associate_id_col) {
  period_end_col <- rlang::ensym(period_end_col)
  company_name_col <- rlang::ensym(company_name_col)
  associate_id_col <- rlang::ensym(associate_id_col)
  
  data <- data %>%
    dplyr::filter(lubridate::year(!!period_end_col) == 2023) %>%
    dplyr::mutate(month_day = factor(lubridate::round_date(!!period_end_col, "month"))) %>%
    dplyr::group_by(!!company_name_col, month_day) %>%
    dplyr::summarise(count = n_distinct(!!associate_id_col))
  
  ggplot(data, aes(x = as.Date(month_day), y = count, color = !!company_name_col)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_date(date_breaks = "1 month") +
    theme_gray() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) +
    labs(x = "", y = "Observations", title = "Enterprise Staffing - Payroll Observations by Month") +
    theme(legend.position = "bottom") +
    facet_wrap(~ !!company_name_col) +
    theme(legend.position = "none") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
      axis.ticks.x = element_blank()
    )
}
