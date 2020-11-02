# ## Read the data
# DRG_data <- read.csv("DRG_data.csv")
#
# ## Write a function that makes a boxplot of payments by DRG code. Make it an option for your function to do this for either the average Medicare payments, the average total payment, or the average covered charges.
# library(ggplot2)
# library(tidyverse)
#
# ## Make a new column for the actual code (makes it easier to put on a plot)
# DRG_data$DRG.Code <-  sub("\\ .*", "", DRG_data$DRG.Definition)
#
# ## save the data as a data object
# save(DRG_data, file ='Lab2/data/DRG_data.RData')

#' drgboxplot
#'
#' Makes a boxplot of payments or charges vs \code{DRG.Code} from the DRG_data dataset with argument for payment type.
#'
#' @param payment the type of payment or charge to plot against the respective \code{DRG.Codes}. Legal values include
#' "Average.Medicare.Payments" "Average.Total.Payments" and "Average.Covered.Charges"
#'
#' @return a plot with Payment (or charge) vs. DRG code
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#'
#' @examples
#' medicareplot <- drgboxplot("Average.Medicare.Payments")
#'
#' medicareplot
#'
#'
drgboxplot <- function(payment = "Average.Medicare.Payments") {
  plt <- ggplot(DRG_data, aes(x = DRG.Code, y = get(payment))) +
    geom_boxplot() +
    labs(title = "Payments by DRG code", x = "DRG code", y = "Payment") +
    theme(plot.title = element_text(hjust = .5)) +
    theme(axis.text.x = element_text(angle = 45))
  return(plt)
}

#' drgstats
#'
#' Produces a data frame containing statistics for \code{Average.Medicare.Payments} for each \code{DRG.Code}.
#'
#' @param st the type of statistic to be reported. Legal values include "mean" "sd" and "median".
#'
#' @return a dataframe containing 3 columns representing the \code{DRG.Code},
#' the desired statistic, and the number of observations used in the statistic's calculation
#' @export
#'
#' @importFrom tidyverse tibble
#' @importFrom tidyverse summarise
#' @importFrom tidyverse group_by
#'
#' @examples
#' meanstats <- drgstats("mean")
#'
#' meanstats
#'
#'
drgstats <- function(st = "mean") {
  tbl <- NULL
  ifelse(
    st %in% "mean",
    tbl <- DRG_data %>%
      group_by(DRG.Code) %>%
      summarise(
        mean = mean(Average.Medicare.Payments),
        n = n()
      ),
    ifelse(
      st %in% "median",
      tbl <- DRG_data %>%
        group_by(DRG.Code) %>%
        summarise(
          median = median(Average.Medicare.Payments),
          n = n()
        ),
      ifelse(
        st %in% "sd",
        tbl <- DRG_data %>%
          group_by(DRG.Code) %>%
          summarise(sd = sd(Average.Medicare.Payments), n = n()),
        print("invalid statistic specified")
      )
    )
  )
  return(tbl)
}
