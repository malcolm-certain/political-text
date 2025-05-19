# project: political-text
# file: RD_data_figures_tables
# purpose: prepare RD data and create basic descriptive figures and tables
# author: Malcolm Certain
# date: 2025-05-14

library(data.table)
library(arrow)
library(table1)
library(stringr)
library(flextable)
library(ggplot2)

#this function will:
#-drop observations with missing caption data
#-truncate the LocalView data to the given start and end dates
#--default is to not truncate
#-created a dummy variable for whether an observation is after treatment date
#-create word count variables for each "target" word
#-create demographic variables as percentages of total population
#-optionally drop long text columns
data_prep <- function(
    x
    , start = "2000-01-01"
    , end = "2050-12-31"
    , treatment
    , targets
) {
  x <- x[meeting_date >= as.Date(start) & meeting_date <= as.Date(end)]
  
  x <- x[caption_text_clean != "<No caption available>"]
  
  x[, word_count := str_count(caption_text_clean, " ")]
  
  x[, caption_text_clean := tolower(caption_text_clean)]
  
  x[, acs_18_amind_pct := acs_18_amind / acs_18_pop]
  x[, acs_18_asian_pct := acs_18_asian / acs_18_pop]
  x[, acs_18_black_pct := acs_18_black / acs_18_pop]
  x[, acs_18_hispanic_pct := acs_18_hispanic / acs_18_pop]
  x[, acs_18_white_pct := acs_18_white / acs_18_pop]
  
  x[, treated := (meeting_date > as.Date(treatment))]
  
  for (word in targets) {
    x[, paste0(word, "_count") := str_count(caption_text_clean, word)]
  }
  
  x
}

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform an ANOVA
    p <- oneway.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

#this function will:
#-create a balance table of ACS characteristics pre/post treatment
balance_table <- function(x, topic, save_folder = ".") {
  x$treated <- factor(
    x$treated
    , levels = c(FALSE, TRUE)
    , labels = c("Pre", "Post")
  )
  
  table1(
    ~ acs_18_pop
    + acs_18_white_pct
    + acs_18_black_pct
    + acs_18_asian_pct
    + acs_18_amind_pct
    + acs_18_hispanic_pct
    + acs_18_median_age
    + acs_18_median_hh_inc
    + acs_18_median_gross_rent
    | treated
    , data = x
    , overall = F
    , extra.col=list(`P-value`=pvalue)
  ) |>
    t1flex() |>
    save_as_image(
      paste0(
        paste0(
          paste0(
            save_folder, "/RD_balance_table_"
          ), topic
        ), ".png"
      )
    )
}

#this function will:
#-create a t-test table of word counts pre/post treatment
ttest_table <- function(x, topic, save_folder = ".") {
  x$treated <- factor(
    x$treated
    , levels = c(FALSE, TRUE)
    , labels = c("Pre", "Post")
  )
  
  keep <- colnames(x)[grep("_count", colnames(x))]
  
  t <- x$treated
  
  x <- x[, keep, with = FALSE]
  
  x$treated <- t
  
  table1(
    ~ .
    | treated
    , data = x
    , overall = F
    , extra.col=list(`P-value`=pvalue)
  ) |>
    t1flex() |>
    save_as_image(
      paste0(
        paste0(
          paste0(
            save_folder, "/RD_ttest_table_"
          ), topic
        ), ".png"
      )
    )
}

#this function will:
#-make a binned scatterplot of word count variables over time
rd_plots <- function(
  x
  , treatment
  , targets
  , topic
  , save_folder = "."
  , binwidth = 3
) {
  for (word in targets) {
    plot <- x |> ggplot(
      aes_string(x = "meeting_date", y = paste0(word, "_count"))
      , group = treated
    ) +
      stat_summary_bin(
        geom = "point"
        , fun = "mean"
        , binwidth = binwidth
      ) +
      geom_vline(xintercept = as.Date(treatment))
    ggsave(
      paste0(
        paste(
          paste0(
            paste0(
              save_folder, "/RD_plot_"
            ), topic
          ), word, sep = "_"
        ), ".png"
      ), plot = plot
    )
  }
}

#this function will run all the helper functions on the data set given
run_all <- function(
  x
  , start = "2000-01-01"
  , end = "2050-12-31"
  , treatment
  , targets
  , topic
  , save_folder = "."
  , binwidth = 3
) {
  x <- x |> data_prep(
    start = start
    , end = end
    , treatment = treatment
    , targets = targets
  )
  
  x |> balance_table(topic = topic, save_folder = save_folder)
  x |> ttest_table(topic = topic, save_folder = save_folder)
  x |> rd_plots(
    treatment = treatment
    , topic = topic
    , targets = targets
    , save_folder = save_folder
  )
}