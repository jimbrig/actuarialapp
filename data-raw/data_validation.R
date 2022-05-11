
#  ------------------------------------------------------------------------
#
# Title : Data Validation
#    By : Jimmy Briggs
#  Date : 2022-05-11
#
#  ------------------------------------------------------------------------

library(validate)
library(qs)

test_dat <- qs::qread("data-raw/cache/loss_data.qs")

end_days <- list(
  "1" = 31,
  "2" = c(28, 29),
  "3" = 31,
  "4" = 30,
  "5" = 31,
  "6" = 30,
  "7" = 31,
  "8" = 31,
  "9" = 30,
  "10" = 31,
  "11" = 30,
  "12" = 31
)

is_end_day <- function(date) {
  hold <- end_of_month(date)
  date == hold
}

loss_data_rules <- validate::validator(
  is.Date(eval_date),
  !any(is.na(eval_date)),
  eval_date == end_of_month(eval_date)
)

label(loss_data_rules) <- c(
  "Field `eval_date` is parseable to type `Date`.",
  "Field `eval_date` has no missing values.",
  "Field `eval_date` only contains 'end of month' dates."
)

loss_data_confront <- validate::confront(test_dat, loss_data_rules)
summary(loss_data_confront)

fs::dir_create("inst/validation")
validate::export_yaml(loss_data_rules, file = "inst/validation/loss_data.yml")




# usethis::use_data(data_validation, overwrite = TRUE)
