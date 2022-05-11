#' Derive Triangles
#'
#' @param loss_dat loss data
#' @param type paid, reported, case, or n_claims
#' @param limit optinal limit
#'
#' @return list of triangle data
#'
#' @importFrom dplyr rename filter mutate
#' @importFrom purrr map2_dfr
#' @importFrom rlang set_names
#' @importFrom tidyr pivot_wider
derive_triangles <- function(loss_dat, type = c("paid", "reported", "case", "n_claims"), limit = NULL) {

  agg_dat <- loss_dat %>% aggregate_loss_data(limit = limit)

  tri_dat <- dev_tri(
    origin = agg_dat$accident_year,
    age = agg_dat$devt,
    value = agg_dat[[type]]
  )

  tri <- tri_dat %>%
    spread_tri() %>%
    dplyr::rename(AYE = origin)

  if (type == "case") {
    return(
      list(
        "aggregate_data" = agg_dat,
        "triangle_data" = tri_dat,
        "triangle" = tri
      )
    )
  }

  ata_dat <- tri_dat %>%
    ata_tri(loss_dat) %>%
    dplyr::filter(!is.na(value))

  ata_tri <- ata_dat %>%
    spread_tri() %>%
    dplyr::rename(AYE = origin) %>%
    dplyr::mutate(AYE = as.character(AYE))

  # ata_tri <- triangle_data[[input$type]]$age_to_age_triangle %>%
  #   mutate(AYE = as.character(AYE))

  ldf_avg <- idf(ldf_avg(tri_dat)$idfs)

  ldf_avg_wtd <- idf(ldf_avg_wtd(tri_dat)$idfs)

  sel <- ldf_avg_wtd

  cdf <- idf2cdf(sel)

  params <- list("Straight Average:" = ldf_avg,
                 "Weighted Average:" = ldf_avg_wtd,
                 "Selected:" = sel,
                 "CDF:" = cdf)

  hold <- purrr::map2_dfr(params, names(params), function(dat, type_ ) {
    dat %>%
      tidyr::pivot_wider(names_from = age, values_from = names(dat)[2]) %>%
      rlang::set_names(names(ata_tri)) %>%
      dplyr::mutate(AYE = type_)
  })

  list(
    "aggregate_data" = agg_dat,
    "triangle_data" = tri_dat,
    "triangle" = tri,
    "age_to_age_data" = ata_dat,
    "age_to_age_triangle" = ata_tri,
    "averages" = hold
  )

}



#' Aggregate Loss data
#'
#' @param claim_dat claims data
#' @param limit optional limit
#'
#' @return df
#'
#' @importFrom dplyr mutate group_by summarise n ungroup
aggregate_loss_data <- function(claim_dat, limit = NA) {

  if (!is.na(limit)) {
    claim_dat <- claim_dat %>%
      dplyr::mutate(
        paid = pmin(limit, .data$paid),
        reported = pmin(limit, .data$reported),
        case = reported - paid
      )
  }

  claim_dat %>%
    dplyr::group_by(accident_year, devt) %>%
    dplyr::summarise(
      paid = sum(.data$paid, na.rm = TRUE),
      reported = sum(.data$reported, na.rm = TRUE),
      n_claims = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(case = reported - paid)
}

#' @importFrom dplyr mutate
#' @importFrom tibble tibble
idf <- function(idfs, first_age = 12) {

  l <- length(idfs)
  last_age <- first_age + l - 1
  stopifnot(is.numeric(first_age) && length(first_age) == 1L)
  stopifnot(first_age > 0)
  stopifnot(is.numeric(idfs) && l > 0)
  tib <- tibble::tibble(age = first_age:last_age, idfs = idfs)
  tib <- tib %>% dplyr::mutate(earned_ratio = pmin(age/1, 1))
  out <- structure(tib, tail_first_age = NA, dev_tri = NA,
                   class = c("idf_", class(tib)))
}

#' @importFrom dplyr select ungroup mutate group_by lead
ata_tri <- function(tri, ...) {

  stopifnot(inherits(tri, "dev_tri"))

  out <- tri |>
    dplyr::group_by(.data$origin) |>
    dplyr::mutate(value_lead = dplyr::lead(.data$value, by = .data$age),
                  value = value_lead / value) |>
    dplyr::ungroup() |>
    dplyr::select(origin, age, value)

  out <- out |> mutate(value = ifelse(value == Inf, NA_real_, value))

  structure(out, class = c("ata", class(out)))

}

#' @importFrom tibble tibble
dev_tri <- function(origin, age, value) {
  tib <- tibble::tibble(origin = origin, age = age, value = value)
  structure(tib, class = c("dev_tri", class(tib)))
}

#' @importFrom tidyr pivot_wider
spread_tri <- function(tri) {
  tri |>
    tidyr::pivot_wider(names_from = age, values_from = value)
}

#' @importFrom dplyr group_by summarise
ldf_avg <- function(tri) {
  ata <- ata_tri(tri)
  out <- ata %>% dplyr::group_by(age) %>% dplyr::summarise(ldfs = mean(value,
                                                                       na.rm = TRUE))
  ldfs <- out$ldfs
  out <- idf(ldfs[-length(ldfs)], first_age = min(tri$age))
  attr(out, "dev_tri") <- tri
  out
}

#' @importFrom dplyr mutate
idf2cdf <- function(idf_) {
  stopifnot(inherits(idf_, "idf_"))
  cdf_new <- idf_
  cdf_new$cdfs <- cdf_new$idfs %>% rev() %>% cumprod() %>%
    rev()
  cdf_new <- cdf_new %>% dplyr::mutate(cdfs = cdfs * earned_ratio)
  out <- cdf(cdfs = cdf_new$cdfs, first_age = cdf_new$age[1])
  attr(out, "tail_first_age") <- attr(idf_, "tail_first_age")
  attr(out, "dev_tri") <- attr(idf_, "dev_tri")
  out
}
