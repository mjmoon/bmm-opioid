library(dplyr)
log_file <- file("logs/parse-us-data-.log", open = "a")
sink(log_file, type = "message")
timestamp <- format(Sys.time(), format = "%Y-%m-%d %H:%M")
message(paste0("----------", timestamp, "----------"))
# mcdus --------------------------------------------------------------------
# icd10: X40-44
causeofdeath <- c(
  paste0("X", c(40:44, 60:64, 85)),
  paste0("Y", 10:14)
)
#' hispanicoriginrace: 
#'   1 - mexican
#'   2 - puerto rican
#'   3 - cuban
#'   4 - central or south american
#'   5 - other or unknown hispanic
#'   6 - non-hipanic white
#'   7 - non-hispanic black
#'   8 - non-hispanic other
#'   9 - hispanic origin unknown
racegroup <- 1:7
# multiple cause: T40.0; T40.1; T40.2; T40.3; T40.4; T40.6
multipcause <- paste0("T", c(400:404, 406))

parse_opioid_counts <- function(
    year, f_icd10, f_hispanicoriginrace, f_multiplecause) {
  mcdus::parse_mcdus(year, read_from_wd = TRUE)[[year]] %>%
    filter(
      .data$residentstatus != "4",
      .data$icd10 %in% f_icd10, 
      .data$hispanicoriginrace %in% f_hispanicoriginrace,
      if_any("recordaxiscondition01":"recordaxiscondition20",
             function(x) x %in% f_multiplecause)
    ) %>%
    select(
      "hispanicoriginrace", "currentdatayear", "icd10", 
      "recordaxiscondition01":"recordaxiscondition20"
    ) %>%
    group_by(across(everything())) %>%
    summarise(n = n(), .groups = "drop")
}
mcdus <- lapply(as.character(2015:2019), parse_opioid_counts, 
                causeofdeath, racegroup, multipcause) %>%
  bind_rows() |>
  rename(race = hispanicoriginrace) |>
  mutate(
    year = as.numeric(currentdatayear), 
    race = case_when(
      race < 6 ~ "hispanic",
      race == 6 ~ "non-hispanic white",
      race == 7 ~ "non-hispanic black",
      .default = NA
    )) %>%
  group_by(year, race) %>%
  summarise(n = n(), .groups = "drop")
saveRDS(mcdus, file.path("data", "us", "mcdus.rds"))
# nsduh ---------------------------------------------------------------------
#' ANALWT_C:      final person-level sample weight
#' NEWRACE2:      1 - nonhisp white; 2 - nonhisp black/afr am
#' heryr:         1 - used heroin in the past year
#' pnranyflag:    1 - used any pain reliever ever
#' pnranyyr:      1 - used any pain reliever within the past year
#' 
#' pnrnmflag:     1 - miused any pain reliever ever
#' pnrnmyr:       1 - misued any pain reliever within the past year
#' irpnrnminit:   1 - misused any pain reliever for the first time in the past year
#' irpnrnmyfu:    1 - year in which first misused any pain reliever
#'
#' herflag:       1 - used heroin ever
#' heryr:         1 - used heroin within the past year
#' irheryfu:      1 - year in which first used heroin
#'
#' (no treatment in the past year)
#' txdrgonly:     22 - never received treatment/counselling for drug use
#' txdrgalcu:     2 / 22 - never received treatment/counselling for drug use
#' txdrgonag:     age when first received treatment/counselling for drug use
#' txdrgaage:     age when first received treatment/counselling for drug use
#' (received treatment in the past year)
#' txyalodrg:     2 - never received treatment/counselling for drug use
#' txyalodag:     age when first received treatment/counselling for drug use (no drug treatment in the past year)
#' txydronag:     age when first received treatment/counselling for drug use
#' txyalddag:     age when first received treatment/counselling for drug use
#'
#' txevrrcvd2:    1 - received treatment/counselling for alcohol or any drug use ever
#' txyrill:       1 - received treatment/counselling for drug use in any location in the past year
#' txltypnrl2:    1 - received last/current treatment for pain relievers
#' txltyhern2:    1 - received last/current treatment for heroin
#' txltcurrsp:    1 - received last/current treatment in a special facility
#' txyrspill:     1 - received treatment a specialty facility for illicit 
#'   drug use in the past year specialty facilities include 
#'     - a hospital as an inpatient, 
#'     - a rehabilitation facility (in or outpatient),
#'     - a mental health center
#'     
#' txltyocom2:   1 - still in treatment
#'               2 - successfully completed treatment
#' txcurrent:    1 - currently receiving treatment
#' 
#' age2:          edited age
#'   1 - 12 years old
#'   2 - 13 years old
#'   3 - 14 years old
#'   4 - 15 years old
#'   5 - 16 years old
#'   6 - 17 years old
#'   7 - 18 years old
#'   8 - 19 years old
#'   9 - 20 years old
#'   10 - 21 years old
#'   11 - 22 or 23 years old
#'   12 - 24 or 25 years old
#'   13 - 26 to 29 years old
#'   14 - 30 to 34 years old
#'   15 - 35 to 49 years old
#'   16 - 50 to 64 years old
#'   17 - 65 or older
s_vars_1519 <- c(
  "newrace2", "analwt_c",
  "heryr", "pnrnmyr",
  "pnranyflag", "pnrnmflag", "herflag",
  "irpnrnminit", "irpnrnmyfu", "irheryfu", 
  "txltypnrl2", "txltyhern2", "txltcurrsp", "txyrspill", 
  "txdrgonly", "txdrgalcu", "txdrgonag", "txdrgaage",
  "txyalodrg", "txyalodag", "txydronag", "txyalddag",
  "txevrrcvd2", "txyrill",
  "txltyocom2", "txcurrent",
  "age2"
)
wsum <- function(s, w) {
  ifelse(all(is.na(s)), -1, round(sum(s * w, na.rm = TRUE)))
}
load_transition_counts <- function(
    year, read_from_wd = TRUE) {
  s_vars <- s_vars_1519
  # if (year < 2015) 
  #   s_vars <- s_vars_0314
  nsduhus::load_nsduhus(year, read_from_wd = read_from_wd)[[year]] |>
    rename_with(tolower) |>
    select(all_of(s_vars)) |>
    mutate(year = year)
}
check_init_by_age <- function(first_age, current_age) {
  case_when(
    current_age < 11 ~ first_age %in% (current_age + 10:11),
    current_age == 11 ~ first_age %in% 21:23,
    current_age == 12 ~ first_age %in% 23:25,
    current_age == 13 ~ first_age %in% 25:29,
    current_age == 14 ~ first_age %in% 29:34,
    current_age == 15 ~ first_age %in% 34:49,
    current_age == 16 ~ first_age %in% 49:64,
    current_age == 17 ~ first_age > 63,
    .default = FALSE
  )
}
nsduh <- lapply(as.character(2015:2019), load_transition_counts) |>
  bind_rows() |>
  rename(
    sample_weight = analwt_c,
    race = newrace2
  ) |>
  mutate(
    year = as.numeric(year),
    race = recode(
      race,
      `1` = "non-hispanic white",
      `2` = "non-hispanic black",
      `3` = "non-hispanic native",
      `4` = "non-hispanic hawaiian/pacific islander",
      `5` = "non-hispanic asian",
      `6` = "non-hispanic more than one race",
      `7` = "hispanic"
    )
  ) |>
  mutate(
    # past year
    pyr_heroin = heryr == 1, # used heroin in the past year
    pyr_ill_pr = pnrnmyr == 1, # misused prescription opioid in the past year
    pyr_ill_opioid = pyr_heroin | pyr_ill_pr,
    # received treatment in the past year for illicit drug use 
    # at a specialty facility: hospital as an inpatient, 
    # rehabilitation facility as an inpatient or an outpatient,
    # mental health center
    pyr_trt_sp = txyrspill, 
    # received treatment for drug use in the past year in any location
    pyr_trt = txyrill,
    # ever
    # Due to potential underreporting of lifetime prescription drug use, 
    # users are cautioned against using the lifetime use or misuse of 
    # prescription pain relievers (ever used/ever misused) in analysis.
    ever_pr = pnranyflag == 1,
    ever_ill_pr = pnrnmflag == 1,
    ever_heroin = herflag == 1,
    ever_ill_opioid = ever_ill_pr | ever_heroin,
    ever_trt = (
      txdrgonly %in% c(1, 11, 12) | 
        txdrgalcu %in% c(1, 11, 12) |
        txyalodrg %in% c(1, 3) |
        txyrill == 1
    ),
    # new initates
    init_heroin = abs(year - irheryfu) < 2,
    init_ill_pr = abs(year - irpnrnmyfu) < 2, # match heroin
    # irpnrnminit == 1
    init_ill_opioid = init_heroin | init_ill_pr,
    # treatments
    # curr_heroin_trt = txltyhern2 == 1,
    # curr_ill_pr_trt = txltypnrl2 == 1,
    # curr_opioid_trt = curr_heroin_trt | curr_ill_pr_trt,
    # past_year_sp_trt = txyrspill == 1,
    init_trt = (
      check_init_by_age(txdrgonag, age2) | 
        check_init_by_age(txdrgaage, age2) |
        check_init_by_age(txyalodag, age2) |
        check_init_by_age(txydronag, age2) |
        check_init_by_age(txyalddag, age2)
    ),
    init_opioid_trt = init_trt & ever_ill_opioid,
    # states
    s1 = ever_pr & !ever_ill_opioid,
    s2 = ever_ill_opioid & !ever_trt,
    # transitions
    r2 = init_ill_opioid & !ever_pr,
    tr1 = init_ill_opioid & ever_pr,
    tr2 = init_opioid_trt
  ) |>
  group_by(year, race) |>
  summarise(
    n = n(),
    pyr_heroin_raw = sum(pyr_heroin, na.rm = TRUE),
    pyr_ill_pr_raw = sum(pyr_ill_pr, na.rm = TRUE),
    pyr_ill_opioid_raw = sum(pyr_ill_opioid, na.rm = TRUE),
    pyr_trt_raw = sum(pyr_trt, na.rm = TRUE),
    pyr_trt_sp_raw = sum(pyr_trt_sp, na.rm = TRUE),
    pyr_heroin = wsum(pyr_heroin, sample_weight),
    pyr_ill_pr = wsum(pyr_ill_pr, sample_weight),
    pyr_ill_opioid = wsum(pyr_ill_opioid, sample_weight),
    pyr_trt = wsum(pyr_trt, sample_weight),
    pyr_trt_sp = wsum(pyr_trt_sp, sample_weight),
    r2_raw = sum(r2, na.rm = TRUE),
    s1_raw = sum(s1, na.rm = TRUE),
    s2_raw = sum(s2, na.rm = TRUE),
    tr1_raw = sum(tr1, na.rm = TRUE),
    tr2_raw = sum(tr2, na.rm = TRUE),
    init_ill_opioid_raw = sum(init_ill_opioid, na.rm = TRUE),
    ever_pr_raw = sum(ever_pr, na.rm = TRUE),
    r2 = wsum(r2, sample_weight),
    s1 = wsum(s1, sample_weight),
    s2 = wsum(s2, sample_weight),
    tr1 = wsum(tr1, sample_weight),
    tr2 = wsum(tr2, sample_weight),
    init_ill_opioid = wsum(init_ill_opioid, sample_weight),
    ever_pr = wsum(ever_pr, sample_weight),
    .groups = "drop"
  )
saveRDS(nsduh, file.path("data", "us", "nsduh.rds"))
# general mortality ----------------------------------------------------------
mort <- readr::read_tsv(
  file.path("data", "ref", "Multiple Cause of Death, 1999-2020.txt"), 
  n_max = 96
) |>
  mutate(Population = as.numeric(Population)) |>
  filter(Year < 2020, Year > 2013, is.na(Notes), !is.na(Population)) |>
  select(-Notes)
saveRDS(mort, file.path("data", "us", "mort.rds"))
sink(NULL, type = "message")
message(paste0("----------", timestamp, "----------"))
message(paste0("------------------------------------"))
rm(list = ls())
