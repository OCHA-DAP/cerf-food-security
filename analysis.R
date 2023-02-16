library(Ripc)
library(rhdx)
library(tidyverse)
library(lubridate)
library(countrycode)
library(httr)
library(readxl)

##################
#### IPC DATA ####
##################

df_ipc <- ipc_get_population()$country

df_ipc_scores <- df_ipc %>%
  filter(
    country != "LAC"
  ) %>%
  mutate(
    analysis_year = year(analysis_period_start),
    phase1_pct = phase1_num / estimated_population,
    phase2_pct = phase2_num / estimated_population,
    phase3_pct = phase3_num / estimated_population,
    phase4_pct = phase4_num / estimated_population,
    phase5_pct = phase5_num / estimated_population,
  ) %>%
  group_by(
    country
  ) %>%
  filter(
    analysis_year >= 2019,
    (period == "current") | (period == "projected" & analysis_date == max(analysis_date)),
    !str_detect(title, "Afar/Amh")
  ) %>%
  summarize(
    avg_phase4_pct = mean(phase4_pct),
    latest_date = max(analysis_period_start),
    phase4_num_latest = phase4_num[analysis_period_start == latest_date],
    phase4_pct_latest = phase4_pct[analysis_period_start == latest_date],
    phase5_num_latest = phase5_num[analysis_period_start == latest_date],
    phase5_pct_latest = phase5_pct[analysis_period_start == latest_date],
    previous_date = if_else(
      any(analysis_year <= 2021),
      max(analysis_period_start[analysis_year <= 2021 & analysis_period_start != latest_date]),
      min(analysis_period_start)
    ),
    phase5_num_previous = phase5_num[analysis_period_start == previous_date],
    phase4pl_scale = phase4_num_latest / 100000 + phase5_num_latest / 10000,
    phase4pl_prev = phase4_pct_latest + phase5_pct_latest,
    phase5_incr = max(0, phase5_num_latest - phase5_num_previous)
  ) %>%
  mutate(
    iso3 = ifelse(
      country != "LAC",
      countrycode(country, origin = "iso2c", destination = "iso3c"),
      "LAC"
    ),
    country = ifelse(
      iso3 == "LAC",
      "Latin America and the Caribbean",
      countrycode(iso3, origin = "iso3c", destination = "country.name")
    ),
    .before = avg_phase4_pct
  )

#################
#### CH DATA ####
#################

# get into singular country format

df_ch <- pull_dataset("cadre-harmonise") %>%
  get_resource(4) %>%
  read_resource(
    guess_max = 10000
  ) %>%
  transmute(
    iso3 = adm0_pcod3,
    analysis_year = reference_year,
    analysis_period_start = dmy(
      paste(
        "1",
        str_extract(reference_label, "^[a-zA-Z]{3}"),
        analysis_year
      )
    ),
    period = chtype,
    across(
      phase1:phase5,
      ~ replace_na(.x, 0)
    ),
    population = ifelse(
      is.na(population),
      phase1 + phase2 + phase3 + phase4 + phase5,
      population
    )
  ) %>%
  group_by(
    iso3,
    analysis_year,
    analysis_period_start,
    period
  ) %>%
  summarize(
    population = sum(population),
    across(
      phase1:phase5,
      sum
    ),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      phase1:phase5,
      ~ .x / population,
      .names = "{.col}_pct"
    )
  ) %>%
  rename_with(
    .fn = ~ paste0(.x, "_num"),
    .cols = phase1:phase5
  )

# pull into scores
df_ch_scores <- df_ch %>%
  group_by(
    iso3
  ) %>%
  filter(
    analysis_year >= 2019,
    (period == "current") | (period == "projected" & analysis_period_start == max(analysis_period_start)),
  ) %>%
  summarize(
    avg_phase4_pct = mean(phase4_pct),
    latest_date = max(analysis_period_start),
    phase4_num_latest = phase4_num[analysis_period_start == latest_date],
    phase4_pct_latest = phase4_pct[analysis_period_start == latest_date],
    phase5_num_latest = phase5_num[analysis_period_start == latest_date],
    phase5_pct_latest = phase5_pct[analysis_period_start == latest_date],
    previous_date = if_else(
      any(analysis_year <= 2021),
      max(analysis_period_start[analysis_year <= 2021 & analysis_period_start != latest_date]),
      min(analysis_period_start)
    ),
    phase5_num_previous = phase5_num[analysis_period_start == previous_date],
    phase4pl_scale = phase4_num_latest / 100000 + phase5_num_latest / 10000,
    phase4pl_prev = phase4_pct_latest + phase5_pct_latest,
    phase5_incr = max(0, phase5_num_latest - phase5_num_previous)
  ) %>%
  mutate(
    country = countrycode(iso3, origin = "iso3c", destination = "country.name"),
    .before = iso3
  )

#######################
#### FOOD SECURITY ####
#######################

# pull all the food security data together
df_fs <- bind_rows(
  df_ch_scores,
  df_ipc_scores
)

#####################
#### INFORM DATA ####
#####################

# severity
df_inform_sev <- pull_dataset("inform-global-crisis-severity-index") %>%
  get_resource(1) %>%
  read_resource(sheet = "INFORM Severity - country")

names(df_inform_sev) <- unlist(df_inform_sev[1,])

df_inform_sev <- df_inform_sev %>%
  select(
    iso3 = ISO3,
    inform_severity = `INFORM Severity Index`
  ) %>%
  slice(4:nrow(.)) %>%
  mutate(
    inform_severity = parse_number(inform_severity)
  )

# climate change
GET(
  "https://drmkc.jrc.ec.europa.eu/inform-index/Portals/0/InfoRM/2022/INFORM%20CC%20Brochure%20data.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

df_inform_cc <- read_excel(tf, skip = 4) %>%
  transmute(
    iso3 = countryname(Country, destination = "iso3c"),
    inform_cc = `INFORM CC Risk Index 2022`
  ) %>%
  type_convert()


# risk
GET(
  "https://drmkc.jrc.ec.europa.eu/inform-index/Portals/0/InfoRM/2022/INFORM_Risk_2023__v065.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

df_inform_risk <- read_excel(tf, sheet = "INFORM Risk 2023 (a-z)") %>%
  slice(-1) %>%
  select(
    iso3 = ISO3,
    inform_risk = `INFORM RISK`
  ) %>%
  type_convert()

# pull all the inform together

df_inform <- full_join(df_inform_cc, df_inform_risk, by = "iso3") %>%
  full_join(df_inform_sev, by = "iso3")

########################
#### FINAL ANALYSIS ####
########################

left_join(
  df_fs,
  df_inform,
  by = "iso3"
) %>%
  mutate(
    across(
      .cols = c(avg_phase4_pct, phase4pl_scale, phase4pl_prev, phase5_incr, starts_with("inform")),
      .fns = ~ 100 * (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)),
      .names = "{.col}_norm"
    ),
    across(
      .cols = ends_with("norm"),
      .fns = ~ replace_na(.x, 0)
    ),
    cerf_score = .15 * avg_phase4_pct_norm + .45 * phase4pl_scale_norm + .15 * phase4pl_prev_norm +.1 * phase5_incr_norm + .05 * inform_severity_norm + .05 * inform_risk_norm + .05 * inform_cc_norm,
    cerf_rank = rank(cerf_score)
  ) %>%
  arrange(
    desc(cerf_score)
  ) %>%
  mutate(
    cerf_rank = row_number()
  ) %>%
  write_csv("cerf_ranking.csv")

