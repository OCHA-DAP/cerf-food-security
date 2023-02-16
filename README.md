
# CERF Food Security rankings

The goal of cerf-food-security is to help UN CERF rank food security crises. The
analysis is simple, and contained solely within `analysis.R`, producing a single
CSV output. The description of the CSV and analysis is below.

## Methodology

Food security estimates rely on data on food insecurity from the IPC and CH in respective
countries, and the use of INFORM severity and risk indices to provide a measure of potential
upcoming risks alongside these food security estimates.

### Food security

A few different aspects of food security in a country are estimated.

**Scale**: The scale of the crisis is estimated by looking at the number of people
in phases 4 and 5 in the most recent analysis period, which is always a projection.
The scale figure is generated by assigning a single point for every 100,000 persons
in phase 4 and for every 10,000 in phase 5. Thus, 150,000 in phase 4 and 10,000
in phase 5 for a country would result in a score of 2.5.

**Prevalence**: To account for prevalence of food insecurity as relative to the
population, not simply the raw scale, % of population in phases 4 and 5 in the
latest projected period is used.

**Protacted crises**: The protracted nature of a crisis is accounted for by looking at the average
% of population in phases 4 and 5 across all current analyses from 2019 to
today, as well as in the latest projected analysis.

**Acute crises**: Acute crises are measured by looking at the # of population
in phase 5 in the latest projected analysis compared to the last current analysis
in 2021. 

The latest projected analysis period is used above because it corresponds to the
most recent time period for all countries, although these do not overlap across
countries due to different schedules for IPC analyses. The previous period from
2021 is used to estimate change from today. To get this, either the most
recent current analysis from 2021 and earlier is used, or if there is no analysis
from 2021 or before, the earliest analysis is used.

### INFORM

[INFORM](https://drmkc.jrc.ec.europa.eu/inform-index)
has its own methodology for analysis, and produces a severity index measuring
current crisis severity across a range of countries, a risk index that measures
the risk of humanitarian crisis globally, and a new climate change index that
measures the risk climate change poses to countries. All 3 of these indices are
used in the ranking together with the food security figures.

### Combined score

The 4 food security indicators and 3 INFORM indices above are used together to
create a combined score. All of the indicators are first normalized to a 0-100
scale based on the observed min and max (with 0s assigned to country's missing
INFORM scores). These normalized indicators are then combinated using a weighted
average with the following weights:

| Indicator         | Weight          |
| ----------------- | --------------- |
| **Food security** | **$\bf{85}\\%$** |
| Scale             | $45\\%$          |
| Prevalence        | $15\\%$          |
| Protected crises  | $15\\%$          |
| Acute crises      | $10\\%$          |
| **INFORM**        | **$\bf{15}\\%$** |
| Climate Change    | $5\\%$           |
| Severity          | $5\\%$           |
| Risk              | $5\\%$           |

## CSV structure

| Column name          | Description |
| -------------------- | ----------- |
| country              | Country name |
| iso3                 | Country ISO3 code |
| latest_date          | Start date of the latest analysis period (CH reference period); note it is always a projection |
| phase4_num_latest    | # of population in phase 4 in this latest projection analysis |
| phase4_pct_latest    | % of population in phase 4 in this latest projection analysis |
| phase5_num_latest    | # of population in phase 5 in this latest projection analysis |
| phase5_pct_latest    | % of population in phase 5 in this latest projection analysis |
| previous_date        | Start date of the last current analysis in 2021 (or later if no analysis prior to 2021) |
| phase5_num_previous  | # of population in phase 5 in this previous current analysis |
| avg_phase4_pct       | Average % of population in phases 4 and 5, for all analyses from 2019 to today and the latest projections |
| phase4pl_scale       | Scale & severity of food insecurity, measured by adding a point for every 100,000 population in phase 4 and 10,000 in phase 5 in the latest (projected) analysis |
| phase4pl_prev        | Prevalence of food insecurity measured as % of population in phases 4 and 5 in the latest (projected) analysis |
| phase5_incr          | Increase in # of population in phase 5 between latest (projected) analysis and 2021 (or around there) current analysis |
| inform_cc            | Latest INFORM Climate Change index score |
| inform_risk          | Latest INFORM Risk index score |
| inform_severity      | Latest INFORM Severity index score |
| avg_phase4_pct_norm  | Variable above normalized to 0 - 100 based on observations |
| phase4pl_scale_norm  | Variable above normalized to 0 - 100 based on observations |
| phase4pl_prev_norm   | Variable above normalized to 0 - 100 based on observations |
| phase5_incr_norm     | Variable above normalized to 0 - 100 based on observations |
| inform_cc_norm       | Variable above normalized to 0 - 100 based on observations |
| inform_risk_norm     | Variable above normalized to 0 - 100 based on observations |
| inform_severity_norm | Variable above normalized to 0 - 100 based on observations |
| cerf_score           | Score calculated as described in the methods section above |
| cerf_rank            | Country ranking based on this score (1 being most severe) |
 
