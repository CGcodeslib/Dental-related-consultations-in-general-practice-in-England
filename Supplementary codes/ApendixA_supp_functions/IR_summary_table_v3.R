# incidence rate summary table create
# this is to create summary table for PY by each group and overall PY and for caluclate IR 
# Author: CG


# please note
#       1. incidence rate here need to consider all dental visits instead of just new cases 
#       2. person year definition remain unchanged based on individual follow-up time 


# this code contains two parts of codes: 
#       1. summary for overall: ir_overall_summary
#       2. summary for annual: ir_annual_summary 



##################################################################
library(dplyr)
library(rlang)

# data:   your dataframe
# var:    grouping variable (unquoted), e.g. gender
# py:     person-year column (unquoted), e.g. person_year
# cases:  dental visit count column (unquoted), e.g. dental_visit_count
# ...:    optional dplyr filters (apply BEFORE grouping)
# drop:   whether to drop unused factor levels in 'var'
library(dplyr)

# Returns: variable, level, total_person_year, total_cases
make_ir_table <- function(data, py, cases, var = NULL, ..., drop = TRUE) {
  filters <- rlang::enquos(...)
  df <- if (length(filters) > 0) dplyr::filter(data, !!!filters) else data
  
  if (is.null(var)) {
    df %>%
      summarise(
        total_person_year = sum(.data[[py]], na.rm = TRUE),
        total_cases       = sum(.data[[cases]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(variable = "overall", level = "overall") %>%
      dplyr::select(variable, level, total_person_year, total_cases)
  } else {
    df %>%
      group_by(.data[[var]], .drop = drop) %>%
      summarise(
        total_person_year = sum(.data[[py]], na.rm = TRUE),
        total_cases       = sum(.data[[cases]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(variable = var, level = as.character(.data[[var]])) %>%
      dplyr::select(variable, level, total_person_year, total_cases)
  }
}

##############################multi variable IR table ######################
make_ir_table_combo <- function(data, py, cases, vars, drop = TRUE) {
  # data  = dataset
  # py    = column name (string) for person-years
  # cases = column name (string) for case counts
  # vars  = character vector of grouping variables (e.g. c("gender","region"))
  # drop  = whether to drop unused factor levels
  
  data %>%
    # group by all variables in vars together
    dplyr::group_by(dplyr::across(dplyr::all_of(vars)), .drop = drop) %>%
    
    # sum person-years and cases for each combination
    dplyr::summarise(
      total_person_year = sum(.data[[py]],    na.rm = TRUE),
      total_cases       = sum(.data[[cases]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    # order columns: grouping vars first, then results
    dplyr::select(dplyr::all_of(vars), total_person_year, total_cases)
}

##############################################################################
library(dplyr)
library(lubridate)

# Count visits per calendar year (and optional group columns)
# visits: data frame with at least a date column (Date or parseable to Date)
# date:   string, name of the visit date column (e.g., "visit_date")
# years:  integer vector of years to include
# group_cols: character vector of column names to group by in addition to year
library(dplyr)
library(tidyr)
library(lubridate)
library(rlang)

# Compute person-time per calendar year by slicing follow-up intervals.
# - data: your cohort (one row per person)
# - id:   unique id column (unquoted), e.g., patid
# - start,end: follow-up start/end date columns (unquoted), Date class
# - years: vector of years to report (e.g., 1998:2023)
# - denom: "gregorian" => divide by 365/366 per year; "365.25" or "365" also allowed
library(dplyr)
library(tidyr)
library(lubridate)
library(rlang)

# Annual person-time; optionally return totals per year
library(dplyr)
library(tidyr)
library(lubridate)
library(rlang)

# Annual person-time with fixed 365.25 denominator.
# Set aggregate = TRUE to return totals per year.
annual_person_time <- function(data, id, start, end, years = 1998:2023, aggregate = FALSE) {
  idq <- enquo(id); stq <- enquo(start); enq <- enquo(end)
  
  # clamp to requested window
  min_date <- make_date(min(years), 1, 1)
  max_date <- make_date(max(years), 12, 31)
  
  df <- data %>%
    mutate(
      .start = as.Date(!!stq),
      .end   = as.Date(!!enq)
    ) %>%
    filter(!is.na(.start), !is.na(.end)) %>%
    mutate(
      .start = if_else(.start < min_date, min_date, .start),
      .end   = if_else(.end   > max_date, max_date, .end)
    ) %>%
    filter(.end >= .start) %>%
    mutate(.y_start = year(.start), .y_end = year(.end)) %>%
    rowwise() %>%
    mutate(.years = list(seq(.y_start, .y_end))) %>%
    ungroup() %>%
    dplyr::select(!!idq, .start, .end, .years)
  
  pt_year <- df %>%
    unnest_longer(.years, values_to = "year") %>%
    mutate(
      year_start = make_date(year, 1, 1),
      year_end   = make_date(year, 12, 31),
      seg_start  = if_else(.start > year_start, .start, year_start),
      seg_end    = if_else(.end   < year_end,   .end,   year_end),
      days       = as.integer(seg_end - seg_start) + 1L
    ) %>%
    filter(days > 0L) %>%
    transmute(year, !!idq, person_years = days / 365.25)
  
  if (isTRUE(aggregate)) {
    return(
      pt_year %>%
        group_by(year) %>%
        summarise(total_person_years = sum(person_years, na.rm = TRUE), .groups = "drop")
    )
  }
  
  pt_year
}


######################### annual IR by subgroups #################################

#### annual event count
library(dplyr)
library(rlang)

annual_cases_subgroups <- function(
    events_df,
    event_year,       # unquoted, e.g. obsyear
    years,            # e.g. 1998:2023
    group_vars        # character vector, e.g. "gender" or c("gender","imd")
) {
  stopifnot(length(group_vars) >= 1)
  
  events_df %>%
    filter(between({{ event_year }}, min(years), max(years))) %>%
    group_by(across(all_of(group_vars)), year = {{ event_year }}) %>%
    summarise(cases = n(), .groups = "drop")
}


#### annual py
library(dplyr)
library(rlang)

annual_py_subgroups <- function(
    sample_df,
    id,           # unquoted: patid
    start,        # unquoted: fu_start_date
    end,          # unquoted: censor_date
    years,        # e.g., 1998:2023
    group_vars    # character vector, e.g. "gender" or c("gender","imd")
) {
  stopifnot(length(group_vars) >= 1)
  
  sample_df %>%
    group_by(across(all_of(group_vars))) %>%
    group_modify(~
                   annual_person_time(
                     data      = .x,
                     id        = {{ id }},
                     start     = {{ start }},
                     end       = {{ end }},
                     years     = years,
                     aggregate = TRUE
                   )
    ) %>%
    ungroup()
}

######################## annual ir by age #################
library(dplyr)
library(tidyr)
library(rlang)
library(dplyr)
library(tidyr)
library(rlang)
library(dplyr)
library(tidyr)
library(lubridate)
library(rlang)


annual_py_by_age <- function(sample_df, id, start, end, yob, years, group_vars = NULL) {
  id_sym <- ensym(id)
  st     <- enquo(start)
  en     <- enquo(end)
  yb     <- enquo(yob)
  
  # 1) standardize + clamp to window
  df2 <- sample_df %>%
    transmute(
      id    = !!id_sym,
      start = as.integer(!!st),
      end   = as.integer(!!en),
      yob   = as.integer(!!yb),
      !!!syms(group_vars %||% character())   # bring grouping cols if any
    ) %>%
    filter(!is.na(id), !is.na(start), !is.na(end), !is.na(yob)) %>%
    mutate(
      start = pmax(start, min(years, na.rm = TRUE)),
      end   = pmin(end,   max(years, na.rm = TRUE))
    ) %>%
    filter(end >= start)
  
  # 2) expand to id × year, compute age band (integer-year logic)
  df_years <- df2 %>%
    rowwise() %>%
    mutate(year = list(seq.int(start, end))) %>%
    ungroup() %>%
    unnest_longer(year) %>%
    mutate(
      age = year - yob,
      age_band = case_when(
        age < 20 ~ "Children (0-19)",
        age >= 20 & age < 60 ~ "Adults (20-59)",
        age >= 60             ~ "Older adults (60+)",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age_band))
  
  # 3) count PY by year × age_band × group_vars
  out <- df_years %>%
    {
      if (length(group_vars)) count(., across(all_of(group_vars)), year, age_band, name = "total_person_years")
      else                    count(., year, age_band, name = "total_person_years")
    }
  
  # 4) ensure all years × bands within each group
  all_bands <- c("Children (0-19)", "Adults (20-59)", "Older adults (60+)")
  out %>%
    {
      if (length(group_vars)) group_by(., across(all_of(group_vars))) else .
    } %>%
    complete(
      year     = years,
      age_band = all_bands,
      fill     = list(total_person_years = 0L)
    ) %>%
    ungroup() %>%
    arrange(across(all_of(group_vars), .names = "{.col}"), year, age_band)
}



#################################### monthly person year####################################
library(dplyr)
library(tidyr)
library(lubridate)

monthly_person_time <- function(data, id, start, end,
                                years = 1998:2023,
                                aggregate = FALSE) {
  idq <- enquo(id); stq <- enquo(start); enq <- enquo(end)
  
  # clamp to requested window
  min_date <- make_date(min(years), 1, 1)
  max_date <- make_date(max(years), 12, 31)
  
  df <- data %>%
    dplyr::mutate(
      .start = as.Date(!!stq),
      .end   = as.Date(!!enq)
    ) %>%
    dplyr::filter(!is.na(.start), !is.na(.end)) %>%
    dplyr::mutate(
      .start = dplyr::if_else(.start < min_date, min_date, .start),
      .end   = dplyr::if_else(.end   > max_date, max_date, .end)
    ) %>%
    dplyr::filter(.end >= .start) %>%
    dplyr::mutate(
      .m_start = lubridate::floor_date(.start, "month"),
      .m_end   = lubridate::floor_date(.end,   "month")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.months = list(seq(.m_start, .m_end, by = "month"))) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!idq, .start, .end, .months)
  
  pt_month <- df %>%
    tidyr::unnest_longer(.months, values_to = "month_start") %>%
    dplyr::mutate(
      month_end = (month_start %m+% months(1)) - days(1),
      
      seg_start = dplyr::if_else(.start > month_start, .start, month_start),
      seg_end   = dplyr::if_else(.end   < month_end,   .end,   month_end),
      days      = as.integer(seg_end - seg_start) + 1L,
      
      year  = lubridate::year(month_start),
      month = lubridate::month(month_start)
    ) %>%
    dplyr::filter(days > 0L) %>%
    dplyr::transmute(
      year,
      month,
      !!idq,
      person_years = days / 365.25
    )
  
  if (isTRUE(aggregate)) {
    return(
      pt_month %>%
        dplyr::group_by(year, month) %>%
        dplyr::summarise(
          total_person_years = sum(person_years, na.rm = TRUE),
          .groups = "drop"
        )
    )
  }
  
  pt_month
}


#################################### semi-monthly person year (minimal change from your monthly code) ####################################
#################################### semi-month person year (1–15, 16–EOM) ####################################
#################################### semi-month person year (1–15, 16–EOM) ####################################
library(dplyr)
library(lubridate)
library(data.table)

semimonth_person_time <- function(data, id, start, end,
                                  aggregate = FALSE) {
  idq <- enquo(id); stq <- enquo(start); enq <- enquo(end)
  
  # Prepare follow-up intervals
  dt <- as.data.table(data)
  dt[, .start := as.IDate(as.Date(eval_tidy(stq, dt)))]
  dt[, .end   := as.IDate(as.Date(eval_tidy(enq, dt)))]
  dt <- dt[!is.na(.start) & !is.na(.end) & .end >= .start]
  
  # If you REALLY need individual output, it can be enormous for big cohorts
  # (and will likely crash). The memory-safe path is aggregate=TRUE.
  if (!isTRUE(aggregate)) {
    stop("For large cohorts, individual semi-month person-time is too large. Use aggregate = TRUE to get totals by year-month-period.")
  }
  
  # Build all semi-month periods covering observed follow-up range
  min_m <- floor_date(as.Date(min(dt$.start)), "month")
  max_m <- floor_date(as.Date(max(dt$.end)),   "month")
  month_starts <- seq(min_m, max_m, by = "month")
  
  periods <- data.table(
    month_start = as.IDate(month_starts)
  )
  periods[, month_end := as.IDate((as.Date(month_start) %m+% months(1)) - days(1))]
  periods[, `:=`(
    year  = year(as.Date(month_start)),
    month = month(as.Date(month_start))
  )]
  
  # Two periods per month
  pA <- periods[, .(
    year, month,
    period = "01-15",
    p_start = as.IDate(as.Date(month_start)),
    p_end   = as.IDate(as.Date(month_start) + days(14))
  )]
  
  pB <- periods[, .(
    year, month,
    period = "16-EOM",
    p_start = as.IDate(as.Date(month_start) + days(15)),
    p_end   = month_end
  )]
  
  per <- rbind(pA, pB)
  setorder(per, year, month, period)
  
  # Compute total overlap days for each period WITHOUT creating person-month rows
  out <- per[, {
    idx <- dt[.start <= p_end & .end >= p_start]
    if (nrow(idx) == 0L) {
      list(total_person_years = 0)
    } else {
      overlap_days <- as.integer(pmin(idx$.end, p_end) - pmax(idx$.start, p_start)) + 1L
      list(total_person_years = sum(overlap_days, na.rm = TRUE) / 365.25)
    }
  }, by = .(year, month, period, p_start, p_end)]
  
  # Return in the same shape you were using downstream
  as.data.frame(out)[, c("year","month","period","total_person_years")]
}



