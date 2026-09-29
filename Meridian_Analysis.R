# ======================================================================
# Meridian Open University: Board Review - R code
# Team: The R five (Divyanshi, Jatin, Chaarvi, Anubhav, Arsee)
# This script contains all the code from Meridian_Analysis.Rmd, in order.
# The full report with narration and business explanations is the knitted
# Meridian_Analysis.html. Run this script from the folder that contains the
# 'Data' folder (in RStudio: Session > Set Working Directory > To Source
# File Location), then Source it.
# ======================================================================

## ----setup, message=FALSE, warning=FALSE--------------------------------------
# WHAT THIS CHUNK DOES: Load the packages (toolboxes) the whole report uses
# Hide package and file-reading messages in the report
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

# Load the packages we need
library(tidyverse)   # data handling and charts
library(data.table)  # fast file reading for the big click file
library(scales)      # nicer percentage labels on charts
library(knitr)       # neat tables in the html report


## ----load-data----------------------------------------------------------------
# WHAT THIS CHUNK DOES: Read the 7 data files into R and count their rows
# Read the six smaller tables
studentInfo         <- read_csv("Data/studentInfo.csv")
studentRegistration <- read_csv("Data/studentRegistration.csv")
courses             <- read_csv("Data/courses.csv")
assessments         <- read_csv("Data/assessments.csv")
studentAssessment   <- read_csv("Data/studentAssessment.csv")
vle                 <- read_csv("Data/vle.csv")

# The click file is very large, so we use fread, which is much faster
studentVle <- fread("Data/studentVle.csv")

# Check how many rows and columns each table has
tibble(
  table = c("studentInfo", "studentRegistration", "courses", "assessments",
            "studentAssessment", "vle", "studentVle"),
  rows  = c(nrow(studentInfo), nrow(studentRegistration), nrow(courses),
            nrow(assessments), nrow(studentAssessment), nrow(vle), nrow(studentVle))
)


## ----data-checks--------------------------------------------------------------
# WHAT THIS CHUNK DOES: Check the data can be trusted: unique IDs, working links, missing values
# The three columns that identify one student in one module-run
k3 <- c("code_module", "code_presentation", "id_student")

# A small helper: TRUE if the given columns uniquely identify every row
is_unique <- function(df, cols) {
  nrow(dplyr::distinct(df, dplyr::across(tidyselect::all_of(cols)))) == nrow(df)
}

# 1. Primary key checks
tibble(
  table = c("studentInfo", "studentRegistration", "courses",
            "assessments", "studentAssessment", "vle"),
  key   = c("module + presentation + student", "module + presentation + student",
            "module + presentation", "id_assessment",
            "id_assessment + id_student", "id_site"),
  unique = c(is_unique(studentInfo, k3),
             is_unique(studentRegistration, k3),
             is_unique(courses, c("code_module", "code_presentation")),
             is_unique(assessments, "id_assessment"),
             is_unique(studentAssessment, c("id_assessment", "id_student")),
             is_unique(vle, "id_site"))
) %>% kable(caption = "Primary key checks")

# 2. Link checks: how many rows fail to find a match (0 = all linked)
tibble(
  link = c("studentRegistration -> studentInfo",
           "studentInfo -> courses",
           "assessments -> courses",
           "vle -> courses",
           "studentAssessment -> assessments",
           "studentVle -> vle",
           "studentVle -> studentInfo"),
  unmatched_rows = c(nrow(anti_join(studentRegistration, studentInfo, by = k3)),
                     nrow(anti_join(studentInfo, courses,
                                    by = c("code_module", "code_presentation"))),
                     nrow(anti_join(assessments, courses,
                                    by = c("code_module", "code_presentation"))),
                     nrow(anti_join(vle, courses,
                                    by = c("code_module", "code_presentation"))),
                     sum(!studentAssessment$id_assessment %in% assessments$id_assessment),
                     sum(!studentVle$id_site %in% vle$id_site),
                     nrow(anti_join(as_tibble(unique(studentVle[, ..k3])),
                                    studentInfo, by = k3)))
) %>% kable(caption = "Link checks")

# 3. Missing values in each table (only columns with at least one gap)
list(studentInfo = studentInfo, studentRegistration = studentRegistration,
     courses = courses, assessments = assessments,
     studentAssessment = studentAssessment, vle = vle) %>%
  map_dfr(~ tibble(column = names(.x), missing = colSums(is.na(.x))), .id = "table") %>%
  filter(missing > 0) %>%
  kable(caption = "Missing values")


## ----cleaning-----------------------------------------------------------------
# WHAT THIS CHUNK DOES: Fix messy labels, fill blank exam dates, and order categories for charts
# Clean the student table
studentInfo_clean <- studentInfo %>%
  mutate(
    # One band is written "10-20" without the % sign; make it consistent
    imd_band = recode(imd_band, "10-20" = "10-20%"),
    # Keep missing deprivation as its own group
    imd_band = replace_na(imd_band, "Unknown"),
    imd_band = factor(imd_band, levels = c("0-10%", "10-20%", "20-30%", "30-40%",
                                           "40-50%", "50-60%", "60-70%", "70-80%",
                                           "80-90%", "90-100%", "Unknown")),
    # Put categories in a sensible order for charts
    age_band = factor(age_band, levels = c("0-35", "35-55", "55<=")),
    highest_education = factor(highest_education,
                               levels = c("No Formal quals", "Lower Than A Level",
                                          "A Level or Equivalent", "HE Qualification",
                                          "Post Graduate Qualification")),
    final_result = factor(final_result,
                          levels = c("Distinction", "Pass", "Fail", "Withdrawn")),
    # A simple two-group version of the outcome (TRUE = Distinction or Pass)
    completed_flag = final_result %in% c("Distinction", "Pass")
  )

# Fill the 11 blank exam dates with the module length (end of presentation)
assessments_clean <- assessments %>%
  left_join(courses, by = c("code_module", "code_presentation")) %>%
  mutate(date = if_else(is.na(date) & assessment_type == "Exam",
                        module_presentation_length, date))

# Quick check: overall outcome split
studentInfo_clean %>%
  count(final_result) %>%
  mutate(share = percent(n / sum(n), accuracy = 0.1)) %>%
  kable(caption = "Final results across all registrations")


## ----master-table-------------------------------------------------------------
# WHAT THIS CHUNK DOES: Build one big table: one row per student per module, with their clicks and assignments
# ---- A. Summarise the click log to one row per student per module-run ----
# data.table syntax is used here because studentVle has 10.6 million rows
vle_summary <- studentVle[, .(
  total_clicks         = sum(sum_click),
  active_days          = uniqueN(date),
  last_active_day      = max(date),
  clicks_before_start  = sum(sum_click[date < 0]),
  clicks_first_14_days = sum(sum_click[date >= 0 & date < 14])
), by = .(code_module, code_presentation, id_student)] %>%
  as_tibble()

# ---- B. Summarise assessments (non-exam only) ----
non_exam <- assessments_clean %>% filter(assessment_type != "Exam")

# How many non-exam assessments each module-run has
available <- non_exam %>%
  count(code_module, code_presentation, name = "n_assess_available")

# The first assessment of each module-run (earliest date)
first_assess <- non_exam %>%
  arrange(code_module, code_presentation, date, id_assessment) %>%
  group_by(code_module, code_presentation) %>%
  slice(1) %>%
  ungroup() %>%
  select(code_module, code_presentation, id_assessment)

# Attach module, presentation and due date to every submission
subs <- studentAssessment %>%
  inner_join(non_exam %>% select(id_assessment, code_module,
                                 code_presentation, due_date = date),
             by = "id_assessment") %>%
  # Banked results were submitted in an earlier run, so their date cannot be
  # compared with this run's due date: never count them as late
  mutate(is_late = is_banked == 0 & date_submitted > due_date)

# One row per student: how many submitted, average score, how many late
assess_summary <- subs %>%
  group_by(code_module, code_presentation, id_student) %>%
  summarise(n_submitted = n(),
            n_timed     = sum(is_banked == 0),   # submissions whose timing can be judged
            avg_score   = mean(score, na.rm = TRUE),
            n_late      = sum(is_late, na.rm = TRUE),
            .groups = "drop")

# Which students submitted their module's first assessment
first_sub <- subs %>%
  semi_join(first_assess,
            by = c("code_module", "code_presentation", "id_assessment")) %>%
  distinct(code_module, code_presentation, id_student) %>%
  mutate(submitted_first = TRUE)

# ---- C. Join everything onto the student table ----
master <- studentInfo_clean %>%
  left_join(studentRegistration, by = k3) %>%
  left_join(courses, by = c("code_module", "code_presentation")) %>%
  left_join(vle_summary, by = k3) %>%
  left_join(available, by = c("code_module", "code_presentation")) %>%
  left_join(assess_summary, by = k3) %>%
  left_join(first_sub, by = k3) %>%
  mutate(
    # No rows in the click log or submissions = zero activity
    across(c(total_clicks, active_days, clicks_before_start,
             clicks_first_14_days, n_submitted, n_timed, n_late), ~ replace_na(.x, 0)),
    submitted_first = replace_na(submitted_first, FALSE),
    submit_rate     = n_submitted / n_assess_available,
    never_clicked   = total_clicks == 0
  )

# Check: still one row per student per module-run?
nrow(master)

# First look: how the four outcome groups differ
master %>%
  group_by(final_result) %>%
  summarise(students            = n(),
            median_clicks       = median(total_clicks),
            median_active_days  = median(active_days),
            median_clicks_first_14_days = median(clicks_first_14_days),
            pct_never_clicked   = percent(mean(never_clicked), 0.1),
            avg_submit_rate     = percent(mean(submit_rate), 0.1),
            avg_score           = round(mean(avg_score, na.rm = TRUE), 1),
            pct_submitted_first = percent(mean(submitted_first), 0.1)) %>%
  kable(caption = "How the four outcome groups differ")


## ----groups-and-colours-------------------------------------------------------
# WHAT THIS CHUNK DOES: Group credits and repeat attempts, and fix one colour per result for all charts
# Group credits and previous attempts into simple categories
master <- master %>%
  mutate(
    credit_group  = cut(studied_credits, breaks = c(0, 60, 120, Inf),
                        labels = c("60 or less", "61-120", "More than 120")),
    attempt_group = if_else(num_of_prev_attempts > 0, "Repeat", "First attempt")
  )

# One fixed colour per outcome, used in every chart
outcome_colours <- c("Distinction" = "#1b7837", "Pass" = "#7fbf7b",
                     "Fail" = "#f4a582", "Withdrawn" = "#b2182b")


## ----enrolment-profile, fig.height=8, fig.width=9-----------------------------
# WHAT THIS CHUNK DOES: Chart who signs up (age, education, credits, and so on)
master %>%
  select(gender, age_band, highest_education, disability,
         credit_group, attempt_group) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(everything(), names_to = "characteristic", values_to = "group") %>%
  count(characteristic, group) %>%
  group_by(characteristic) %>%
  mutate(share = n / sum(n)) %>%
  ggplot(aes(x = share, y = group)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = if_else(share >= 0.04, percent(share, 1), "")), hjust = -0.1, size = 3) +
  facet_wrap(~ characteristic, scales = "free_y", ncol = 2) +
  scale_x_continuous(labels = percent, limits = c(0, 1.1)) +
  labs(title = "Who enrols at Meridian", x = "Share of registrations", y = NULL)


## ----outcome-function---------------------------------------------------------
# WHAT THIS CHUNK DOES: A reusable function that draws "results by group" for any column
# Draws a stacked bar of final results for any student characteristic
plot_outcome <- function(var, label) {
  outcome_data <- dplyr::count(master, {{ var }}, rlang::.data$final_result) |>
    dplyr::group_by({{ var }}) |>
    dplyr::mutate(share = rlang::.data$n / sum(rlang::.data$n)) |>
    dplyr::ungroup()

  ggplot2::ggplot(
    outcome_data,
    ggplot2::aes(x = {{ var }}, y = rlang::.data$share,
                 fill = rlang::.data$final_result)
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(rlang::.data$share, 1)),
      position = ggplot2::position_stack(vjust = 0.5), size = 3
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_manual(values = outcome_colours) +
    ggplot2::labs(title = paste("Final result by", label),
                  x = NULL, y = "Share of registrations", fill = "Final result")
}

## ----outcome-by-group, fig.height=4, fig.width=9------------------------------
# WHAT THIS CHUNK DOES: Results by credits, education, deprivation and repeat attempts
plot_outcome(credit_group, "credits studied")
plot_outcome(highest_education, "highest education")
plot_outcome(imd_band, "deprivation band")
plot_outcome(attempt_group, "previous attempts")


## ----chi-square-tests---------------------------------------------------------
# WHAT THIS CHUNK DOES: Test which background factors are linked to results, and how strongly (Cramer's V)
test_vars <- c("gender", "region", "age_band", "highest_education", "imd_band",
               "disability", "credit_group", "attempt_group", "code_module")

map_dfr(test_vars, function(v) {
  tab  <- table(master[[v]], master$final_result)
  test <- suppressWarnings(chisq.test(tab))
  tibble(characteristic = v,
         chi_square = round(unname(test$statistic), 1),
         df         = unname(test$parameter),
         p_value    = format.pval(test$p.value, digits = 2),
         cramers_v  = round(sqrt(unname(test$statistic) /
                                 (sum(tab) * (min(dim(tab)) - 1))), 3))
}) %>%
  arrange(desc(cramers_v)) %>%
  kable(caption = "Chi-square tests: characteristic vs final result")


## ----outcome-by-module, fig.height=4, fig.width=9-----------------------------
# WHAT THIS CHUNK DOES: Results for each module (course)
plot_outcome(code_module, "module")


## ----withdrawal-heatmap, fig.height=5, fig.width=8----------------------------
# WHAT THIS CHUNK DOES: Quit rate for every module in every year, as a coloured grid
# Withdrawal rate for every module-run
master %>%
  group_by(code_module, code_presentation) %>%
  summarise(withdrawal_rate = mean(final_result == "Withdrawn"),
            students = n(), .groups = "drop") %>%
  ggplot(aes(x = code_presentation, y = code_module, fill = withdrawal_rate)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = paste0(percent(withdrawal_rate, 0.1),
                               "\n(n = ", students, ")")), size = 3) +
  scale_fill_gradient(low = "#fee0d2", high = "#b2182b", labels = percent) +
  labs(title = "Withdrawal rate by module and presentation",
       subtitle = "Blank cells: module not run in that presentation",
       x = "Presentation", y = "Module", fill = "Withdrawal rate")


## ----outcome-by-presentation, fig.height=4, fig.width=9-----------------------
# WHAT THIS CHUNK DOES: Results by year, plus a fair comparison using only modules that ran in both years
plot_outcome(code_presentation, "presentation")

# Fair comparison: the module mix changed between years (CCC, the module with
# the highest withdrawal, only ran in 2014), so also compare using only the
# modules that ran in both 2013J and 2014J
both_years <- master %>%
  filter(code_presentation %in% c("2013J", "2014J")) %>%
  distinct(code_module, code_presentation) %>%
  count(code_module) %>%
  filter(n == 2) %>%
  pull(code_module)

bind_rows(
  master %>% filter(code_presentation %in% c("2013J", "2014J")) %>%
    mutate(comparison = "All modules"),
  master %>% filter(code_presentation %in% c("2013J", "2014J"),
                    code_module %in% both_years) %>%
    mutate(comparison = paste0("Same modules only (", paste(both_years, collapse = ", "), ")"))
) %>%
  group_by(comparison, code_presentation) %>%
  summarise(withdrawal_rate = percent(mean(final_result == "Withdrawn"), 0.1),
            .groups = "drop") %>%
  pivot_wider(names_from = code_presentation, values_from = withdrawal_rate) %>%
  kable(caption = "Withdrawal rate, 2013J vs 2014J")


## ----withdrawal-timing, fig.height=4.5, fig.width=9---------------------------
# WHAT THIS CHUNK DOES: When students quit: before the start, in the first month, or later
# Withdrawn students with a recorded unregistration date
withdrawn <- master %>%
  filter(final_result == "Withdrawn", !is.na(date_unregistration))

ggplot(withdrawn, aes(x = date_unregistration)) +
  geom_histogram(binwidth = 7, fill = "#b2182b", colour = "white") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  annotate("text", x = 5, y = Inf, label = "Day 0",
           hjust = 0, vjust = 1.5) +
  labs(title = "When withdrawn students unregister",
       subtitle = "Each bar is one week",
       x = "Day of unregistration (relative to day 0)",
       y = "Number of students")

# Timing in periods, with the running total
withdrawn %>%
  mutate(period = cut(date_unregistration,
                      breaks = c(-Inf, 0, 30, 60, 90, 120, 180, Inf),
                      labels = c("On or before day 0", "Days 1-30", "Days 31-60",
                                 "Days 61-90", "Days 91-120", "Days 121-180",
                                 "After day 180"))) %>%
  count(period) %>%
  mutate(share      = percent(n / sum(n), 0.1),
         cumulative = percent(cumsum(n) / sum(n), 0.1)) %>%
  kable(caption = "When withdrawals happen")

# Share of each module's withdrawals that happen on or before day 0
withdrawn %>%
  group_by(code_module) %>%
  summarise(withdrawals = n(),
            pct_on_or_before_day_0 = percent(mean(date_unregistration <= 0), 0.1)) %>%
  kable(caption = "Withdrawals on or before day 0, by module")


## ----weekly-engagement, fig.height=5, fig.width=9-----------------------------
# WHAT THIS CHUNK DOES: Average clicks per week for each result group
# Attach each student's final result to the click log (done once, reused below)
outcomes_dt <- as.data.table(master %>% select(all_of(k3), final_result))
outcomes_dt[, id_student := as.integer(id_student)]
vle_out <- studentVle[outcomes_dt, on = k3, nomatch = 0]

# Total clicks per outcome group per week
weekly <- vle_out[, .(clicks = sum(sum_click)),
                  by = .(final_result, week = floor(date / 7))] %>%
  as_tibble()

group_sizes <- master %>% count(final_result, name = "students")

weekly %>%
  left_join(group_sizes, by = "final_result") %>%
  mutate(clicks_per_student = clicks / students) %>%
  ggplot(aes(x = week, y = clicks_per_student, colour = final_result)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_colour_manual(values = outcome_colours) +
  labs(title = "Average weekly clicks per student, by final result",
       x = "Week (week 0 starts at day 0)", y = "Clicks per student",
       colour = "Final result")


## ----early-engagement, fig.height=4, fig.width=9------------------------------
# WHAT THIS CHUNK DOES: How clicks in the first 2 weeks (and before the start) relate to results
master <- master %>%
  mutate(
    early_clicks_band = cut(clicks_first_14_days,
                            breaks = c(-Inf, 0, 50, 150, Inf),
                            labels = c("0", "1-50", "51-150", "151+")),
    # Students who never clicked at all are kept as their own group; mixing them
    # into "No" would make pre-start activity look far more important than it is
    clicked_before_start = factor(case_when(
      never_clicked           ~ "Never clicked at all",
      clicks_before_start > 0 ~ "Yes",
      TRUE                    ~ "No, but clicked later"),
      levels = c("Yes", "No, but clicked later", "Never clicked at all"))
  )

# How many students fall in each band
master %>% count(early_clicks_band) %>%
  mutate(share = percent(n / sum(n), 0.1)) %>%
  kable(caption = "Students by clicks in days 0-13")

plot_outcome(early_clicks_band, "clicks in days 0-13")
plot_outcome(clicked_before_start, "any clicks before day 0")


## ----never-clicked------------------------------------------------------------
# WHAT THIS CHUNK DOES: What happened to students who never clicked at all
master %>%
  filter(never_clicked) %>%
  count(final_result) %>%
  mutate(share = percent(n / sum(n), 0.1)) %>%
  kable(caption = "Final result of students with zero clicks in the whole module")


## ----activity-types, fig.height=5, fig.width=9--------------------------------
# WHAT THIS CHUNK DOES: Which kinds of pages each group uses, and how much
# Clicks per page type per outcome group
site_clicks <- vle_out[, .(clicks = sum(sum_click)),
                       by = .(id_site, final_result)] %>%
  as_tibble() %>%
  left_join(vle %>% select(id_site, activity_type), by = "id_site")

activity <- site_clicks %>%
  group_by(activity_type, final_result) %>%
  summarise(clicks = sum(clicks), .groups = "drop") %>%
  left_join(group_sizes, by = "final_result") %>%
  mutate(clicks_per_student = clicks / students) %>%
  group_by(final_result) %>%
  mutate(share_of_group_clicks = clicks / sum(clicks)) %>%
  ungroup()

# The six most-used page types
top_types <- activity %>%
  group_by(activity_type) %>%
  summarise(total = sum(clicks)) %>%
  slice_max(total, n = 6) %>%
  pull(activity_type)

activity %>%
  filter(activity_type %in% top_types) %>%
  ggplot(aes(x = reorder(activity_type, -clicks), y = clicks_per_student,
             fill = final_result)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = outcome_colours) +
  labs(title = "Clicks per student on the six most-used page types",
       x = "Page type", y = "Clicks per student", fill = "Final result")

# Share of each group's clicks going to each page type
activity %>%
  filter(activity_type %in% top_types) %>%
  select(activity_type, final_result, share_of_group_clicks) %>%
  mutate(share_of_group_clicks = percent(share_of_group_clicks, 0.1)) %>%
  pivot_wider(names_from = final_result, values_from = share_of_group_clicks) %>%
  kable(caption = "Share of each group's clicks by page type")


## ----first-assessment, fig.height=4, fig.width=9------------------------------
# WHAT THIS CHUNK DOES: The first assignment: on time, late or missed, with a fair check on students still enrolled
# Was the first assessment submitted, and was it on time?
first_timing <- subs %>%
  semi_join(first_assess,
            by = c("code_module", "code_presentation", "id_assessment")) %>%
  transmute(code_module, code_presentation, id_student,
            # Banked results count as "On time": they were already passed earlier
            first_assessment = if_else(is_late, "Late", "On time"))

master <- master %>%
  select(-any_of("first_assessment")) %>%
  left_join(first_timing, by = k3) %>%
  mutate(first_assessment = factor(replace_na(first_assessment, "Not submitted"),
                                   levels = c("On time", "Late", "Not submitted")))

master %>% count(first_assessment) %>%
  mutate(share = percent(n / sum(n), 0.1)) %>%
  kable(caption = "First assessment of the module")

plot_outcome(first_assessment, "first assessment submission")

# Fair check: many students who "missed" the first assessment had already left
# before it was due, so missing it was a result of leaving, not a warning sign.
# Keep only students still registered on the first assessment's due date.
first_due <- non_exam %>%
  semi_join(first_assess, by = c("code_module", "code_presentation", "id_assessment")) %>%
  select(code_module, code_presentation, first_due_date = date)

master <- master %>%
  select(-any_of("first_due_date")) %>%
  left_join(first_due, by = c("code_module", "code_presentation")) %>%
  mutate(left_before_first_due = !is.na(date_unregistration) &
                                 date_unregistration <= first_due_date)

master %>%
  mutate(group = if_else(left_before_first_due,
                         "Had already left before it was due",
                         "Still registered on the due date")) %>%
  filter(first_assessment == "Not submitted") %>%
  count(group, name = "missed_first") %>%
  kable(caption = "Students who missed the first assessment")

master %>%
  filter(!left_before_first_due) %>%
  group_by(first_assessment) %>%
  summarise(students        = n(),
            withdrawal_rate = percent(mean(final_result == "Withdrawn"), 0.1),
            completion_rate = percent(mean(completed_flag), 0.1)) %>%
  kable(caption = "First assessment and outcome: students still registered on its due date")


## ----submission-sequence, fig.height=4.5, fig.width=9-------------------------
# WHAT THIS CHUNK DOES: How hand-ins drop off across the first 4 assignments
# Number each module-run's non-exam assessments in date order
seq_assess <- non_exam %>%
  arrange(code_module, code_presentation, date, id_assessment) %>%
  group_by(code_module, code_presentation) %>%
  mutate(assessment_no = row_number()) %>%
  ungroup() %>%
  filter(assessment_no <= 4) %>%
  select(code_module, code_presentation, id_assessment, assessment_no)

# Every student x every one of their first four assessments: submitted or not
seq_assess %>%
  inner_join(master %>% select(all_of(k3), final_result),
             by = c("code_module", "code_presentation"),
             relationship = "many-to-many") %>%
  left_join(studentAssessment %>% select(id_assessment, id_student) %>%
              mutate(submitted = 1),
            by = c("id_assessment", "id_student")) %>%
  mutate(submitted = replace_na(submitted, 0)) %>%
  group_by(assessment_no, final_result) %>%
  summarise(submission_rate = mean(submitted), .groups = "drop") %>%
  ggplot(aes(x = assessment_no, y = submission_rate, colour = final_result)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  scale_colour_manual(values = outcome_colours) +
  labs(title = "Share of students submitting each of the first four assessments",
       x = "Assessment number (in date order)", y = "Submitted",
       colour = "Final result")


## ----silent-fail, fig.height=4, fig.width=9-----------------------------------
# WHAT THIS CHUNK DOES: Show that failing students mostly stop handing work in, not score badly
# Distribution of submission rate and average score by outcome
ggplot(master, aes(x = final_result, y = submit_rate, fill = final_result)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = outcome_colours) +
  labs(title = "Share of non-exam assessments submitted, by final result",
       x = NULL, y = "Submission rate")

ggplot(master %>% filter(!is.na(avg_score)),
       aes(x = final_result, y = avg_score, fill = final_result)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = outcome_colours) +
  labs(title = "Average score on submitted assessments, by final result",
       subtitle = "Students with at least one scored submission",
       x = NULL, y = "Average score")

# Key facts about failing students
fail <- master %>% filter(final_result == "Fail")
pass_median <- median(master$avg_score[master$final_result == "Pass"], na.rm = TRUE)

tibble(
  measure = c("Failing students",
              "Submitted fewer than half of their assessments",
              paste0("Averaged at or above the Pass median score (",
                     round(pass_median, 1), ")")),
  value = c(comma(nrow(fail)),
            percent(mean(fail$submit_rate < 0.5), 0.1),
            percent(sum(fail$avg_score >= pass_median, na.rm = TRUE) / nrow(fail), 0.1))
) %>% kable(caption = "Failing students: submission versus score")

# Share of submissions made after the listed date
master %>%
  group_by(final_result) %>%
  summarise(pct_late = percent(sum(n_late) / sum(n_timed), 0.1)) %>%
  kable(caption = "Share of submissions made after the listed date")


## ----exam-coverage------------------------------------------------------------
# WHAT THIS CHUNK DOES: Show that exam results exist only for modules CCC and DDD
studentAssessment %>%
  inner_join(assessments_clean %>% filter(assessment_type == "Exam"),
             by = "id_assessment") %>%
  count(code_module, name = "exam_submissions") %>%
  kable(caption = "Recorded exam submissions by module")


## ----concentration-setup------------------------------------------------------
# WHAT THIS CHUNK DOES: Make deprivation groups and the 4 engagement segments
master <- master %>%
  mutate(
    # Three broad deprivation groups (plus Unknown)
    deprivation_group = case_when(
      imd_band %in% c("0-10%", "10-20%", "20-30%")           ~ "0-30%",
      imd_band %in% c("30-40%", "40-50%", "50-60%", "60-70%") ~ "30-70%",
      imd_band == "Unknown"                                  ~ "Unknown",
      TRUE                                                   ~ "70-100%"),
    deprivation_group = factor(deprivation_group,
                               levels = c("0-30%", "30-70%", "70-100%", "Unknown")),
    # Engagement segments, built only from what students actually did
    engagement_segment = case_when(
      never_clicked      ~ "1. Never clicked",
      !submitted_first   ~ "2. Clicked, missed first assessment",
      submit_rate < 0.5  ~ "3. Submitted first, then fewer than half",
      TRUE               ~ "4. Submitted at least half")
  )


## ----credit-deprivation, fig.height=4.5, fig.width=8--------------------------
# WHAT THIS CHUNK DOES: Completion by workload and deprivation together
master %>%
  group_by(deprivation_group, credit_group) %>%
  summarise(completion = mean(completed_flag), students = n(), .groups = "drop") %>%
  ggplot(aes(x = credit_group, y = deprivation_group, fill = completion)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = paste0(percent(completion, 0.1), "\n(n = ", students, ")")),
            size = 3.2) +
  scale_fill_gradient(low = "#b2182b", high = "#7fbf7b", labels = percent) +
  labs(title = "Completion rate by credit load and deprivation group",
       x = "Credits studied", y = "Deprivation band", fill = "Completion")


## ----module-deprivation, fig.height=5, fig.width=8----------------------------
# WHAT THIS CHUNK DOES: Completion by module and deprivation together
master %>%
  filter(deprivation_group != "Unknown") %>%
  group_by(code_module, deprivation_group) %>%
  summarise(completion = mean(completed_flag), students = n(), .groups = "drop") %>%
  ggplot(aes(x = deprivation_group, y = code_module, fill = completion)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = percent(completion, 0.1)), size = 3.2) +
  scale_fill_gradient(low = "#b2182b", high = "#7fbf7b", labels = percent) +
  labs(title = "Completion rate by module and deprivation group",
       x = "Deprivation band", y = "Module", fill = "Completion")


## ----engagement-segments, fig.height=4.5, fig.width=9-------------------------
# WHAT THIS CHUNK DOES: Where the non-completers are, by engagement segment
segments <- master %>%
  group_by(engagement_segment) %>%
  summarise(students        = n(),
            completion_rate = mean(completed_flag),
            non_completers  = sum(!completed_flag),
            withdrawn       = sum(final_result == "Withdrawn"),
            failed          = sum(final_result == "Fail")) %>%
  mutate(share_of_students       = students / sum(students),
         share_of_non_completers = non_completers / sum(non_completers))

segments %>%
  mutate(across(c(completion_rate, share_of_students, share_of_non_completers),
                ~ percent(.x, 0.1))) %>%
  kable(caption = "Engagement segments")

segments %>%
  select(engagement_segment, share_of_students, share_of_non_completers) %>%
  pivot_longer(-engagement_segment, names_to = "measure", values_to = "share") %>%
  mutate(measure = recode(measure,
                          share_of_students = "Share of all students",
                          share_of_non_completers = "Share of all non-completers")) %>%
  ggplot(aes(x = engagement_segment, y = share, fill = measure)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percent(share, 0.1)),
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("grey60", "#b2182b")) +
  labs(title = "Where the non-completers are",
       x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))


## ----deprivation-within-segments----------------------------------------------
# WHAT THIS CHUNK DOES: Does the deprivation gap shrink once students engage?
master %>%
  filter(deprivation_group != "Unknown") %>%
  group_by(deprivation_group) %>%
  summarise(
    overall_completion       = percent(mean(completed_flag), 0.1),
    pct_in_segments_1_to_3   = percent(mean(engagement_segment !=
                                             "4. Submitted at least half"), 0.1),
    completion_in_segment_4  = percent(mean(completed_flag[engagement_segment ==
                                             "4. Submitted at least half"]), 0.1)
  ) %>%
  kable(caption = "Deprivation, engagement and completion")


## ----silent-stayers, fig.height=4.5, fig.width=9------------------------------
# WHAT THIS CHUNK DOES: OUR INNOVATION: find the silent stayers (never quit, but went silent) and profile them
master <- master %>%
  mutate(
    last_active_pct = pmax(last_active_day, 0) / module_presentation_length,
    student_segment = factor(case_when(
      final_result == "Withdrawn"           ~ "Official quitters",
      never_clicked | last_active_pct < 0.5 ~ "Silent stayers",
      TRUE                                  ~ "Active stayers"),
      levels = c("Active stayers", "Silent stayers", "Official quitters"))
  )

segment_colours <- c("Active stayers" = "#2F6690", "Silent stayers" = "#E0A43A",
                     "Official quitters" = "#b2182b")

# Size and final result of each segment
master %>%
  group_by(student_segment) %>%
  summarise(students        = n(),
            share           = percent(n() / nrow(master), 0.1),
            ended_as_fail   = percent(mean(final_result == "Fail"), 0.1),
            completed       = percent(mean(completed_flag), 0.1)) %>%
  kable(caption = "Three segments of students")

# How the three segments differ
master %>%
  group_by(student_segment) %>%
  summarise(
    median_last_click       = percent(median(pmin(last_active_pct, 1), na.rm = TRUE), 1),
    submitted_first         = percent(mean(submitted_first), 0.1),
    average_score           = round(mean(avg_score, na.rm = TRUE), 1),
    more_than_120_credits   = percent(mean(studied_credits > 120), 0.1),
    repeat_attempt          = percent(mean(num_of_prev_attempts > 0), 0.1),
    below_A_level           = percent(mean(highest_education %in%
                                           c("No Formal quals", "Lower Than A Level")), 0.1),
    most_deprived_0_30      = percent(mean(deprivation_group == "0-30%"), 0.1)) %>%
  kable(caption = "Profile of the three segments")

# When do silent stayers go quiet?
master %>%
  filter(student_segment == "Silent stayers", !never_clicked) %>%
  ggplot(aes(x = last_active_pct)) +
  geom_histogram(binwidth = 0.05, boundary = 0, fill = "#E0A43A", colour = "white") +
  scale_x_continuous(labels = percent, limits = c(0, 0.5)) +
  labs(title = "When silent stayers made their last click",
       x = "Last active day, as a share of module length", y = "Students")

# Silent stayers by module
master %>%
  group_by(code_module) %>%
  summarise(silent_stayers = sum(student_segment == "Silent stayers"),
            share_of_module = percent(mean(student_segment == "Silent stayers"), 0.1)) %>%
  kable(caption = "Silent stayers by module")


## ----key-figures--------------------------------------------------------------
# WHAT THIS CHUNK DOES: One summary table of the headline numbers for the Board
tibble(
  measure = c("Registrations analysed",
              "Completed (Pass or Distinction)",
              "Withdrawn",
              "Silent stayers (did not withdraw, silent before halfway)",
              "Withdrawal rate, 2013J vs 2014J",
              "Same, only modules that ran in both years",
              "Withdrawals on or before day 0",
              "Withdrawals by day 30",
              "Registrations with no clicks at all",
              "Registrations that missed the first assessment",
              "Non-completers found in engagement segments 1-3"),
  value = c(
    comma(nrow(master)),
    percent(mean(master$completed_flag), 0.1),
    percent(mean(master$final_result == "Withdrawn"), 0.1),
    percent(mean(master$student_segment == "Silent stayers"), 0.1),
    paste(percent(mean(master$final_result[master$code_presentation == "2013J"] == "Withdrawn"), 0.1),
          "vs",
          percent(mean(master$final_result[master$code_presentation == "2014J"] == "Withdrawn"), 0.1)),
    paste(percent(mean(master$final_result[master$code_presentation == "2013J" &
                                           master$code_module %in% both_years] == "Withdrawn"), 0.1),
          "vs",
          percent(mean(master$final_result[master$code_presentation == "2014J" &
                                           master$code_module %in% both_years] == "Withdrawn"), 0.1)),
    percent(mean(withdrawn$date_unregistration <= 0), 0.1),
    percent(mean(withdrawn$date_unregistration <= 30), 0.1),
    percent(mean(master$never_clicked), 0.1),
    percent(mean(master$first_assessment == "Not submitted"), 0.1),
    percent(sum(!master$completed_flag &
                master$engagement_segment != "4. Submitted at least half") /
            sum(!master$completed_flag), 0.1))
) %>% kable(caption = "Key figures for the Board")


## ----traffic-light, fig.height=4, fig.width=9---------------------------------
# WHAT THIS CHUNK DOES: APPENDIX, outside the brief: an idea for later, a day-14 Green/Amber/Red check
traffic <- master %>%
  # Only students who had not left by day 14
  filter(is.na(date_unregistration) | date_unregistration > 14) %>%
  mutate(
    flag_low_activity = clicks_first_14_days < 50,
    flag_heavy_load   = studied_credits > 120,
    flag_repeat       = num_of_prev_attempts > 0,
    flag_low_prior_ed = highest_education %in% c("No Formal quals", "Lower Than A Level"),
    warning_points    = flag_low_activity + flag_heavy_load + flag_repeat + flag_low_prior_ed,
    traffic_light     = factor(case_when(warning_points == 0 ~ "Green",
                                         warning_points == 1 ~ "Amber",
                                         TRUE                ~ "Red"),
                               levels = c("Green", "Amber", "Red"))
  )

# How each colour turned out
traffic_summary <- traffic %>%
  group_by(traffic_light) %>%
  summarise(students        = n(),
            withdrawal_rate = mean(final_result == "Withdrawn"),
            completion_rate = mean(completed_flag),
            non_completers  = sum(!completed_flag)) %>%
  mutate(share_of_students       = students / sum(students),
         share_of_non_completers = non_completers / sum(non_completers))

traffic_summary %>%
  mutate(across(c(withdrawal_rate, completion_rate, share_of_students,
                  share_of_non_completers), ~ percent(.x, 0.1))) %>%
  kable(caption = "Day-14 traffic light: how each group turned out")

traffic_summary %>%
  ggplot(aes(x = traffic_light, y = completion_rate, fill = traffic_light)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(percent(completion_rate, 0.1), " complete\n(",
                               comma(students), " students)")),
            vjust = -0.3, size = 3.2) +
  scale_fill_manual(values = c(Green = "#1b7837", Amber = "#f4a582", Red = "#b2182b")) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(title = "Completion rate by day-14 traffic light",
       x = NULL, y = "Completion rate")


