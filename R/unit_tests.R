# =============================================================================
# Unit tests for Open Play Demographics helper functions
# Run with: testthat::test_file("R/unit_tests.R")
# =============================================================================

library(testthat)
library(tidyverse)
library(glue)

source("R/helpers.R")

# -----------------------------------------------------------------------------
# FORMATTING FUNCTIONS
# -----------------------------------------------------------------------------

test_that("format_mean_sd handles normal input", {

expect_equal(format_mean_sd(c(10, 20, 30)), "20 (10)")
  expect_equal(format_mean_sd(c(1, 1, 1)), "1 (0)")
})

test_that("format_mean_sd handles NA values", {
  expect_equal(format_mean_sd(c(10, NA, 30)), "20 (14.1)")
  expect_equal(format_mean_sd(c(NA, NA)), "—")
})
test_that("format_mean_sd handles empty input", {
  expect_equal(format_mean_sd(numeric(0)), "—")
  expect_equal(format_mean_sd(c()), "—")
})

test_that("format_n_pct calculates percentages correctly", {
  expect_equal(format_n_pct(50, 100), "50 (50\\%)")
  expect_equal(format_n_pct(1, 3), "1 (33.3\\%)")
  expect_equal(format_n_pct(0, 100), "0 (0.0\\%)")
})

test_that("format_n_pct handles edge cases", {
  expect_equal(format_n_pct(100, 100), "100 (100\\%)")
  expect_equal(format_n_pct(1, 1000), "1 (0.1\\%)")
})

# -----------------------------------------------------------------------------
# GENRE CLEANING
# -----------------------------------------------------------------------------

test_that("clean_genre maps strategy subgenres correctly", {
  expect_equal(clean_genre("Turn-based strategy (TBS)"), "Strategy")
  expect_equal(clean_genre("Real Time Strategy (RTS)"), "Strategy")
  expect_equal(clean_genre("Tactical"), "Strategy")
  expect_equal(clean_genre("MOBA"), "Strategy")
})

test_that("clean_genre maps action subgenres correctly", {
  expect_equal(clean_genre("Hack and slash/Beat 'em up"), "Action")
  expect_equal(clean_genre("Action"), "Action")
})

test_that("clean_genre maps adventure subgenres correctly", {
  expect_equal(clean_genre("Point-and-click"), "Adventure")
  expect_equal(clean_genre("Visual Novel"), "Adventure")
  expect_equal(clean_genre("Adventure"), "Adventure")
})

test_that("clean_genre maps puzzle subgenres correctly", {
  expect_equal(clean_genre("Card & Board Game"), "Puzzle")
  expect_equal(clean_genre("Quiz/Trivia"), "Puzzle")
  expect_equal(clean_genre("Puzzle"), "Puzzle")
})

test_that("clean_genre maps arcade subgenres correctly", {
  expect_equal(clean_genre("Pinball"), "Arcade")
  expect_equal(clean_genre("Music"), "Arcade")
  expect_equal(clean_genre("Arcade"), "Arcade")
})

test_that("clean_genre excludes Indie", {
  expect_true(is.na(clean_genre("Indie")))
})

test_that("clean_genre passes through unmapped genres", {
  expect_equal(clean_genre("Shooter"), "Shooter")
  expect_equal(clean_genre("Racing"), "Racing")
  expect_equal(clean_genre("Sport"), "Sport")
  expect_equal(clean_genre("Role-playing (RPG)"), "Role-playing (RPG)")
})

# -----------------------------------------------------------------------------
# INDIVIDUAL GENRE PROPORTION CALCULATIONS
# -----------------------------------------------------------------------------

test_that("calc_individual_genre_props calculates per-person allocations", {
  test_data <- tibble(
    pid = c("p1", "p1", "p2", "p2"),
    genre_clean = c("Action", "RPG", "Action", "RPG"),
    minutes = c(60, 40, 20, 80)
  )

  result <- calc_individual_genre_props(test_data)

  # p1: 60% Action, 40% RPG
  p1_action <- result |> filter(pid == "p1", genre_clean == "Action")
  expect_equal(p1_action$individual_prop, 0.6)

  # p2: 20% Action, 80% RPG
  p2_rpg <- result |> filter(pid == "p2", genre_clean == "RPG")
  expect_equal(p2_rpg$individual_prop, 0.8)
})

# -----------------------------------------------------------------------------
# GENRE PROPORTION CALCULATIONS (MEDIAN-BASED)
# -----------------------------------------------------------------------------

test_that("calc_genre_props calculates median of individual proportions", {
  # Three people in 18-24, one in 25-30
  test_data <- tibble(
    pid = c("p1", "p1", "p2", "p2", "p3", "p3", "p4"),
    genre_clean = c("Action", "RPG", "Action", "RPG", "Action", "RPG", "RPG"),
    age_group = c("18-24", "18-24", "18-24", "18-24", "18-24", "18-24", "25-30"),
    minutes = c(60, 40, 80, 20, 50, 50, 100)
  )
  # p1: 60% Action, 40% RPG
  # p2: 80% Action, 20% RPG
  # p3: 50% Action, 50% RPG
  # Median Action for 18-24: median(0.6, 0.8, 0.5) = 0.6
  # Median RPG for 18-24: median(0.4, 0.2, 0.5) = 0.4

  result <- calc_genre_props(test_data, "age_group")

  age_18_24 <- result |> filter(group == "18-24")
  expect_equal(age_18_24$prop[age_18_24$genre_clean == "Action"], 0.6)
  expect_equal(age_18_24$prop[age_18_24$genre_clean == "RPG"], 0.4)

  # 25-30: only p4 with 100% RPG
  age_25_30 <- result |> filter(group == "25-30")
  expect_equal(age_25_30$prop, 1.0)
})

test_that("calc_genre_props excludes NA groups", {
  test_data <- tibble(
    pid = c("p1", "p2"),
    genre_clean = c("Action", "RPG"),
    age_group = c("18-24", NA),
    minutes = c(60, 100)
  )

  result <- calc_genre_props(test_data, "age_group")
  expect_equal(nrow(result), 1)
  expect_equal(result$group, "18-24")
})

# -----------------------------------------------------------------------------
# GENRE DEVIATION CALCULATIONS
# -----------------------------------------------------------------------------

test_that("calc_genre_deviation computes leave-one-out median ratios", {
  # Two age groups with different preferences
  # Young: 2 people who prefer Action (80%, 80%)
  # Old: 2 people who prefer RPG (80%, 80%)
  test_data <- tibble(
    pid = c("p1", "p1", "p2", "p2", "p3", "p3", "p4", "p4"),
    genre_clean = rep(c("Action", "RPG"), 4),
    age_group = c(rep("18-24", 4), rep("25-30", 4)),
    gender = rep("Man", 8),
    ethnicity = rep("White", 8),
    is_neurotypical = rep(TRUE, 8),
    is_adhd = rep(FALSE, 8),
    is_autism = rep(FALSE, 8),
    minutes = c(80, 20, 80, 20, 20, 80, 20, 80)
  )

  test_props <- build_genre_props(test_data)
  result <- calc_genre_deviation(test_data, test_props)

  # For 18-24 group:
  # Group median Action = 0.8, reference (25-30) median Action = 0.2
  # Ratio = 0.8 / 0.2 = 4.0 (capped at 2.0)
  young_action <- result |>
    filter(demographic == "Age", group == "18-24", genre_clean == "Action")
  expect_equal(young_action$dev_ratio, 4.0)
  expect_equal(young_action$dev_ratio_capped, 2.0)

  # Group median RPG = 0.2, reference (25-30) median RPG = 0.8
  # Ratio = 0.2 / 0.8 = 0.25 (capped at 0.5)
  young_rpg <- result |>
    filter(demographic == "Age", group == "18-24", genre_clean == "RPG")
  expect_equal(young_rpg$dev_ratio, 0.25)
  expect_equal(young_rpg$dev_ratio_capped, 0.5)
})

test_that("calc_genre_deviation caps extreme values", {
  # Create extreme case: one group plays only Action, other only RPG
  test_data <- tibble(
    pid = c("p1", "p2"),
    genre_clean = c("Action", "RPG"),
    age_group = c("18-24", "25-30"),
    gender = rep("Man", 2),
    ethnicity = rep("White", 2),
    is_neurotypical = rep(TRUE, 2),
    is_adhd = rep(FALSE, 2),
    is_autism = rep(FALSE, 2),
    minutes = c(100, 100)
  )

  test_props <- build_genre_props(test_data)
  result <- calc_genre_deviation(test_data, test_props)

  # All capped values should be within [0.5, 2.0]
  expect_true(all(result$dev_ratio_capped >= 0.5, na.rm = TRUE))
  expect_true(all(result$dev_ratio_capped <= 2.0, na.rm = TRUE))
})

# -----------------------------------------------------------------------------
# GROUP SAMPLE SIZE CALCULATIONS
# -----------------------------------------------------------------------------

test_that("calc_group_ns counts unique participants", {
  test_data <- tibble(
    pid = c("p1", "p1", "p2", "p3"),  # p1 appears twice
    age_group = c("18-24", "18-24", "18-24", "25-30"),
    gender = c("Man", "Man", "Woman", "Man"),
    ethnicity = c("White", "White", "Asian", "White"),
    is_neurotypical = c(TRUE, TRUE, TRUE, FALSE),
    is_adhd = c(FALSE, FALSE, FALSE, TRUE),
    is_autism = c(FALSE, FALSE, FALSE, FALSE)
  )

  result <- calc_group_ns(test_data)

  # Age: 18-24 has 2 unique pids, 25-30 has 1
  age_ns <- result |> filter(demographic == "Age")
  expect_equal(age_ns$n[age_ns$group == "18-24"], 2)
  expect_equal(age_ns$n[age_ns$group == "25-30"], 1)

  # Gender: Man has 2 unique, Woman has 1
  gender_ns <- result |> filter(demographic == "Gender")
  expect_equal(gender_ns$n[gender_ns$group == "Man"], 2)
  expect_equal(gender_ns$n[gender_ns$group == "Woman"], 1)
})

test_that("calc_group_ns handles neurodiversity correctly", {
  test_data <- tibble(
    pid = c("p1", "p2", "p3"),
    age_group = rep("18-24", 3),
    gender = rep("Man", 3),
    ethnicity = rep("White", 3),
    is_neurotypical = c(TRUE, FALSE, FALSE),
    is_adhd = c(FALSE, TRUE, TRUE),
    is_autism = c(FALSE, FALSE, TRUE)
  )

  result <- calc_group_ns(test_data)
  neuro_ns <- result |> filter(demographic == "Neurodiversity")

  expect_equal(neuro_ns$n[neuro_ns$group == "Neurotypical"], 1)
  expect_equal(neuro_ns$n[neuro_ns$group == "ADHD"], 2)
  expect_equal(neuro_ns$n[neuro_ns$group == "Autism spectrum"], 1)
})

# -----------------------------------------------------------------------------
# BUILD GENRE PROPS (INTEGRATION TEST)
# -----------------------------------------------------------------------------

test_that("build_genre_props returns all demographic dimensions", {
  test_data <- tibble(
    pid = rep(c("p1", "p2"), each = 2),
    genre_clean = rep(c("Action", "RPG"), 2),
    minutes = c(60, 40, 30, 70),
    age_group = rep(c("18-24", "25-30"), each = 2),
    gender = rep(c("Man", "Woman"), each = 2),
    ethnicity = rep(c("White", "Asian"), each = 2),
    is_neurotypical = rep(c(TRUE, FALSE), each = 2),
    is_adhd = rep(c(FALSE, TRUE), each = 2),
    is_autism = rep(c(FALSE, FALSE), each = 2)
  )

  result <- build_genre_props(test_data)

  # Should have all four demographic dimensions
  expect_true("Age" %in% result$demographic)
  expect_true("Gender" %in% result$demographic)
  expect_true("Ethnicity" %in% result$demographic)
  expect_true("Neurodiversity" %in% result$demographic)

  # Proportions within each group should sum to 1
  prop_sums <- result |>
    group_by(demographic, group) |>
    summarise(total_prop = sum(prop), .groups = "drop")

  expect_true(all(near(prop_sums$total_prop, 1.0)))
})

test_that("build_genre_props demographic factor has correct order", {
  test_data <- tibble(
    pid = "p1",
    genre_clean = "Action",
    minutes = 60,
    age_group = "18-24",
    gender = "Man",
    ethnicity = "White",
    is_neurotypical = TRUE,
    is_adhd = FALSE,
    is_autism = FALSE
  )

  result <- build_genre_props(test_data)

  expect_equal(
    levels(result$demographic),
    c("Age", "Gender", "Ethnicity", "Neurodiversity")
  )
})

# -----------------------------------------------------------------------------
# RUN ALL TESTS
# -----------------------------------------------------------------------------

if (interactive()) {
  test_file("R/unit_tests.R")
}
