# =============================================================================
# Helper functions for Open Play Demographics analysis
# =============================================================================

# -----------------------------------------------------------------------------
# TABLE FORMATTING
# -----------------------------------------------------------------------------

#' Format mean and standard deviation
#' @param x Numeric vector
#' @return Character string "M (SD)" or em-dash if empty
format_mean_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return("—")
  }
  glue("{round(mean(x), 1)} ({round(sd(x), 1)})")
}

#' Format count and percentage
#' @param n Count
#' @param total Denominator for percentage
#' @return Character string "n (pct%)" with LaTeX-escaped percent sign
format_n_pct <- function(n, total) {
  if (n == 0) {
    return("0 (0.0\\%)")
  }
  glue("{n} ({round(100 * n / total, 1)}\\%)")
}

#' Create a table row with Total/US/UK columns
#' @param char Characteristic name
#' @param val_total Value for Total column
#' @param val_us Value for US column
#' @param val_uk Value for UK column
#' @return Single-row tibble
make_table_row <- function(char, val_total, val_us, val_uk) {
  tibble(
    Characteristic = char,
    Total = val_total,
    US = val_us,
    UK = val_uk
  )
}

#' Create categorical rows for demographics table
#' @param data Data frame with demographic data
#' @param var Variable name to tabulate
#' @param label Header label for the variable
#' @param levels Character vector of category levels
#' @param n_total Total sample size
#' @param n_us US sample size
#' @param n_uk UK sample size
#' @return Tibble with header and category rows
make_demo_rows <- function(data, var, label, levels, n_total, n_us, n_uk) {
  header <- make_table_row(glue("**{label}**"), "", "", "")

  rows <- map_dfr(levels, function(lvl) {
    n_tot <- sum(data[[var]] == lvl, na.rm = TRUE)
    n_us_val <- sum(data[[var]] == lvl & data$country == "US", na.rm = TRUE)
    n_uk_val <- sum(data[[var]] == lvl & data$country == "UK", na.rm = TRUE)

    make_table_row(
      glue("    {lvl}"),
      format_n_pct(n_tot, n_total),
      format_n_pct(n_us_val, n_us),
      format_n_pct(n_uk_val, n_uk)
    )
  })

  bind_rows(header, rows)
}

# -----------------------------------------------------------------------------
# GENRE ANALYSIS
# -----------------------------------------------------------------------------

#' Clean and collapse genre categories
#' @param genre_raw Raw genre string from metadata
#' @return Cleaned genre category
clean_genre <- function(genre_raw) {
  case_when(
    genre_raw == "Indie" ~ NA_character_,
    genre_raw %in%
      c(
        "Turn-based strategy (TBS)",
        "Real Time Strategy (RTS)",
        "Tactical",
        "MOBA"
      ) ~ "Strategy",
    str_detect(genre_raw, "Hack and slash") ~ "Action",
    genre_raw == "Point-and-click" ~ "Adventure",
    genre_raw == "Visual Novel" ~ "Adventure",
    genre_raw == "Card & Board Game" ~ "Puzzle",
    genre_raw == "Quiz/Trivia" ~ "Puzzle",
    genre_raw == "Pinball" ~ "Arcade",
    genre_raw == "Music" ~ "Arcade",
    TRUE ~ genre_raw
  )
}

#' Calculate individual-level genre allocations
#' @param data Data frame with pid, genre_clean, and minutes
#' @return Data frame with each individual's proportion of playtime per genre
calc_individual_genre_props <- function(data) {
  data |>
    group_by(pid, genre_clean) |>
    summarise(genre_minutes = sum(minutes, na.rm = TRUE), .groups = "drop") |>
    group_by(pid) |>
    mutate(individual_prop = genre_minutes / sum(genre_minutes)) |>
    ungroup()
}

#' Calculate genre proportions for a single demographic variable
#' Uses median of individual allocations rather than aggregate minutes
#' @param data Data frame with genre_clean, minutes, pid, and grouping variable
#' @param group_var Name of the grouping variable
#' @return Data frame with median genre proportions by group
calc_genre_props <- function(data, group_var) {
  # Calculate individual-level proportions
  individual_props <- calc_individual_genre_props(data)

  # Join demographic info
  demo_info <- data |>
    distinct(pid, .data[[group_var]])

  individual_props <- individual_props |>
    left_join(demo_info, by = "pid")

  # Calculate median proportion per genre within each group
  individual_props |>
    filter(!is.na(.data[[group_var]])) |>
    group_by(genre_clean, .data[[group_var]]) |>
    summarise(
      prop = median(individual_prop, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(group = all_of(group_var))
}

#' Build genre proportions across all demographic dimensions
#' Uses median of individual allocations for each demographic group
#' @param genre_by_demo Data frame with genre and demographic info
#' @return Combined data frame with median proportions for all demographics
build_genre_props <- function(genre_by_demo) {
  props_age <- calc_genre_props(genre_by_demo, "age_group") |>
    mutate(demographic = "Age")
  props_gender <- calc_genre_props(genre_by_demo, "gender") |>
    mutate(demographic = "Gender")
  props_ethnicity <- calc_genre_props(genre_by_demo, "ethnicity") |>
    mutate(demographic = "Ethnicity")

  # Neurodiversity requires special handling (non-exclusive categories)
  # First get individual-level proportions
  individual_props <- calc_individual_genre_props(genre_by_demo)

  # Join neurodiversity flags
  neuro_info <- genre_by_demo |>
    distinct(pid, is_neurotypical, is_adhd, is_autism)

  individual_props_neuro <- individual_props |>
    left_join(neuro_info, by = "pid")

  # Calculate median for each neuro group separately
  props_neuro <- bind_rows(
    individual_props_neuro |>
      filter(is_neurotypical == TRUE) |>
      group_by(genre_clean) |>
      summarise(
        prop = median(individual_prop, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(group = "Neurotypical"),
    individual_props_neuro |>
      filter(is_adhd == TRUE) |>
      group_by(genre_clean) |>
      summarise(
        prop = median(individual_prop, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(group = "ADHD"),
    individual_props_neuro |>
      filter(is_autism == TRUE) |>
      group_by(genre_clean) |>
      summarise(
        prop = median(individual_prop, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(group = "Autism spectrum")
  ) |>
    mutate(demographic = "Neurodiversity")

  bind_rows(props_age, props_gender, props_ethnicity, props_neuro) |>
    mutate(
      demographic = factor(
        demographic,
        levels = c("Age", "Gender", "Ethnicity", "Neurodiversity")
      )
    )
}

#' Calculate group sample sizes from genre data
#' @param genre_by_demo Data frame with genre and demographic info
#' @return Data frame with n per demographic group
calc_group_ns <- function(genre_by_demo) {
  bind_rows(
    genre_by_demo |>
      distinct(pid, age_group) |>
      filter(!is.na(age_group)) |>
      count(age_group) |>
      rename(group = age_group) |>
      mutate(demographic = "Age"),
    genre_by_demo |>
      distinct(pid, gender) |>
      filter(!is.na(gender)) |>
      count(gender) |>
      rename(group = gender) |>
      mutate(demographic = "Gender"),
    genre_by_demo |>
      distinct(pid, ethnicity) |>
      filter(!is.na(ethnicity)) |>
      count(ethnicity) |>
      rename(group = ethnicity) |>
      mutate(demographic = "Ethnicity"),
    genre_by_demo |>
      distinct(pid, is_neurotypical, is_adhd, is_autism) |>
      summarise(
        Neurotypical = sum(is_neurotypical, na.rm = TRUE),
        ADHD = sum(is_adhd, na.rm = TRUE),
        `Autism spectrum` = sum(is_autism, na.rm = TRUE)
      ) |>
      pivot_longer(everything(), names_to = "group", values_to = "n") |>
      mutate(demographic = "Neurodiversity")
  )
}

#' Calculate leave-one-out deviation ratios for genre preferences
#' Compares each group's median to the median of everyone NOT in that group
#' @param genre_by_demo Original data frame with individual-level data
#' @param genre_props Data frame from build_genre_props (with group medians)
#' @return Data frame with deviation ratios added
calc_genre_deviation <- function(genre_by_demo, genre_props) {
  # Get individual-level proportions with all demographic info
  individual_props <- calc_individual_genre_props(genre_by_demo)

  demo_info <- genre_by_demo |>
    distinct(
      pid,
      age_group,
      gender,
      ethnicity,
      is_neurotypical,
      is_adhd,
      is_autism
    )

  individual_props <- individual_props |>
    left_join(demo_info, by = "pid")

  # Helper to compute leave-one-out median for a demographic variable
  calc_loo_median <- function(ind_data, group_var, demo_name) {
    groups <- unique(ind_data[[group_var]])
    groups <- groups[!is.na(groups)]

    map_dfr(groups, function(g) {
      # Median of everyone NOT in this group
      ref_data <- ind_data |>
        filter(.data[[group_var]] != g | is.na(.data[[group_var]]))

      ref_data |>
        group_by(genre_clean) |>
        summarise(
          ref_prop = median(individual_prop, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(group = g, demographic = demo_name)
    })
  }

  # Calculate leave-one-out reference medians for each demographic
  ref_age <- calc_loo_median(individual_props, "age_group", "Age")
  ref_gender <- calc_loo_median(individual_props, "gender", "Gender")
  ref_ethnicity <- calc_loo_median(individual_props, "ethnicity", "Ethnicity")

  # Neurodiversity: leave-one-out for non-exclusive groups
  ref_neuro <- bind_rows(
    individual_props |>
      filter(is_neurotypical == FALSE | is.na(is_neurotypical)) |>
      group_by(genre_clean) |>
      summarise(
        ref_prop = median(individual_prop, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(group = "Neurotypical", demographic = "Neurodiversity"),
    individual_props |>
      filter(is_adhd == FALSE | is.na(is_adhd)) |>
      group_by(genre_clean) |>
      summarise(
        ref_prop = median(individual_prop, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(group = "ADHD", demographic = "Neurodiversity"),
    individual_props |>
      filter(is_autism == FALSE | is.na(is_autism)) |>
      group_by(genre_clean) |>
      summarise(
        ref_prop = median(individual_prop, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(group = "Autism spectrum", demographic = "Neurodiversity")
  )

  ref_props <- bind_rows(ref_age, ref_gender, ref_ethnicity, ref_neuro)

  # Join reference proportions to group proportions and calculate deviation
  genre_props |>
    left_join(ref_props, by = c("demographic", "group", "genre_clean")) |>
    mutate(
      dev_ratio = prop / ref_prop,
      dev_ratio_capped = pmin(pmax(dev_ratio, 0.5), 2.0)
    )
}

# -----------------------------------------------------------------------------
# RADAR CHART FUNCTIONS
# -----------------------------------------------------------------------------

#' Create a single radar plot for genre preferences
#' @param data Data frame with genre deviations for one group
#' @param fill_color Fill color for the polygon
#' @param title Plot title (group name)
#' @param subtitle Plot subtitle (sample size)
#' @param genre_levels Character vector of genre names in order
#' @param theme_radar Pre-defined theme object
#' @return ggplot object
make_radar <- function(
  data,
  fill_color,
  title,
  subtitle,
  genre_levels,
  theme_radar
) {
  n_genres <- length(genre_levels)

  data <- data |>
    mutate(genre_clean = factor(genre_clean, levels = genre_levels)) |>
    arrange(genre_clean) |>
    mutate(
      angle = (as.numeric(genre_clean) - 1) * 2 * pi / n_genres,
      r = dev_ratio_capped,
      x = r * sin(angle),
      y = r * cos(angle)
    )

  data_closed <- bind_rows(data, data |> slice(1))

  baseline <- tibble(
    theta = seq(0, 2 * pi, length.out = 100),
    x = 1.0 * sin(theta),
    y = 1.0 * cos(theta)
  )

  grid_circles <- tibble(r = c(0.5, 2.0)) |>
    rowwise() |>
    mutate(
      data = list(tibble(
        theta = seq(0, 2 * pi, length.out = 100),
        x = r * sin(theta),
        y = r * cos(theta)
      ))
    ) |>
    unnest(data)

  # Plot limits sized to fit 2x outer circle plus labels
  lim <- 2.2

  axis_data <- tibble(
    genre = genre_levels,
    angle = (seq_along(genre_levels) - 1) * 2 * pi / n_genres,
    x_end = 2.0 * sin(angle),
    y_end = 2.0 * cos(angle),
    x_lab = 2.12 * sin(angle),
    y_lab = 2.12 * cos(angle),
    hjust = case_when(
      abs(sin(angle)) < 0.15 ~ 0.5,
      sin(angle) > 0 ~ 0,
      TRUE ~ 1
    ),
    vjust = case_when(
      abs(cos(angle)) < 0.15 ~ 0.5,
      cos(angle) > 0 ~ 0,
      TRUE ~ 1
    )
  )

  ggplot() +
    geom_path(
      data = grid_circles,
      aes(x = x, y = y, group = r),
      color = "grey80",
      linewidth = 0.2,
      linetype = "dashed"
    ) +
    geom_path(
      data = baseline,
      aes(x = x, y = y),
      color = "grey50",
      linewidth = 0.5
    ) +
    geom_segment(
      data = axis_data,
      aes(x = 0, y = 0, xend = x_end, yend = y_end),
      color = "grey70",
      linewidth = 0.2
    ) +
    geom_polygon(
      data = data_closed,
      aes(x = x, y = y),
      fill = fill_color,
      alpha = 0.4,
      color = "black",
      linewidth = 0.5
    ) +
    geom_point(
      data = data,
      aes(x = x, y = y),
      fill = fill_color,
      color = "black",
      shape = 21,
      size = 1.2
    ) +
    geom_text(
      data = axis_data,
      aes(x = x_lab, y = y_lab, label = genre, hjust = hjust, vjust = vjust),
      size = 1.8,
      color = "black"
    ) +
    coord_fixed(
      xlim = c(-lim, lim),
      ylim = c(-lim, lim),
      expand = FALSE,
      clip = "off"
    ) +
    labs(title = title, subtitle = subtitle) +
    theme_radar +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 7,
        face = "bold",
        margin = margin(b = 1)
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 6,
        colour = "grey40",
        margin = margin(b = 8)
      ),
      plot.margin = margin(t = 2, b = 8, l = 0, r = 0, unit = "pt")
    )
}

#' Create row label plot for radar grid
#' @param label Text label for the row
#' @param theme_radar_label Pre-defined theme object
#' @return ggplot object
make_row_label <- function(label, theme_radar_label) {
  ggplot() +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = "black"
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = label,
      color = "white",
      size = 3.5,
      fontface = "bold",
      angle = 90
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_radar_label
}

#' Create column header label for radar grid
#' @param label Text label for the column
#' @param theme_radar_label Pre-defined theme object
#' @return ggplot object
make_col_label <- function(label, theme_radar_label) {
  ggplot() +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = "black"
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = label,
      color = "white",
      size = 2.5,
      fontface = "bold"
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_radar_label
}

#' Build full radar grid from genre deviation data
#' Uses cowplot for assembly to avoid patchwork's letterboxing with coord_fixed()
#' @param genre_props_dev Data frame with genre deviations
#' @param group_ns Data frame with group sample sizes
#' @param genre_levels Character vector of genre names
#' @param demo_colors List of color vectors by demographic
#' @param theme_radar Theme for radar plots
#' @param theme_radar_empty Theme for empty plots
#' @param theme_radar_label Theme for row labels
#' @return cowplot grid object
build_radar_grid <- function(
  genre_props_dev,
  group_ns,
  genre_levels,
  demo_colors,
  theme_radar,
  theme_radar_empty,
  theme_radar_label
) {
  # Flatten colors and fix names (unlist adds prefixes like "Age.18-24")
  all_demo_colors <- unlist(demo_colors)
  names(all_demo_colors) <- sub(".*\\.", "", names(all_demo_colors))
  empty_plot <- ggplot() + theme_radar_empty

  make_single <- function(demo_name, grp_name) {
    grp_data <- genre_props_dev |>
      filter(demographic == demo_name, group == grp_name)
    if (nrow(grp_data) == 0) {
      return(empty_plot)
    }

    grp_n <- group_ns |>
      filter(demographic == demo_name, group == grp_name) |>
      pull(n)
    subtitle <- glue("n = {scales::comma(grp_n)}")

    make_radar(
      grp_data,
      all_demo_colors[grp_name],
      grp_name,
      subtitle,
      genre_levels,
      theme_radar
    )
  }

  # Build each column as a vertical stack, then combine horizontally
  # This avoids patchwork's panel-alignment issues with coord_fixed()
  header_height <- 0.18
  plot_heights <- c(header_height, rep(1, 5))

  col_age <- cowplot::plot_grid(
    make_col_label("Age", theme_radar_label),
    make_single("Age", "18-24"),
    make_single("Age", "25-30"),
    make_single("Age", "31-35"),
    make_single("Age", "36-40"),
    empty_plot,
    ncol = 1,
    rel_heights = plot_heights,
    align = "v"
  )

  col_gen <- cowplot::plot_grid(
    make_col_label("Gender", theme_radar_label),
    make_single("Gender", "Man"),
    make_single("Gender", "Woman"),
    make_single("Gender", "Non-binary/Other"),
    empty_plot,
    empty_plot,
    ncol = 1,
    rel_heights = plot_heights,
    align = "v"
  )

  col_eth <- cowplot::plot_grid(
    make_col_label("Ethnicity", theme_radar_label),
    make_single("Ethnicity", "Asian"),
    make_single("Ethnicity", "Black"),
    make_single("Ethnicity", "Mixed/Multiple"),
    make_single("Ethnicity", "Other"),
    make_single("Ethnicity", "White"),
    ncol = 1,
    rel_heights = plot_heights,
    align = "v"
  )

  col_neu <- cowplot::plot_grid(
    make_col_label("Neurodiversity", theme_radar_label),
    make_single("Neurodiversity", "ADHD"),
    make_single("Neurodiversity", "Autism spectrum"),
    make_single("Neurodiversity", "Neurotypical"),
    empty_plot,
    empty_plot,
    ncol = 1,
    rel_heights = plot_heights,
    align = "v"
  )

  # Combine columns horizontally

  cowplot::plot_grid(col_age, col_gen, col_eth, col_neu, nrow = 1, align = "h")
}
