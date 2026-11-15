# ================================================================
# WEEK 1: DATA CLEANING AND PRELIMINARY ANALYSIS WITH R
# Dataset: Titanic passenger dataset
# Run this script from the RStudio project root directory.
# It creates cleaned data, tables, charts, and a Word report.
# ================================================================

# 0. SETUP --------------------------------------------------------
options(stringsAsFactors = FALSE, scipen = 999)
set.seed(123)

required_packages <- c(
  "tidyverse", "janitor", "naniar", "corrplot",
  "skimr", "officer","flextable"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(naniar)
  library(corrplot)
  library(skimr)
  library(officer)
})

# Project folders
folders <- c("Dataset", "R_Scripts", "Outputs", "Visualizations", "Report")
for (folder in folders) {
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
}

# Specify the actual dataset location

dataset_path <- "D:/YuvaIntern_Internships/Virtual R Data Analyst Internship in Pune/Dataset/titanic.csv"

# Verify dataset exists

if (!file.exists(dataset_path)) {
  stop("Dataset file not found. Check the file path.")
}

cat("Dataset found successfully!\n")

# 1. IMPORT AND STANDARDIZE ---------------------------------------
raw_data <- readr::read_csv(dataset_path, show_col_types = FALSE) |>
  janitor::clean_names()

# Ensure the expected Titanic fields are present.
required_columns <- c("survived", "pclass", "sex", "age", "sib_sp",
                      "parch", "fare", "embarked")
missing_columns <- setdiff(required_columns, names(raw_data))

if (length(missing_columns) > 0) {
  stop(
    "The CSV does not contain expected columns: ",
    paste(missing_columns, collapse = ", "),
    ". Check that you downloaded the Titanic training dataset."
  )
}

# Keep a raw copy unchanged for auditability.
readr::write_csv(raw_data, "Outputs/titanic_raw_import.csv")

# 2. INITIAL DATA PROFILE -----------------------------------------
n_rows <- nrow(raw_data)
n_cols <- ncol(raw_data)
duplicate_rows <- sum(duplicated(raw_data))

missing_counts <- colSums(is.na(raw_data))
missing_summary <- tibble(
  column = names(missing_counts),
  missing_count = as.integer(missing_counts),
  missing_percent = round(100 * missing_counts / n_rows, 2)
) |>
  arrange(desc(missing_count))

readr::write_csv(missing_summary, "Outputs/missing_value_summary.csv")
capture.output(str(raw_data), file = "Outputs/structure_output.txt")
capture.output(summary(raw_data), file = "Outputs/summary_output.txt")
capture.output(skimr::skim(raw_data), file = "Outputs/skim_summary.txt")

# 3. DATA CLEANING ------------------------------------------------
clean_data <- raw_data

# Convert appropriate fields to numeric safely.
numeric_fields <- intersect(
  c("survived", "pclass", "age", "sib_sp", "parch", "fare"),
  names(clean_data)
)
clean_data <- clean_data |>
  mutate(across(all_of(numeric_fields), ~ suppressWarnings(as.numeric(.x))))

# Add a missing-cabin indicator before deciding whether to exclude Cabin.
if ("cabin" %in% names(clean_data)) {
  clean_data <- clean_data |>
    mutate(cabin_missing = if_else(is.na(cabin) | trimws(cabin) == "",
                                   "Missing", "Recorded"))
}

# Convert categorical fields to factors.
clean_data <- clean_data |>
  mutate(
    sex = as.factor(sex),
    pclass = as.factor(pclass),
    embarked = as.factor(embarked)
  )

# Impute Age using median within passenger class and sex;
# use the overall median as a fallback if a group has no observed age.
overall_age_median <- median(clean_data$age, na.rm = TRUE)
if (!is.finite(overall_age_median)) overall_age_median <- 0

clean_data <- clean_data |>
  group_by(pclass, sex) |>
  mutate(
    age = if_else(
      is.na(age),
      {
        group_median <- median(age, na.rm = TRUE)
        if (is.finite(group_median)) group_median else overall_age_median
      },
      age
    )
  ) |>
  ungroup()

# Impute missing Embarked with its mode.
mode_value <- function(x) {
  x_non_missing <- x[!is.na(x) & trimws(as.character(x)) != ""]
  if (length(x_non_missing) == 0) return(NA_character_)
  names(sort(table(as.character(x_non_missing)), decreasing = TRUE))[1]
}
embarked_mode <- mode_value(clean_data$embarked)
clean_data <- clean_data |>
  mutate(
    embarked = as.character(embarked),
    embarked = if_else(
      is.na(embarked) | trimws(embarked) == "",
      embarked_mode,
      embarked
    ),
    embarked = as.factor(embarked)
  )

# Impute missing Fare with the overall median.
fare_median <- median(clean_data$fare, na.rm = TRUE)
if (!is.finite(fare_median)) fare_median <- 0
clean_data <- clean_data |>
  mutate(fare = if_else(is.na(fare), fare_median, fare))

# Remove exact duplicate rows only; retain distinct passengers.
duplicates_removed <- sum(duplicated(clean_data))
clean_data <- clean_data |> distinct()

# Create useful derived variables.
clean_data <- clean_data |>
  mutate(
    family_size = sib_sp + parch + 1,
    is_alone = factor(if_else(family_size == 1, "Alone", "With family"))
  )

# Cabin is highly sparse in the common Titanic training dataset.
# Preserve cabin_missing as information, then exclude raw cabin text from analysis.
if ("cabin" %in% names(clean_data)) {
  cabin_missing_pct <- mean(is.na(clean_data$cabin) |
                              trimws(as.character(clean_data$cabin)) == "") * 100
  if (cabin_missing_pct > 50) {
    clean_data <- clean_data |> select(-cabin)
  }
} else {
  cabin_missing_pct <- NA_real_
}

# 4. OUTLIER DETECTION AND TREATMENT -------------------------------
iqr_bounds <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE, names = FALSE)
  q3 <- quantile(x, 0.75, na.rm = TRUE, names = FALSE)
  iqr <- q3 - q1
  c(lower = q1 - 1.5 * iqr, upper = q3 + 1.5 * iqr)
}

outlier_summary <- purrr::map_dfr(
  intersect(c("age", "fare", "sib_sp", "parch"), names(clean_data)),
  function(v) {
    x <- clean_data[[v]]
    bounds <- iqr_bounds(x)
    tibble(
      variable = v,
      lower_bound = unname(bounds["lower"]),
      upper_bound = unname(bounds["upper"]),
      outlier_count = sum(x < bounds["lower"] | x > bounds["upper"], na.rm = TRUE)
    )
  }
)
readr::write_csv(outlier_summary, "Outputs/outlier_summary.csv")

# Winsorize into separate columns; preserve the imputed source variables.
for (v in intersect(c("age", "fare"), names(clean_data))) {
  bounds <- iqr_bounds(clean_data[[v]])
  new_name <- paste0(v, "_capped")
  clean_data[[new_name]] <- pmin(
    pmax(clean_data[[v]], bounds["lower"]),
    bounds["upper"]
  )
}

# 5. NORMALIZATION ------------------------------------------------
# Min-max scaling is applied to selected continuous/count variables.
min_max_scale <- function(x) {
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  if (!is.finite(x_min) || !is.finite(x_max) || x_max == x_min) {
    return(rep(0, length(x)))
  }
  (x - x_min) / (x_max - x_min)
}

scale_fields <- intersect(
  c("age_capped", "fare_capped", "sib_sp", "parch", "family_size"),
  names(clean_data)
)

for (v in scale_fields) {
  clean_data[[paste0(v, "_scaled")]] <- min_max_scale(clean_data[[v]])
}

# 6. CATEGORICAL ENCODING -----------------------------------------
# Retain readable factor columns and create one-hot dummy variables
# for sex, pclass and embarked.
encoding_data <- clean_data |>
  select(any_of(c("sex", "pclass", "embarked"))) |>
  mutate(across(everything(), as.factor))

dummy_matrix <- model.matrix(~ . - 1, data = encoding_data)
dummy_data <- as.data.frame(dummy_matrix)
names(dummy_data) <- make.names(names(dummy_data), unique = TRUE)

encoded_data <- bind_cols(clean_data, as_tibble(dummy_data))

# Save outputs
readr::write_csv(clean_data, "Outputs/titanic_cleaned.csv")
readr::write_csv(encoded_data, "Outputs/titanic_cleaned_encoded.csv")

# 7. DESCRIPTIVE STATISTICS AND CORRELATION ------------------------
numeric_data <- clean_data |>
  select(where(is.numeric))

descriptive_stats <- purrr::map_dfr(names(numeric_data), function(v) {
  x <- numeric_data[[v]]
  tibble(
    variable = v,
    count = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    standard_deviation = sd(x, na.rm = TRUE),
    minimum = min(x, na.rm = TRUE),
    q1 = quantile(x, 0.25, na.rm = TRUE, names = FALSE),
    q3 = quantile(x, 0.75, na.rm = TRUE, names = FALSE),
    maximum = max(x, na.rm = TRUE)
  )
})
readr::write_csv(descriptive_stats, "Outputs/descriptive_statistics.csv")

cor_data <- clean_data |>
  select(any_of(c("age", "fare", "sib_sp", "parch", "family_size", "survived"))) |>
  mutate(across(everything(), as.numeric))

cor_matrix <- cor(cor_data, use = "pairwise.complete.obs", method = "pearson")
readr::write_csv(
  as.data.frame(cor_matrix) |> tibble::rownames_to_column("variable"),
  "Outputs/correlation_matrix.csv"
)

# Survival rate and group summaries
survival_rate <- mean(as.numeric(as.character(clean_data$survived)), na.rm = TRUE) * 100
survival_by_sex <- clean_data |>
  group_by(sex) |>
  summarise(
    passengers = n(),
    survival_percent = mean(as.numeric(as.character(survived)), na.rm = TRUE) * 100,
    .groups = "drop"
  )
survival_by_class <- clean_data |>
  group_by(pclass) |>
  summarise(
    passengers = n(),
    survival_percent = mean(as.numeric(as.character(survived)), na.rm = TRUE) * 100,
    .groups = "drop"
  )
readr::write_csv(survival_by_sex, "Outputs/survival_by_sex.csv")
readr::write_csv(survival_by_class, "Outputs/survival_by_class.csv")

# 8. VISUALIZATIONS ------------------------------------------------
theme_set(theme_minimal(base_size = 12))

p1 <- ggplot(clean_data, aes(x = factor(survived))) +
  geom_bar(fill = "#2878B5") +
  labs(title = "Passenger Survival Distribution",
       x = "Survival (0 = No, 1 = Yes)", y = "Passenger count")
ggsave("Visualizations/01_survival_distribution.png", p1,
       width = 7, height = 4.5, dpi = 300)

p2 <- ggplot(clean_data, aes(x = age)) +
  geom_histogram(bins = 30, fill = "#3A9D78", color = "white") +
  labs(title = "Age Distribution", x = "Age", y = "Passenger count")
ggsave("Visualizations/02_age_distribution.png", p2,
       width = 7, height = 4.5, dpi = 300)

p3 <- ggplot(clean_data, aes(x = factor(pclass), y = fare)) +
  geom_boxplot(fill = "#E5A13D", alpha = 0.8) +
  labs(title = "Fare Distribution by Passenger Class",
       x = "Passenger class", y = "Fare")
ggsave("Visualizations/03_fare_outliers.png", p3,
       width = 7, height = 4.5, dpi = 300)

p4 <- ggplot(survival_by_sex, aes(x = sex, y = survival_percent, fill = sex)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Survival Percentage by Sex",
       x = "Sex", y = "Survival (%)")
ggsave("Visualizations/04_survival_by_sex.png", p4,
       width = 7, height = 4.5, dpi = 300)

p5 <- ggplot(survival_by_class,
             aes(x = pclass, y = survival_percent, fill = pclass)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Survival Percentage by Passenger Class",
       x = "Passenger class", y = "Survival (%)")
ggsave("Visualizations/05_survival_by_class.png", p5,
       width = 7, height = 4.5, dpi = 300)

png("Visualizations/06_correlation_matrix.png",
    width = 1800, height = 1500, res = 220)
corrplot::corrplot(cor_matrix, method = "color", type = "upper",
                   addCoef.col = "black", tl.col = "black",
                   tl.srt = 45, number.cex = 0.65)
dev.off()

p7 <- naniar::vis_miss(raw_data) +
  labs(title = "Missing Values in the Original Dataset")
ggsave("Visualizations/07_missing_values.png", p7,
       width = 8, height = 5, dpi = 300)

# 9. QUALITY CHECKS ------------------------------------------------
remaining_missing <- sum(is.na(clean_data))
final_rows <- nrow(clean_data)
final_cols <- ncol(clean_data)

quality_checks <- tibble(
  check = c("Original rows", "Original columns", "Exact duplicate rows initially",
            "Exact duplicate rows removed", "Rows after cleaning",
            "Columns after cleaning", "Missing cells after cleaning"),
  result = c(n_rows, n_cols, duplicate_rows, duplicates_removed,
             final_rows, final_cols, remaining_missing)
)
readr::write_csv(quality_checks, "Outputs/quality_checks.csv")


# 10. BUILD A PROFESSIONAL WORD REPORT -----------------------------

doc <- officer::read_docx()

# Landscape page layout for readable statistical tables
landscape_section <- officer::prop_section(
  page_size = officer::page_size(orient = "landscape"),
  page_margins = officer::page_mar(
    top = 0.65,
    bottom = 0.65,
    left = 0.7,
    right = 0.7
  )
)

doc <- officer::body_set_default_section(
  doc,
  value = landscape_section
)

# Professional heading helper
add_heading <- function(doc, text, level = 1) {
  officer::body_add_par(
    doc,
    text,
    style = paste0("heading ", level)
  )
}

# Standard paragraph helper
add_text <- function(doc, text) {
  officer::body_add_par(
    doc,
    text,
    style = "Normal"
  )
}

# Professional table formatting
add_table <- function(doc, data) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(add_text(doc, "No records available for this table."))
  }
  
  ft <- flextable::flextable(as.data.frame(data))
  
  ft <- flextable::theme_vanilla(ft)
  
  ft <- flextable::bold(
    ft,
    part = "header",
    bold = TRUE
  )
  
  ft <- flextable::bg(
    ft,
    part = "header",
    bg = "#24476B"
  )
  
  ft <- flextable::color(
    ft,
    part = "header",
    color = "white"
  )
  
  ft <- flextable::fontsize(
    ft,
    size = 9,
    part = "all"
  )
  
  ft <- flextable::font(
    ft,
    fontname = "Arial",
    part = "all"
  )
  
  ft <- flextable::align(
    ft,
    align = "center",
    part = "all"
  )
  
  ft <- flextable::valign(
    ft,
    valign = "center",
    part = "all"
  )
  
  ft <- flextable::padding(
    ft,
    padding = 4,
    part = "all"
  )
  
  ft <- flextable::autofit(ft)
  
  ft <- flextable::set_table_properties(
    ft,
    layout = "autofit",
    width = 1
  )
  
  flextable::body_add_flextable(
    doc,
    value = ft
  )
}

# Centered image insertion
add_image <- function(doc, path, width = 6.2, height = 3.9) {
  
  if (!file.exists(path)) {
    return(doc)
  }
  
  img <- officer::external_img(
    src = path,
    width = width,
    height = height
  )
  
  officer::body_add_fpar(
    doc,
    officer::fpar(
      img,
      fp_p = officer::fp_par(
        text.align = "center"
      )
    )
  )
}


doc <- add_heading(doc, "1. Executive Summary")
doc <- add_text(doc, paste0(
  "This report documents data inspection, cleaning, transformation and preliminary ",
  "exploratory analysis of the Titanic passenger dataset. The original file contained ",
  n_rows, " records and ", n_cols, " variables. The analysis was performed in R using ",
  "tidyverse, janitor, naniar, corrplot, skimr and officer."
))
doc <- add_text(doc, paste0(
  "The overall survival proportion in the available dataset was ",
  round(survival_rate, 2),
  "%. This is a descriptive statistic and does not imply causation."
))

doc <- add_heading(doc, "2. Dataset Overview")
doc <- add_text(doc, paste0(
  "The dataset contains passenger-level information, including survival outcome, ",
  "passenger class, sex, age, family-related counts, fare and embarkation port. ",
  "The target variable Survived is coded 0 for did not survive and 1 for survived. ",
  "The dataset includes numerical and categorical attributes, making it suitable ",
  "for a data-cleaning and exploratory-analysis exercise."
))
doc <- add_text(doc, paste0("Initial dimensions: ", n_rows, " rows and ", n_cols, " columns."))
doc <- add_text(doc, "R functions used for initial inspection included dim(), names(), head(), str(), summary(), sapply() and skim().")

doc <- add_heading(doc, "3. Tools and Packages")
doc <- add_text(doc, paste(
  "R and RStudio were used as the analysis environment. tidyverse supported data",
  " manipulation and visualization; janitor standardized column names; naniar",
  " visualized missingness; corrplot displayed correlations; skimr generated",
  " detailed summaries; officer generated this Word report."
))

doc <- add_heading(doc, "4. Initial Data Quality Assessment")
doc <- add_text(doc, paste0(
  "The initial audit identified ", duplicate_rows,
  " exact duplicate rows. Missing-value counts and percentages are presented below."
))
doc <- add_table(doc, head(missing_summary, 20))
doc <- add_image(doc, "Visualizations/07_missing_values.png")

doc <- add_heading(doc, "5. Data Cleaning Methodology")
doc <- add_text(doc, paste(
  "Column names were standardized to lowercase snake_case. Numeric fields were",
  " converted to numeric types and categorical fields to factors. Missing Age values",
  " were imputed using the median within passenger-class and sex groups, with the",
  " overall median as a fallback. Missing Embarked values were replaced with the",
  " modal category. Missing Fare values were replaced with the median Fare."
))
doc <- add_text(doc, paste(
  "A cabin_missing indicator was created before considering the raw Cabin field.",
  " When Cabin was more than 50% missing, the sparse raw text field was excluded",
  " from the analysis while retaining the missingness indicator. Exact duplicate",
  " rows were removed; distinct passenger records were retained."
))
doc <- add_text(doc, paste0(
  "After cleaning, the dataset contained ", final_rows, " rows and ", final_cols,
  " columns, with ", remaining_missing, " missing cells."
))
doc <- add_table(doc, quality_checks)

doc <- add_heading(doc, "6. Outlier Detection and Treatment")
doc <- add_text(doc, paste(
  "Potential outliers were identified for Age, Fare, SibSp and Parch using the",
  " interquartile range (IQR) rule. Values below Q1 - 1.5 x IQR or above Q3 +",
  " 1.5 x IQR were flagged. For Age and Fare, capped versions were created using",
  " the calculated IQR limits. Original imputed values were retained to preserve",
  " traceability. Capping is a treatment choice and should be interpreted carefully."
))
doc <- add_table(doc, outlier_summary)
doc <- add_image(doc, "Visualizations/03_fare_outliers.png")

doc <- add_heading(doc, "7. Data Transformation and Encoding")
doc <- add_text(doc, paste(
  "Min-max normalization was applied to capped Age and Fare and selected count",
  " variables. Scaled columns range from 0 to 1 when the source variable has",
  " non-zero range. Categorical variables Sex, Pclass and Embarked were retained",
  " as factors and also converted into one-hot dummy columns using model.matrix().",
  " The original readable columns were preserved."
))
doc <- add_text(doc, paste(
  "Example R code: (1) x_scaled <- (x - min(x)) / (max(x) - min(x));",
  "(2) model.matrix(~ sex + pclass + embarked - 1, data = clean_data)."
))

doc <- add_heading(doc, "8. Exploratory Data Analysis")
doc <- add_text(doc, paste(
  "Exploratory analysis examined survival distribution, passenger age, fare",
  " distribution by class, and survival percentages by sex and passenger class."
))
doc <- add_image(doc, "Visualizations/01_survival_distribution.png")
doc <- add_image(doc, "Visualizations/02_age_distribution.png")
doc <- add_image(doc, "Visualizations/04_survival_by_sex.png")
doc <- add_image(doc, "Visualizations/05_survival_by_class.png")

doc <- add_heading(doc, "9. Descriptive Statistics")
doc <- add_text(doc, "The following table reports count, mean, median, standard deviation, minimum, quartiles and maximum for numeric variables.")
doc <- add_table(doc, descriptive_stats)
descriptive_stats <- descriptive_stats |>
  mutate(
    across(where(is.numeric), ~ round(.x, 2)),
    count = as.integer(count)
  )

doc <- add_heading(doc, "10. Correlation Analysis")
doc <- add_text(doc, paste(
  "Pearson correlations were calculated for selected numeric variables using",
  " pairwise complete observations. Correlation describes linear association",
  " and does not establish causation. The matrix and heatmap are provided below."
))
correlation_table <- as.data.frame(cor_matrix) |>
  tibble::rownames_to_column("variable") |>
  mutate(across(-variable, ~ round(.x, 3)))
doc <- add_table(doc, correlation_table)
doc <- add_image(doc, "Visualizations/06_correlation_matrix.png")

doc <- add_heading(doc, "11. Preliminary Findings")
top_sex <- survival_by_sex |> arrange(desc(survival_percent)) |> slice(1)
top_class <- survival_by_class |> arrange(desc(survival_percent)) |> slice(1)
doc <- add_text(doc, paste0(
  "The overall survival proportion was ", round(survival_rate, 2), "%. ",
  "Among the sex groups, the highest observed survival percentage was for ",
  as.character(top_sex$sex), " (", round(top_sex$survival_percent, 2), "%)."
))
doc <- add_text(doc, paste0(
  "Among passenger classes, the highest observed survival percentage was in class ",
  as.character(top_class$pclass), " (", round(top_class$survival_percent, 2), "%). ",
  "These are descriptive findings for this dataset and should not be interpreted ",
  "as causal effects or generalized without further validation."
))
doc <- add_table(doc, survival_by_sex)
doc <- add_table(doc, survival_by_class)

doc <- add_heading(doc, "12. Limitations")
doc <- add_text(doc, paste(
  "Median and mode imputation reduce missingness but may reduce variability and",
  " can affect statistical relationships. IQR-based capping changes extreme values;",
  " both original and capped variables are retained where applicable. One-hot",
  " encoding is included for analysis readiness, not as evidence that a predictive",
  " model has been trained. This assignment is preliminary EDA, not a causal study."
))

doc <- add_heading(doc, "13. Conclusion")
doc <- add_text(doc, paste(
  "The analysis established a reproducible workflow for importing, inspecting,",
  " cleaning, transforming and exploring the Titanic dataset. Missing values were",
  " treated using documented rules, potential outliers were assessed using IQR,",
  " numerical fields were normalized, and categorical fields were encoded. Tables",
  " and visualizations provide evidence for the preliminary findings."
))

doc <- add_heading(doc, "14. Appendix: Key R Code")
code_lines <- c(
  "dim(raw_data)",
  "str(raw_data)",
  "summary(raw_data)",
  "colSums(is.na(raw_data))",
  "bounds <- quantile(x, c(0.25, 0.75), na.rm = TRUE)",
  "scaled_x <- (x - min(x)) / (max(x) - min(x))",
  "dummy_matrix <- model.matrix(~ sex + pclass + embarked - 1, data = clean_data)",
  "cor(cor_data, use = 'pairwise.complete.obs', method = 'pearson')"
)
for (line in code_lines) {
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(line,
      prop = officer::fp_text(font.family = "Consolas", font.size = 9)))
  )
}
doc <- add_text(doc, paste(
  "The complete executable source code is supplied separately as",
  "01_complete_titanic_analysis.R. All reported statistics and figures are generated",
  "from the dataset at execution time."
))

report_file <- "Report/Week1_R_Data_Cleaning_Report.docx"
print(doc, target = report_file)

# 11. COMPLETION SUMMARY ------------------------------------------
cat("\n====================================================\n")
cat("ANALYSIS COMPLETED\n")
cat("====================================================\n")
cat("Original dimensions:", n_rows, "rows x", n_cols, "columns\n")
cat("Rows after cleaning:", final_rows, "\n")
cat("Missing cells after cleaning:", remaining_missing, "\n")
cat("Overall survival percentage:", round(survival_rate, 2), "%\n")
cat("Word report:", report_file, "\n")
cat("Cleaned dataset: Outputs/titanic_cleaned.csv\n")
cat("Encoded dataset: Outputs/titanic_cleaned_encoded.csv\n")
cat("Charts: Visualizations folder\n")
cat("\nPlease review the generated report and verify all outputs before submission.\n")
