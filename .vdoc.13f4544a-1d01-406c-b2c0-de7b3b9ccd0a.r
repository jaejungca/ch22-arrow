#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: install-packages
#| eval: true
#| cache: true

# Run this chunk once if you have not installed these packages yet.
# After installing, you do not need to run it again.

# Packages required for this notebook
packages <- c(
  "bigrquery", # BigQuery connection and querying
  "DBI", # Standard database interface
  "dplyr", # Data manipulation
  "tidyr", # Rectangling nested columns
  "purrr", # Iteration over lists and vectors
  "arrow", # Parquet read/write and Arrow datasets
  "lubridate", # Date and time handling
  "glue", # String interpolation
  "fs", # File system operations
  "cli", # Formatted console messages
  "stringr", # String manipulation
  "scales", # Number formatting
  "bigrquerystorage" # Activates BigQuery Storage API streaming
)
# Detect and install only missing packages
missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]

if (length(missing_packages) > 0) {
  message(
    "Installing missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
  install.packages(missing_packages)
} else {
  message("All packages are already installed.")
}
#
#
#
#| label: load-packages
#| cache: true

library(bigrquery)
library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(arrow)
library(lubridate)
library(glue)
library(fs) # file system operations
library(cli) # Command Line Interface Tools to create rich, readable console output in R
library(stringr)
library(scales)
library(bigrquerystorage)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: set-project-id
#| eval: true

# ── Replace this string with your actual GCP Project ID ──────────────────────
# Example: "marketing-analytics-391408"
BILLING_PROJECT <- "marketing-analytics-491507" # ← CHANGE THIS LINE: your-actual-project-id

# ── Validate ──────────────────────────────────────────────────────────────────
if (BILLING_PROJECT == "your-actual-project-id") {
  stop(
    "\n",
    "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
    "  ACTION REQUIRED\n",
    "\n",
    "  You must replace 'your-actual-project-id' with your\n",
    "  real GCP Project ID before running this notebook.\n",
    "\n",
    "  Find it at: console.cloud.google.com\n",
    "  It looks like: 'my-project-name-391408'\n",
    "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
  )
}

if (nchar(trimws(BILLING_PROJECT)) == 0) {
  stop("BILLING_PROJECT cannot be an empty string.")
}

cli::cli_alert_success("Billing project set to: {.val {BILLING_PROJECT}}")
#
#
#
#
#
#
#
#
#
#| label: authenticate
#| eval: true
#| cache: true

# This opens a browser window asking you to sign in with your Google account.
# After signing in once, your credentials are cached — you will not need
# to do this every time you open R.
#
# use_oob = TRUE works reliably in Positron and RStudio when the
# automatic browser redirect does not complete correctly.

bq_auth(use_oob = TRUE)

# Confirm which account you are authenticated as
cli::cli_alert_info("Authenticated as: {.val {bq_user()}}")
#
#
#
#
#
#
#
#
#
#
#
#| label: connect
#| eval: true

# ── Connect to the public GA4 dataset ─────────────────────────────────────────
# project  = where the PUBLIC data lives (always "bigquery-public-data")
# dataset  = the specific GA4 dataset name (must be exact — case sensitive)
# billing  = YOUR project (this is what gets charged for query processing)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  dataset = "ga4_obfuscated_sample_ecommerce",
  billing = BILLING_PROJECT,
  bigint = "numeric" # Enforces safe conversion of 64-bit timestamps
)

# ── Verify the connection ─────────────────────────────────────────────────────
available_tables <- dbListTables(con)

cli::cli_alert_success(
  "Connected. {.val {length(available_tables)}} tables available."
)
cli::cli_alert_info(
  "Date range: {.val {head(available_tables, 1)}} to
  {.val {tail(available_tables, 1)}}"
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: inspect-schema
#| eval: true

# ── Inspect the schema of a single day ────────────────────────────────────────
# tbl() creates a lazy reference — nothing is downloaded yet
sample_tbl <- tbl(con, "events_20201101") #warning appears but no data pulled yet: no worries until you are ready to download the data

# glimpse() fetches a small sample to show column names and types
glimpse(sample_tbl)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: check-row-counts
#| eval: true

# ── Sample a few days to understand data volume ────────────────────────────────
sample_dates <- c(
  "events_20201101", # Start of dataset
  "events_20201201", # One month in
  "events_20210101" # Two months in
)

day_sizes <- sample_dates |>
  map_df(\(tbl_name) {
    n <- tbl(con, tbl_name) |>
      summarise(n = n()) |>
      collect(page_size = 10000) |>
      pull(n)
    tibble(
      table = tbl_name,
      date = ymd(str_extract(tbl_name, "\\d{8}")),
      row_count = n
    )
  })

day_sizes |>
  mutate(row_count = scales::comma(row_count)) |>
  knitr::kable(
    col.names = c("Table", "Date", "Row Count"),
    caption = "Sample row counts by day"
  )

cli::cli_alert_info(
  "Estimated total rows for 90 days: ~{scales::comma(mean(day_sizes$row_count) * 90)}"
)
#
#
#
#
#
#| label: plan-download
#| eval: true

# ── Define the three months to download ───────────────────────────────────────
# November 2020, December 2020, January 2021
download_dates <- seq.Date(
  from = as.Date("2020-11-01"),
  to = as.Date("2021-01-31"),
  by = "day"
)

# Format as BigQuery table name suffixes (YYYYMMDD)
date_strings <- format(download_dates, "%Y%m%d")
table_names <- paste0("events_", date_strings)

# ── Cross-check against tables that actually exist in BigQuery ────────────────
target_tables <- table_names[table_names %in% available_tables]
missing_tables <- table_names[!table_names %in% available_tables]

cli::cli_h2("Download Plan")
cli::cli_alert_info("Dates requested:   {length(date_strings)}")
cli::cli_alert_success("Tables confirmed:  {length(target_tables)}")

if (length(missing_tables) > 0) {
  cli::cli_alert_warning(
    "Tables not found:  {length(missing_tables)} (will be skipped)"
  )
  cat("Missing:", paste(missing_tables, collapse = ", "), "\n")
}

cat("\n")
cli::cli_bullets(c(
  "*" = "Tables to download: {length(target_tables)}",
  "*" = "Estimated rows:     ~{scales::comma(length(target_tables) * 35000)}",
  "*" = "Estimated time:     20-40 minutes",
  "*" = "Estimated storage:  300-500 MB total (all formats)"
))
#
#
#
#
#
#
#
#| label: create-folders

# ── Create all required directories ───────────────────────────────────────────
# fs::dir_create() is safer than base R dir.create() — it does not error If directory already exists → silently does nothing, leaves contents intact
# if the folder already exists, and creates nested folders automatically

dirs_to_create <- c(
  "data",
  "data/ga4_raw",
  "data/ga4_parquet",
  "data/ga4_parquet_params",
  "data/ga4_parquet_items",
  "data/ga4_partitioned",
  "data/ga4_params_partitioned",
  "data/ga4_items_partitioned"
)

walk(dirs_to_create, \(d) {
  fs::dir_create(d, recurse = TRUE)
})

cli::cli_alert_success("All data directories created.")

# ── Confirm they exist ────────────────────────────────────────────────────────
dirs_to_create |>
  keep(fs::dir_exists) |>
  walk(\(d) cli::cli_alert_info("  {d}"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: helper-flatten-structs

# Flatten all struct (RECORD) columns in a GA4 day's data
#
# Applies unnest_wider() to device, geo, traffic_source, and ecommerce.
# Also adds derived date/time columns useful for partitioning and analysis.
#
#' @param data A tibble downloaded from one GA4 BigQuery table
#' @return A flat tibble with no list-columns from struct fields

flatten_structs <- function(data) {
  # Identify which struct columns are present
  # (not all days have all columns)
  struct_cols <- c("device", "geo", "traffic_source", "ecommerce")
  present <- struct_cols[struct_cols %in% names(data)]

  result <- data |>
    # Remove list-columns that cannot go to Parquet
    # (event_params and items are handled by separate functions)
    select(-any_of(c("event_params", "items")))

  # Unnest each present struct column
  for (col in present) {
  if (is.data.frame(result[[col]])) {
    # If pre-flattened into a data frame, unpack it
    result <- result |> tidyr::unpack(!!rlang::sym(col), names_sep = "_")
  } else if (is.list(result[[col]])) {
    # If nested as a list, unnest it
    result <- result |> tidyr::unnest_wider(!!rlang::sym(col), names_sep = "_")
  }
}

  # Add derived columns for partitioning and convenience
  result |>
    mutate(
      event_date_parsed = ymd(event_date),
      year = year(event_date_parsed),
      month = month(event_date_parsed),
      day = day(event_date_parsed),
      event_dt = as.POSIXct(
        as.numeric(event_timestamp) / 1e6,
        origin = "1970-01-01",
        tz = "UTC"
      )
    ) |>
    select(-event_date_parsed) |> 
    select(where(~ !is.list(.))) # Dynamically drops remaining lists like user_properties
}
#
#
#
#
#
#| label: helper-flatten-params

# Flatten the event_params repeated RECORD column
#
# GA4 stores event parameters as an array of key-value structs where
# the value is itself a struct with four typed sub-fields (string, int,
# float, double). This function applies the three-stage pipeline:
#'   Stage 1: unnest_longer() — one row per parameter
#'   Stage 2: unnest_wider()  — expand key and value sub-fields
#'   Stage 3: coalesce()      — consolidate four value types into one
#'
# The result is kept in LONG format (one row per parameter per event)
# because pivot_wider() to fully wide format can produce thousands of
# columns across 90 days of data.
#
#' @param data A tibble downloaded from one GA4 BigQuery table
#' @return A long tibble: event_date, event_timestamp, event_name,
#'         user_pseudo_id, key, param_value — or NULL if no params exist

flatten_event_params <- function(data) {
  # Guard: check column exists
  if (!"event_params" %in% names(data)) {
    return(NULL)
  }

  # Guard: check at least some rows have params
  has_params <- !all(map_lgl(data$event_params, is.null))
  if (!has_params) {
    return(NULL)
  }

  result <- data |>
    select(
      event_date,
      event_timestamp,
      event_name,
      user_pseudo_id,
      event_params
    ) |>
    # Stage 1: Expand array — one row per parameter per event
    unnest_longer(event_params) |>
    # Stage 2a: Expand struct — get key and value columns
    unnest_wider(event_params) |>
    # Stage 2b: Expand value sub-struct — get typed value columns
    unnest_wider(value, names_sep = "_")
    
    # Enforce schema existence
    expected_cols <- c("value_string_value", "value_int_value", "value_float_value", "value_double_value")

    for (col in expected_cols) {
        if (!col %in% names(result)) {
            result[[col]] <- NA_character_
        }
    }

    # Stage 3: Consolidate four typed value columns into one safely
  result <- result |> 
    mutate(
      param_value = coalesce(
        as.character(value_string_value),
        as.character(value_int_value),
        as.character(value_float_value),
        as.character(value_double_value)
      )
    ) |>
    select(
      event_date,
      event_timestamp,
      event_name,
      user_pseudo_id,
      key,
      param_value
    )

  if (nrow(result) == 0) {
    return(NULL)
  }
  result
}
#
#
#
#
#
#| label: helper-flatten-items

#' Flatten the items repeated RECORD column
#'
#' The items column contains e-commerce line items as an array of named
#' structs. Unlike event_params, each item struct has named fields
#' (item_id, item_name, price, quantity, etc.) so a simple
#' unnest_longer() + unnest_wider() pipeline is sufficient.
#'
#' @param data A tibble downloaded from one GA4 BigQuery table
#' @return A flat tibble with one row per item per purchase event,
#'         or NULL if no purchase events exist in this day's data

flatten_items <- function(data) {
  # Guard: check column exists
  if (!"items" %in% names(data)) {
    return(NULL)
  }

  # Only purchase events have items
  purchase_data <- data |>
    filter(event_name == "purchase") |>
    select(event_date, event_timestamp, user_pseudo_id, items)

  if (nrow(purchase_data) == 0) {
    return(NULL)
  }

  # Guard: check at least some purchases have items populated
  has_items <- purchase_data |>
    filter(!map_lgl(items, \(x) is.null(x) || nrow(x) == 0))

  if (nrow(has_items) == 0) {
    return(NULL)
  }

  has_items |>
    unnest_longer(items) |> # One row per item per purchase
    unnest_wider(items) # Named item fields become columns
}
#
#
#
#| label: confirm-helpers

cli::cli_alert_success("All three helper functions are defined and ready.")
cli::cli_bullets(c(
  "v" = "flatten_structs()       — flattens device, geo, traffic_source, ecommerce",
  "v" = "flatten_event_params()  — applies 3-stage pipeline to event_params",
  "v" = "flatten_items()         — expands items array for purchase events"
))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: download-method-update
#| eval: false
library(bigrquery)
library(bigrquerystorage) # Activates BigQuery Storage API streaming

# Inside the loop:
raw_day <- tbl(con, tbl_name) |> collect() # page_size removed entirely
#
#
#
#
#
#
#
#
#
#| label: download-loop
#| eval: true
#| cache: true

cli::cli_h1("Starting GA4 Download")
cli::cli_alert_info(
  "Downloading {length(target_tables)} tables. Please wait..."
)
cat("\n")

# ── Initialise the download log ────────────────────────────────────────────────
download_log <- vector("list", length(target_tables))
names(download_log) <- target_tables

# ── Main loop ─────────────────────────────────────────────────────────────────
for (i in seq_along(target_tables)) {
  tbl_name <- target_tables[i]
  date_str <- str_extract(tbl_name, "\\d{8}")

  # File paths for each format
  rds_path <- glue("data/ga4_raw/ga4_{date_str}.rds")
  parq_path <- glue("data/ga4_parquet/ga4_{date_str}.parquet")
  params_path <- glue("data/ga4_parquet_params/params_{date_str}.parquet")
  items_path <- glue("data/ga4_parquet_items/items_{date_str}.parquet")

  all_exist <- all(
    fs::file_exists(rds_path),
    fs::file_exists(parq_path),
    fs::file_exists(params_path)
  )

  # ── Skip if already downloaded ─────────────────────────────────────────────
  if (all_exist) {
    cli::cli_alert_info(
      "[{i}/{length(target_tables)}] Skipping {date_str} — already exists"
    )
    download_log[[tbl_name]] <- list(status = "skipped", rows = NA_integer_)
    next
  }

  # ── Download with error handling ───────────────────────────────────────────
  cli::cli_progress_step(
    "[{i}/{length(target_tables)}] Downloading {date_str}..."
  )

  day_result <- tryCatch(
    {
      # ── Download raw data ────────────────────────────────────────────────────
      raw_day <- tbl(con, tbl_name) |> collect()
      n_rows <- nrow(raw_day)

      # ── Save raw nested data as .rds ─────────────────────────────────────────
      # compress = FALSE: fastest read/write — use "gz" to save disk space
      saveRDS(raw_day, rds_path, compress = FALSE)

      # ── Flatten structs and save as Parquet ──────────────────────────────────
      flat_day <- flatten_structs(raw_day)
      write_parquet(flat_day, parq_path, compression = "snappy")

      # ── Flatten event_params and save as Parquet ─────────────────────────────
      params_flat <- flatten_event_params(raw_day)
      if (!is.null(params_flat) && nrow(params_flat) > 0) {
        write_parquet(params_flat, params_path, compression = "snappy")
      }

      # ── Flatten items and save as Parquet ────────────────────────────────────
      items_flat <- flatten_items(raw_day)
      if (!is.null(items_flat) && nrow(items_flat) > 0) {
        write_parquet(items_flat, items_path, compression = "snappy")
      }

      # ── Clean up to free RAM before next iteration ───────────────────────────
      rm(raw_day, flat_day, params_flat, items_flat) # remove
      gc(full = TRUE, verbose = FALSE) # verbose = FALSE argument just suppresses the printed summary that gc() would normally output

      cli::cli_alert_success(
        "  Done: {scales::comma(n_rows)} rows saved in 3 formats"
      )

      list(status = "success", rows = n_rows)
    },
    error = function(e) {
      cli::cli_alert_danger(
        "  FAILED: {date_str} — {conditionMessage(e)}"
      )
      list(status = "error", error = conditionMessage(e), rows = NA_integer_)
    }
  )

  download_log[[tbl_name]] <- day_result

  # ── Brief pause every 10 downloads to avoid rate limiting ─────────────────
  if (i %% 10 == 0 && i < length(target_tables)) {
    cli::cli_alert_info("  Pausing briefly to avoid rate limits...")
    Sys.sleep(3)
  }
}

cli::cli_alert_success("Download loop complete!")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: partition-events
#| eval: true
#| cache: true

cli::cli_h2("Building partitioned events dataset")

# ── Find all daily Parquet files ──────────────────────────────────────────────
event_files <- fs::dir_ls("data/ga4_parquet", regexp = "\\.parquet$")

if (length(event_files) == 0) {
  cli::cli_abort(
    "No Parquet files found in data/ga4_parquet/.
    Run the download loop first."
  )
}

cli::cli_alert_info(
  "Found {length(event_files)} daily Parquet files to combine"
)

# ── Open all files as one Arrow dataset and write partitioned ─────────────────
open_dataset(event_files) |>
  write_dataset(
    path = "data/ga4_partitioned",
    format = "parquet",
    partitioning = c("year", "month"),
    compression = "snappy",
    max_rows_per_file = 500000
  )

cli::cli_alert_success(
  "Events partitioned dataset written to data/ga4_partitioned/"
)
#
#
#
#
#
#| label: partition-params
#| eval: false
#| cache: false

cli::cli_h2("Building partitioned event_params dataset")

params_files <- fs::dir_ls(
  "data/ga4_parquet_params",
  regexp = "\\.parquet$"
)

if (length(params_files) == 0) {
  cli::cli_alert_warning(
    "No params Parquet files found. Skipping params partitioning."
  )
} else {
  cli::cli_alert_info(
    "Found {length(params_files)} params files to combine"
  )

  open_dataset(params_files) |>
    mutate(
      year = as.integer(str_sub(event_date, 1L, 4L)),
      month = as.integer(str_sub(event_date, 5L, 6L))
    ) |>
    write_dataset(
      path = "data/ga4_params_partitioned",
      format = "parquet",
      partitioning = c("year", "month"),
      compression = "snappy"
    )

  cli::cli_alert_success(
    "Params partitioned dataset written to data/ga4_params_partitioned/"
  )
}
#
#
#
#
#
#| label: partition-items
#| eval: false
#| cache: false

cli::cli_h2("Building partitioned items dataset")

items_files <- fs::dir_ls(
  "data/ga4_parquet_items",
  regexp = "\\.parquet$"
)

if (length(items_files) == 0) {
  cli::cli_alert_warning(
    "No items Parquet files found.
    This is expected if no purchase events occurred.
    Skipping items partitioning."
  )
} else {
  cli::cli_alert_info(
    "Found {length(items_files)} items files to combine"
  )

  open_dataset(items_files) |>
    mutate(
      year = as.integer(str_sub(event_date, 1L, 4L)),
      month = as.integer(str_sub(event_date, 5L, 6L))
    ) |>
    write_dataset(
      path = "data/ga4_items_partitioned",
      format = "parquet",
      partitioning = c("year", "month"),
      compression = "snappy"
    )

  cli::cli_alert_success(
    "Items partitioned dataset written to data/ga4_items_partitioned/"
  )
}
#
#
#
#
#
#
#
#
#
#
#
#| label: verify-file-counts

cli::cli_h2("Verification: File Counts")

# Define what we expect in each folder
expected_counts <- tibble::tribble(
  ~folder                   , ~description                         ,
  "data/ga4_raw"            , "Raw nested .rds files"              ,
  "data/ga4_parquet"        , "Flat events Parquet"                ,
  "data/ga4_parquet_params" , "Event params Parquet"               ,
  "data/ga4_parquet_items"  , "Items Parquet (purchase days only)"
)

# Count files actually present
file_counts <- expected_counts |>
  mutate(
    exists = map_lgl(folder, fs::dir_exists),
    n_files = map_int(folder, \(d) {
      if (!fs::dir_exists(d)) {
        return(0L)
      }
      length(fs::dir_ls(d, regexp = "\\.(rds|parquet)$"))
    }),
    status = case_when(
      !exists ~ "folder missing",
      n_files == 0 ~ "empty",
      n_files < 85 ~ glue("⚠ only {n_files} files"),
      n_files >= 85 ~ glue("✓ {n_files} files"),
      TRUE ~ as.character(n_files)
    )
  ) |>
  select(description, folder, n_files, status)

knitr::kable(
  file_counts,
  col.names = c("Dataset", "Folder", "Files", "Status"),
  caption = "File count by folder"
)
#
#
#
#
#
#| label: verify-storage

cli::cli_h2("Verification: Storage Used")

all_data_folders <- c(
  "data/ga4_raw",
  "data/ga4_parquet",
  "data/ga4_parquet_params",
  "data/ga4_parquet_items",
  "data/ga4_partitioned",
  "data/ga4_params_partitioned",
  "data/ga4_items_partitioned"
)

storage_summary <- all_data_folders |>
  keep(fs::dir_exists) |>
  map_df(\(d) {
    files <- fs::dir_ls(d, recurse = TRUE, regexp = "\\.(rds|parquet)$")
    size_mb <- sum(as.numeric(fs::file_size(files))) / 1024^2
    tibble(
      folder = d,
      n_files = length(files),
      size_mb = round(size_mb, 1)
    )
  })

storage_summary |>
  mutate(size_mb = glue("{size_mb} MB")) |>
  knitr::kable(
    col.names = c("Folder", "Files", "Size"),
    caption = "Storage used by folder"
  )

total_mb <- sum(storage_summary$size_mb)
cli::cli_alert_info(
  "Total storage used: {round(total_mb / 1024, 2)} GB
  ({scales::comma(total_mb)} MB)"
)
#
#
#
#
#
#| label: verify-partitions
#| eval: false

cli::cli_h2("Verification: Partition Structure")

if (fs::dir_exists("data/ga4_partitioned")) {
  ga4_check <- open_dataset("data/ga4_partitioned")

  cat("Schema:\n")
  print(ga4_check$schema)
  cat("\n")

  partition_summary <- ga4_check |>
    count(year, month, name = "rows") |>
    collect() |>
    arrange(year, month) |>
    mutate(
      month_name = month.abb[month],
      period = glue("{month_name} {year}"),
      rows = scales::comma(rows)
    ) |>
    select(period, year, month, rows)

  knitr::kable(
    partition_summary,
    col.names = c("Period", "Year", "Month", "Rows"),
    caption = "Row counts by partition"
  )

  total_rows <- ga4_check |>
    summarise(n = n()) |>
    collect() |>
    pull(n)

  cli::cli_alert_success(
    "Total rows across all partitions: {scales::comma(total_rows)}"
  )
} else {
  cli::cli_alert_warning(
    "Partitioned dataset not found. Run the partitioning chunks above."
  )
}
#
#
#
#
#
#| label: verify-sample-data
#| eval: false

cli::cli_h2("Verification: Sample Data")

# ── Core events ───────────────────────────────────────────────────────────────
cat("── Core events (first 5 rows, November 2020) ──────────────────────\n")
if (fs::dir_exists("data/ga4_partitioned")) {
  open_dataset("data/ga4_partitioned") |>
    filter(year == 2020, month == 11) |>
    select(
      event_date,
      event_name,
      user_pseudo_id,
      device_category,
      geo_country
    ) |>
    head(5) |>
    collect() |>
    knitr::kable()
}

# ── Event params ──────────────────────────────────────────────────────────────
cat("\n── Event params (first 5 rows) ─────────────────────────────────────\n")
if (fs::dir_exists("data/ga4_params_partitioned")) {
  open_dataset("data/ga4_params_partitioned") |>
    filter(year == 2020, month == 11) |>
    head(5) |>
    collect() |>
    knitr::kable()
}

# ── Items ─────────────────────────────────────────────────────────────────────
cat("\n── Items (first 5 rows) ─────────────────────────────────────────────\n")
if (fs::dir_exists("data/ga4_items_partitioned")) {
  open_dataset("data/ga4_items_partitioned") |>
    head(5) |>
    collect() |>
    knitr::kable()
}
#
#
#
#
#
#
#
#| label: download-summary
#| eval: false

cli::cli_h1("Download Summary")

if (exists("download_log") && length(download_log) > 0) {
  # ── Build log data frame ──────────────────────────────────────────────────
  log_df <- download_log |>
    imap_df(\(x, nm) {
      tibble(
        table = nm,
        status = x$status,
        rows = x$rows %||% NA_integer_
      )
    }) |>
    mutate(
      date = ymd(str_extract(table, "\\d{8}")),
      month_label = glue("{month.abb[month(date)]} {year(date)}")
    )

  # ── Status summary ────────────────────────────────────────────────────────
  cat("Status Summary:\n")
  log_df |>
    count(status, name = "tables") |>
    knitr::kable(col.names = c("Status", "Tables"))

  # ── Monthly summary ───────────────────────────────────────────────────────
  cat("\nRows by Month:\n")
  log_df |>
    filter(status == "success") |>
    group_by(month_label) |>
    summarise(
      days_downloaded = n(),
      total_rows = sum(rows, na.rm = TRUE),
      avg_rows_per_day = round(mean(rows, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(
      total_rows = scales::comma(total_rows),
      avg_rows_per_day = scales::comma(avg_rows_per_day)
    ) |>
    knitr::kable(
      col.names = c("Month", "Days", "Total Rows", "Avg Rows/Day")
    )

  # ── Flag any failures ─────────────────────────────────────────────────────
  failed <- log_df |> filter(status == "error")

  if (nrow(failed) > 0) {
    cli::cli_alert_danger("{nrow(failed)} tables failed:")
    failed |>
      select(table, status) |>
      print()
    cli::cli_alert_info(
      "Re-run the download loop — it skips successful days automatically."
    )
  } else {
    cli::cli_alert_success("All tables downloaded successfully!")
  }
}
#
#
#
#
#
#
#
#| label: disconnect
#| eval: false
#| cache: false

# Close the BigQuery connection — no longer needed after download
dbDisconnect(con)
cli::cli_alert_success("BigQuery connection closed.")
cli::cli_alert_info(
  "You will not need to reconnect to BigQuery for the rest of this course."
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: loading-patterns
#| eval: false

library(arrow)
library(dplyr)
library(duckdb)
library(duckplyr)

# ── Pattern 1: Partitioned dataset — recommended for most analyses ─────────────
# Lazy: nothing loaded into RAM until collect()
# Fast: Arrow skips irrelevant year/month folders automatically
ga4_events <- open_dataset("data/ga4_partitioned")
ga4_params <- open_dataset("data/ga4_params_partitioned")
ga4_items <- open_dataset("data/ga4_items_partitioned")

# Query a specific month
november_events <- ga4_events |>
  filter(year == 2020, month == 11) |>
  select(event_date, event_name, user_pseudo_id, device_category) |>
  collect()

# ── Pattern 2: Single day flat Parquet ────────────────────────────────────────
# Eager: loads immediately into RAM as a tibble
# Use for: single-day analysis, quick exploration
day_data <- read_parquet("data/ga4_parquet/ga4_20201101.parquet")

# ── Pattern 3: Raw nested .rds ────────────────────────────────────────────────
# Use for: teaching and practising rectangling techniques
raw_day <- readRDS("data/ga4_raw/ga4_20201101.rds")
glimpse(raw_day) # List-columns are still nested — ready for rectangling

# ── Pattern 4: DuckDB on partitioned Parquet ──────────────────────────────────
# Use for: SQL queries, window functions, complex multi-table joins
con <- dbConnect(duckdb::duckdb())
duckdb_register_arrow(con, "events", open_dataset("data/ga4_partitioned"))
duckdb_register_arrow(
  con,
  "params",
  open_dataset("data/ga4_params_partitioned")
)
duckdb_register_arrow(con, "items", open_dataset("data/ga4_items_partitioned"))

DBI::dbGetQuery(
  con,
  "
  SELECT event_name, COUNT(*) AS n
  FROM events
  GROUP BY event_name
  ORDER BY n DESC
  LIMIT 10
"
)

DBI::dbDisconnect(con, shutdown = TRUE)

# ── Pattern 5: duckplyr on Parquet ────────────────────────────────────────────
# Use for: dplyr syntax with DuckDB speed on large data
ga4_duck <- duckplyr::df_from_parquet(
  fs::dir_ls("data/ga4_parquet", regexp = "\\.parquet$")
)

ga4_duck |>
  count(event_name, sort = TRUE) |>
  head(10)
#
#
#
#
#
#
#
#| label: troubleshooting
#| eval: false

# ── Problem 1: "notFound" error ───────────────────────────────────────────────
# Symptom: Error in bq_post(): Request couldn't be served. [notFound]
# Causes and fixes:

# Cause A: BILLING_PROJECT is wrong
cat("Current billing project:", BILLING_PROJECT, "\n")
bq_projects() # Your project MUST appear in this list

# Cause B: BigQuery API not enabled on your project
# Fix: Go to console.cloud.google.com/apis/library/bigquery.googleapis.com
#      select your project and click Enable

# Cause C: Dataset name has a typo
# The correct name is exactly:
# "ga4_obfuscated_sample_ecommerce"
# Check available datasets:
bq_project_datasets("bigquery-public-data") |>
  as_tibble() |>
  dplyr::filter(stringr::str_detect(dataset, "ga4"))

# ── Problem 2: Integer overflow warning ───────────────────────────────────────
# Symptom: NAs produced by integer overflow
# Cause: event_timestamp is INT64, too large for R's 32-bit integer
# Fix: collect() via tbl() handles this automatically through Arrow
# If using bq_table_download() directly, add: bigint = "numeric"

# ── Problem 3: Download stalls or times out ───────────────────────────────────
# Fix: The loop saves each day before moving to the next
# Simply re-run the download chunk — already-saved files are skipped
already_saved <- fs::dir_ls("data/ga4_parquet") |>
  fs::path_file() |>
  str_extract("\\d{8}") |>
  na.omit()

still_needed <- date_strings[!date_strings %in% already_saved]
cli::cli_alert_info(
  "{length(still_needed)} tables still need downloading"
)

# ── Problem 4: "Cannot allocate vector" — out of memory ──────────────────────
# Cause: One day's data is larger than available RAM
# Fix: Download fewer columns per day
tbl(con, "events_20201101") |>
  select(
    event_date,
    event_timestamp,
    event_name,
    user_pseudo_id,
    device,
    geo,
    traffic_source
  ) |>
  collect() # Excludes event_params and items — much smaller

# ── Problem 5: Parquet write error with list-columns ─────────────────────────
# Symptom: Error writing Parquet — type not supported
# Cause: Trying to write a tibble that still has list-columns
# Fix: The flatten_structs() function removes event_params and items
# before writing. Check that it ran without error.

# ── Problem 6: Partitioned dataset reads wrong columns ───────────────────────
# Symptom: year and month columns show as integers but filtering fails
# Fix: Confirm the partition columns exist in the schema
open_dataset("data/ga4_partitioned")$schema
# year and month should appear as int32 or int64
#
#
#
#
#
#
#
#| label: session-info
#| code-fold: true
#| code-summary: "Show session info"
#| cache: false
#| eval: false

sessioninfo::session_info()
#
#
#
#
#
#
#
#
#
#
#
# ❌ Base R - plain and hard to scan
message("Downloading table 1 of 92")
print("Done: 51941 rows saved")

# ✅ cli - formatted, colored, and easy to scan
cli::cli_alert_info("Downloading table {1} of {92}")
cli::cli_alert_success("Done: {scales::comma(51941)} rows saved")
#
#
#
#
#
#
#
cli::cli_alert_info("Starting download...")       # ℹ blue  - general info
cli::cli_alert_success("Download complete!")      # ✔ green - success
cli::cli_alert_warning("Token refreshing...")     # ! yellow - warning
cli::cli_alert_danger("Download failed!")         # ✖ red   - error
#
#
#
#
#
cli::cli_h1("GA4 Download Pipeline")   # Large header
cli::cli_h2("Processing event_params") # Medium header
cli::cli_h3("Flattening structs")      # Small header
#
#
#
#
#
pb <- cli::cli_progress_bar(
  total  = 92,
  format = "Downloading {cli::pb_current}/{cli::pb_total} | {cli::pb_bar} | ETA: {cli::pb_eta}"
)

for (i in 1:92) {
  # your download code
  cli::cli_progress_update(id = pb)
}

cli::cli_progress_done(id = pb)
#
#
#
#
#
date  <- "20201101"
rows  <- 51941

# cli automatically evaluates {} expressions
cli::cli_alert_success("Downloaded {date}: {scales::comma(rows)} rows")
# ✔ Downloaded 20201101: 51,941 rows
#
#
#
#
#
