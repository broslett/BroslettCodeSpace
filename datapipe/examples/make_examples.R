#!/usr/bin/env Rscript
# Generates the example input files used by the demo pipeline and the tests.
# The data is deliberately messy: mixed key formats, leading zeros lost to
# Excel, stray punctuation, blanks, duplicates and an unmatched row.

app_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa)) dirname(dirname(normalizePath(sub("^--file=", "", fa[1])))) else getwd()
})
suppressPackageStartupMessages(library(writexl))
# Brings dp_ensure_utf8_locale() with it, so the accented name below survives
# even when this script runs in a bare "C" locale.
source(file.path(app_dir, "R", "utils.R"))

dir.create(file.path(app_dir, "examples", "data"), recursive = TRUE, showWarnings = FALSE)
out <- function(...) file.path(app_dir, "examples", "data", ...)

# --- transactions: the base table ------------------------------------------
# Account numbers arrive with a branch prefix and punctuation.
transactions <- data.frame(
  `Txn Ref`   = sprintf("TX%04d", 1:12),
  `Account`   = c("AC-000123 ", "ac-000456", "AC/000789", "AC-001011",
                  "AC-000123", "AC-000456 ", "AC-001213", "AC-000789",
                  "AC-001415", "AC-000123", "AC-9999999", ""),
  `Prod Code` = c("wid-01", "WID-02", "wid-03", "WID-01", "WID-04", "wid-02",
                  "WID-03", "WID-05", "WID-01", "WID-02", "WID-01", "WID-03"),
  `Amount`    = c("1250.5", "  99.99", "3,400.00", "12.5", "780", "45.25",
                  "1,199.99", "620.10", "88", "310.75", "5000", "17.6"),
  `Txn Date`  = c("01/03/2026", "02/03/2026", "03/03/2026", "05/03/2026",
                  "07/03/2026", "11/03/2026", "12/03/2026", "15/03/2026",
                  "18/03/2026", "21/03/2026", "22/03/2026", "28/03/2026"),
  `Status`    = c("posted", "posted", "PENDING", "posted", "posted", "posted",
                  "pending", "Posted", "posted", "POSTED", "posted", "void"),
  check.names = FALSE, stringsAsFactors = FALSE
)
write.csv(transactions, out("transactions.csv"), row.names = FALSE, na = "")

# --- customers: an Excel export that dropped its leading zeros -------------
customers <- data.frame(
  AccountNo   = c(123, 456, 789, 1011, 1213, 1415, 1617),
  CustomerName = c("Ada Lovelace", "Grace Hopper", "Alan Turing", "Karen Spärck Jones",
                   "Edsger Dijkstra", "Barbara Liskov", "Unused Account"),
  Region      = c("North", "South", "North", "East", "West", "South", "North"),
  `Signed Up` = c("2019-04-01", "2020-11-23", "2018-01-15", "2021-07-30",
                  "2017-09-09", "2022-02-14", "2023-05-05"),
  check.names = FALSE, stringsAsFactors = FALSE
)
writexl::write_xlsx(list(Customers = customers), out("customers.xlsx"))

# --- product mapping: the mapping document ---------------------------------
# Contains a deliberate duplicate key (WID-02) to exercise duplicate handling.
products <- data.frame(
  Code        = c("WID-01", "WID-02", "WID-03", "WID-04", "WID-05", "WID-02"),
  ProductName = c("Standard Widget", "Deluxe Widget", "Compact Widget",
                  "Industrial Widget", "Spare Part Kit", "Deluxe Widget (old)"),
  Category    = c("Widgets", "Widgets", "Widgets", "Industrial", "Parts", "Widgets"),
  `Unit Cost` = c("10.00", "25.00", "7.50", "180.00", "42.00", "24.00"),
  check.names = FALSE, stringsAsFactors = FALSE
)
write.csv(products, out("product_mapping.csv"), row.names = FALSE, na = "")

# --- region mapping: a second lookup, keyed off a joined column ------------
regions <- data.frame(
  Region      = c("North", "South", "East", "West"),
  RegionCode  = c("N", "S", "E", "W"),
  Manager     = c("J. Baker", "P. Osei", "L. Chen", "M. Rossi"),
  stringsAsFactors = FALSE
)
write.csv(regions, out("region_mapping.csv"), row.names = FALSE, na = "")

cat("Example data written to ", out(""), "\n", sep = "")
