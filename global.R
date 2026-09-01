# =============================================================================
# global.R
# =============================================================================
# Run with:  shiny::runApp()
#
# Helper files live in funs/ rather than R/ and are sourced explicitly. Shiny
# auto-sources R/, but the load order relative to global.R is a detail I would
# rather not depend on -- global.R needs these functions available immediately.
# =============================================================================

# --- Dependencies ------------------------------------------------------------
# Verifies the R version and the version of every package the app uses, and
# installs anything missing or outdated when the session is interactive. See
# dependencies.R for why this checks versions rather than just presence, and
# for how to turn auto-install off (ECHARTS4R_POC_AUTO_INSTALL=false).
#
# The library() calls below are kept explicit and unconditional on purpose:
# rsconnect and Posit Connect discover an app's dependencies by static analysis
# of library() calls, so hiding them behind a loop would break deployment.
source("dependencies.R")
ensure_dependencies()

library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(echarts4r)
library(visNetwork)

purrr::walk(
  list.files("funs", pattern = "\\.[Rr]$", full.names = TRUE),
  source
)

# --- Defaults ----------------------------------------------------------------
DEFAULT_N       <- 2000
DEFAULT_HORIZON <- 5
DEFAULT_SEED    <- 2026
DEFAULT_DEPTH   <- 3

DEFERRED_ARMS <- c("Active surveillance", "Watchful waiting")

# --- Minimal styling ---------------------------------------------------------
app_css <- shiny::HTML("
  body { background:#fbfbfc; }
  .app-title { font-weight:600; font-size:20px; margin:14px 0 2px 0; }
  .app-sub   { color:#777; font-size:13px; margin-bottom:14px; }
  .synthetic-banner {
    background:#fff6e5; border:1px solid #f0d9a8; border-radius:5px;
    padding:8px 12px; font-size:12.5px; color:#6b5322; margin-bottom:14px;
  }
  .panel-card {
    background:#fff; border:1px solid #e6e6e9; border-radius:6px;
    padding:14px 16px; margin-bottom:16px;
  }
  .kpi-row { display:flex; gap:12px; flex-wrap:wrap; margin-bottom:14px; }
  .kpi {
    flex:1 1 130px; background:#fff; border:1px solid #e6e6e9;
    border-radius:6px; padding:10px 14px;
  }
  .kpi-val   { font-size:21px; font-weight:600; color:#243b53; }
  .kpi-lab   { font-size:11.5px; color:#777; text-transform:uppercase;
               letter-spacing:.4px; }
  .branch-header { border-left:3px solid #2E6DA4; padding-left:10px; margin-bottom:10px; }
  .branch-path   { font-weight:600; font-size:14px; color:#243b53; }
  .branch-n      { font-size:12.5px; color:#666; }
  .branch-method { color:#aaa; font-size:11.5px; }
  .branch-empty  { color:#888; font-size:13.5px; padding:18px 4px; }
  .view-note     { color:#667; font-size:12.5px; background:#f4f6f8;
                   border-left:3px solid #ccd4dd; padding:7px 11px;
                   margin:4px 0 12px 0; }
  .branch-warn   {
    background:#fdecea; border:1px solid #f5c6c0; border-radius:4px;
    padding:7px 10px; font-size:12px; color:#8a2c22; margin-bottom:10px;
  }
")
