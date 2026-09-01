# =============================================================================
# dependencies.R -- declare, check and (optionally) install what the app needs
# =============================================================================
# Sourced by global.R BEFORE any library() call.
#
# BASE R ONLY. Nothing in this file may depend on a package, because the whole
# point is that those packages might not be installed yet.
#
# Three design decisions worth knowing about:
#
#  1. VERSIONS, NOT JUST PRESENCE. The common idiom
#         if (!require(x)) install.packages(x)
#     does not help here. The app uses dplyr::case_match() and dplyr::pick()
#     (dplyr >= 1.1.0) and purrr::list_rbind() (purrr >= 1.0.0). With an older
#     dplyr present, that idiom reports success and the app then dies with
#     "could not find function case_match" -- a far more confusing failure than
#     a clear version message at startup.
#
#  2. NEVER INSTALLS SILENTLY WHEN NON-INTERACTIVE. Installing packages as a
#     side effect of app startup is reasonable at a console and a bad idea on a
#     deployment target, where it can block on a prompt, write to a read-only
#     library, or quietly change the versions a served app is running.
#     Non-interactive sessions get a clear error listing what to install.
#
#  3. USES pak WHEN AVAILABLE. Faster resolution and better system-dependency
#     handling than install.packages(); falls back cleanly when absent.
#
# To disable auto-install:  Sys.setenv(ECHARTS4R_POC_AUTO_INSTALL = "false")
# To force it:              Sys.setenv(ECHARTS4R_POC_AUTO_INSTALL = "true")
# =============================================================================


APP_MIN_R <- "4.1.0"   # native pipe |> and \(x) lambda syntax

APP_DEPS <- c(
  shiny       = "1.7.0",
  dplyr       = "1.1.0",   # case_match(), pick()
  tidyr       = "1.2.0",
  purrr       = "1.0.0",   # list_rbind()
  tibble      = "3.0.0",
  ggplot2     = "3.4.0",   # linewidth aesthetic
  echarts4r   = "0.4.5",   # e_on()
  htmlwidgets = "1.5.4",
  visNetwork  = "2.1.0",   # visIgraphLayout(), visEvents()
  igraph      = "1.3.0"    # layout precomputation for visIgraphLayout()
)


#' Report the installed status of each dependency
#'
#' @return data.frame(package, required, installed, status) where status is one
#'   of "ok", "outdated", "missing".
dep_status <- function(deps = APP_DEPS) {

  pkgs      <- names(deps)
  installed <- character(length(pkgs))
  status    <- character(length(pkgs))

  for (i in seq_along(pkgs)) {
    have <- tryCatch(utils::packageVersion(pkgs[i]), error = function(e) NULL)
    if (is.null(have)) {
      installed[i] <- NA_character_
      status[i]    <- "missing"
    } else {
      installed[i] <- as.character(have)
      status[i]    <- if (have < package_version(deps[[i]])) "outdated" else "ok"
    }
  }

  data.frame(
    package   = pkgs,
    required  = unname(unlist(deps)),
    installed = installed,
    status    = status,
    stringsAsFactors = FALSE
  )
}


#' Install the packages that are missing or too old
#'
#' @param quiet Suppress the "installing..." message.
#' @return Character vector of packages installed, invisibly.
install_deps <- function(deps = APP_DEPS, quiet = FALSE) {

  st   <- dep_status(deps)
  need <- st$package[st$status != "ok"]

  if (!length(need)) {
    if (!quiet) message("All dependencies satisfied.")
    return(invisible(character(0)))
  }

  if (!quiet) {
    message("Installing: ", paste(need, collapse = ", "))
  }

  if (requireNamespace("pak", quietly = TRUE)) {
    pak::pak(need, ask = FALSE)
  } else {
    utils::install.packages(need)
  }

  # Re-check: install.packages() does not error on failure, it warns
  after   <- dep_status(deps)
  still   <- after$package[after$status != "ok"]
  if (length(still)) {
    warning(
      "Still unsatisfied after installation: ", paste(still, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(need)
}


#' Resolve whether auto-install is wanted
#'
#' Env var wins if set to something interpretable; otherwise auto-install only
#' in an interactive session.
auto_install_enabled <- function() {
  raw <- Sys.getenv("ECHARTS4R_POC_AUTO_INSTALL", unset = "")
  if (nzchar(raw)) {
    val <- suppressWarnings(as.logical(raw))
    if (!is.na(val)) return(val)
  }
  interactive()
}


#' Startup gate: verify R version and dependencies, installing if permitted
#'
#' Called from global.R. Stops with an actionable message rather than letting
#' the app fail later with an obscure "could not find function" error.
ensure_dependencies <- function(deps = APP_DEPS, auto = auto_install_enabled()) {

  # --- R version ----------------------------------------------------------
  if (getRversion() < package_version(APP_MIN_R)) {
    stop(
      sprintf(
        paste0(
          "This app needs R >= %s (native pipe and \\(x) lambda syntax); ",
          "you are running %s."
        ),
        APP_MIN_R, as.character(getRversion())
      ),
      call. = FALSE
    )
  }

  # --- Packages -----------------------------------------------------------
  st   <- dep_status(deps)
  need <- st[st$status != "ok", , drop = FALSE]

  if (!nrow(need)) return(invisible(st))

  detail <- paste0(
    "  - ", need$package,
    ifelse(
      need$status == "missing",
      paste0(" (not installed; need >= ", need$required, ")"),
      paste0(" (have ", need$installed, "; need >= ", need$required, ")")
    ),
    collapse = "\n"
  )

  if (isTRUE(auto)) {
    message(
      "Some dependencies are missing or out of date:\n", detail,
      "\nInstalling now. Set ECHARTS4R_POC_AUTO_INSTALL=false to disable this."
    )
    install_deps(deps, quiet = TRUE)

    final <- dep_status(deps)
    if (any(final$status != "ok")) {
      stop(
        "Dependency installation did not resolve everything. Still unsatisfied:\n",
        paste0("  - ", final$package[final$status != "ok"], collapse = "\n"),
        call. = FALSE
      )
    }
    return(invisible(final))
  }

  stop(
    "Missing or outdated dependencies:\n", detail,
    "\n\nInstall them with:\n",
    "  source(\"setup.R\")\n",
    "or directly:\n",
    "  install.packages(c(",
    paste0("\"", need$package, "\"", collapse = ", "), "))",
    call. = FALSE
  )
}
