# =============================================================================
# mod_branch_detail.R -- detail panel for the currently selected tree branch
# =============================================================================
# WHY A MODULE HERE AND NOWHERE ELSE
#   This is the only part of the app with a genuine reuse case: the obvious
#   next step for this work is an AS-vs-WW side-by-side view, which is two
#   instances of exactly this panel driven by two different node selections.
#   The tree itself is a singleton and lives in server.R unmodularised --
#   wrapping it would add namespacing ceremony for no benefit at MVP scope.
# =============================================================================

mod_branch_detail_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shiny::tabsetPanel(
      id = ns("tabs"),
      shiny::tabPanel(
        "PSA trajectories",
        shiny::br(),
        shiny::plotOutput(ns("psa_plot"), height = "400px")
      ),
      shiny::tabPanel(
        "Branch summary",
        shiny::br(),
        shiny::tableOutput(ns("summary_tbl"))
      ),
      shiny::tabPanel(
        "Where they go next",
        shiny::br(),
        shiny::tableOutput(ns("downstream_tbl"))
      )
    )
  )
}


#' @param selection Reactive returning NULL, or a list with
#'   `node_id`, `label`, `patient_ids`, `method`, `ambiguous`.
#' @param cohort    Reactive returning the patient-level tibble.
#' @param psa_long  Reactive returning the longitudinal PSA tibble.
mod_branch_detail_server <- function(id, selection, cohort, psa_long) {

  shiny::moduleServer(id, function(input, output, session) {

    # --- Patients in the selected branch -----------------------------------
    branch <- shiny::reactive({
      sel <- selection()
      shiny::req(sel)
      dplyr::filter(cohort(), patient_id %in% sel$patient_ids)
    })

    branch_psa <- shiny::reactive({
      sel <- selection()
      shiny::req(sel)
      dplyr::filter(psa_long(), patient_id %in% sel$patient_ids)
    })

    # --- Header -------------------------------------------------------------
    output$header <- shiny::renderUI({
      sel <- selection()

      if (is.null(sel)) {
        return(shiny::div(
          class = "branch-empty",
          shiny::icon("hand-pointer"),
          " Click any node on the tree above to inspect the patients on that branch."
        ))
      }

      warn <- NULL
      if (isTRUE(sel$ambiguous)) {
        warn <- shiny::div(
          class = "branch-warn",
          "Node resolved by label only and the label appears on more than one branch; ",
          "showing the largest match. See README note 1."
        )
      }

      shiny::tagList(
        shiny::div(
          class = "branch-header",
          shiny::div(class = "branch-path", sel$label),
          shiny::div(
            class = "branch-n",
            format(nrow(branch()), big.mark = ","), " patients",
            shiny::span(class = "branch-method", paste0("  \u00b7  matched via ", sel$method))
          )
        ),
        warn
      )
    })

    # --- PSA spaghetti ------------------------------------------------------
    output$psa_plot <- shiny::renderPlot({
      sel <- selection()
      shiny::validate(shiny::need(!is.null(sel), "No branch selected."))

      dat <- branch_psa()
      shiny::validate(shiny::need(
        nrow(dat) > 0,
        paste("No surveillance PSA series for this branch.",
              "Serial PSA is only simulated for men managed with active",
              "surveillance or watchful waiting -- select a node under one of",
              "those arms.")
      ))

      # Cap the spaghetti so large branches stay legible and fast
      max_lines <- 200
      ids <- unique(dat$patient_id)
      if (length(ids) > max_lines) {
        dat <- dplyr::filter(dat, patient_id %in% sample(ids, max_lines))
      }

      # Median trajectory across all patients in the branch (not just the plotted sample)
      med <- branch_psa() |>
        dplyr::mutate(visit_yrs = round(visit_yrs * 2) / 2) |>
        dplyr::group_by(visit_yrs) |>
        dplyr::summarise(psa = stats::median(psa), n = dplyr::n(), .groups = "drop") |>
        dplyr::filter(n >= 5)   # drop the thin tail where the median is unstable

      subtitle <- sprintf(
        "%s trajectories shown of %s in branch; red line = branch median PSA",
        format(length(unique(dat$patient_id)), big.mark = ","),
        format(length(ids), big.mark = ",")
      )

      ggplot2::ggplot() +
        ggplot2::geom_line(
          data = dat,
          ggplot2::aes(x = visit_yrs, y = psa, group = patient_id, colour = init_mgmt),
          alpha = 0.18, linewidth = 0.4
        ) +
        ggplot2::geom_line(
          data = med,
          ggplot2::aes(x = visit_yrs, y = psa),
          colour = "#C0392B", linewidth = 1.3
        ) +
        ggplot2::scale_y_log10() +
        ggplot2::scale_colour_manual(
          values = c("Active surveillance" = "#2E6DA4", "Watchful waiting" = "#7B5EA7"),
          name   = NULL
        ) +
        ggplot2::labs(
          title    = "Surveillance PSA trajectories",
          subtitle = subtitle,
          x        = "Years from diagnosis",
          y        = "PSA (ng/mL, log scale)"
        ) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1, linewidth = 1))) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          legend.position   = "top",
          panel.grid.minor  = ggplot2::element_blank(),
          plot.subtitle     = ggplot2::element_text(colour = "grey35", size = 11)
        )
    })

    # --- Branch summary -----------------------------------------------------
    output$summary_tbl <- shiny::renderTable({
      shiny::req(selection())
      d <- branch()

      iqr <- function(x) {
        q <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
        sprintf("%.1f (%.1f\u2013%.1f)", stats::median(x, na.rm = TRUE), q[1], q[2])
      }

      tibble::tibble(
        Measure = c(
          "Patients",
          "Age at diagnosis, median (IQR)",
          "PSA at diagnosis, median (IQR), ng/mL",
          "PSA doubling time, median (IQR), yrs",
          "Time to transition, median (IQR), yrs",
          "PSA at transition, median (IQR), ng/mL",
          "High comorbidity, n (%)",
          "Still event-free at horizon, n (%)"
        ),
        Value = c(
          format(nrow(d), big.mark = ","),
          iqr(d$age_dx),
          iqr(d$psa_dx),
          iqr(d$psadt_yrs),
          if (all(is.na(d$t_event_yrs))) "\u2014" else iqr(d$t_event_yrs),
          if (all(is.na(d$psa_at_event))) "\u2014" else iqr(d$psa_at_event),
          sprintf("%d (%.1f%%)", sum(d$comorbidity == "High"),
                  100 * mean(d$comorbidity == "High")),
          sprintf("%d (%.1f%%)", sum(d$event_type == "stable"),
                  100 * mean(d$event_type == "stable"))
        )
      )
    }, striped = TRUE, width = "100%", align = "lr")

    # --- Downstream distribution -------------------------------------------
    output$downstream_tbl <- shiny::renderTable({
      shiny::req(selection())

      branch() |>
        dplyr::count(outcome_state, next_step, name = "n") |>
        dplyr::mutate(
          `% of branch` = round(100 * n / sum(n), 1),
          next_step     = dplyr::coalesce(next_step, "\u2014")
        ) |>
        dplyr::arrange(dplyr::desc(n)) |>
        dplyr::rename(
          `Outcome at horizon` = outcome_state,
          `Next line of management` = next_step,
          Patients = n
        )
    }, striped = TRUE, width = "100%")
  })
}
