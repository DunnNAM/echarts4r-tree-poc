# =============================================================================
# server.R
# =============================================================================

function(input, output, session) {

  # ---------------------------------------------------------------------------
  # 1. Simulated cohort
  #    eventReactive with ignoreNULL = FALSE so it fires once on startup and
  #    then only when the user asks -- resimulating on every slider nudge would
  #    make the sliders feel broken on a 6000-patient cohort.
  # ---------------------------------------------------------------------------
  sim <- shiny::eventReactive(input$resimulate, {
    shiny::withProgress(message = "Simulating cohort", value = 0.4, {
      build_cohort(
        n_patients  = input$n_patients,
        horizon_yrs = input$horizon,
        seed        = input$seed
      )
    })
  }, ignoreNULL = FALSE)

  cohort   <- shiny::reactive(sim()$cohort)
  psa_long <- shiny::reactive(sim()$psa_long)
  horizon  <- shiny::reactive(sim()$horizon_yrs)   # as simulated, not as slid

  # ---------------------------------------------------------------------------
  # 2. Scope the cohort, then aggregate into nodes
  # ---------------------------------------------------------------------------
  scoped <- shiny::reactive({
    d <- cohort()
    if (identical(input$scope, "deferred")) {
      d <- d |>
        dplyr::filter(init_mgmt %in% DEFERRED_ARMS) |>
        dplyr::mutate(path_1 = "Deferred treatment (localised)")
    }
    d
  })

  membership <- shiny::reactive(build_membership(scoped()))
  nodes      <- shiny::reactive(build_node_table(scoped(), membership()))
  tree_df    <- shiny::reactive(nest_tree_df(nodes()))

  # ---------------------------------------------------------------------------
  # 3. KPI strip
  # ---------------------------------------------------------------------------
  output$kpis <- shiny::renderUI({
    d <- cohort()

    kpi <- function(value, label) {
      shiny::div(class = "kpi",
                 shiny::div(class = "kpi-val", value),
                 shiny::div(class = "kpi-lab", label))
    }

    pct <- function(x) sprintf("%.1f%%", 100 * mean(x))

    as_arm <- dplyr::filter(d, init_mgmt == "Active surveillance")

    shiny::div(
      class = "kpi-row",
      kpi(format(nrow(d), big.mark = ","), "Patients"),
      kpi(pct(d$stage_dx == "Localised"), "Localised at dx"),
      kpi(pct(d$init_mgmt == "Active surveillance"), "Active surveillance"),
      kpi(pct(d$init_mgmt == "Watchful waiting"), "Watchful waiting"),
      kpi(
        if (nrow(as_arm) == 0) "\u2014" else pct(startsWith(as_arm$outcome_state, "Reclassified")),
        sprintf("AS reclassified by %d yrs", horizon())
      )
    )
  })

  # ---------------------------------------------------------------------------
  # 4. The tree
  # ---------------------------------------------------------------------------
  output$pathway_tree <- echarts4r::renderEcharts4r({
    plot_pathway_tree(
      tree_df(),
      title = if (identical(input$scope, "deferred")) {
        "Active surveillance vs watchful waiting"
      } else {
        "Prostate cancer treatment pathways"
      },
      subtitle = sprintf(
        "Synthetic cohort, n = %s, %d-year horizon",
        format(nrow(scoped()), big.mark = ","), horizon()
      ),
      depth       = input$depth,
      click_input = "pathway_click"
    )
  })

  # ---------------------------------------------------------------------------
  # 5. Click -> selection
  # ---------------------------------------------------------------------------
  selected <- shiny::reactiveVal(NULL)

  # Clear the selection whenever the tree underneath it changes, otherwise the
  # detail panel silently keeps showing patients from a cohort that no longer
  # exists.
  shiny::observeEvent(list(input$scope, sim()), {
    selected(NULL)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$pathway_click, {
    res <- resolve_clicked_node(input$pathway_click, nodes())

    if (is.null(res)) {
      shiny::showNotification(
        "Could not resolve that node back to a set of patients.",
        type = "warning"
      )
      return()
    }

    ids <- membership() |>
      dplyr::filter(node_id == res$node_id) |>
      dplyr::pull(patient_id)

    selected(list(
      node_id     = res$node_id,
      label       = res$node_id,   # node_id IS the readable "A > B > C" path
      patient_ids = ids,
      method      = res$method,
      ambiguous   = res$ambiguous
    ))
  })

  # ---------------------------------------------------------------------------
  # 6. Detail panel module
  # ---------------------------------------------------------------------------
  mod_branch_detail_server(
    "branch",
    selection = selected,
    cohort    = cohort,
    psa_long  = psa_long
  )
}
