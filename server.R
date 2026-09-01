# =============================================================================
# server.R
# =============================================================================
# Thin now that each view is a module. The only real work here is simulating
# the cohort, scoping it, aggregating it, and routing whichever view is active
# into the shared detail panel.
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
  # 2. Scope, then aggregate
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
  # 4. Views -- each returns a reactive selection with the same shape
  # ---------------------------------------------------------------------------
  sel_tree <- mod_tree_view_server(
    "tree",
    cohort     = scoped,
    membership = membership,
    nodes      = nodes,
    tree_df    = tree_df,
    depth      = shiny::reactive(input$depth),
    scope      = shiny::reactive(input$scope),
    horizon    = horizon
  )

  sel_network <- mod_network_view_server("network", cohort = scoped, membership = membership)
  sel_sankey  <- mod_sankey_view_server ("sankey",  cohort = scoped, membership = membership)

  # ---------------------------------------------------------------------------
  # 5. Route the active view into the shared detail panel
  # ---------------------------------------------------------------------------
  selected <- shiny::reactive({
    # %||% is only in base R from 4.4; the project floor is 4.1.
    tab <- input$view_tabs
    if (is.null(tab)) tab <- "tree"

    switch(
      tab,
      tree    = sel_tree(),
      network = sel_network(),
      sankey  = sel_sankey(),
      NULL
    )
  })

  mod_branch_detail_server(
    "branch",
    selection = selected,
    cohort    = cohort,
    psa_long  = psa_long
  )
}
