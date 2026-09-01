# =============================================================================
# ui.R
# =============================================================================

shiny::fluidPage(

  shiny::tags$head(shiny::tags$style(app_css)),

  shiny::div(class = "app-title", "Prostate cancer treatment pathways"),
  shiny::div(class = "app-sub",
             "Interactive pathway tree with linked surveillance PSA trajectories"),

  shiny::div(
    class = "synthetic-banner",
    shiny::strong("Synthetic data. "),
    "Every patient in this app is simulated. Parameters were tuned to be ",
    "clinically plausible, not calibrated to any registry. Nothing here should ",
    "be read as an estimate of real outcomes."
  ),

  shiny::sidebarLayout(

    # ---------------------------------------------------------------- sidebar
    shiny::sidebarPanel(
      width = 3,

      shiny::h5("Cohort"),
      shiny::sliderInput("n_patients", "Patients", min = 500, max = 6000,
                         value = DEFAULT_N, step = 250, sep = ","),
      shiny::sliderInput("horizon", "Follow-up horizon (years)",
                         min = 3, max = 10, value = DEFAULT_HORIZON, step = 1),
      shiny::numericInput("seed", "Random seed", value = DEFAULT_SEED, step = 1),
      shiny::actionButton("resimulate", "Re-simulate cohort",
                          class = "btn-primary", width = "100%"),

      shiny::hr(),

      shiny::h5("Tree"),
      shiny::radioButtons(
        "scope", "Scope",
        choices = c(
          "Whole cohort"                      = "all",
          "Deferred treatment only (AS vs WW)" = "deferred"
        ),
        selected = "all"
      ),
      shiny::sliderInput("depth", "Levels expanded initially",
                         min = 1, max = 6, value = DEFAULT_DEPTH, step = 1),

      shiny::hr(),
      shiny::helpText(
        shiny::HTML(
          "Hover a node for per-node statistics. Click a node to load the ",
          "patients on that branch into the panel below. Scroll to zoom and ",
          "drag to pan the tree."
        )
      )
    ),

    # ------------------------------------------------------------- main panel
    shiny::mainPanel(
      width = 9,

      shiny::uiOutput("kpis"),

      shiny::div(
        class = "panel-card",
        echarts4r::echarts4rOutput("pathway_tree", height = "620px")
      ),

      shiny::div(
        class = "panel-card",
        mod_branch_detail_ui("branch")
      )
    )
  )
)
