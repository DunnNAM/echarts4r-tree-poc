# =============================================================================
# mod_tree_view.R -- echarts4r e_tree view
# =============================================================================
# Extracted from server.R now that there are three interchangeable views. Each
# view module owns its own chart and returns a reactive selection with a common
# shape, which the shared detail panel consumes:
#
#   list(label, patient_ids, method, ambiguous)
#
# The click target passed to plot_pathway_tree() must be namespaced with ns(),
# since Shiny.setInputValue writes to the global input space.
# =============================================================================

mod_tree_view_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "view-note",
      "Node identity is the full ancestral path, so convergent routes appear as ",
      "separate nodes. Hover for statistics, click to load the branch below."
    ),
    echarts4r::echarts4rOutput(ns("chart"), height = "620px")
  )
}


mod_tree_view_server <- function(id, cohort, membership, nodes, tree_df,
                                 depth, scope, horizon) {

  shiny::moduleServer(id, function(input, output, session) {

    ns       <- session$ns
    selected <- shiny::reactiveVal(NULL)

    output$chart <- echarts4r::renderEcharts4r({
      plot_pathway_tree(
        tree_df(),
        title = if (identical(scope(), "deferred")) {
          "Active surveillance vs watchful waiting"
        } else {
          "Prostate cancer treatment pathways"
        },
        subtitle = sprintf(
          "Synthetic cohort, n = %s, %d-year horizon",
          format(nrow(cohort()), big.mark = ","), horizon()
        ),
        depth       = depth(),
        click_input = ns("click")   # namespaced: Shiny.setInputValue is global
      )
    })

    # Stale selections point at patients from a cohort that no longer exists
    shiny::observeEvent(list(scope(), cohort()), selected(NULL), ignoreInit = TRUE)

    shiny::observeEvent(input$click, {
      res <- resolve_clicked_node(input$click, nodes())

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
        label       = res$node_id,   # node_id IS the readable "A > B > C" path
        patient_ids = ids,
        method      = res$method,
        ambiguous   = res$ambiguous
      ))
    })

    selected
  })
}
