# =============================================================================
# mod_network_view.R -- visNetwork graph view
# =============================================================================
# The reason this view exists: with "merge identical labels" on, a node reached
# by several routes is drawn ONCE with multiple inbound edges. A tree cannot
# express that -- it has to duplicate the node down each branch. Toggle the
# merge off to reproduce the tree's topology as a graph and see the difference
# directly on the same data.
#
# Note how the tooltips are built: an HTML string column assembled in R in
# build_graph_data(), with no JavaScript. Compare pathway_tooltip_js().
# =============================================================================

mod_network_view_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "view-note",
      "Nodes sharing a label are merged, so convergent routes collapse into a ",
      "single node with several inbound edges. Merged nodes are highlighted."
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::checkboxInput(ns("merge"), "Merge identical labels", value = TRUE)
      ),
      shiny::column(
        4,
        shiny::checkboxInput(ns("hierarchical"), "Hierarchical layout", value = TRUE)
      ),
      shiny::column(
        4,
        shiny::checkboxInput(ns("physics"), "Physics (drag to rearrange)", value = FALSE)
      )
    ),
    visNetwork::visNetworkOutput(ns("chart"), height = "620px")
  )
}


mod_network_view_server <- function(id, cohort, membership) {

  shiny::moduleServer(id, function(input, output, session) {

    ns       <- session$ns
    selected <- shiny::reactiveVal(NULL)

    graph <- shiny::reactive({
      build_graph_data(cohort(), membership(), merge_labels = input$merge)
    })

    output$chart <- visNetwork::renderVisNetwork({
      g <- graph()

      vn <- visNetwork::visNetwork(g$nodes, g$edges) |>
        visNetwork::visNodes(
          shape  = "dot",
          scaling = list(min = 8, max = 45),
          font   = list(size = 15, face = "sans-serif")
        ) |>
        visNetwork::visEdges(
          smooth    = list(enabled = TRUE, type = "cubicBezier"),
          color     = list(color = "#c3c8d0", highlight = "#2E6DA4"),
          scaling   = list(min = 1, max = 8)
        ) |>
        visNetwork::visGroups(groupname = "single",     color = "#7FA8D0") |>
        visNetwork::visGroups(groupname = "convergent", color = "#E8A33D") |>
        visNetwork::visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
          nodesIdSelection = FALSE
        ) |>
        visNetwork::visInteraction(
          hover = TRUE, tooltipDelay = 120, navigationButtons = TRUE
        ) |>
        visNetwork::visEvents(selectNode = sprintf(
          "function(params) {
             Shiny.setInputValue('%s', {id: params.nodes[0], nonce: Math.random()},
                                 {priority: 'event'});
           }", ns("click")
        ))

      if (isTRUE(input$hierarchical)) {
        vn <- vn |>
          visNetwork::visHierarchicalLayout(
            direction        = "LR",
            sortMethod       = "directed",
            levelSeparation  = 260,
            nodeSpacing      = 130
          )
      } else {
        # vis.js physics gets sluggish past a few hundred nodes; precomputing a
        # static layout with igraph is the usual escape hatch.
        vn <- vn |> visNetwork::visIgraphLayout(layout = "layout_with_sugiyama")
      }

      vn |> visNetwork::visPhysics(enabled = isTRUE(input$physics))
    })

    shiny::observeEvent(list(cohort(), input$merge), selected(NULL), ignoreInit = TRUE)

    shiny::observeEvent(input$click, {
      gid <- input$click$id
      shiny::req(gid)

      ids <- graph_node_patients(membership(), gid, merge_labels = input$merge)
      if (length(ids) == 0) return()

      n_levels <- graph()$nodes$n_levels[graph()$nodes$id == gid]

      selected(list(
        label       = gid,
        patient_ids = ids,
        method      = if (isTRUE(input$merge)) "merged label" else "full path",
        # A merged node genuinely pools patients from different pathway depths,
        # so flag it -- the detail panel is then summarising a mixed group.
        ambiguous   = isTRUE(input$merge) && length(n_levels) == 1 && n_levels > 1
      ))
    })

    selected
  })
}
