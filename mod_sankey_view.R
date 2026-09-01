# =============================================================================
# mod_sankey_view.R -- echarts4r Sankey view
# =============================================================================
# Ribbon width encodes patient volume directly, which is the number clinicians
# ask about first. Uses e_sankey() from echarts4r, so this view adds no new
# package dependency.
#
# All six levels at once is unreadable, so the level range is selectable and
# defaults to 2-5 (stage -> risk group -> management -> outcome).
# =============================================================================

mod_sankey_view_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "view-note",
      "Ribbon width is patient volume. Labels appearing at more than one ",
      "pathway level are suffixed with that level, because Sankey diagrams ",
      "must be acyclic."
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::sliderInput(ns("levels"), "Pathway levels shown",
                           min = 1, max = 6, value = c(2, 5), step = 1)
      )
    ),
    echarts4r::echarts4rOutput(ns("chart"), height = "620px")
  )
}


mod_sankey_view_server <- function(id, cohort, membership) {

  shiny::moduleServer(id, function(input, output, session) {

    ns       <- session$ns
    selected <- shiny::reactiveVal(NULL)

    sankey <- shiny::reactive({
      lv <- seq(input$levels[1], input$levels[2])
      shiny::validate(shiny::need(
        length(lv) >= 2, "Select a range spanning at least two levels."
      ))
      build_sankey_data(membership(), levels = lv)
    })

    output$chart <- echarts4r::renderEcharts4r({
      edges <- sankey()$edges
      shiny::validate(shiny::need(
        nrow(edges) > 0, "No transitions in the selected level range."
      ))

      edges |>
        echarts4r::e_charts() |>
        echarts4r::e_sankey(
          source, target, value,
          layoutIterations = 64,
          nodeGap   = 12,
          nodeWidth = 14,
          label     = list(fontSize = 11),
          emphasis  = list(focus = "adjacency")
        ) |>
        echarts4r::e_tooltip(
          trigger   = "item",
          triggerOn = "mousemove",
          confine   = TRUE
        ) |>
        echarts4r::e_on(
          query   = "series.sankey",
          handler = sankey_click_js(ns("click")),
          event   = "click"
        ) |>
        echarts4r::e_title(
          "Patient flow through treatment pathways",
          sprintf("Synthetic cohort, n = %s", format(nrow(cohort()), big.mark = ","))
        ) |>
        echarts4r::e_toolbox_feature("saveAsImage")
    })

    shiny::observeEvent(list(cohort(), input$levels), selected(NULL), ignoreInit = TRUE)

    shiny::observeEvent(input$click, {
      click <- input$click
      shiny::req(click)

      if (!identical(click$dataType, "node")) {
        shiny::showNotification(
          "Click a node rather than a ribbon to load a patient set.",
          type = "message", duration = 3
        )
        return()
      }

      ids <- sankey()$lookup |>
        dplyr::filter(slabel == click$name) |>
        dplyr::pull(patient_id) |>
        unique()

      if (length(ids) == 0) return()

      selected(list(
        label       = click$name,
        patient_ids = ids,
        method      = "sankey node",
        ambiguous   = FALSE
      ))
    })

    selected
  })
}
