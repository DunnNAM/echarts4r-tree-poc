# =============================================================================
# fct_tree.R -- pathway aggregation, nesting and echarts4r rendering
# =============================================================================

PATH_SEP <- " > "


#' Map every patient to every node they pass through
#'
#' A node is identified by its FULL ancestral path, so "Radical prostatectomy"
#' reached via active surveillance is a different node from the same operation
#' performed upfront. This membership table is what lets a click on the tree
#' resolve back to a specific set of patients.
#'
#' Deliberately uses base subsetting rather than dplyr verbs -- the path columns
#' are addressed programmatically and this avoids any data-masking surprises.
#'
#' @return tibble(patient_id, depth, node_name, node_id, parent_id)
build_membership <- function(cohort, max_depth = 6) {

  path_cols <- paste0("path_", seq_len(max_depth))
  path_cols <- path_cols[path_cols %in% names(cohort)]

  purrr::map(seq_along(path_cols), function(k) {
    keep <- !is.na(cohort[[path_cols[k]]])
    sub  <- cohort[keep, , drop = FALSE]
    if (nrow(sub) == 0) return(NULL)

    tibble::tibble(
      patient_id = sub$patient_id,
      depth      = k,
      node_name  = sub[[path_cols[k]]],
      node_id    = do.call(paste, c(as.list(sub[path_cols[seq_len(k)]]), sep = PATH_SEP)),
      parent_id  = if (k == 1) {
        NA_character_
      } else {
        do.call(paste, c(as.list(sub[path_cols[seq_len(k - 1)]]), sep = PATH_SEP))
      }
    )
  }) |>
    purrr::list_rbind()
}


#' Collapse membership into one row per node, with summary statistics
#'
#' These summaries are what the tooltip renders, so add a column here and it
#' becomes available to the JS formatter (and to the click payload) for free.
build_node_table <- function(cohort, membership = NULL, max_depth = 6) {

  if (is.null(membership)) membership <- build_membership(cohort, max_depth)
  n_total <- nrow(cohort)

  stats_by_patient <- dplyr::select(
    cohort, patient_id, age_dx, psa_dx, psadt_yrs, t_event_yrs, psa_at_event
  )

  nodes <- membership |>
    dplyr::left_join(stats_by_patient, by = "patient_id") |>
    dplyr::group_by(depth, node_id, parent_id, node_name) |>
    dplyr::summarise(
      n              = dplyr::n(),
      median_age     = stats::median(age_dx),
      median_psa_dx  = stats::median(psa_dx),
      median_psadt   = stats::median(psadt_yrs),
      median_t_event = stats::median(t_event_yrs,  na.rm = TRUE),
      median_psa_evt = stats::median(psa_at_event, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(pct_cohort = round(100 * n / n_total, 1))

  # Share of the immediate parent -- the number clinicians actually want
  nodes |>
    dplyr::left_join(
      dplyr::select(nodes, parent_id = node_id, .n_parent = n),
      by = "parent_id"
    ) |>
    dplyr::mutate(pct_parent = round(100 * n / dplyr::coalesce(.n_parent, n), 1)) |>
    dplyr::select(-.n_parent) |>
    dplyr::arrange(depth, dplyr::desc(n))
}


#' Recursively nest the flat node table for e_tree()
#'
#' Leaf nodes carry `children = list(NULL)`, the pattern used in the official
#' echarts4r documentation. `node_id` is carried onto every node so the click
#' handler can send it back to R.
nest_tree_df <- function(nodes, id = NA_character_) {

  kids <- if (is.na(id)) {
    dplyr::filter(nodes, is.na(parent_id))
  } else {
    dplyr::filter(nodes, !is.na(parent_id), parent_id == id)
  }
  if (nrow(kids) == 0) return(NULL)

  purrr::map(seq_len(nrow(kids)), function(i) {
    row <- kids[i, ]
    tibble::tibble(
      name           = row$node_name,
      node_id        = row$node_id,
      value          = row$n,
      pct_cohort     = row$pct_cohort,
      pct_parent     = row$pct_parent,
      median_age     = row$median_age,
      median_psa_dx  = row$median_psa_dx,
      median_psadt   = row$median_psadt,
      median_t_event = row$median_t_event,
      median_psa_evt = row$median_psa_evt,
      children       = list(nest_tree_df(nodes, row$node_id))
    )
  }) |>
    purrr::list_rbind()
}


#' Tooltip formatter -- reads the extra fields attached to each node
pathway_tooltip_js <- function() {
  htmlwidgets::JS("
    function(params) {
      var d = params.data;
      var num = function(x, dp, unit) {
        if (x === null || x === undefined || x !== x) return '\u2013';
        return Number(x).toFixed(dp) + (unit || '');
      };
      var row = function(lab, val) {
        return '<tr><td style=\"padding-right:14px;color:#555\">' + lab +
               '</td><td style=\"text-align:right;font-variant-numeric:tabular-nums\">' +
               val + '</td></tr>';
      };
      return '<div style=\"font-weight:600;font-size:13px;margin-bottom:6px;' +
               'max-width:270px;white-space:normal\">' + d.name + '</div>' +
             '<table style=\"font-size:12px;border-collapse:collapse\">' +
               row('Patients',            '<b>' + d.value + '</b>') +
               row('% of parent node',    num(d.pct_parent, 1, '%')) +
               row('% of whole cohort',   num(d.pct_cohort, 1, '%')) +
               '<tr><td colspan=2 style=\"border-top:1px solid #ddd;height:6px\"></td></tr>' +
               row('Median age at dx',    num(d.median_age, 0, ' yrs')) +
               row('Median PSA at dx',    num(d.median_psa_dx, 1, ' ng/mL')) +
               row('Median PSADT',        num(d.median_psadt, 1, ' yrs')) +
               row('Median time to node', num(d.median_t_event, 1, ' yrs')) +
               row('Median PSA at event', num(d.median_psa_evt, 1, ' ng/mL')) +
             '</table>' +
             '<div style=\"margin-top:6px;font-size:11px;color:#888\">Click to inspect PSA trajectories</div>';
    }
  ")
}


#' Click handler
#'
#' Sends only the fields needed. This is deliberate: the built-in
#' `input$<id>_clicked_data` would return `params.data`, which on a tree series
#' includes the node's entire `children` array -- a large payload on every
#' click. `treeAncestors` is included as a fallback path reconstruction in case
#' `node_id` does not survive serialisation (see NOTES in README).
pathway_click_js <- function(input_id = "pathway_click") {
  sprintf("
    function(params) {
      var anc = null;
      if (params.treeAncestors) {
        anc = params.treeAncestors
                .map(function(a) { return a.name; })
                .filter(function(x) { return x !== undefined && x !== null; })
                .join(' > ');
      }
      Shiny.setInputValue(
        '%s',
        {
          node_id  : (params.data && params.data.node_id) ? params.data.node_id : null,
          name     : params.name,
          ancestors: anc,
          n        : (params.data && params.data.value) ? params.data.value : null,
          nonce    : Math.random()
        },
        { priority: 'event' }
      );
    }
  ", input_id)
}


#' Draw the pathway tree
plot_pathway_tree <- function(tree_df,
                              title      = "Prostate cancer treatment pathways",
                              subtitle   = "Synthetic cohort \u2014 hover for detail, click to drill in",
                              depth      = 3,
                              click_input = "pathway_click",
                              height     = "620px") {

  tree_df |>
    echarts4r::e_charts(height = height) |>
    echarts4r::e_tree(
      layout            = "orthogonal",
      orient            = "LR",
      initialTreeDepth  = depth,
      expandAndCollapse = TRUE,
      roam              = TRUE,     # pan/zoom -- essential once you expand
      top    = "8%",  bottom = "6%",
      left   = "14%", right  = "24%",
      symbol = "circle",
      symbolSize = htmlwidgets::JS(
        "function(value, params) {
           var n = (params.data && params.data.value) ? params.data.value : 1;
           return Math.max(6, Math.min(28, Math.sqrt(n) * 1.1));
         }"
      ),
      label = list(position = "left", verticalAlign = "middle",
                   align = "right", fontSize = 11, distance = 8),
      leaves = list(label = list(position = "right", verticalAlign = "middle",
                                 align = "left", fontSize = 11)),
      emphasis  = list(focus = "descendant"),
      lineStyle = list(width = 1.2, curveness = 0.45),
      animationDuration = 500
    ) |>
    echarts4r::e_tooltip(
      trigger         = "item",
      triggerOn       = "mousemove",
      confine         = TRUE,
      backgroundColor = "rgba(255,255,255,0.97)",
      borderColor     = "#bbb",
      borderWidth     = 1,
      textStyle       = list(color = "#222"),
      formatter       = pathway_tooltip_js()
    ) |>
    echarts4r::e_on(
      query   = "series.tree",
      handler = pathway_click_js(click_input),
      event   = "click"
    ) |>
    echarts4r::e_title(title, subtitle) |>
    echarts4r::e_toolbox_feature("saveAsImage") |>
    echarts4r::e_toolbox_feature("restore")
}


#' Resolve a click payload back to a node_id
#'
#' Three routes, in order of reliability:
#'   1. `node_id` carried on the node (expected path)
#'   2. `treeAncestors` reconstruction, if ECharts supplied it
#'   3. name matching -- ambiguous when a label repeats across branches, so we
#'      take the largest matching node and flag it
#'
#' @return list(node_id, method, ambiguous)
resolve_clicked_node <- function(click, nodes) {

  if (is.null(click)) return(NULL)

  if (!is.null(click$node_id) && click$node_id %in% nodes$node_id) {
    return(list(node_id = click$node_id, method = "node_id", ambiguous = FALSE))
  }

  if (!is.null(click$ancestors) && click$ancestors %in% nodes$node_id) {
    return(list(node_id = click$ancestors, method = "treeAncestors", ambiguous = FALSE))
  }

  if (!is.null(click$name)) {
    hits <- nodes[nodes$node_name == click$name, ]
    if (nrow(hits) >= 1) {
      hits <- hits[order(-hits$n), ]
      return(list(node_id   = hits$node_id[1],
                  method    = "name match",
                  ambiguous = nrow(hits) > 1))
    }
  }

  NULL
}
