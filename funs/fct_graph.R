# =============================================================================
# fct_graph.R -- data prep for the network and Sankey views
# =============================================================================
# The tree view treats a node's identity as its full ancestral path, so
# convergent routes are drawn as separate nodes. These two views relax that in
# different ways:
#
#   build_graph_data()   optionally merges nodes sharing a label, so
#                        "Radical prostatectomy" reached via surveillance and
#                        performed upfront become ONE node with two inbound
#                        edges. This is the thing a tree structurally cannot do.
#
#   build_sankey_data()  keeps levels separate and encodes patient volume as
#                        ribbon width, which is the standard patient-flow idiom.
# =============================================================================


#' Build node and edge tables for visNetwork
#'
#' @param merge_labels When TRUE, nodes are keyed by label; when FALSE, by full
#'   ancestral path (reproducing the tree's topology as a graph).
#' @return list(nodes, edges) in visNetwork's expected column naming.
build_graph_data <- function(cohort, membership, merge_labels = TRUE) {

  stats_by_patient <- dplyr::select(
    cohort, patient_id, age_dx, psa_dx, psadt_yrs, t_event_yrs, psa_at_event
  )

  lut <- dplyr::distinct(membership, node_id, node_name)

  m <- membership |>
    dplyr::left_join(
      dplyr::rename(lut, parent_id = node_id, parent_name = node_name),
      by = "parent_id"
    ) |>
    dplyr::mutate(
      gid        = if (merge_labels) node_name   else node_id,
      parent_gid = if (merge_labels) parent_name else parent_id
    )

  node_tbl <- m |>
    dplyr::left_join(stats_by_patient, by = "patient_id") |>
    dplyr::group_by(gid) |>
    dplyr::summarise(
      label          = dplyr::first(node_name),
      # A merged node can span depths; place it at its deepest occurrence so
      # inbound edges from shallower levels read left-to-right.
      level          = max(depth),
      n_levels       = dplyr::n_distinct(depth),
      n              = dplyr::n_distinct(patient_id),
      median_age     = stats::median(age_dx),
      median_psa_dx  = stats::median(psa_dx),
      median_psadt   = stats::median(psadt_yrs),
      median_t_event = stats::median(t_event_yrs,  na.rm = TRUE),
      .groups = "drop"
    )

  n_total <- nrow(cohort)

  fmt <- function(x, dp, unit = "") {
    ifelse(is.na(x) | is.nan(x), "\u2013", paste0(formatC(x, format = "f", digits = dp), unit))
  }

  # visNetwork tooltips are just an HTML string column built in R -- no JS.
  nodes <- node_tbl |>
    dplyr::mutate(
      id    = gid,
      value = n,
      title = paste0(
        "<div style='font-family:sans-serif;font-size:12px;max-width:260px'>",
        "<b>", label, "</b><br>",
        "Patients: <b>", n, "</b> (", fmt(100 * n / n_total, 1), "% of cohort)<br>",
        "Median age at dx: ", fmt(median_age, 0, " yrs"), "<br>",
        "Median PSA at dx: ", fmt(median_psa_dx, 1, " ng/mL"), "<br>",
        "Median PSADT: ",     fmt(median_psadt, 1, " yrs"), "<br>",
        "Median time to node: ", fmt(median_t_event, 1, " yrs"),
        ifelse(n_levels > 1,
               paste0("<br><i>Reached at ", n_levels, " different pathway depths</i>"),
               ""),
        "</div>"
      ),
      # Convergent nodes get a distinct colour -- they are the point of this view
      group = ifelse(n_levels > 1, "convergent", "single")
    ) |>
    dplyr::select(id, label, level, value, title, group, n, n_levels)

  edges <- m |>
    dplyr::filter(!is.na(parent_gid)) |>
    dplyr::count(from = parent_gid, to = gid, name = "n") |>
    dplyr::mutate(
      value = n,
      title = paste0(n, " patients"),
      arrows = "to"
    )

  list(nodes = nodes, edges = edges)
}


#' Map a merged/unmerged graph node id back to patient ids
graph_node_patients <- function(membership, gid, merge_labels = TRUE) {
  key <- if (merge_labels) membership$node_name else membership$node_id
  unique(membership$patient_id[key == gid])
}


#' Build Sankey edges, with labels disambiguated by depth
#'
#' ECharts Sankey requires an acyclic graph, and merging by bare label would
#' create cycles here: "Radical prostatectomy" appears both as an upfront
#' management option (level 4) and as a post-reclassification treatment (level
#' 6), so a merged node would have edges running in both directions. Any label
#' occurring at more than one depth is therefore suffixed with its level.
#'
#' @param levels Consecutive pathway levels to include, e.g. 2:5.
#' @return list(edges, lookup) where lookup maps sankey label -> patient_id.
build_sankey_data <- function(membership, levels = 2:5) {

  ambiguous <- membership |>
    dplyr::distinct(depth, node_name) |>
    dplyr::count(node_name, name = "n_depths") |>
    dplyr::filter(n_depths > 1) |>
    dplyr::pull(node_name)

  lookup <- membership |>
    dplyr::mutate(
      slabel = ifelse(node_name %in% ambiguous,
                      paste0(node_name, " (L", depth, ")"),
                      node_name)
    ) |>
    dplyr::select(patient_id, depth, slabel)

  wide <- lookup |>
    tidyr::pivot_wider(names_from = depth, values_from = slabel, names_prefix = "d")

  edges <- purrr::map(utils::head(levels, -1), function(k) {
    from <- paste0("d", k)
    to   <- paste0("d", k + 1)
    if (!all(c(from, to) %in% names(wide))) return(NULL)

    wide |>
      dplyr::filter(!is.na(.data[[from]]), !is.na(.data[[to]])) |>
      dplyr::count(source = .data[[from]], target = .data[[to]], name = "value")
  }) |>
    purrr::list_rbind()

  list(edges = edges, lookup = lookup)
}


#' Click handler shared by the Sankey view
#'
#' Sankey click params carry dataType "node" or "edge"; only node clicks resolve
#' cleanly to a patient set, so edge clicks are reported and ignored.
sankey_click_js <- function(input_id) {
  sprintf("
    function(params) {
      Shiny.setInputValue(
        '%s',
        {
          name     : params.name,
          dataType : params.dataType,
          nonce    : Math.random()
        },
        { priority: 'event' }
      );
    }
  ", input_id)
}
