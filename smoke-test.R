source("funs/fct_simulate.R")
source("funs/fct_tree.R")

d     <- build_cohort(n_patients = 500, horizon_yrs = 5, seed = 2026)
nodes <- build_node_table(d$cohort)
tree  <- nest_tree_df(nodes)

# Sanity check the AS/WW split looks clinically plausible
d$cohort |>
  dplyr::filter(init_mgmt %in% c("Active surveillance", "Watchful waiting")) |>
  dplyr::count(init_mgmt, outcome_state) |>
  dplyr::group_by(init_mgmt) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1)) |>
  print(n = 30)

p <- plot_pathway_tree(tree)
p   # renders in the Viewer pane

# THE key check from the README: did node_id and the summary stats
# survive serialisation into the ECharts series data?
str(p$x$opts$series[[1]]$data, max.level = 3)