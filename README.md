# Prostate cancer pathway explorer — MVP

Interactive `echarts4r` pathway tree with a linked surveillance-PSA panel, built
on a synthetic cohort. Scoping prototype, not a validated tool.

## Run

```r
shiny::runApp("pca-pathway-app")
```

## Structure

```
pca-pathway-app/
├── global.R                    # packages, sourcing, defaults, CSS
├── ui.R                        # layout
├── server.R                    # reactive wiring, click handling
└── funs/
    ├── fct_simulate.R          # synthetic cohort + PSA series
    ├── fct_tree.R              # aggregation, nesting, rendering, click resolution
    └── mod_branch_detail.R     # Shiny module: detail panel
```

Helpers live in `funs/` and are sourced explicitly from `global.R`. Shiny
auto-sources `R/`, but `global.R` needs these functions available immediately
and I would rather not depend on the load order.

## On modularisation

One module only: `mod_branch_detail`. It is the single part of the app with a
real reuse case — the obvious next step is an AS-vs-WW side-by-side view, which
is two instances of that panel driven by two selections. Everything else stays
unmodularised. The tree is a singleton; wrapping it would add namespacing
ceremony without buying anything at this scope.

## Dependencies

`shiny`, `dplyr` (>= 1.1.0, for `case_match`/`pick`), `tidyr`, `purrr`
(>= 1.0.0, for `list_rbind`), `tibble`, `ggplot2`, `echarts4r`, `htmlwidgets`.
Requires R >= 4.1 for the native pipe and `\(x)` lambda syntax.

## How the click plumbing works

Clicking a node fires a JS handler registered with `echarts4r::e_on()`, which
calls `Shiny.setInputValue("pathway_click", {...})`.

This is deliberate rather than using the built-in `input$<id>_clicked_data`.
On a tree series that input returns `params.data`, which includes the node's
entire `children` array — on a six-level tree that is a large payload sent to R
on every click. The custom handler sends five scalars instead.

The key field is `node_id`: the full ancestral path (`"All new diagnoses >
Localised > Low risk > Active surveillance"`), attached to every node when the
tree is nested. Node identity is the full path by design, so "Radical
prostatectomy" reached via active surveillance is a distinct node from the same
operation performed upfront. `build_membership()` maps `node_id` back to
patient IDs.

## Things to verify on first run

**1. Do the extra node fields survive serialisation?**

Both the tooltip and the click handler assume `echarts4r` passes extra tibble
columns (`node_id`, `pct_parent`, `median_psadt`, …) straight through to the
ECharts series data. The nested-tibble input to `e_tree()` is documented, but
the published examples only ever use `name` and `children`, so I have not been
able to confirm the extra columns survive. Check with:

```r
p <- plot_pathway_tree(nest_tree_df(build_node_table(cohort)))
str(p$x$opts$series[[1]]$data, max.level = 3)
```

If they have been dropped, `resolve_clicked_node()` degrades gracefully — it
falls back to `treeAncestors`, then to label matching (flagged in the UI when
the label is ambiguous). The clean fix is to build the options list yourself and
pass it via `echarts4r::e_list()`, bypassing `e_tree()`, using the same nesting
logic but returning plain nested lists instead of tibbles.

**2. `params.treeAncestors` on a `tree` series.** Documented for `treemap` and
`sunburst`; I could not confirm it for `tree`. It is guarded with a null check,
so its absence costs nothing.

**3. PSA panel coverage.** Serial PSA is only simulated for the two deferred-
treatment arms. Clicking a node under radical treatment or metastatic disease
shows an explanatory message rather than an empty plot.

## Parameters to replace with real data

In priority order — the first two set every branch width in the tree:

1. `.rate_progress` in `assign_outcomes()`
2. `.rate_other_death` in `assign_outcomes()`
3. the `.u_mgmt` thresholds in `assign_initial_management()`
4. the `as_trigger` weights

Current values were tuned so the deferred arms land in a plausible range
(AS ≈ 60% treatment-free at 5 years, ≈ 34% reclassified to radical treatment;
WW median age 77 with ≈ 30% other-cause mortality at 5 years). They are not
calibrated to any registry.

## Known rough edges

- The spaghetti plot caps at 200 trajectories for legibility; the red median
  line is computed across the whole branch, not the plotted sample.
- Moving the depth slider re-renders the tree and resets its expansion state.
- Very deep branches can push labels outside the plot area; `roam = TRUE` lets
  users pan, but the right margin may need tuning for longer node labels.
