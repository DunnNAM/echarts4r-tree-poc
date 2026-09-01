# Prostate cancer pathway explorer — MVP

Interactive `echarts4r` pathway tree with a linked surveillance-PSA panel, built
on a synthetic cohort. Scoping prototype, not a validated tool.

## Run

```r
shiny::runApp()
```

Dependencies install themselves on first run in an interactive session. To do
it deliberately instead:

```r
source("setup.R")
```

## Structure

```
echarts4r-tree-poc/
├── DESCRIPTION                 # dependency declaration (read by renv / pak)
├── dependencies.R              # version-aware bootstrap, base R only
├── setup.R                     # one-shot installer, plus the renv path
├── global.R                    # packages, sourcing, defaults, CSS
├── ui.R                        # layout, view tabs
├── server.R                    # simulation, aggregation, view routing
└── funs/
    ├── fct_simulate.R          # synthetic cohort + PSA series
    ├── fct_tree.R              # hierarchy aggregation, nesting, e_tree rendering
    ├── fct_graph.R             # graph + Sankey data prep
    ├── mod_tree_view.R         # view 1: echarts4r e_tree
    ├── mod_network_view.R      # view 2: visNetwork
    ├── mod_sankey_view.R       # view 3: echarts4r e_sankey
    └── mod_branch_detail.R     # shared: PSA trajectories, branch summary
```

## Three views, one cohort

All three views render the same aggregated pathway data and resolve a click
back to the same patient set, so the detail panel below them is directly
comparable. They differ in what they can structurally express:

| View | Node identity | Can show convergence? | Tooltips built with |
|---|---|---|---|
| Tree (`e_tree`) | full ancestral path | No — duplicates the node per branch | JS formatter |
| Network (`visNetwork`) | label, or full path (toggle) | Yes — one node, several inbound edges | HTML string in R |
| Sankey (`e_sankey`) | label + level | Partially — merges within a level | ECharts default |

The network view's "merge identical labels" toggle is the comparison worth
making: on, "Radical prostatectomy" reached via surveillance and performed
upfront become one node; off, the topology reverts to the tree's. Merged nodes
are coloured differently, and the detail panel flags that it is summarising a
group pooled across pathway depths.

Sankey labels appearing at more than one level are suffixed (`(L4)`, `(L6)`).
This is not cosmetic: ECharts Sankey requires an acyclic graph, and merging
those labels would create a cycle, since radical prostatectomy is both an
upfront option at level 4 and a post-reclassification treatment at level 6.


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

Declared in `DESCRIPTION` and enforced at startup by `dependencies.R`:

| Package | Minimum | Why that floor |
|---|---|---|
| R | 4.1 | native pipe, `\(x)` lambda |
| dplyr | 1.1.0 | `case_match()`, `pick()` |
| purrr | 1.0.0 | `list_rbind()` |
| ggplot2 | 3.4.0 | `linewidth` aesthetic |
| echarts4r | 0.4.5 | `e_on()` |
| shiny, tidyr, tibble, htmlwidgets | — | no specific feature floor |

`ensure_dependencies()` checks **versions, not just presence**. The usual
`if (!require(x)) install.packages(x)` idiom would report success against an
old dplyr and then fail at runtime with "could not find function case_match",
which is a much worse error to debug than a version message at startup.

It installs automatically in interactive sessions and **never** installs in a
non-interactive one — auto-installing on a deployment target can block on a
prompt, write to a read-only library, or silently shift the versions a served
app is running. Non-interactive sessions get an error listing what to install.

Override either way:

```r
Sys.setenv(ECHARTS4R_POC_AUTO_INSTALL = "false")   # never auto-install
Sys.setenv(ECHARTS4R_POC_AUTO_INSTALL = "true")    # always, including headless
```

`pak` is used when installed (faster, better system-dependency handling), with
a fallback to `install.packages()`.

The `library()` calls in `global.R` are deliberately explicit and
unconditional. `rsconnect` and Posit Connect discover an app's dependencies by
static analysis of `library()` calls, so replacing them with a loop over a
package vector would break deployment.

### Moving to renv

Auto-install is right for local scoping work and wrong for anything shared —
it pins nothing, so two people can run different versions of the same commit.
Once this leaves your machine:

```r
install.packages("renv")
renv::init()       # picks up DESCRIPTION, builds a project-local library
renv::snapshot()   # writes renv.lock -- commit it
```

Collaborators then run `renv::restore()`. At that point set
`ECHARTS4R_POC_AUTO_INSTALL=false` in `.Renviron`, since the bootstrap becomes
redundant and could fight renv over versions.

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
