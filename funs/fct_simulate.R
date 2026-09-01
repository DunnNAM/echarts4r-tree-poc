# =============================================================================
# fct_simulate.R -- synthetic prostate cancer cohort + pathway assignment
# =============================================================================
# All parameters are ILLUSTRATIVE. They were tuned so the deferred-treatment
# arms land in a clinically plausible range (AS ~60% treatment-free at 5 yrs;
# WW median age ~77 with ~30% other-cause mortality at 5 yrs). Not calibrated
# to any registry -- replace with local data before drawing any conclusion.
# =============================================================================


#' Simulate baseline characteristics at diagnosis
#'
#' A single latent variable `aggr` (tumour aggressiveness, 0-1) drives stage,
#' risk group, PSA level, PSA doubling time and progression hazard together, so
#' the synthetic data stays internally coherent: men who later appear on the
#' "PSA kinetics" branch really do have short doubling times.
#'
#' @param n_patients Number of patients to simulate.
#' @return A tibble, one row per patient.
simulate_pca_cohort <- function(n_patients = 2000) {

  tibble::tibble(
    patient_id  = sprintf("PT%05d", seq_len(n_patients)),
    age_dx      = pmin(pmax(round(stats::rnorm(n_patients, 67, 8)), 45), 92),
    comorbidity = sample(
      c("Low", "Moderate", "High"), n_patients,
      replace = TRUE, prob = c(0.55, 0.32, 0.13)
    ),
    aggr = stats::rbeta(n_patients, 2, 4.5)
  ) |>
    dplyr::mutate(
      # ---- Stage at diagnosis (~80% / ~10% / ~10%) -----------------------
      .p_met     = stats::plogis(-4.2 + 5.5 * aggr),
      .p_loc_adv = stats::plogis(-3.6 + 4.0 * aggr),
      .u_stage   = stats::runif(dplyr::n()),
      stage_dx = dplyr::case_when(
        .u_stage < .p_met              ~ "Metastatic (de novo)",
        .u_stage < .p_met + .p_loc_adv ~ "Locally advanced / node+",
        TRUE                           ~ "Localised"
      ),

      # ---- NCCN-style risk group (localised only) -----------------------
      risk_group = dplyr::case_when(
        stage_dx != "Localised" ~ NA_character_,
        aggr < 0.15             ~ "Low risk",
        aggr < 0.30             ~ "Intermediate \u2014 favourable",
        aggr < 0.45             ~ "Intermediate \u2014 unfavourable",
        TRUE                    ~ "High risk"
      ),

      # ---- Sub-stratum for non-localised disease ------------------------
      adv_stratum = dplyr::case_when(
        stage_dx == "Locally advanced / node+" & aggr > 0.45 ~ "cN1 (node positive)",
        stage_dx == "Locally advanced / node+"               ~ "cT3\u20134 N0",
        stage_dx == "Metastatic (de novo)" & aggr > 0.45     ~ "High-volume M1",
        stage_dx == "Metastatic (de novo)"                   ~ "Low-volume M1",
        TRUE ~ NA_character_
      ),

      # ---- PSA at diagnosis (ng/mL) and doubling time (years) -----------
      psa_dx    = round(exp(stats::rnorm(dplyr::n(), 1.5 + 2.2 * aggr, 0.45)), 1),
      psadt_yrs = round(pmax(0.4, exp(stats::rnorm(dplyr::n(), log(6) - 3.2 * aggr, 0.5))), 1)
    ) |>
    dplyr::select(-dplyr::starts_with("."))
}


#' Assign initial management
#'
#' The AS-vs-WW split is the clinically important one: watchful waiting is
#' reserved for men with limited life expectancy (age >= 75, or high
#' comorbidity from 68), which yields roughly 20% of the deferral-eligible
#' group and a median WW age around 77.
assign_initial_management <- function(cohort) {

  cohort |>
    dplyr::mutate(
      .u_mgmt = stats::runif(dplyr::n()),

      .deferral_eligible = stage_dx == "Localised" &
        risk_group %in% c("Low risk", "Intermediate \u2014 favourable"),

      .limited_le = age_dx >= 75 | (comorbidity == "High" & age_dx >= 68),

      init_mgmt = dplyr::case_when(

        # --- Deferred treatment -------------------------------------------
        .deferral_eligible & .limited_le                               ~ "Watchful waiting",
        .deferral_eligible & risk_group == "Low risk" & .u_mgmt < 0.88 ~ "Active surveillance",
        .deferral_eligible & risk_group != "Low risk" & .u_mgmt < 0.45 ~ "Active surveillance",

        # --- Localised, radical treatment ---------------------------------
        stage_dx == "Localised" & age_dx < 70 & .u_mgmt < 0.55 ~ "Radical prostatectomy",
        stage_dx == "Localised" & .u_mgmt < 0.85               ~ "EBRT \u00b1 ADT",
        stage_dx == "Localised"                                ~ "Brachytherapy (LDR/HDR)",

        # --- Locally advanced / node positive -----------------------------
        stage_dx == "Locally advanced / node+" & .u_mgmt < 0.60 ~ "EBRT + long-course ADT",
        stage_dx == "Locally advanced / node+" & .u_mgmt < 0.90 ~ "RP + extended PLND",
        stage_dx == "Locally advanced / node+"                  ~ "ADT alone",

        # --- De novo metastatic -------------------------------------------
        adv_stratum == "Low-volume M1"  & .u_mgmt < 0.45 ~ "ADT + ARPI + prostate RT",
        adv_stratum == "Low-volume M1"  & .u_mgmt < 0.85 ~ "ADT + ARPI",
        adv_stratum == "Low-volume M1"                   ~ "ADT alone",
        adv_stratum == "High-volume M1" & .u_mgmt < 0.50 ~ "ADT + ARPI",
        adv_stratum == "High-volume M1" & .u_mgmt < 0.85 ~ "ADT + docetaxel (triplet)",
        TRUE                                             ~ "ADT alone"
      )
    ) |>
    dplyr::select(-dplyr::starts_with("."))
}


#' Simulate outcomes over the follow-up horizon and build the pathway sequence
#'
#' Two competing exponential clocks per patient: disease progression (hazard
#' driven by aggressiveness and management intensity) and non-prostate-cancer
#' death (hazard driven by age and comorbidity). Whichever fires first inside
#' the horizon determines the branch; neither firing means "remains stable".
#' This competing-risk structure is what makes the WW arm behave differently
#' from the AS arm rather than just being a relabelled copy of it.
assign_outcomes <- function(cohort, horizon_yrs = 5) {

  cohort |>
    dplyr::mutate(

      # ---- Competing event times ----------------------------------------
      .rate_other_death = exp(-11.5 + 0.115 * age_dx) *
        dplyr::case_match(comorbidity, "Low" ~ 0.7, "Moderate" ~ 1.2, "High" ~ 2.2),

      .rate_progress = dplyr::case_when(
        init_mgmt == "Active surveillance"      ~ 0.020 + 0.40 * aggr,
        init_mgmt == "Watchful waiting"         ~ 0.015 + 0.30 * aggr,
        stage_dx  == "Metastatic (de novo)"     ~ 0.180 + 0.60 * aggr,  # -> CRPC
        stage_dx  == "Locally advanced / node+" ~ 0.070 + 0.35 * aggr,
        TRUE                                    ~ 0.030 + 0.30 * aggr   # -> BCR
      ),

      .t_death    = stats::rexp(dplyr::n(), .rate_other_death),
      .t_progress = stats::rexp(dplyr::n(), .rate_progress),
      .t_first    = pmin(.t_death, .t_progress),

      event_type = dplyr::case_when(
        .t_first    > horizon_yrs ~ "stable",
        .t_progress <= .t_death   ~ "progression",
        TRUE                      ~ "other_death"
      ),

      t_event_yrs  = dplyr::if_else(event_type == "stable", NA_real_, round(.t_first, 2)),
      psa_at_event = round(psa_dx * 2^(t_event_yrs / psadt_yrs), 1),

      # ---- Reclassification trigger for AS ------------------------------
      # Short doubling time makes a PSA-kinetics trigger far more likely.
      .u_trig = stats::runif(dplyr::n()),
      as_trigger = dplyr::case_when(
        psadt_yrs < 3 & .u_trig < 0.45 ~ "PSA kinetics (PSADT < 3 yrs)",
        psadt_yrs < 3 & .u_trig < 0.80 ~ "Grade reclassification on rebiopsy",
        psadt_yrs < 3 & .u_trig < 0.93 ~ "MRI / clinical progression",
        psadt_yrs < 3                  ~ "Patient preference / anxiety",
        .u_trig < 0.15                 ~ "PSA kinetics (PSADT < 3 yrs)",
        .u_trig < 0.55                 ~ "Grade reclassification on rebiopsy",
        .u_trig < 0.80                 ~ "MRI / clinical progression",
        TRUE                           ~ "Patient preference / anxiety"
      ),

      .u_next = stats::runif(dplyr::n()),

      # ---- LEVEL 5: state at end of horizon -----------------------------
      outcome_state = dplyr::case_when(

        event_type == "other_death" ~ "Died \u2014 other cause",

        init_mgmt == "Active surveillance" & event_type == "progression" ~
          paste0("Reclassified \u2014 ", as_trigger),
        init_mgmt == "Active surveillance" & age_dx + horizon_yrs >= 80 ~
          "Transitioned to watchful waiting",
        init_mgmt == "Active surveillance" ~ "Remains on active surveillance",

        init_mgmt == "Watchful waiting" & event_type == "progression" ~
          "Symptomatic local / metastatic progression",
        init_mgmt == "Watchful waiting" ~ "Remains on watchful waiting",

        stage_dx == "Metastatic (de novo)" & event_type == "progression" ~
          "Castration-resistant progression",
        stage_dx == "Metastatic (de novo)" ~ "Ongoing castration-sensitive response",

        event_type == "progression" ~ "Biochemical recurrence",
        TRUE                        ~ "Undetectable PSA \u2014 no recurrence"
      ),

      # ---- LEVEL 6: next line of management -----------------------------
      next_step = dplyr::case_when(

        startsWith(outcome_state, "Reclassified") &
          age_dx + t_event_yrs < 70 & .u_next < 0.55       ~ "Radical prostatectomy",
        startsWith(outcome_state, "Reclassified") & .u_next < 0.80 ~ "EBRT \u00b1 short-course ADT",
        startsWith(outcome_state, "Reclassified") & .u_next < 0.93 ~ "Brachytherapy (LDR/HDR)",
        startsWith(outcome_state, "Reclassified")                  ~ "Continued AS after MDT review",

        outcome_state == "Symptomatic local / metastatic progression" &
          .u_next < 0.55                                              ~ "Palliative ADT",
        outcome_state == "Symptomatic local / metastatic progression" &
          .u_next < 0.80                                              ~ "TURP + palliative ADT",
        outcome_state == "Symptomatic local / metastatic progression" ~ "Palliative RT to bone",

        outcome_state == "Biochemical recurrence" & psadt_yrs > 1 & .u_next < 0.45 ~
          "Salvage RT \u00b1 ADT",
        outcome_state == "Biochemical recurrence" & .u_next < 0.70 ~
          "PSMA PET \u2192 metastasis-directed therapy",
        outcome_state == "Biochemical recurrence" & .u_next < 0.88 ~ "Salvage ADT \u00b1 ARPI",
        outcome_state == "Biochemical recurrence"                  ~ "Observation (long PSADT)",

        outcome_state == "Castration-resistant progression" & .u_next < 0.40 ~ "Docetaxel",
        outcome_state == "Castration-resistant progression" & .u_next < 0.65 ~ "177Lu-PSMA-617",
        outcome_state == "Castration-resistant progression" & .u_next < 0.80 ~
          "PARP inhibitor (HRR-mutated)",
        outcome_state == "Castration-resistant progression" ~ "Best supportive care",

        TRUE ~ NA_character_
      ),

      # ---- Assemble the ordered pathway ---------------------------------
      path_1 = "All new diagnoses",
      path_2 = stage_dx,
      path_3 = dplyr::coalesce(risk_group, adv_stratum),
      path_4 = init_mgmt,
      path_5 = outcome_state,
      path_6 = next_step
    ) |>
    dplyr::select(-dplyr::starts_with("."))
}


#' Simulate serial surveillance PSA for men on deferred treatment
#'
#' 6-monthly on active surveillance, 12-monthly on watchful waiting, censored at
#' the transition event. PSA(t) = PSA_0 * 2^(t / PSADT) with multiplicative
#' assay + biological noise.
simulate_psa_series <- function(cohort, horizon_yrs = 5, noise_sd = 0.15) {

  cohort |>
    dplyr::filter(init_mgmt %in% c("Active surveillance", "Watchful waiting")) |>
    dplyr::mutate(
      interval_yrs = dplyr::if_else(init_mgmt == "Active surveillance", 0.5, 1.0),
      stop_yrs     = dplyr::coalesce(t_event_yrs, horizon_yrs)
    ) |>
    dplyr::select(patient_id, init_mgmt, psa_dx, psadt_yrs, interval_yrs,
                  stop_yrs, outcome_state) |>
    dplyr::mutate(visit_yrs = purrr::map2(interval_yrs, stop_yrs,
                                          \(i, s) seq(0, s, by = i))) |>
    tidyr::unnest(visit_yrs) |>
    dplyr::mutate(
      visit_month = round(visit_yrs * 12),
      psa = round(psa_dx * 2^(visit_yrs / psadt_yrs) *
                    exp(stats::rnorm(dplyr::n(), 0, noise_sd)), 2)
    ) |>
    dplyr::group_by(patient_id) |>
    dplyr::mutate(visit_n = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(patient_id, init_mgmt, visit_n, visit_month, visit_yrs,
                  psa, psadt_yrs, outcome_state)
}


#' Convenience wrapper: full cohort in one call
build_cohort <- function(n_patients = 2000, horizon_yrs = 5, seed = 2026) {
  set.seed(seed)
  cohort <- simulate_pca_cohort(n_patients) |>
    assign_initial_management() |>
    assign_outcomes(horizon_yrs = horizon_yrs)

  # Return the horizon with the data: the slider can move without the user
  # pressing "Re-simulate", so labels must read the horizon the data was
  # actually built with, not the current input value.
  list(
    cohort      = cohort,
    psa_long    = simulate_psa_series(cohort, horizon_yrs = horizon_yrs),
    horizon_yrs = horizon_yrs,
    n_patients  = n_patients,
    seed        = seed
  )
}
