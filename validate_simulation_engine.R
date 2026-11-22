# ============================================================
# VALIDATION EXTERNE DU MOTEUR DE SIMULATION — ExpDesign2
#
# Objectifs :
#   1. vérifier le comportement sous H0;
#   2. activer un seul effet fixe à la fois;
#   3. vérifier que les autres effets restent au niveau nominal;
#   4. distinguer un problème de génération d'un problème d'analyse.
#
# Utilisation :
#   - placer ce fichier dans le même dossier que global.R;
#   - ouvrir ce fichier dans RStudio;
#   - exécuter le script par sections.
# ============================================================


# ============================================================
# 0. INITIALISATION
# ============================================================

source("global.R")


# ============================================================
# 1. PRÉPARER UN PLAN ET IDENTIFIER SES PARAMÈTRES
# ============================================================

prepare_validation_design <- function(
    design_str,
    n_levels
) {

  r <- prepare_design_result(
    design_str = design_str,
    n_levels = n_levels
  )

  # ----------------------------------------------------------
  # Effets fixes simulables
  # Même logique que dans l'application
  # ----------------------------------------------------------
  fixed_effects <- names(r$effects)[
    vapply(
      names(r$effects),
      function(effect_name) {

        effect_info <- r$effects[[effect_name]]

        non_subject_factors <- setdiff(
          effect_info$factors,
          "s"
        )

        !isTRUE(effect_info$is_error) &&
          length(non_subject_factors) > 0L &&
          all(
            vapply(
              non_subject_factors,
              function(factor_name) {

                !is.null(r$fcts[[factor_name]]) &&
                  isTRUE(r$fcts[[factor_name]]$fixed)
              },
              logical(1)
            )
          )
      },
      logical(1)
    )
  ]

  # ----------------------------------------------------------
  # Termes d'erreur
  # ----------------------------------------------------------
  error_terms <- names(r$effects)[
    vapply(
      r$effects,
      function(effect_info) {
        isTRUE(effect_info$is_error)
      },
      logical(1)
    )
  ]

  residual_term <- if (length(error_terms) > 0L) {
    tail(error_terms, 1L)
  } else {
    character(0)
  }

  # ----------------------------------------------------------
  # Composantes aléatoires autres que la variance résiduelle
  # ----------------------------------------------------------
  random_terms <- names(r$effects)[
    vapply(
      r$effects,
      function(effect_info) {

        effect_factors <- setdiff(
          effect_info$factors,
          "s"
        )

        contains_random_factor <- any(
          vapply(
            effect_factors,
            function(factor_name) {

              !is.null(r$fcts[[factor_name]]) &&
                isTRUE(r$fcts[[factor_name]]$random)
            },
            logical(1)
          )
        )

        isTRUE(effect_info$is_error) ||
          contains_random_factor
      },
      logical(1)
    )
  ]

  # La variance résiduelle est passée séparément à sim_data()
  random_terms <- setdiff(
    random_terms,
    residual_term
  )

  list(
    r = r,
    fixed_effects = fixed_effects,
    random_terms = random_terms,
    residual_term = residual_term
  )
}


# ============================================================
# 2. CRÉER DES VECTEURS DE PARAMÈTRES NULS
# ============================================================

zero_effect_sizes <- function(validation_design) {

  stats::setNames(
    rep(
      0,
      length(validation_design$fixed_effects)
    ),
    validation_design$fixed_effects
  )
}


zero_random_variances <- function(validation_design) {

  stats::setNames(
    rep(
      0,
      length(validation_design$random_terms)
    ),
    validation_design$random_terms
  )
}


# ============================================================
# 3. EXÉCUTER UN SCÉNARIO MONTE-CARLO
# ============================================================

validate_one_scenario <- function(
    validation_design,
    effect_sizes = NULL,
    random_variances = NULL,
    residual_variance = 1,
    B = 500L,
    seed = 1234L,
    alpha = 0.05,
    scenario_name = "Scénario"
) {

  r <- validation_design$r

  if (is.null(effect_sizes)) {
    effect_sizes <- zero_effect_sizes(
      validation_design
    )
  }

  if (is.null(random_variances)) {
    random_variances <- zero_random_variances(
      validation_design
    )
  }

  # ----------------------------------------------------------
  # Vérifications des noms
  # ----------------------------------------------------------
  missing_fixed <- setdiff(
    validation_design$fixed_effects,
    names(effect_sizes)
  )

  if (length(missing_fixed) > 0L) {
    stop(
      paste(
        "Effets fixes manquants :",
        paste(missing_fixed, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  missing_random <- setdiff(
    validation_design$random_terms,
    names(random_variances)
  )

  if (length(missing_random) > 0L) {
    stop(
      paste(
        "Composantes aléatoires manquantes :",
        paste(missing_random, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (
    length(residual_variance) != 1L ||
    !is.finite(residual_variance) ||
    residual_variance <= 0
  ) {
    stop(
      "La variance résiduelle doit être strictement positive.",
      call. = FALSE
    )
  }

  cat(
    "\n============================================================\n",
    scenario_name,
    "\n============================================================\n",
    sep = ""
  )

  cat("\nEffets fixes :\n")
  print(effect_sizes)

  cat("\nVariances aléatoires :\n")
  print(random_variances)

  cat(
    "\nVariance résiduelle :",
    residual_variance,
    "\n"
  )

  mc <- run_monte_carlo(
    r = r,
    effect_sizes = effect_sizes,
    random_variances = random_variances,
    residual_variance = residual_variance,
    B = as.integer(B),
    seed = as.integer(seed),
    alpha = alpha,
    progress = FALSE
  )

  summary_columns <- intersect(
    c(
      "Effet",
      "EMS_theorique",
      "MS_moyen",
      "Ecart_standardise",
      "N_F_valide",
      "F_moyen",
      "Taux_rejet"
    ),
    names(mc$summary)
  )

  summary_table <- mc$summary[
    ,
    summary_columns,
    drop = FALSE
  ]

  cat("\nRésumé Monte-Carlo :\n")
  print(
    summary_table,
    row.names = FALSE
  )

  list(
    name = scenario_name,
    parameters = list(
      effect_sizes = effect_sizes,
      random_variances = random_variances,
      residual_variance = residual_variance,
      B = B,
      seed = seed,
      alpha = alpha
    ),
    mc = mc,
    summary = summary_table
  )
}


# ============================================================
# 4. ACTIVER UN SEUL EFFET FIXE
# ============================================================

effect_sizes_one_active <- function(
    validation_design,
    active_effect,
    value = 1
) {

  effects <- zero_effect_sizes(
    validation_design
  )

  if (!active_effect %in% names(effects)) {
    stop(
      paste(
        "Effet fixe inconnu :",
        active_effect,
        "\nEffets disponibles :",
        paste(names(effects), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  effects[[active_effect]] <- value

  effects
}


# ============================================================
# 5. ACTIVER UNE SEULE COMPOSANTE ALÉATOIRE
# ============================================================

random_variances_one_active <- function(
    validation_design,
    active_term,
    value = 1
) {

  variances <- zero_random_variances(
    validation_design
  )

  if (!active_term %in% names(variances)) {
    stop(
      paste(
        "Composante aléatoire inconnue :",
        active_term,
        "\nComposantes disponibles :",
        paste(names(variances), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  variances[[active_term]] <- value

  variances
}


# ============================================================
# 6. COMPARER PLUSIEURS SCÉNARIOS
# ============================================================

combine_validation_summaries <- function(
    scenario_results
) {

  rows <- lapply(
    scenario_results,
    function(x) {

      tbl <- x$summary

      tbl$Scenario <- x$name

      tbl
    }
  )

  combined <- do.call(
    rbind,
    rows
  )

  rownames(combined) <- NULL

  first_columns <- c(
    "Scenario",
    "Effet"
  )

  combined[
    ,
    c(
      first_columns,
      setdiff(
        names(combined),
        first_columns
      )
    ),
    drop = FALSE
  ]
}


# ============================================================
# 7. INSPECTER UN ÉCHANTILLON UNIQUE AVANT L'ANOVA
# ============================================================

inspect_one_sample <- function(
    validation_design,
    effect_sizes = NULL,
    random_variances = NULL,
    residual_variance = 1,
    seed = 1234L
) {

  r <- validation_design$r

  if (is.null(effect_sizes)) {
    effect_sizes <- zero_effect_sizes(
      validation_design
    )
  }

  if (is.null(random_variances)) {
    random_variances <- zero_random_variances(
      validation_design
    )
  }

  dat <- sim_data(
    factors = r$fcts,
    effects = r$effects,
    n_levels = r$n_levels,
    effect_sizes = effect_sizes,
    random_variances = random_variances,
    sigma2_error = residual_variance,
    seed = seed
  )

  factor_names <- setdiff(
    names(r$fcts),
    "s"
  )

  response_name <- if ("y" %in% names(dat)) {
    "y"
  } else if ("Y" %in% names(dat)) {
    "Y"
  } else {
    stop(
      paste(
        "Impossible d'identifier la variable réponse.",
        "Colonnes disponibles :",
        paste(names(dat), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Moyennes marginales pour chaque facteur
  # ----------------------------------------------------------
  marginal_means <- stats::setNames(
    lapply(
      factor_names,
      function(factor_name) {

        stats::aggregate(
          dat[[response_name]],
          by = list(
            Niveau = dat[[factor_name]]
          ),
          FUN = mean
        )
      }
    ),
    factor_names
  )

  # ----------------------------------------------------------
  # Moyennes de toutes les cellules
  # ----------------------------------------------------------
  cell_formula <- stats::as.formula(
    paste(
      response_name,
      "~",
      paste(
        factor_names,
        collapse = " + "
      )
    )
  )

  cell_means <- stats::aggregate(
    formula = cell_formula,
    data = dat,
    FUN = mean
  )

  list(
    data = dat,
    marginal_means = marginal_means,
    cell_means = cell_means
  )
}


# ============================================================
# 8. PLAN À TESTER
# ============================================================

validation_design <- prepare_validation_design(
  design_str = "A,B,c,s(A)",

  # Adaptez ces nombres aux valeurs utilisées dans l'application.
  n_levels = c(
    A = 2,
    B = 2,
    c = 2,
    s = 5
  )
)

cat("\nEffets fixes détectés :\n")
print(validation_design$fixed_effects)

cat("\nComposantes aléatoires détectées :\n")
print(validation_design$random_terms)

cat("\nTerme résiduel détecté :\n")
print(validation_design$residual_term)


# ============================================================
# 9. TEST RAPIDE — H0 COMPLÈTE
# ============================================================

test_h0 <- validate_one_scenario(
  validation_design = validation_design,
  effect_sizes = zero_effect_sizes(
    validation_design
  ),
  random_variances = zero_random_variances(
    validation_design
  ),
  residual_variance = 1,
  B = 500,
  seed = 1000,
  scenario_name = "H0 complète"
)


# ============================================================
# 10. TEST — A SEUL
# ============================================================

test_A <- validate_one_scenario(
  validation_design = validation_design,
  effect_sizes = effect_sizes_one_active(
    validation_design,
    active_effect = "A",
    value = 1
  ),
  random_variances = zero_random_variances(
    validation_design
  ),
  residual_variance = 1,
  B = 500,
  seed = 2000,
  scenario_name = "A = 1; autres effets = 0"
)


# ============================================================
# 11. TEST — B SEUL
# ============================================================

test_B <- validate_one_scenario(
  validation_design = validation_design,
  effect_sizes = effect_sizes_one_active(
    validation_design,
    active_effect = "B",
    value = 1
  ),
  random_variances = zero_random_variances(
    validation_design
  ),
  residual_variance = 1,
  B = 500,
  seed = 3000,
  scenario_name = "B = 1; autres effets = 0"
)


# ============================================================
# 12. TEST — AB SEUL
# ============================================================

test_AB <- validate_one_scenario(
  validation_design = validation_design,
  effect_sizes = effect_sizes_one_active(
    validation_design,
    active_effect = "AB",
    value = 1
  ),
  random_variances = zero_random_variances(
    validation_design
  ),
  residual_variance = 1,
  B = 500,
  seed = 4000,
  scenario_name = "AB = 1; autres effets = 0"
)


# ============================================================
# 13. TEST — c SEUL
# ============================================================

test_c <- validate_one_scenario(
  validation_design = validation_design,
  effect_sizes = effect_sizes_one_active(
    validation_design,
    active_effect = "c",
    value = 1
  ),
  random_variances = zero_random_variances(
    validation_design
  ),
  residual_variance = 1,
  B = 500,
  seed = 5000,
  scenario_name = "c = 1; autres effets = 0"
)


# ============================================================
# 14. TABLEAU COMPARATIF FINAL
# ============================================================

validation_results <- list(
  test_h0,
  test_A,
  test_B,
  test_AB,
  test_c
)

validation_summary <- combine_validation_summaries(
  validation_results
)

cat(
  "\n============================================================\n",
  "TABLEAU COMPARATIF FINAL\n",
  "============================================================\n",
  sep = ""
)

print(
  validation_summary,
  row.names = FALSE
)


# ============================================================
# 15. INSPECTION D'UN ÉCHANTILLON : c SEUL
# ============================================================

sample_c <- inspect_one_sample(
  validation_design = validation_design,
  effect_sizes = effect_sizes_one_active(
    validation_design,
    active_effect = "c",
    value = 1
  ),
  random_variances = zero_random_variances(
    validation_design
  ),
  residual_variance = 1,
  seed = 9876
)

cat("\nMoyennes marginales — scénario c seul :\n")

for (factor_name in names(sample_c$marginal_means)) {

  cat(
    "\nFacteur",
    factor_name,
    ":\n"
  )

  print(
    sample_c$marginal_means[[factor_name]]
  )
}

cat("\nMoyennes des cellules — scénario c seul :\n")

print(
  sample_c$cell_means
)