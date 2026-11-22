library(shiny)
library(bslib)
library(katex)
library(shinyjs)
library(httr)
library(jsonlite)
library(car)


# Graine par défaut basée sur date/heure
default_seed <- function() as.integer(Sys.time()) %% 100000

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================
# Modules EMS (copie de modules_ems_complet.R)
# ============================================================

parse_design <- function(design_string) {
  design_string <- gsub(" ", "", design_string)
  terms <- unlist(strsplit(design_string, ","))
  factors <- list()
  for (term in terms) {
    if (grepl("\\(", term)) {
      main      <- sub("\\(.*\\)", "", term)
      inner     <- sub(".*\\((.*)\\)", "\\1", term)
      nested_in <- strsplit(inner, "")[[1]]
      factors[[main]] <- list(fixed=main==toupper(main),
                              random=main!=toupper(main),
                              subject=(main=="s"),
                              nested_in=nested_in)
    } else {
      factors[[term]] <- list(fixed=term==toupper(term),
                              random=term!=toupper(term),
                              subject=(term=="s"),
                              nested_in=character(0))
    }
  }
  all_f <- names(factors)
  for (f1 in all_f) {
    crossed <- character(0)
    for (f2 in all_f) {
      if (f1==f2) next
      if (!f2 %in% factors[[f1]]$nested_in && !f1 %in% factors[[f2]]$nested_in)
        crossed <- c(crossed, f2)
    }
    factors[[f1]]$crossed_with <- crossed
  }
  return(factors)
}

generate_effects <- function(factors) {
  all_f <- names(factors)
  n <- length(all_f)
  effects <- list()
  for (mask in 1:(2^n-1)) {
    subset_f <- all_f[as.logical(intToBits(mask)[1:n])]
    valid <- TRUE
    for (fi in subset_f) for (fj in subset_f)
      if (fi!=fj && fj %in% factors[[fi]]$nested_in) { valid<-FALSE; break }
    if (!valid) next
    nested <- setdiff(unique(unlist(lapply(subset_f,
                function(f) factors[[f]]$nested_in))), subset_f)
    non_s <- sort(subset_f[subset_f!="s"])
    has_s <- "s" %in% subset_f
    label <- if (has_s) {
      if (length(nested)>0)
        paste0(paste(non_s,collapse=""),"s(",paste(sort(nested),collapse=""),")")
      else paste0(paste(non_s,collapse=""),"s")
    } else {
      if (length(nested)>0)
        paste0(paste(non_s,collapse=""),"(",paste(sort(nested),collapse=""),")")
      else paste(non_s,collapse="")
    }
    effects[[label]] <- list(factors=subset_f, nested_in=nested,
                             is_error=has_s, label=label)
  }
  return(effects)
}



find_error_terms <- function(factors, effects, ems_table) {
  pure_errors <- list()
  quasi_f     <- list()
  error_cands  <- names(effects)[vapply(names(effects),
    function(e) isTRUE(effects[[e]]$is_error), logical(1))]
  test_effects <- names(effects)[!vapply(names(effects),
    function(e) isTRUE(effects[[e]]$is_error), logical(1))]
  for (eff_name in test_effects) {
    ems_e   <- ems_table[[eff_name]]
    rand_e  <- names(ems_e)[!vapply(names(ems_e),
      function(t) isTRUE(ems_e[[t]]$is_fixed), logical(1))]
    other_r <- setdiff(rand_e, eff_name)
    found <- FALSE
    for (cand in error_cands) {
      ems_c  <- ems_table[[cand]]
      rand_c <- names(ems_c)
      if (!setequal(rand_c, other_r)) next
      coefs_ok <- all(vapply(other_r, function(t) {
        ce <- ems_e[[t]]$coef_str; cc <- ems_c[[t]]$coef_str
        if (identical(ce,cc)||identical(cc,"1")) return(TRUE)
        all(strsplit(cc,"\u00b7")[[1]] %in% strsplit(ce,"\u00b7")[[1]])
      }, logical(1)))
      if (coefs_ok) { pure_errors[[eff_name]] <- cand; found<-TRUE; break }
    }
    if (!found)
      quasi_f[[eff_name]] <- list(ems_terms=other_r, error_cands=error_cands)
  }
  return(list(pure=pure_errors, quasi=quasi_f))
}

compute_df <- function(factors, effects, n_levels) {
  df_list <- list()
  for (eff_name in names(effects)) {
    eff <- effects[[eff_name]]
    p1s <- character(0); p1n <- 1
    for (f in eff$factors) {
      p1s <- c(p1s, paste0("(n_",f,"-1)"))
      if (!is.null(n_levels[[f]])) p1n <- p1n*(n_levels[[f]]-1)
    }
    p2s <- character(0); p2n <- 1
    for (p in eff$nested_in) {
      p2s <- c(p2s, paste0("n_",p))
      if (!is.null(n_levels[[p]])) p2n <- p2n*n_levels[[p]]
    }
    df_list[[eff_name]] <- list(
      symbolic=paste(c(p1s,p2s),collapse="\u00b7"), numeric=p1n*p2n)
  }
  return(df_list)
}

verify_df <- function(factors, effects, n_levels, df_list) {
  N <- 1; sym_N <- character(0)
  for (f in names(factors)) {
    n_f <- n_levels[[f]]
    if (!is.null(n_f)) { N <- N*n_f; sym_N <- c(sym_N,paste0("n_",f)) }
  }
  df_sum <- sum(sapply(df_list, function(x) x$numeric))
  list(N=N, N_sym=paste(sym_N,collapse="\u00b7"), df_total=N-1, df_sum=df_sum,
       verified=isTRUE(all.equal(df_sum, N-1)))
}

parse_coef_numeric <- function(coef_str, n_levels) {
  if (coef_str=="1") return(1)
  result <- 1
  for (p in strsplit(coef_str,"\u00b7")[[1]]) {
    n_f <- n_levels[[sub("n_","",p)]]
    result <- if(!is.null(n_f)) result*n_f else result*NA
  }
  return(result)
}

###################################################
# build_quasi_f()
# IMPORTANT
#
# Les carrés moyens contenant une contribution fixe
# ne doivent pas être utilisés comme candidats dans
# la reconstruction Cornfield–Tukey.
#
# Sinon, un effet fixe actif peut contaminer le test
# d'un autre effet fixe (validation Monte-Carlo,
# août 2026).
####################################################
build_quasi_f <- function(eff_name, factors, effects, ems_table,
                          df_list, n_levels) {
  ems_e <- ems_table[[eff_name]]
  rand_terms  <- names(ems_e)[!vapply(names(ems_e),
    function(t) isTRUE(ems_e[[t]]$is_fixed), logical(1))]
  sigma_terms <- setdiff(rand_terms, eff_name)
  if (length(sigma_terms) == 0) {
  return(list(
    effect = eff_name,
    sigma_terms = sigma_terms,
    satt_coefficients = numeric(0),
    ct = NULL,
    satterthwaite = NULL
  ))
}
  # ----------------------------------------------------------
# Carrés moyens candidats
# ----------------------------------------------------------
error_cands <- names(effects)[
  vapply(
    names(effects),
    function(effect_name) {
      isTRUE(effects[[effect_name]]$is_error)
    },
    logical(1)
  )
]

random_cands <- names(effects)[
  vapply(
    names(effects),
    function(candidate_name) {

      candidate_effect <- effects[[candidate_name]]
      candidate_ems    <- ems_table[[candidate_name]]

      has_fixed_contribution <- any(
        vapply(
          candidate_ems,
          function(term_info) {
            isTRUE(term_info$is_fixed)
          },
          logical(1)
        )
      )

      !isTRUE(candidate_effect$is_error) &&
        length(candidate_effect$factors) > 0L &&
        !has_fixed_contribution &&
        any(
          sigma_terms %in% names(candidate_ems)
        )
    },
    logical(1)
  )
]

all_cands <- unique(
  c(
    error_cands,
    random_cands
  )
)

all_cands <- setdiff(
  all_cands,
  eff_name
)

# ----------------------------------------------------------
# Effets fixes parasites présents dans les EMS candidats
#
# Pour tester eff_name, seule sa propre contribution fixe
# disparaît sous H0. Toute autre contribution fixe doit
# s'annuler exactement dans la combinaison CT.
# ----------------------------------------------------------
nuisance_fixed_terms <- unique(
  unlist(
    lapply(
      all_cands,
      function(candidate_name) {

        candidate_ems <- ems_table[[candidate_name]]

        names(candidate_ems)[
          vapply(
            candidate_ems,
            function(term_info) {
              isTRUE(term_info$is_fixed)
            },
            logical(1)
          )
        ]
      }
    ),
    use.names = FALSE
  )
)

nuisance_fixed_terms <- setdiff(
  nuisance_fixed_terms,
  eff_name
)

# ----------------------------------------------------------
# Contraintes du système
#
# 1. Reproduire les composantes de variance de EMS(eff_name)
#    sous H0.
#
# 2. Annuler toutes les contributions fixes parasites.
# ----------------------------------------------------------
constraint_terms <- unique(
  c(
    sigma_terms,
    nuisance_fixed_terms
  )
)

b <- stats::setNames(
  numeric(
    length(constraint_terms)
  ),
  constraint_terms
)

# Cible des composantes de variance
b[sigma_terms] <- vapply(
  sigma_terms,
  function(term_name) {

    parse_coef_numeric(
      ems_e[[term_name]]$coef_str,
      n_levels
    )
  },
  numeric(1)
)

# Les cibles des effets fixes parasites demeurent à zéro.
if (length(nuisance_fixed_terms) > 0L) {
  b[nuisance_fixed_terms] <- 0
}

# ----------------------------------------------------------
# Matrice candidats × contraintes
# ----------------------------------------------------------
M <- matrix(
  0,
  nrow = length(all_cands),
  ncol = length(constraint_terms),
  dimnames = list(
    all_cands,
    constraint_terms
  )
)

for (candidate_name in all_cands) {

  candidate_ems <- ems_table[[candidate_name]]

  common_terms <- intersect(
    names(candidate_ems),
    constraint_terms
  )

  for (term_name in common_terms) {

    M[candidate_name, term_name] <-
      parse_coef_numeric(
        candidate_ems[[term_name]]$coef_str,
        n_levels
      )
  }
}
  ct_result <- NULL
  tryCatch({
    rows_ok    <- rowSums(abs(M)) > 0
    M2         <- M[rows_ok, , drop=FALSE]
    cands_used <- rownames(M2)
    M_T  <- t(M2)
    MTMt <- M_T %*% t(M_T)
    sol  <- as.vector(t(M_T) %*% solve(MTMt + 1e-10*diag(nrow(MTMt))) %*% b)
    names(sol) <- cands_used
    resid <- max(abs(M_T %*% sol - b))
    if (resid < 1e-4) {
      sol <- sol[abs(sol) > 1e-6]
      terms_pos <- names(sol)[sol > 0]
      terms_neg <- names(sol)[sol < 0]
      fp_pos <- sapply(terms_pos, function(e) {
        ci <- sol[[e]]
        if(abs(ci-1)<1e-6) paste0("MS(",e,")")
        else paste0(round(ci,4),"\\cdot MS(",e,")")
      })
      fp_neg <- sapply(terms_neg, function(e) {
        ci <- abs(sol[[e]])
        if(abs(ci-1)<1e-6) paste0("MS(",e,")")
        else paste0(round(ci,4),"\\cdot MS(",e,")")
      })
      formula_str <- paste0("MS_{quasi} = ",
        paste(fp_pos, collapse=" + "),
        if(length(fp_neg)>0) paste0(" - ", paste(fp_neg, collapse=" - ")) else "")
      dp <- sapply(names(sol), function(e)
        paste0("\\frac{(",round(sol[[e]],4),"\\cdot MS(",e,"))^2}{",
               df_sym_to_latex_raw(df_list[[e]]$symbolic),"}"))
      ct_result <- list(
        coefficients = sol,
        formula      = formula_str,
        f_formula    = paste0("F' = \\frac{MS(",eff_name,")}{MS_{quasi}}"),
        df_formula   = paste0("df_{quasi} = \\frac{MS_{quasi}^2}{",
                              paste(dp, collapse=" + "),"}"),
        terms_pos    = terms_pos,
        terms_neg    = terms_neg
      )
    }
  }, error=function(e) NULL)
  satt_coefficients <- setNames(
  vapply(
    sigma_terms,
    function(t) {
      parse_coef_numeric(
        ems_e[[t]]$coef_str,
        n_levels
      )
    },
    numeric(1)
  ),
  sigma_terms
)
  satt_result <- NULL
  all_direct  <- all(vapply(sigma_terms, function(t)
    !is.null(effects[[t]]) &&
    (isTRUE(effects[[t]]$is_error) || t %in% random_cands), logical(1)))
  if (all_direct) {
    fp <- sapply(sigma_terms, function(t) {
      ci <- ems_e[[t]]$coef_str
      if(ci=="1") paste0("MS(",t,")")
      else paste0(coef_to_latex_raw(ci),"\\cdot MS(",t,")")
    })
    dp <- sapply(sigma_terms, function(t) {
      ci  <- ems_e[[t]]$coef_str
      dfe <- df_sym_to_latex_raw(df_list[[t]]$symbolic)
      if(ci=="1") paste0("\\frac{MS(",t,")^2}{",dfe,"}")
      else paste0("\\frac{(",coef_to_latex_raw(ci),"\\cdot MS(",t,"))^2}{",dfe,"}")
    })
    satt_result <- list(
      formula   = paste0("MS_{quasi} = ", paste(fp, collapse=" + ")),
      f_formula = paste0("F' = \\frac{MS(",eff_name,")}{MS_{quasi}}"),
      df_formula= paste0("df_{quasi} = \\frac{MS_{quasi}^2}{",
                         paste(dp, collapse=" + "),"}")
    )
  }
  return(list(
  effect = eff_name,
  sigma_terms = sigma_terms,
  satt_coefficients = satt_coefficients,
  ct = ct_result,
  satterthwaite = satt_result
))
}

# ============================================================
# Conversion LaTeX — fonctions utilitaires
# ============================================================
coef_to_latex_raw <- function(coef_str) {
  if (coef_str == "1") return("1")
  parts <- strsplit(coef_str, "\u00b7")[[1]]
  paste(gsub("n_(.+)", "n_{\\1}", parts), collapse="")
}

ems_to_latex_raw <- function(ems_entry) {
  co <- ems_entry$coef_str
  sy <- ems_entry$symbol
  co_tex <- coef_to_latex_raw(co)
  sy_tex <- if(grepl("^\u03c6", sy)) {
    eff <- gsub("\u03c6\\((.+)\\)", "\\1", sy)
    paste0("\\phi_{", eff, "}")
  } else {
    eff <- gsub("\u03c3\u00b2\\((.+)\\)", "\\1", sy)
    paste0("\\sigma^2_{", eff, "}")
  }
  if (co_tex == "1") sy_tex else paste0(co_tex, "\\,", sy_tex)
}

df_sym_to_latex_raw <- function(sym) {
  if (is.null(sym) || sym == "") return("1")
  parts <- strsplit(sym, "\u00b7")[[1]]
  tex_parts <- sapply(parts, function(p) {
    p <- gsub("\\(n_([^-]+)-1\\)", "(n_{\\1}-1)", p)
    p <- gsub("^n_(.+)$", "n_{\\1}", p)
    p
  })
  paste(tex_parts, collapse="\\cdot ")
}

# Rendu HTML via katex R
k <- function(latex_str, display=FALSE) {
  HTML(katex::katex_html(latex_str, display=display, preview=FALSE))
}

# ------------------------------------------------------------
# Identifier les composantes de variance communes entre
# un effet sélectionné et son terme d'erreur pur
# ------------------------------------------------------------
ems_common_variance_info <- function(
    effect_name,
    ems_table,
    error_info
) {

  pure_error <- error_info$pure[[effect_name]]

  # Aucun terme d'erreur pur :
  # effet non testé, terme d'erreur ou quasi-F
  if (
    is.null(pure_error) ||
    is.null(ems_table[[effect_name]]) ||
    is.null(ems_table[[pure_error]])
  ) {
    return(
      list(
        error = NULL,
        terms = character(0)
      )
    )
  }

  ems_effect <- ems_table[[effect_name]]
  ems_error  <- ems_table[[pure_error]]

  variance_effect <- names(ems_effect)[
    vapply(
      ems_effect,
      function(x) !isTRUE(x$is_fixed),
      logical(1)
    )
  ]

  variance_error <- names(ems_error)[
    vapply(
      ems_error,
      function(x) !isTRUE(x$is_fixed),
      logical(1)
    )
  ]

  common_terms <- intersect(
    variance_effect,
    variance_error
  )

  # Par prudence, ne jamais considérer la contribution propre
  # de l'effet comme une composante commune.
  common_terms <- setdiff(
    common_terms,
    effect_name
  )

  list(
    error = pure_error,
    terms = common_terms
  )
}


# ------------------------------------------------------------
# Rendu d'un EMS avec mise en évidence facultative de certains
# termes. Les appels existants restent compatibles.
# ------------------------------------------------------------
ems_row_to_html <- function(
    ems,
    highlight_terms = character(0)
) {

  term_names <- names(ems)

  rendered_parts <- list()

  for (i in seq_along(term_names)) {

    term_name <- term_names[i]

    term_math <- k(
      ems_to_latex_raw(
        ems[[term_name]]
      )
    )

    if (term_name %in% highlight_terms) {

      term_math <- tags$span(
        class = "ems-common-component",
        style = paste(
          "display:inline-block;",
          "font-weight:800;",
          "color:#084298;",
          "background-color:#cfe2ff;",
          "border:1.5px solid #6ea8fe;",
          "border-radius:0.3rem;",
          "padding:0.10rem 0.32rem;"
        ),
        term_math
      )
    }

    rendered_parts[[length(rendered_parts) + 1]] <- term_math

    if (i < length(term_names)) {
      rendered_parts[[length(rendered_parts) + 1]] <- tags$span(
        style = "margin:0 0.22rem;",
        "+"
      )
    }
  }

  tagList(rendered_parts)
}

ems_under_h0_to_html <- function(
    ems,
    highlight_terms = character(0)
) {

  if (
    is.null(ems) ||
    length(ems) == 0L
  ) {
    return(
      tags$span(
        class = "text-muted",
        "EMS non disponible"
      )
    )
  }

  # Sous H0, la contribution propre de l'effet testé
  # est nulle. On conserve uniquement les composantes
  # de variance.
  keep_terms <- names(ems)[
    !vapply(
      ems,
      function(term_info) {
        isTRUE(term_info$is_fixed)
      },
      logical(1)
    )
  ]

  ems_h0 <- ems[
    keep_terms
  ]

  if (length(ems_h0) == 0L) {
    return(
      tags$span(
        class = "text-muted",
        "Aucune composante de variance ne demeure sous H₀."
      )
    )
  }

  ems_row_to_html(
    ems = ems_h0,
    highlight_terms = intersect(
      highlight_terms,
      names(ems_h0)
    )
  )
}

df_sym_to_html <- function(sym) {
  k(df_sym_to_latex_raw(sym))
}

ems_summary_card <- function(
    effects,
    error_info,
    df_verification = NULL
) {

  if (is.null(effects) || length(effects) == 0) {
    return(NULL)
  }

  effect_names <- names(effects)

  error_effects <- effect_names[
    vapply(
      effects,
      function(e) isTRUE(e$is_error),
      logical(1)
    )
  ]

  test_effects <- setdiff(
    effect_names,
    error_effects
  )

  pure_effects <- intersect(
    test_effects,
    names(error_info$pure %||% list())
  )

  quasi_effects <- intersect(
    test_effects,
    names(error_info$quasi %||% list())
  )

  df_text <- if (is.null(df_verification)) {

    "Non disponible"

  } else if (isTRUE(df_verification$verified)) {

    paste0(
      "réussie — somme des df = ",
      df_verification$df_sum,
      " et N - 1 = ",
      df_verification$df_total
    )

  } else {

    paste0(
      "à vérifier — somme des df = ",
      df_verification$df_sum,
      "; N - 1 = ",
      df_verification$df_total
    )
  }

  div(
    class = "alert alert-success mb-3",

    tags$div(
      class = "row g-3",

      tags$div(
        class = "col-md-3",
        tags$b("Effets du modèle"),
        tags$div(
          style = "font-size:1.35rem;",
          length(effect_names)
        )
      ),

      tags$div(
        class = "col-md-3",
        tags$b("Effets testables directement"),
        tags$div(
          style = "font-size:1.35rem;",
          length(pure_effects)
        )
      ),

      tags$div(
        class = "col-md-3",
        tags$b("Tests nécessitant un quasi-F"),
        tags$div(
          style = "font-size:1.35rem;",
          length(quasi_effects)
        )
      ),

      tags$div(
        class = "col-md-3",
        tags$b("Termes d’erreur du plan"),
        tags$div(
          style = "font-size:1.35rem;",
          length(error_effects)
        )
      )
    ),

    tags$hr(class = "my-2"),

    tags$div(
      tags$b("Vérification des degrés de liberté : "),
      df_text
    )
  )
}



interpret_ems_effect <- function(
    effect_name,
    factors,
    effects,
    ems_table,
    error_info,
    df_list = NULL,
    effect_names = NULL
) {

  if (is.null(effect_names)) {
    effect_names <- names(ems_table)
  }

  # ----------------------------------------------------------
  # Vérifications
  # ----------------------------------------------------------
  if (
    is.null(effect_name) ||
    !nzchar(effect_name) ||
    is.null(effects[[effect_name]]) ||
    is.null(ems_table[[effect_name]])
  ) {
    return(
      div(
        class = "alert alert-info",
        "Sélectionnez un effet pour afficher son interprétation."
      )
    )
  }

  effect_info <- effects[[effect_name]]
  ems_entry   <- ems_table[[effect_name]]

  is_error_effect <- isTRUE(effect_info$is_error)

  # ----------------------------------------------------------
  # Identifier le type de test
  # ----------------------------------------------------------
  pure_error <- error_info$pure[[effect_name]]
  quasi_info <- error_info$quasi[[effect_name]]

  test_type <- if (is_error_effect) {
    "error"
  } else if (!is.null(pure_error)) {
    "pure"
  } else if (!is.null(quasi_info)) {
    "quasi"
  } else {
    "none"
  }

  # ----------------------------------------------------------
  # Composantes de variance communes
  # Seulement lorsqu'un terme d'erreur pur existe
  # ----------------------------------------------------------
  common_variance_terms <- character(0)

  if (
    identical(test_type, "pure") &&
    !is.null(ems_table[[pure_error]])
  ) {

    effect_variance_terms <- names(ems_entry)[
      vapply(
        ems_entry,
        function(x) !isTRUE(x$is_fixed),
        logical(1)
      )
    ]

    error_entry <- ems_table[[pure_error]]

    error_variance_terms <- names(error_entry)[
      vapply(
        error_entry,
        function(x) !isTRUE(x$is_fixed),
        logical(1)
      )
    ]

    common_variance_terms <- intersect(
      effect_variance_terms,
      error_variance_terms
    )

    common_variance_terms <- setdiff(
      common_variance_terms,
      effect_name
    )
  }

  # ----------------------------------------------------------
  # Nature de l'effet
  # ----------------------------------------------------------
  effect_factors <- effect_info$factors

  non_subject_factors <- setdiff(
    effect_factors,
    "s"
  )

  fixed_members <- non_subject_factors[
    vapply(
      non_subject_factors,
      function(f) {
        !is.null(factors[[f]]) &&
          isTRUE(factors[[f]]$fixed)
      },
      logical(1)
    )
  ]

  random_members <- non_subject_factors[
    vapply(
      non_subject_factors,
      function(f) {
        !is.null(factors[[f]]) &&
          isTRUE(factors[[f]]$random)
      },
      logical(1)
    )
  ]

  effect_description <- if (is_error_effect) {

    paste0(
      effect_name,
      " est un terme aléatoire associé aux sujets. ",
      "Il représente une source de variation dans le plan."
    )

  } else if (
    length(fixed_members) > 0 &&
    length(random_members) == 0
  ) {

    if (length(fixed_members) == 1) {

      paste0(
        effect_name,
        " est un effet fixe. Il représente les différences ",
        "systématiques entre les niveaux du facteur ",
        fixed_members,
        "."
      )

    } else {

      paste0(
        effect_name,
        " est une interaction entre les facteurs fixes ",
        paste(fixed_members, collapse = ", "),
        ". Elle indique si l'effet d'un facteur dépend du niveau ",
        "des autres facteurs."
      )
    }

  } else if (length(random_members) > 0) {

    paste0(
      effect_name,
      " contient au moins un facteur aléatoire (",
      paste(random_members, collapse = ", "),
      "). Son EMS s'interprète principalement à partir des ",
      "composantes de variance."
    )

  } else {

    paste0(
      effect_name,
      " est un effet du modèle expérimental."
    )
  }

  # ----------------------------------------------------------
  # Formule EMS
  # ----------------------------------------------------------
  ems_formula <- paste0(
    "\\operatorname{EMS}(",
    effect_name,
    ") = ",
    paste(
      vapply(
        ems_entry,
        ems_to_latex_raw,
        character(1)
      ),
      collapse = " + "
    )
  )

  # ----------------------------------------------------------
  # Degrés de liberté
  # ----------------------------------------------------------
  df_block <- if (
    !is.null(df_list) &&
    !is.null(df_list[[effect_name]])
  ) {

    div(
      class = "alert alert-light border mb-0",
      style = "padding:0.65rem 0.85rem;",

      tags$b("Degrés de liberté : "),

      df_sym_to_html(
        df_list[[effect_name]]$symbolic
      ),

      tags$span(
        class = "ms-2 text-muted",
        paste0(
          "= ",
          df_list[[effect_name]]$numeric
        )
      )
    )

  } else {

    NULL
  }

  # ----------------------------------------------------------
  # Interprétation du test
  # ----------------------------------------------------------
  test_block <- switch(

    test_type,

    error = div(
      class = "alert alert-secondary mb-0",

      tags$h5(
        class = "alert-heading",
        "Rôle dans l'analyse"
      ),

      p(
        class = "mb-0",
        "Ce terme n'est pas testé comme un effet fixe. Il représente ",
        "une source de variabilité et peut constituer le dénominateur ",
        "d'un ou de plusieurs rapports F."
      )
    ),

    pure = div(
      class = "alert alert-success mb-0",

      tags$h5(
        class = "alert-heading",
        "Terme d'erreur approprié"
      ),

      div(
        style = "text-align:center; margin:0.4rem 0 0.7rem 0;",

        k(
          paste0(
            "F = \\frac{MS_{",
            effect_name,
            "}}{MS_{",
            pure_error,
            "}}"
          ),
          display = TRUE
        )
      ),

      p(
        class = "mb-0",

        "L'EMS de ",

        tags$b(pure_error),

        " possède les mêmes composantes de variance que celui de ",

        tags$b(effect_name),

        ", sans contenir la contribution propre à l'effet testé."
      )
    ),

    quasi = div(
      class = "alert alert-warning mb-0",

      tags$h5(
        class = "alert-heading",
        "Un quasi-F est nécessaire"
      ),

      p(
        "Aucun carré moyen unique ne possède exactement les ",
        "composantes de variance requises pour tester ",
        tags$b(effect_name),
        "."
      ),

      p(
        class = "mb-0",
        "Le dénominateur doit donc être construit à partir d'une ",
        "combinaison de plusieurs carrés moyens. Les détails sont ",
        "présentés dans l'onglet Quasi-F."
      )
    ),

    none = div(
      class = "alert alert-info mb-0",

      tags$h5(
        class = "alert-heading",
        "Test non identifié"
      ),

      p(
        class = "mb-0",
        "Aucun terme d'erreur pur ni quasi-F n'a été associé ",
        "automatiquement à cet effet."
      )
    )
  )

  # ----------------------------------------------------------
  # Décomposition des composantes
  # ----------------------------------------------------------
  component_rows <- lapply(
    names(ems_entry),
    function(term_name) {

      component   <- ems_entry[[term_name]]
      coefficient <- component$coef_str %||% "1"

      component_type <- if (isTRUE(component$is_fixed)) {
        "Contribution propre à l'effet"
      } else {
        "Composante de variance"
      }

      component_explanation <- if (isTRUE(component$is_fixed)) {

        paste0(
          "Contribution propre de l'effet ",
          term_name,
          ". Sous H\u2080, elle est nulle."
        )

      } else if (identical(coefficient, "1")) {

        paste0(
          "La composante ",
          term_name,
          " entre directement dans l'EMS avec un coefficient de 1."
        )

      } else {

        factor_codes <- sub(
          "^n_",
          "",
          strsplit(
            coefficient,
            "\u00b7",
            fixed = TRUE
          )[[1]]
        )

        paste0(
          "Cette composante est multipliée par ",
          paste0("n", factor_codes, collapse = " \u00d7 "),
          ", selon la structure de réplication du plan."
        )
      }

      is_common <- term_name %in% common_variance_terms

      common_style <- if (is_common) {
        paste(
          "font-weight:800;",
          "color:#084298;",
          "background-color:#cfe2ff;"
        )
      } else {
        NULL
      }

      tags$tr(
        style = common_style,

        tags$td(
          tags$code(term_name)
        ),

        tags$td(
          if (is_common) {
            tags$span(
              style = paste(
                "display:inline-block;",
                "font-weight:800;",
                "color:#084298;",
                "background-color:#cfe2ff;",
                "border:1.5px solid #6ea8fe;",
                "border-radius:0.3rem;",
                "padding:0.10rem 0.32rem;"
              ),
              k(ems_to_latex_raw(component))
            )
          } else {
            k(ems_to_latex_raw(component))
          }
        ),

        tags$td(component_type),
        tags$td(component_explanation)
      )
    }
  )

  components_table <- tags$table(
    class = "table table-sm table-bordered table-striped mb-0",

    tags$thead(
      tags$tr(
        tags$th("Terme"),
        tags$th("Contribution"),
        tags$th("Nature"),
        tags$th("Interprétation")
      )
    ),

    tags$tbody(component_rows)
  )

  # ----------------------------------------------------------
  # Carte finale
  # ----------------------------------------------------------
  card(
    fill = FALSE,
    style = "height:auto; overflow:visible;",

    card_header(
      div(
        style = paste(
          "display:flex;",
          "align-items:center;",
          "justify-content:space-between;",
          "gap:1rem;",
          "width:100%;"
        ),

        tags$span(
          tags$b(
            paste(
              "Interprétation de l'effet",
              effect_name
            )
          )
        ),

        div(
          style = "width:145px; flex:0 0 145px;",

          selectInput(
            inputId = "ems_effect_selected",
            label = NULL,
            choices = effect_names,
            selected = effect_name,
            width = "145px"
          )
        )
      )
    ),

    card_body(
      fill = FALSE,
      style = paste(
        "height:auto;",
        "overflow:visible;",
        "padding-bottom:1rem;"
      ),

      layout_columns(
        col_widths = c(6, 6),

        div(
          tags$h5("Espérance du carré moyen"),

          div(
            style = paste(
              "text-align:center;",
              "font-size:1.08rem;",
              "margin:0.65rem 0;"
            ),

            k(
              ems_formula,
              display = TRUE
            )
          ),

          p(effect_description),

          df_block
        ),

        div(test_block)
      ),

      hr(),

      tags$h5("Décomposition de l'EMS"),

      components_table
    )
  )
}


make_table <- function(header, rows) {
  tags$table(
    class = "table table-striped table-hover table-bordered",
    style = "width:100%",
    tags$thead(tags$tr(lapply(header, tags$th))),
    tags$tbody(rows)
  )
}

center_mixed_random_component <- function(
    values,
    grid,
    fixed_dimensions
) {

  original_names <- names(values)
  values <- as.numeric(values)

  if (
    length(values) == 0L ||
    !is.data.frame(grid) ||
    nrow(grid) != length(values)
  ) {
    stop(
      "Dimensions incompatibles dans center_mixed_random_component().",
      call. = FALSE
    )
  }

  fixed_dimensions <- intersect(
    fixed_dimensions,
    names(grid)
  )

  if (length(fixed_dimensions) == 0L) {
    names(values) <- original_names
    return(values)
  }

  work <- grid
  work$.component_value <- values

  for (fixed_factor in fixed_dimensions) {

    conditioning_variables <- setdiff(
      names(grid),
      fixed_factor
    )

    if (length(conditioning_variables) == 0L) {

      conditional_mean <- rep(
        mean(work$.component_value),
        nrow(work)
      )

    } else {

      group_key <- interaction(
        work[conditioning_variables],
        drop = TRUE,
        lex.order = TRUE
      )

      conditional_mean <- ave(
        work$.component_value,
        group_key,
        FUN = mean
      )
    }

    work$.component_value <-
      work$.component_value -
      conditional_mean
  }

  centered_values <- work$.component_value
  names(centered_values) <- original_names

  centered_values
}

# ===========================================================================
# MODULE 7 — Simulation de données et calcul ANOVA numérique
# ===========================================================================

# ---------------------------------------------------------------------------
# Générer un jeu de données selon le plan
# ---------------------------------------------------------------------------
sim_data <- function(
    factors,
    effects,
    n_levels,
    effect_sizes = NULL,
    random_variances = NULL,
    sigma2_error = 1.0,
    seed = NULL,
    keep_components = FALSE
) {

  # ==========================================================
  # Vérifications générales
  # ==========================================================
  fnames <- names(factors)

  if (!"s" %in% fnames) {
    stop(
      "Le plan doit contenir le facteur sujet 's'.",
      call. = FALSE
    )
  }

  missing_levels <- fnames[
    !vapply(
      fnames,
      function(f) {
        !is.null(n_levels[[f]]) &&
          is.numeric(n_levels[[f]]) &&
          length(n_levels[[f]]) == 1L &&
          is.finite(n_levels[[f]]) &&
          n_levels[[f]] >= 1
      },
      logical(1)
    )
  ]

  if (length(missing_levels) > 0) {
    stop(
      paste(
        "Nombre de niveaux manquant ou invalide pour :",
        paste(missing_levels, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  sigma2_error <- as.numeric(sigma2_error)

  if (
    length(sigma2_error) != 1L ||
    !is.finite(sigma2_error) ||
    sigma2_error < 0
  ) {
    stop(
      "La variance résiduelle doit être un nombre positif ou nul.",
      call. = FALSE
    )
  }

  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }

  # ==========================================================
  # Grille complète du plan
  # ==========================================================
  grid_args <- lapply(
    fnames,
    function(f) {
      seq_len(n_levels[[f]])
    }
  )

  names(grid_args) <- fnames

  dat <- do.call(
    expand.grid,
    c(
      grid_args,
      KEEP.OUT.ATTRS = FALSE
    )
  )

  for (f in fnames) {
    dat[[f]] <- factor(dat[[f]])
  }

  # ==========================================================
  # Identifier le terme résiduel
  # ==========================================================
  error_terms <- names(effects)[
    vapply(
      effects,
      function(effect_info) {
        isTRUE(effect_info$is_error)
      },
      logical(1)
    )
  ]

  if (length(error_terms) == 0) {
    stop(
      "Aucun terme d'erreur n'a été identifié dans le plan.",
      call. = FALSE
    )
  }

  residual_term <- tail(
    error_terms,
    1
  )

  # ==========================================================
  # Identifier toutes les composantes aléatoires structurelles
  # ==========================================================
  random_terms <- names(effects)[
    vapply(
      effects,
      function(effect_info) {

        effect_factors <- setdiff(
          effect_info$factors,
          "s"
        )

        contains_random_factor <- any(
          vapply(
            effect_factors,
            function(factor_name) {
              !is.null(factors[[factor_name]]) &&
                isTRUE(factors[[factor_name]]$random)
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

  structural_random_terms <- setdiff(
    random_terms,
    residual_term
  )

  # ==========================================================
  # Générer chaque composante aléatoire séparément
  # ==========================================================
  random_components <- list()

  for (effect_name in structural_random_terms) {

    effect_info <- effects[[effect_name]]

    variance_value <- if (
      !is.null(random_variances) &&
      effect_name %in% names(random_variances)
    ) {
      as.numeric(random_variances[[effect_name]])
    } else {
      sigma2_error
    }

    if (
      length(variance_value) != 1L ||
      !is.finite(variance_value) ||
      variance_value < 0
    ) {
      stop(
        paste0(
          "La variance de ",
          effect_name,
          " doit être un nombre positif ou nul."
        ),
        call. = FALSE
      )
    }

    effect_columns <- unique(
      c(
        effect_info$factors,
        effect_info$nested_in
      )
    )

    effect_columns <- intersect(
      effect_columns,
      fnames
    )

    effect_key <- if (length(effect_columns) == 0) {

      rep(
        "1",
        nrow(dat)
      )

    } else {

      do.call(
        paste,
        c(
          lapply(
            effect_columns,
            function(f) {
              dat[[f]]
            }
          ),
          sep = "_"
        )
      )
    }

    unique_keys <- unique(
      effect_key
    )

    if (variance_value == 0) {

      component_values <- setNames(
        numeric(length(unique_keys)),
        unique_keys
      )

    } else {

      component_values <- stats::rnorm(
        length(unique_keys),
        mean = 0,
        sd = sqrt(variance_value)
      )

      names(component_values) <- unique_keys
    }

    # Centrer uniquement les composantes aléatoires structurelles.
# Les termes d'erreur associés aux sujets ne sont pas centrés.
if (!isTRUE(effect_info$is_error)) {

  fixed_dimensions <- effect_info$factors[
    vapply(
      effect_info$factors,
      function(factor_name) {

        factor_name != "s" &&
          !is.null(factors[[factor_name]]) &&
          isTRUE(factors[[factor_name]]$fixed)
      },
      logical(1)
    )
  ]

  random_dimensions <- effect_info$factors[
    vapply(
      effect_info$factors,
      function(factor_name) {

        factor_name != "s" &&
          !is.null(factors[[factor_name]]) &&
          isTRUE(factors[[factor_name]]$random)
      },
      logical(1)
    )
  ]

  if (
    length(fixed_dimensions) > 0L &&
    length(random_dimensions) > 0L
  ) {

    unique_indices <- match(
      unique_keys,
      effect_key
    )

    component_grid <- dat[
      unique_indices,
      effect_columns,
      drop = FALSE
    ]

    component_values <- center_mixed_random_component(
      values = component_values,
      grid = component_grid,
      fixed_dimensions = fixed_dimensions
    )

    names(component_values) <- unique_keys
  }
}
    
    random_components[[effect_name]] <- unname(
      component_values[effect_key]
    )
  }

  # ==========================================================
  # Contributions des effets fixes
  # ==========================================================
  fixed_component <- numeric(
    nrow(dat)
  )

  for (effect_name in names(effects)) {

    effect_info <- effects[[effect_name]]

    if (isTRUE(effect_info$is_error)) {
      next
    }

    effect_factors <- effect_info$factors

    fixed_factors <- effect_factors[
      vapply(
        effect_factors,
        function(f) {
          !is.null(factors[[f]]) &&
            isTRUE(factors[[f]]$fixed)
        },
        logical(1)
      )
    ]

    # Un effet contenant un facteur aléatoire ne reçoit pas
    # de contribution systématique réglable.
    contains_random_factor <- any(
      vapply(
        setdiff(effect_factors, "s"),
        function(f) {
          !is.null(factors[[f]]) &&
            isTRUE(factors[[f]]$random)
        },
        logical(1)
      )
    )

    if (
      length(fixed_factors) == 0 ||
      contains_random_factor
    ) {
      next
    }

    effect_size <- if (
      !is.null(effect_sizes) &&
      effect_name %in% names(effect_sizes)
    ) {
      as.numeric(effect_sizes[[effect_name]])
    } else {
      0
    }

    if (
      length(effect_size) != 1L ||
      !is.finite(effect_size) ||
      effect_size == 0
    ) {
      next
    }

    # --------------------------------------------------------
    # Effet principal fixe
    # --------------------------------------------------------
    if (length(fixed_factors) == 1) {

      factor_name <- fixed_factors[1]
      n_factor_levels <- n_levels[[factor_name]]

      level_effects <- seq(
        -effect_size * (n_factor_levels - 1) / 2,
         effect_size * (n_factor_levels - 1) / 2,
        length.out = n_factor_levels
      )

      fixed_component <- fixed_component +
        level_effects[
          as.integer(
            dat[[factor_name]]
          )
        ]

    } else {

  # ------------------------------------------------------
  # Interaction entre facteurs fixes
  # ------------------------------------------------------
  interaction_sign <- vapply(
    seq_len(nrow(dat)),
    function(i) {

      prod(
        vapply(
          fixed_factors,
          function(factor_name) {

            level_number <- as.integer(
              dat[[factor_name]][i]
            )

            if (level_number %% 2L == 0L) {
              1
            } else {
              -1
            }
          },
          numeric(1)
        )
      )
    },
    numeric(1)
  )

  interaction_component <-
    effect_size * interaction_sign

  interaction_component <-
    center_mixed_random_component(
      values = interaction_component,
      grid = dat[, fixed_factors, drop = FALSE],
      fixed_dimensions = fixed_factors
    )

  fixed_component <-
    fixed_component +
    interaction_component
}
  }

  # ==========================================================
  # Générer la composante résiduelle
  # ==========================================================
  residual_info <- effects[[residual_term]]

  residual_columns <- unique(
    c(
      residual_info$factors,
      residual_info$nested_in
    )
  )

  residual_columns <- intersect(
    residual_columns,
    fnames
  )

  residual_key <- if (length(residual_columns) == 0) {

    rep(
      "1",
      nrow(dat)
    )

  } else {

    do.call(
      paste,
      c(
        lapply(
          residual_columns,
          function(f) {
            dat[[f]]
          }
        ),
        sep = "_"
      )
    )
  }

  unique_residual_keys <- unique(
    residual_key
  )

  if (sigma2_error == 0) {

    residual_values <- setNames(
      numeric(length(unique_residual_keys)),
      unique_residual_keys
    )

  } else {

    residual_values <- stats::rnorm(
      length(unique_residual_keys),
      mean = 0,
      sd = sqrt(sigma2_error)
    )

    names(residual_values) <- unique_residual_keys
  }

  residual_component <- unname(
    residual_values[residual_key]
  )

  # ==========================================================
  # Assemblage de la variable dépendante
  # ==========================================================
  random_component <- if (
    length(random_components) == 0
  ) {

    numeric(
      nrow(dat)
    )

  } else {

    Reduce(
      `+`,
      random_components
    )
  }

  dat$Y <- fixed_component +
    random_component +
    residual_component
  
    # ==========================================================
  # Conserver les composantes pour les diagnostics
  # ==========================================================
  if (isTRUE(keep_components)) {

    component_data <- if (length(random_components) > 0L) {

      as.data.frame(
        random_components,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

    } else {

      data.frame(
        row.names = seq_len(nrow(dat))
      )
    }

    component_data[[residual_term]] <- residual_component

    attr(dat, "components") <- component_data
  }

  # ==========================================================
  # Métadonnées utiles
  # ==========================================================
  attr(dat, "seed") <- seed
  attr(dat, "residual_term") <- residual_term
  attr(dat, "effect_sizes") <- effect_sizes
  attr(dat, "random_variances") <- random_variances
  attr(dat, "residual_variance") <- sigma2_error

  dat
}

# ---------------------------------------------------------------------------
# Calcul ANOVA numérique à partir des données simulées
# ---------------------------------------------------------------------------
compute_anova <- function(dat, factors, effects, df_list) {

  fnames <- names(factors)

  if (!"s" %in% fnames) {
    return(list(
      error = "Le plan doit contenir le facteur sujet s."
    ))
  }

  if (!"s" %in% names(dat)) {
    return(list(
      error = "Les données ne contiennent pas la variable sujet s."
    ))
  }

  # Identifier les facteurs between et within
  s_parents <- factors[["s"]]$nested_in %||% character(0)

  all_non_s <- fnames[fnames != "s"]

  between_f <- s_parents[s_parents %in% all_non_s]
  within_f  <- all_non_s[!all_non_s %in% between_f]

  # Tous les facteurs expérimentaux participent
  # à la décomposition ANOVA
  design_f <- all_non_s

  if (length(design_f) == 0) {
    return(list(
      error = "Aucun facteur expérimental à tester."
    ))
  }

  # Créer un identifiant sujet unique
  if (length(between_f) > 0) {

    dat$subj_id <- do.call(
      paste,
      c(
        lapply(between_f, function(f) dat[[f]]),
        list(dat[["s"]]),
        sep = "_"
      )
    )

  } else {

    dat$subj_id <- dat[["s"]]
  }

  dat$subj_id <- factor(dat$subj_id)

  # Construire la partie expérimentale de la formule
  design_part <- paste(design_f, collapse = " * ")

  if (length(within_f) > 0) {

    within_part <- paste(within_f, collapse = " * ")

    frm_str <- paste0(
      "Y ~ ",
      design_part,
      " + Error(subj_id / (",
      within_part,
      "))"
    )

  } else {

    frm_str <- paste0(
      "Y ~ ",
      design_part,
      " + Error(subj_id)"
    )
  }

  frm <- stats::as.formula(frm_str)

  tryCatch({

    fit  <- stats::aov(frm, data = dat)
    summ <- summary(fit)

    list(
      fit = fit,
      summary = summ,
      dat = dat,
      formula = frm_str
    )

  }, error = function(e) {

    list(error = e$message)
  })
}

# ---------------------------------------------------------------------------
# Formater le tableau ANOVA avec MS théoriques et numériques
# ---------------------------------------------------------------------------
format_anova_table <- function(anova_result, ems_table, error_info,
                               df_list, n_levels, factors,
                               quasi_numeric  = NULL,
                               quasi_method   = "ct",
                               eps_by_factor  = NULL,
                               spher_method   = "none",
                               within_f_ext   = NULL) {
  if (!is.null(anova_result$error))
    return(tags$p(class="text-danger",
                  paste("Erreur ANOVA :", anova_result$error)))
  summ <- anova_result$summary
  if (is.null(summ) || length(summ) == 0)
    return(tags$p(class="text-muted", "Aucun résultat disponible."))

  fnames    <- names(factors)
  s_nest    <- factors[["s"]]$nested_in
  between_f <- s_nest[s_nest %in% fnames]
  within_f  <- fnames[!fnames %in% c("s", between_f)]

  # ── Dictionnaire strate R → étiquette EMS ──────────────────────────────
  build_stratum_map <- function(ems_table, fnames, within_f) {
    err_names <- names(ems_table)[sapply(names(ems_table), function(e)
      grepl("s", e))]
    map <- list()
    for (en in err_names) {
      non_s_in_term <- intersect(
        fnames[fnames != "s"],
        unlist(strsplit(gsub("[^A-Za-z]", "", en), ""))
      )
      non_s_in_term <- non_s_in_term[non_s_in_term != "s"]
      ws_like <- intersect(non_s_in_term, within_f)
      stratum_suffix <- if (length(ws_like) == 0) {
        "subj_id"
      } else {
        paste(c("subj_id", sort(ws_like)), collapse=":")
      }
      map[[stratum_suffix]] <- en
    }
    return(map)
  }
  stratum_map <- build_stratum_map(ems_table, fnames, within_f)

  # ── Première passe : collecter tous les SS/MS/df ────────────────────────
  all_ss <- list()
  all_ms <- list()
  all_df <- list()

  for (stratum_name in names(summ)) {
    strat <- summ[[stratum_name]]
    tbl   <- if (is.list(strat) && !is.data.frame(strat)) strat[[1]] else strat
    if (is.null(tbl) || !is.data.frame(tbl)) next
    stratum_key <- trimws(sub("^Error:\\s*", "", stratum_name))
    src <- trimws(rownames(tbl))
    for (i in seq_len(nrow(tbl))) {
      nm <- src[i]
      display_nm <- if (nm == "Residuals") {
        ems_label <- stratum_map[[stratum_key]]
        if (!is.null(ems_label)) ems_label else stratum_key
      } else nm
      all_ss[[display_nm]] <- tbl[i, "Sum Sq"]
      all_ms[[display_nm]] <- tbl[i, "Mean Sq"]
      all_df[[display_nm]] <- tbl[i, "Df"]
      # Indexer aussi sans ":" pour les interactions
      nm_ems <- gsub(":", "", nm)
      if (nm_ems != nm) {
        all_ms[[nm_ems]] <- tbl[i, "Mean Sq"]
        all_df[[nm_ems]] <- tbl[i, "Df"]
      }
    }
  }

  ss_total <- sum(unlist(all_ss))

  # ── Construction du tableau ─────────────────────────────────────────────
  tbl_rows <- list()

  for (stratum_name in names(summ)) {
    strat <- summ[[stratum_name]]
    tbl   <- if (is.list(strat) && !is.data.frame(strat)) strat[[1]] else strat
    if (is.null(tbl) || !is.data.frame(tbl)) next

    stratum_key <- trimws(sub("^Error:\\s*", "", stratum_name))
    src <- trimws(rownames(tbl))

    for (i in seq_len(nrow(tbl))) {
      nm  <- src[i]
      df  <- tbl[i, "Df"]
      ss  <- tbl[i, "Sum Sq"]
      ms  <- tbl[i, "Mean Sq"]
      Fv  <- if ("F value" %in% colnames(tbl)) tbl[i, "F value"] else NA
      pv  <- if ("Pr(>F)"  %in% colnames(tbl)) tbl[i, "Pr(>F)"]  else NA

      fmtF <- if (!is.na(Fv) && is.finite(Fv)) sprintf("%.3f", Fv) else "—"
      fmtp <- if (!is.na(pv) && is.finite(pv)) {
        if (pv < .001) "< .001" else sprintf("%.3f", pv)
      } else "—"

      row_cls <- if (!is.na(pv) && is.finite(pv) && pv < .05)
        "table-success" else ""

      # Étiquette de la ligne
      display_nm <- if (nm == "Residuals") {
        ems_label <- stratum_map[[stratum_key]]
        if (!is.null(ems_label)) ems_label else stratum_key
      } else nm

      is_error_row <- (nm == "Residuals")

      # ── Terme d'erreur associé ──────────────────────────────────────────
      err_label <- if (!is_error_row) {
        pure_err <- error_info$pure[[display_nm]]
        if (is.null(pure_err)) pure_err <- error_info$pure[[nm]]
        if (is.null(pure_err)) pure_err <- error_info$pure[[gsub(":", "", nm)]]
        if (!is.null(pure_err)) pure_err
        else if (!is.null(error_info$quasi[[display_nm]]) ||
                 !is.null(error_info$quasi[[nm]]) ||
                 !is.null(error_info$quasi[[gsub(":", "", nm)]])) "Quasi-F"
        else "—"
      } else "—"

      # ── Correction sphéricité ───────────────────────────────────────────
      if (!is.null(eps_by_factor) && !is_error_row &&
          !is.na(Fv) && is.finite(Fv)) {
        nm_parts <- unlist(strsplit(gsub(":", "", nm), ""))
        wf_match <- intersect(nm_parts, names(eps_by_factor))
        if (length(wf_match) > 0) {
          eps <- min(sapply(wf_match, function(wf) eps_by_factor[[wf]]))
          df_err_nm <- error_info$pure[[display_nm]]
          if (is.null(df_err_nm)) df_err_nm <- error_info$pure[[nm]]
          if (is.null(df_err_nm)) df_err_nm <- error_info$pure[[gsub(":", "", nm)]]
          df_err <- if (!is.null(df_err_nm)) all_df[[df_err_nm]] else NA
          if (!is.na(df_err)) {
            p_corr <- pf(Fv, df * eps, df_err * eps, lower.tail=FALSE)
            fmtp   <- if (p_corr < .001) "< .001"
                      else sprintf("%.3f", p_corr)
            fmtp   <- paste0(fmtp, "\u1d9c")
            row_cls <- if (p_corr < .05) "table-success" else ""
          }
        }
      }

      # ── Tailles d'effet ─────────────────────────────────────────────────
      eta2_str  <- "—"
      eta2p_str <- "—"
      if (!is_error_row && !is.na(Fv) && is.finite(Fv)) {
        eta2     <- ss / ss_total
        eta2_str <- sprintf("%.3f", eta2)
        pure_err <- error_info$pure[[display_nm]]
        if (is.null(pure_err)) pure_err <- error_info$pure[[nm]]
        if (is.null(pure_err)) pure_err <- error_info$pure[[gsub(":", "", nm)]]
        if (!is.null(pure_err) && !is.null(all_ss[[pure_err]])) {
          ss_err    <- all_ss[[pure_err]]
          eta2p_str <- sprintf("%.3f", ss / (ss + ss_err))
        }
      }

      # ── Quasi-F numérique si applicable ────────────────────────────────
      is_quasi <- !is.null(error_info$quasi[[display_nm]]) ||
                  !is.null(error_info$quasi[[nm]]) ||
                  !is.null(error_info$quasi[[gsub(":", "", nm)]])

      fmtF_final <- fmtF
      fmtp_final <- fmtp
      df_final   <- as.character(df)

      if (is_quasi && !is.null(quasi_numeric)) {
        eff_key <- if (!is.null(quasi_numeric[[display_nm]])) display_nm
                   else if (!is.null(quasi_numeric[[nm]])) nm
                   else gsub(":", "", nm)
        qn <- quasi_numeric[[eff_key]]
        if (!is.null(qn)) {
          res <- if (quasi_method == "ct" && !is.null(qn$ct) &&
                     !is.na(qn$ct$F))
            qn$ct
          else if (!is.null(qn$satt))
            qn$satt
          else NULL

          if (!is.null(res)) {
            fmtF_final <- if (!is.na(res$F))
              paste0(sprintf("%.3f", res$F), "\u2019")
            else "—"
            fmtp_final <- if (!is.na(res$p)) {
              if (res$p < .001) "< .001" else sprintf("%.3f", res$p)
            } else "—"
            df_final <- if (!is.na(res$df_quasi))
              paste0(df, " / ", sprintf("%.1f", res$df_quasi))
            else as.character(df)
            row_cls <- if (!is.na(res$p) && is.finite(res$p) && res$p < .05)
              "table-success" else ""
          }
        }
      }

# ── Tooltip EMS ────────────────────────────────────────────────────
      ems_tooltip <- if (!is_error_row) {
        ems_key <- if (!is.null(ems_table[[display_nm]])) display_nm
                   else if (!is.null(ems_table[[nm]])) nm
                   else gsub(":", "", nm)
        ems_e <- ems_table[[ems_key]]
        if (!is.null(ems_e) && length(ems_e) > 0) {
          parts <- sapply(names(ems_e), function(t) {
            e       <- ems_e[[t]]
            co      <- e$coef_str
            sym     <- e$symbol
            sym_txt <- sym
            co_clean <- gsub("n_([A-Za-z])", "n\\1", co)
            if (co_clean == "1") sym_txt
            else paste0(co_clean, "\u00b7", sym_txt)
            if (co == "1") sym_txt
            else paste0(co, "\u00b7", sym_txt)
          })
          paste0("EMS = ", paste(parts, collapse=" + "))
        } else NULL
      } else NULL

      td_source <- if (!is.null(ems_tooltip)) {
        tags$td(
          tags$span(
            display_nm,
            class        = "ems-tooltip",
            `data-tooltip` = ems_tooltip,
            style        = "cursor:help; text-decoration:underline dotted;"
          )
        )
      } else {
        tags$td(display_nm)
      }

      tbl_rows[[length(tbl_rows)+1]] <- tags$tr(
        class = row_cls,
        td_source,
        tags$td(err_label),
        tags$td(df_final),
        tags$td(sprintf("%.4f", ss)),
        tags$td(sprintf("%.4f", ms)),
        tags$td(fmtF_final),
        tags$td(fmtp_final),
        tags$td(eta2_str),
        tags$td(eta2p_str)
      )
    }

    # Séparateur entre strates
    tbl_rows[[length(tbl_rows)+1]] <- tags$tr(
      tags$td(colspan="9",
        style="border-top:2px solid #dee2e6; padding:0;")
    )
  }

  # ── Notes méthodologiques ─────────────────────────────────────
  note <- accordion(
    open = FALSE,

    accordion_panel(
      title = "Notes méthodologiques",

      div(
        class = "text-muted",
        style = "font-size:0.90em;",

        tags$ul(
          class = "mb-0",

          if (
            !is.null(quasi_numeric) &&
            length(quasi_numeric) > 0
          ) {
            tags$li(
              HTML(
                paste(
                  "<b>F′</b> désigne un quasi-F calculé par la méthode",
                  "de Cornfield–Tukey ou de Satterthwaite. Lorsque la",
                  "méthode de Satterthwaite est utilisée, les degrés de",
                  "liberté affichés prennent la forme <i>df / df′</i>."
                )
              )
            )
          },

          if (
            !is.null(eps_by_factor) &&
            spher_method != "none"
          ) {
            tags$li(
              HTML(
                paste0(
                  "<b>pᶜ</b> indique une valeur p corrigée pour la sphéricité (",
                  switch(
                    spher_method,
                    gg  = "Greenhouse-Geisser",
                    hf  = "Huynh-Feldt",
                    box = "Box (conservateur)"
                  ),
                  ")."
                )
              )
            )
          },

          tags$li(
            HTML(
              "<b>η²</b> représente la proportion de variance totale expliquée par un effet."
            )
          ),

          tags$li(
            HTML(
              paste(
                "<b>η²<sub>p</sub></b> représente la proportion de variance",
                "expliquée par un effet relativement à son terme d'erreur",
                "(eta-carré partiel)."
              )
            )
          )
        )
      )
    )
  )

  # ── Interface finale ──────────────────────────────────────────
  tagList(

  div(
    class = "alert alert-info mb-3",

    tags$b("Lecture : "),

    paste(
      "Les lignes surlignées correspondent aux effets statistiquement",
      "significatifs. Lorsqu'un quasi-F est requis ou qu'une correction",
      "de sphéricité est appliquée, les statistiques affichées sont",
      "calculées selon les options sélectionnées."
    )
  ),

  make_table(
    list(
      "Source",
      "Erreur",
      "df",
      "SS",
      "MS",
      "F",
      "p",
      HTML("&eta;&sup2;"),
      HTML("&eta;&sup2;<sub>p</sub>")
    ),
    tbl_rows
  ),

  note
)
}

draw_pure_bs_matrix <- function(
    factors,
    row_factor = NULL,
    col_factor = NULL,
    n_levels_symbolic = 2,
    n_subjects_symbolic = 3
) {

  # ----------------------------------------------------------
  # Identifier les facteurs inter-sujets
  # ----------------------------------------------------------
  fnames <- names(factors)

  if (!"s" %in% fnames) {
    return(
      tags$p(
        class = "text-danger",
        "Le facteur sujet s est absent du plan."
      )
    )
  }

  between_f <- factors[["s"]]$nested_in %||% character(0)
  between_f <- intersect(between_f, fnames)

  if (length(between_f) < 2) {
    return(
      tags$p(
        class = "text-muted",
        paste(
          "La représentation matricielle exige au moins",
          "deux facteurs inter-sujets."
        )
      )
    )
  }

  # ----------------------------------------------------------
  # Déterminer les facteurs en rangées et en colonnes
  # ----------------------------------------------------------
  if (
    is.null(row_factor) ||
    length(row_factor) == 0 ||
    !row_factor %in% between_f
  ) {
    row_factor <- between_f[1]
  }

  available_columns <- setdiff(between_f, row_factor)

  if (
    is.null(col_factor) ||
    length(col_factor) == 0 ||
    !col_factor %in% available_columns
  ) {
    # Par défaut, le dernier facteur inter-sujets
    # disponible est placé en colonnes.
    col_factor <- tail(available_columns, 1)
  }

  # Les autres facteurs inter-sujets définissent des blocs
  block_factors <- setdiff(
    between_f,
    c(row_factor, col_factor)
  )

  # ----------------------------------------------------------
  # Niveaux symboliques
  # ----------------------------------------------------------
  row_levels <- paste0(
    row_factor,
    seq_len(n_levels_symbolic)
  )

  col_levels <- paste0(
    col_factor,
    seq_len(n_levels_symbolic)
  )

  # ----------------------------------------------------------
  # Construire les combinaisons des facteurs de blocs
  # ----------------------------------------------------------
  block_grid <- if (length(block_factors) > 0) {

    block_args <- lapply(
      block_factors,
      function(f) seq_len(n_levels_symbolic)
    )

    names(block_args) <- block_factors

    do.call(
      expand.grid,
      c(
        block_args,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    )

  } else {

    data.frame(.block = 1)
  }

  # Numérotation continue des sujets
  subject_counter <- 0L

  # ----------------------------------------------------------
  # Construire un tableau pour chaque bloc
  # ----------------------------------------------------------
  block_tables <- lapply(
    seq_len(nrow(block_grid)),
    function(block_index) {

      # Étiquette du bloc, par exemple B1 ou B1, C2
      block_label <- if (length(block_factors) > 0) {

        paste(
          vapply(
            block_factors,
            function(f) {
              paste0(
                f,
                block_grid[[f]][block_index]
              )
            },
            character(1)
          ),
          collapse = ", "
        )

      } else {

        NULL
      }

      # ------------------------------------------------------
      # En-tête
      # ------------------------------------------------------
      header <- tags$tr(

        tags$th(
          class = "table-secondary text-center align-middle",
          style = paste(
            "min-width:90px;",
            "border:1px solid #aaa;"
          ),
          paste0(row_factor, " \\ ", col_factor)
        ),

        lapply(
          col_levels,
          function(level) {
            tags$th(
              class = "table-info text-center align-middle",
              style = paste(
                "min-width:115px;",
                "border:1px solid #aaa;"
              ),
              level
            )
          }
        )
      )

      # ------------------------------------------------------
      # Lignes du tableau
      # ------------------------------------------------------
      rows <- lapply(
        row_levels,
        function(row_level) {

          cells <- lapply(
            col_levels,
            function(col_level) {

              subject_ids <- seq.int(
                from = subject_counter + 1L,
                length.out = n_subjects_symbolic
              )

              subject_counter <<- subject_counter +
                n_subjects_symbolic

              tags$td(
                class = "text-center align-middle",
                style = paste(
                  "border:1px solid #aaa;",
                  "padding:9px;",
                  "min-width:115px;"
                ),

                tags$div(
                  class = "mb-1",
                  lapply(
                    subject_ids,
                    function(subject_id) {
                      tags$span(
                        class = "badge bg-primary me-1 mb-1",
                        paste0("s", subject_id)
                      )
                    }
                  )
                )
              )
            }
          )

          tags$tr(

            tags$th(
              class = "table-warning text-center align-middle",
              style = paste(
                "border:1px solid #aaa;",
                "min-width:90px;"
              ),
              row_level
            ),

            cells
          )
        }
      )

      # ------------------------------------------------------
      # Tableau complet du bloc
      # ------------------------------------------------------
      tags$div(
        class = "mb-4",

        if (!is.null(block_label)) {
          tags$h6(
            class = "mb-2",
            paste("Bloc :", block_label)
          )
        },

        tags$table(
          class = "table table-bordered table-sm",
          style = paste(
            "width:auto;",
            "border-collapse:collapse;",
            "table-layout:fixed;",
            "margin-bottom:0;"
          ),

          tags$thead(header),
          tags$tbody(rows)
        )
      )
    }
  )

  # ----------------------------------------------------------
  # Texte expliquant l'organisation du diagramme
  # ----------------------------------------------------------
  block_description <- if (length(block_factors) > 0) {
    paste0(
      ", tandis que ",
      paste(block_factors, collapse = ", "),
      if (length(block_factors) == 1) {
        " définit les blocs."
      } else {
        " définissent les blocs."
      }
    )
  } else {
    "."
  }

  # ----------------------------------------------------------
  # Sortie finale
  # ----------------------------------------------------------
  tags$div(

    tags$p(
      class = "text-muted fst-italic",
      paste0(
        "Représentation symbolique : ",
        n_levels_symbolic,
        " niveaux par facteur et ",
        n_subjects_symbolic,
        " sujets par cellule."
      )
    ),

    tags$p(
      class = "text-muted",
      tags$b("Lecture : "),
      row_factor,
      " est affiché en rangées, ",
      col_factor,
      " en colonnes",
      block_description
    ),

    tags$p(
      class = "text-muted",
      tags$b("Légende : "),
      tags$span(
        class = "badge bg-warning text-dark",
        "Facteur en rangées"
      ),
      " — ",
      tags$span(
        class = "badge bg-info text-dark",
        "Facteur en colonnes"
      ),
      " — ",
      tags$span(
        class = "badge bg-primary",
        "sx"
      ),
      " = sujet dans cette cellule"
    ),

    tags$div(
      style = "overflow-x:auto;",
      tagList(block_tables)
    )
  )
}

# ===========================================================================
# Représentation graphique du plan (style Kirk)
# ===========================================================================
draw_design <- function(
    factors,
    n_levels,
    column_factor = NULL,
    row_factor = NULL,
    col_factor = NULL
) {
  fnames  <- names(factors)
  s_nest  <- factors[["s"]]$nested_in

  between_f <- s_nest[s_nest %in% fnames]
  within_f  <- fnames[!fnames %in% c("s", between_f)]
  
# Cas purement inter-sujets avec au moins deux facteurs
if (
  length(within_f) == 0 &&
  length(between_f) >= 2
) {

  # Par défaut :
  # premier facteur en rangées
  # dernier facteur en colonnes
  if (
    is.null(row_factor) ||
    !row_factor %in% between_f
  ) {
    row_factor <- between_f[1]
  }

  if (
    is.null(col_factor) ||
    !col_factor %in% between_f ||
    identical(row_factor, col_factor)
  ) {
    col_factor <- tail(
      setdiff(between_f, row_factor),
      1
    )
  }

  return(
    draw_pure_bs_matrix(
      factors = factors,
      row_factor = row_factor,
      col_factor = col_factor
    )
  )
}
  
  # Plan entièrement inter-sujets à deux facteurs :
# matrice facteur en rangées × facteur en colonnes
if (
  length(within_f) == 0 &&
  length(between_f) == 2
) {
  return(
    draw_pure_bs_matrix(
      factors = factors,
      row_factor = row_factor,
      col_factor = col_factor
    )
  )
}

  # Le facteur choisi est placé en dernier :
  # il varie le plus rapidement dans les colonnes.
  if (
    !is.null(column_factor) &&
    column_factor %in% within_f
  ) {
    within_f <- c(
      setdiff(within_f, column_factor),
      column_factor
    )
  }

  # Niveaux symboliques fixes
  n_s_sym <- 3
  n_bs_sym <- 2   # niveaux BS
  n_ws_sym <- 2   # niveaux WS par facteur

  # Niveaux symboliques pour chaque facteur
  lev_b <- setNames(lapply(between_f, function(f) seq_len(n_bs_sym)), between_f)
  lev_w <- setNames(lapply(within_f,  function(f) seq_len(n_ws_sym)), within_f)

  # Grilles
  grid_b <- if(length(between_f)>0) {
    g <- do.call(expand.grid, rev(lev_b))
    g[, rev(names(g)), drop=FALSE]
  } else data.frame(dummy=1)

  grid_w <- if(length(within_f)>0) {
    g <- do.call(expand.grid, rev(lev_w))
    g[, rev(names(g)), drop=FALSE]
  } else data.frame(dummy=1)

  n_within_cols <- nrow(grid_w)

  # En-têtes WS — une ligne par facteur WS
  header_rows <- list()
  if(length(within_f) > 0) {
    for (fi in seq_along(within_f)) {
      f     <- within_f[fi]
      n_f   <- n_ws_sym
      # Combien de colonnes par niveau de ce facteur ?
      # = produit des niveaux des facteurs WS suivants
      if(fi < length(within_f)) {
        span_per_lev <- prod(sapply(within_f[(fi+1):length(within_f)],
                                    function(x) n_ws_sym))
      } else {
        span_per_lev <- 1
      }
      # Combien de fois répéter le bloc ?
      if(fi > 1) {
        n_repeats <- prod(sapply(within_f[1:(fi-1)],
                                  function(x) n_ws_sym))
      } else {
        n_repeats <- 1
      }

      cells <- lapply(seq_len(n_repeats), function(r) {
        lapply(seq_len(n_f), function(lv) {
          tags$th(
            colspan = as.character(span_per_lev),
            class   = "text-center table-info",
            style   = "border:1px solid #aaa;",
            paste0(f, lv)
          )
        })
      })

      prefix <- if (length(between_f) > 0) {
  tags$th(
    colspan = "1",
    style   = "border:0; background:transparent;"
  )
} else {
  NULL
}
      header_rows[[fi]] <- tags$tr(prefix, tagList(unlist(cells, recursive=FALSE)))
    }
  }

  # Lignes de données
  data_rows    <- list()
  subj_counter <- 0

  for (b_row in seq_len(nrow(grid_b))) {
    b_label <- if(length(between_f)>0)
      paste(sapply(between_f, function(f)
        paste0(f, grid_b[b_row, f])), collapse=", ")
    else ""

    for (s_idx in seq_len(n_s_sym)) {
      subj_counter <- subj_counter + 1
      s_label   <- paste0("s", subj_counter)
      row_cells <- list()

      # Cellule BS fusionnée sur n_s_sym lignes
      if(s_idx == 1 && length(between_f) > 0) {
        row_cells[[1]] <- tags$td(
          rowspan = as.character(n_s_sym),
          class   = "text-center align-middle table-warning",
          style   = "border:1px solid #aaa; font-weight:bold;",
          b_label
        )
      }

      # Colonnes WS
      for (w_col in seq_len(n_within_cols)) {
        row_cells[[length(row_cells)+1]] <- tags$td(
          class = "text-center",
          style = "border:1px solid #aaa; font-size:0.85em;",
          tags$span(class="badge bg-primary", s_label)
        )
      }

      data_rows[[length(data_rows)+1]] <- tags$tr(tagList(row_cells))
    }

    # Séparateur entre groupes BS
    if(b_row < nrow(grid_b)) {
    data_rows[[length(data_rows)+1]] <- tags$tr(
        tags$td(
          colspan = as.character(length(between_f) + n_within_cols),
          style   = "border-top:3px solid #555; padding:0;"
        )
      )
    }
  }

  # En-tête principale (noms des facteurs)
  top_labels <- c(
    if(length(between_f)>0) lapply(between_f, function(f)
      tags$th(class="table-secondary",
              style="border:1px solid #aaa; font-weight:bold;", f)),
    list(tags$th(class="table-secondary",
                 style="border:1px solid #aaa;", "Sujet")),
    if(length(within_f)==0) list(tags$th(
      class="table-secondary",
      style="border:1px solid #aaa;", "(pas de WS)"))
  )

  note <- if(length(within_f)>0 || length(between_f)>0)
    paste0("Représentation symbolique : ",
           n_bs_sym, " niveaux BS, ",
           n_ws_sym, " niveaux WS, ",
           n_s_sym, " sujets par groupe.")
  else "Plan sans facteurs between ni within."

  tags$div(
    tags$p(class="text-muted fst-italic", note),
    tags$p(class="text-muted",
      tags$b("Légende : "),
      tags$span(class="badge bg-warning text-dark", "Groupe BS"),
      " — ",
      tags$span(class="badge bg-info", style="color:#1a1a1a;", "Facteur WS"),
      " — ",
      tags$span(class="badge bg-primary", "sx"),
      " = sujet dans cette condition"
    ),
    tags$div(style="overflow-x:auto;",
      tags$table(
        class = "table table-bordered table-sm",
        style = "width:auto;",
        tags$thead(
          tagList(header_rows),
        ),
        tags$tbody(tagList(data_rows))
      )
    )
  )
}

prepare_stata_data <- function(dat, factors, wide_dat) {
  fnames    <- names(factors)
  s_nest    <- factors[["s"]]$nested_in
  between_f <- s_nest[s_nest %in% fnames]
  within_f  <- fnames[!fnames %in% c("s", between_f)]

  # Créer identifiant sujet unique (numérique)
  if (length(between_f) > 0) {
    dat$sujet <- as.integer(interaction(
      c(lapply(between_f, function(f) dat[[f]]),
        list(dat[["s"]])), drop=TRUE))
  } else {
    dat$sujet <- as.integer(dat$s)
  }

  # Construire le format long avec tous les facteurs en entier
  long_dat <- data.frame(
    sujet = dat$sujet,
    Y     = dat$Y
  )

  # Tous les facteurs non-s convertis en integer
  for (f in fnames[fnames != "s"]) {
    long_dat[[f]] <- as.integer(dat[[f]])
  }

  # Ajouter étiquettes de valeur via haven::labelled pour Stata
  # (optionnel mais propre)
  long_dat <- long_dat[, c("sujet",
                            fnames[fnames != "s"],
                            "Y")]

  return(long_dat)
}

#### sim_export ####

# ===========================================================================
# MODULE — Export des données simulées et syntaxe SPSS/Stata
# ===========================================================================
library(haven)
library(writexl)
library(plotly)

# ---------------------------------------------------------------------------
# Convertir données long -> large (format analyse)
# ---------------------------------------------------------------------------
sim_to_wide <- function(dat, factors) {
  fnames    <- names(factors)
  s_nest    <- factors[["s"]]$nested_in
  between_f <- s_nest[s_nest %in% fnames]
  within_f  <- fnames[!fnames %in% c("s", between_f)]

  if(length(within_f) == 0) {
    # Plan purement BS — format déjà correct
    dat$sujet <- as.integer(dat$s)
    cols_keep <- c("sujet", between_f, "Y")
    return(dat[, cols_keep])
  }

  # Créer ID sujet unique
  if(length(between_f) > 0) {
dat$sujet <- as.integer(interaction(
      c(lapply(between_f, function(f) dat[[f]]),
        list(dat$s)), drop=TRUE))
  } else {
    dat$sujet <- as.integer(dat$s)
  }

  # Colonnes WS : une colonne par combinaison de niveaux within
  grid_w <- do.call(expand.grid,
    lapply(within_f, function(f) levels(dat[[f]])))
  names(grid_w) <- within_f

  # Nommer les colonnes WS
  col_names <- apply(grid_w, 1, function(row)
    paste(paste0(within_f, row), collapse="_"))

  # Construire le format large
  wide_list <- list()
  sujets    <- unique(dat$sujet)

  for(sid in sujets) {
    d_s   <- dat[dat$sujet == sid, ]
    row_w <- list(sujet = sid)
    if(length(between_f) > 0)
      for(f in between_f)
        row_w[[f]] <- as.character(d_s[1, f])

    for(ci in seq_len(nrow(grid_w))) {
      mask <- rep(TRUE, nrow(d_s))
      for(f in within_f)
        mask <- mask & (as.character(d_s[[f]]) == grid_w[ci, f])
      row_w[[col_names[ci]]] <- if(any(mask)) d_s$Y[mask][1] else NA
    }
    wide_list[[length(wide_list)+1]] <- as.data.frame(row_w)
  }

  do.call(rbind, wide_list)
}

# ---------------------------------------------------------------------------
# Syntaxe SPSS
# ---------------------------------------------------------------------------
spss_syntax <- function(factors, wide_dat, design_str="donnees_simulees") {
  fnames    <- names(factors)
  s_nest    <- factors[["s"]]$nested_in
  between_f <- s_nest[s_nest %in% fnames]
  within_f  <- fnames[!fnames %in% c("s", between_f)]

  excl   <- c("sujet", between_f, "Y")
  y_cols <- names(wide_dat)[!names(wide_dat) %in% excl]

  lines <- c(
    "* ============================================================",
    "* Syntaxe SPSS — générée par ExpDesign",
    "* ============================================================",
    "",
    "* Lire les données (adapter le chemin).",
    paste0("GET FILE='", design_str, ".sav'."),
    ""
  )

  if(length(within_f) == 0) {
    lines <- c(lines,
      "* Analyse BS pur.",
      if(length(between_f)==1)
        paste0("ONEWAY Y BY ", between_f[1], " /STATISTICS DESCRIPTIVES.")
      else
        paste0("UNIANOVA Y BY ", paste(between_f, collapse=" "),
               " /METHOD=SSTYPE(3)",
               " /PRINT=DESCRIPTIVE ETASQ",
               " /CRITERIA=ALPHA(.05).")
    )
  } else {
    ws_vars <- paste(y_cols, collapse=" ")
    ws_factor_str <- paste(sapply(within_f, function(f) {
      nf <- sum(grepl(paste0("^", f, "[0-9]+$"), y_cols))
      if(nf == 0) nf <- length(y_cols)^(1/length(within_f))
      paste0(f, " ", round(nf))
    }), collapse="\n  ")

    lines <- c(lines,
      "* Analyse avec mesures répétées (GLM).",
      paste0("GLM ", ws_vars,
             if(length(between_f)>0)
               paste0(" BY ", paste(between_f, collapse=" "))
             else "",
             " /WSFACTOR=", ws_factor_str,
             " /METHOD=SSTYPE(3)",
             " /PRINT=DESCRIPTIVE ETASQ",
             " /WSDESIGN=", paste(within_f, collapse=" "),
             if(length(between_f)>0)
               paste0(" /DESIGN=", paste(between_f, collapse=" "))
             else "",
             ".")
    )
  }

  paste(lines, collapse="\n")
}

# ---------------------------------------------------------------------------
# Syntaxe Stata
# ---------------------------------------------------------------------------
stata_syntax <- function(factors, wide_dat, design_str="donnees_simulees") {
  fnames    <- names(factors)
  s_nest    <- factors[["s"]]$nested_in
  between_f <- s_nest[s_nest %in% fnames]
  within_f  <- fnames[!fnames %in% c("s", between_f)]

  lines <- c(
    "* ============================================================",
    "* Syntaxe Stata — générée par ExpDesign",
    "* Données en format long (une ligne par observation)",
    "* ============================================================",
    "",
    "* Installation requise (une seule fois) :",
    "* net install sg103",
    "",
    paste0('use "', design_str, '.dta", clear'),
    ""
  )

  if (length(within_f) == 0) {
    lines <- c(lines,
      "* Plan entièrement between-subjects :",
      paste0("anova Y ", paste(between_f, collapse="##"))
    )

  } else if (length(within_f) == 1) {
    ws      <- within_f[1]
    bet_str <- if (length(between_f) > 0)
      paste(between_f, collapse=" ")
    else NULL

    cmd <- paste0("wsanova Y ", ws, ", id(sujet)",
      if (!is.null(bet_str)) paste0(" between(", bet_str, ")") else "",
      " epsilon")

    lines <- c(lines,
      "* Plan avec un facteur within (wsanova) :",
      cmd
    )

  } else {
    ws_combos <- unlist(lapply(seq_along(within_f), function(k)
      apply(combn(within_f, k), 2, function(x)
        paste(sort(x), collapse="#"))), use.names=FALSE)

    strates <- if (length(between_f) > 0) {
      c(paste(between_f, collapse="#"),
        paste0("/ sujet|", paste(between_f, collapse="#")))
    } else {
      c("/ sujet")
    }

    for (combo in ws_combos) {
      err <- if (length(between_f) > 0)
        paste0(combo, "#sujet|", paste(between_f, collapse="#"))
      else
        paste0(combo, "#sujet")
      strates <- c(strates, paste0("/ ", combo), paste0("/ ", err))
    }

    ws_str <- paste(within_f, collapse=" ")

    lines <- c(lines,
      "* Plan avec plusieurs facteurs within (anova strate par strate) :",
      paste0("anova Y ", paste(strates, collapse=" "),
             ", repeated(", ws_str, ")")
    )
  }

  paste(lines, collapse="\n")
}

# ---------------------------------------------------------------------------
# Graphiques ggplot2 / plotly
# ---------------------------------------------------------------------------
plot_boxplot <- function(dat, factors) {
  fnames    <- names(factors)
  s_nest    <- factors[["s"]]$nested_in
  between_f <- s_nest[s_nest %in% fnames]
  within_f  <- fnames[!fnames %in% c("s", between_f)]

  if(length(between_f) == 0 && length(within_f) == 0)
    return(plotly::plot_ly() %>%
             plotly::add_annotations(text="Aucun facteur"))

  x_var   <- if(length(between_f)>0) between_f[1] else within_f[1]
  col_var <- if(length(within_f)>0) within_f[1]
             else if(length(between_f)>1) between_f[2]
             else NULL

  # Forcer les variables en facteur pour dodge
  dat[[x_var]] <- factor(dat[[x_var]])
  if(!is.null(col_var)) dat[[col_var]] <- factor(dat[[col_var]])

if(!is.null(col_var)) {
    # Créer variable d'interaction pour séparer les boîtes
    dat$x_combined <- interaction(dat[[x_var]], dat[[col_var]])

    p <- ggplot2::ggplot(dat,
      ggplot2::aes(x=x_combined, y=Y,
                   fill=.data[[col_var]])) +
      ggplot2::geom_boxplot(alpha=0.7, outlier.shape=NA) +
      ggplot2::geom_jitter(
        ggplot2::aes(color=.data[[col_var]]),
        width=0.15, alpha=0.5, size=1.5, show.legend=FALSE) +
      ggplot2::scale_x_discrete(
        labels=function(l) gsub("\\.", "\n", l)) +
      ggplot2::labs(
        title = "Distribution de Y par groupe",
        x     = paste0(x_var, " : ", col_var),
        y     = "Y",
        fill  = col_var
      )
  } else {
    p <- ggplot2::ggplot(dat,
      ggplot2::aes(x=.data[[x_var]], y=Y,
                   fill=.data[[x_var]])) +
      ggplot2::geom_boxplot(alpha=0.7, outlier.shape=NA) +
      ggplot2::geom_jitter(width=0.15, alpha=0.5, size=1.5,
                           show.legend=FALSE)
  }

  p <- p +
    ggplot2::scale_fill_brewer(palette="Set2") +
    ggplot2::scale_color_brewer(palette="Set2") +
    ggplot2::labs(
      title = "Distribution de Y par groupe",
      x     = x_var, y = "Y",
      fill  = if(!is.null(col_var)) col_var else x_var
    ) +
    ggplot2::theme_minimal(base_size=13) +
    ggplot2::theme(legend.position="bottom")

  plotly::ggplotly(p)
}

plot_interaction <- function(dat, factors) {
  fnames    <- names(factors)
  s_nest    <- factors[["s"]]$nested_in
  between_f <- s_nest[s_nest %in% fnames]
  within_f  <- fnames[!fnames %in% c("s", between_f)]

  fixed_f <- fnames[sapply(fnames, function(f)
    f != "s" && isTRUE(factors[[f]]$fixed))]

  if(length(fixed_f) < 1)
    return(plotly::plot_ly() %>%
             plotly::add_annotations(text="Aucun facteur fixe"))

  x_var   <- fixed_f[1]
  col_var <- if(length(fixed_f) > 1) fixed_f[2] else NULL

  # Calculer moyennes et ET sans .data
  if(!is.null(col_var)) {
    grp  <- interaction(dat[[x_var]], dat[[col_var]])
    moyennes <- tapply(dat$Y, grp, mean)
    se_vals  <- tapply(dat$Y, grp,
                       function(x) sd(x)/sqrt(length(x)))
    summ <- data.frame(
      x   = rep(levels(factor(dat[[x_var]])),
                each=nlevels(factor(dat[[col_var]]))),
      col = rep(levels(factor(dat[[col_var]])),
                times=nlevels(factor(dat[[x_var]]))),
      Y   = as.vector(t(matrix(moyennes,
              nrow=nlevels(factor(dat[[col_var]]))))),
      se  = as.vector(t(matrix(se_vals,
              nrow=nlevels(factor(dat[[col_var]])))))
    )
    names(summ)[1:2] <- c(x_var, col_var)
  } else {
    moyennes <- tapply(dat$Y, dat[[x_var]], mean)
    se_vals  <- tapply(dat$Y, dat[[x_var]],
                       function(x) sd(x)/sqrt(length(x)))
    summ <- data.frame(
      x  = names(moyennes),
      Y  = as.vector(moyennes),
      se = as.vector(se_vals)
    )
    names(summ)[1] <- x_var
  }

  if(!is.null(col_var)) {
    p <- ggplot2::ggplot(summ,
      ggplot2::aes(x=.data[[x_var]], y=Y,
                   color=.data[[col_var]],
                   group=.data[[col_var]])) +
      ggplot2::geom_line(linewidth=1) +
      ggplot2::geom_point(size=3) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin=Y-se, ymax=Y+se), width=0.15) +
      ggplot2::scale_color_brewer(palette="Set1")
  } else {
    p <- ggplot2::ggplot(summ,
      ggplot2::aes(x=.data[[x_var]], y=Y, group=1)) +
      ggplot2::geom_line(linewidth=1, color="#2196F3") +
      ggplot2::geom_point(size=3, color="#2196F3") +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin=Y-se, ymax=Y+se),
        width=0.15, color="#2196F3")
  }

  p <- p +
    ggplot2::labs(
      title = "Profil des moyennes (+/- 1 ET)",
      x     = x_var, y = "Moyenne de Y",
      color = if(!is.null(col_var)) col_var else ""
    ) +
    ggplot2::theme_minimal(base_size=13) +
    ggplot2::theme(legend.position="bottom")

  plotly::ggplotly(p)
}

plot_boxplot_flex <- function(
    dat,
    x_var,
    col_var = "",
    facet_var = ""
) {

  # ----------------------------------------------------------
  # Normaliser les arguments
  # ----------------------------------------------------------
  x_var <- if (
    is.null(x_var) ||
    length(x_var) == 0
  ) {
    ""
  } else {
    as.character(x_var[1])
  }

  col_var <- if (
    is.null(col_var) ||
    length(col_var) == 0
  ) {
    ""
  } else {
    as.character(col_var[1])
  }

  facet_var <- if (
    is.null(facet_var) ||
    length(facet_var) == 0
  ) {
    ""
  } else {
    as.character(facet_var[1])
  }

  # ----------------------------------------------------------
  # Fonction locale pour afficher un message dans Plotly
  # ----------------------------------------------------------
  empty_plot <- function(message) {

    plotly::plot_ly() |>
      plotly::layout(
        xaxis = list(
          visible = FALSE,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          visible = FALSE,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        annotations = list(
          list(
            text = message,
            x = 0.5,
            y = 0.5,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 15)
          )
        )
      )
  }

  # ----------------------------------------------------------
  # Vérifier les données
  # ----------------------------------------------------------
  if (
    is.null(dat) ||
    !is.data.frame(dat) ||
    nrow(dat) == 0
  ) {
    return(
      empty_plot("Aucune donnée disponible.")
    )
  }

  if (!"Y" %in% names(dat)) {
    return(
      empty_plot("La variable de réponse Y est absente.")
    )
  }

  if (!nzchar(x_var)) {
    return(
      empty_plot("Sélectionnez un facteur pour l’axe X.")
    )
  }

  requested_vars <- unique(
    c(
      x_var,
      if (nzchar(col_var)) col_var else NULL,
      if (nzchar(facet_var)) facet_var else NULL
    )
  )

  missing_vars <- setdiff(
    requested_vars,
    names(dat)
  )

  if (length(missing_vars) > 0) {
    return(
      empty_plot(
        paste0(
          "Variable introuvable dans les données : ",
          paste(missing_vars, collapse = ", "),
          "."
        )
      )
    )
  }

  # Une variable ne doit pas remplir plusieurs rôles.
  if (identical(col_var, x_var)) {
    col_var <- ""
  }

  if (
    facet_var %in% c(x_var, col_var)
  ) {
    facet_var <- ""
  }

  has_col   <- nzchar(col_var)
  has_facet <- nzchar(facet_var)

  # ----------------------------------------------------------
  # Préparer une copie locale des données
  # ----------------------------------------------------------
  dat <- as.data.frame(dat)

  dat[[x_var]] <- factor(dat[[x_var]])

  if (has_col) {
    dat[[col_var]] <- factor(dat[[col_var]])
  }

  if (has_facet) {
    dat[[facet_var]] <- factor(dat[[facet_var]])
  }

  # ----------------------------------------------------------
  # Construire le graphique
  # ----------------------------------------------------------
  if (has_col) {

    dat$x_combined <- interaction(
      dat[[x_var]],
      dat[[col_var]],
      drop = TRUE,
      sep = "."
    )

    p <- ggplot2::ggplot(
      dat,
      ggplot2::aes(
        x = x_combined,
        y = Y,
        fill = .data[[col_var]]
      )
    ) +
      ggplot2::geom_boxplot(
        alpha = 0.7,
        outlier.shape = NA
      ) +
      ggplot2::geom_jitter(
        ggplot2::aes(
          color = .data[[col_var]]
        ),
        width = 0.15,
        alpha = 0.5,
        size = 1.5,
        show.legend = FALSE
      ) +
      ggplot2::scale_x_discrete(
        labels = function(x) {
          gsub("\\.", "\n", x)
        }
      ) +
      ggplot2::labs(
        x = paste0(x_var, " : ", col_var),
        fill = col_var,
        color = col_var
      )

  } else {

    p <- ggplot2::ggplot(
      dat,
      ggplot2::aes(
        x = .data[[x_var]],
        y = Y,
        fill = .data[[x_var]]
      )
    ) +
      ggplot2::geom_boxplot(
        alpha = 0.7,
        outlier.shape = NA
      ) +
      ggplot2::geom_jitter(
        width = 0.15,
        alpha = 0.5,
        size = 1.5,
        show.legend = FALSE
      ) +
      ggplot2::labs(
        x = x_var,
        fill = x_var
      )
  }

  if (has_facet) {
    p <- p +
      ggplot2::facet_wrap(
        stats::as.formula(
          paste("~", facet_var)
        ),
        labeller = ggplot2::label_both
      )
  }

  p <- p +
    ggplot2::scale_fill_brewer(
      palette = "Set2"
    ) +
    ggplot2::scale_color_brewer(
      palette = "Set2"
    ) +
    ggplot2::labs(
      title = "Distribution de Y par groupe",
      y = "Y"
    ) +
    ggplot2::theme_minimal(
      base_size = 13
    ) +
    ggplot2::theme(
      legend.position = "bottom"
    )

  plotly::ggplotly(p)
}

plot_interaction_flex <- function(
    dat,
    x_var,
    col_var = "",
    facet_var = ""
) {

  # ----------------------------------------------------------
  # Normaliser les arguments
  # ----------------------------------------------------------
  x_var <- if (
    is.null(x_var) ||
    length(x_var) == 0
  ) {
    ""
  } else {
    as.character(x_var[1])
  }

  col_var <- if (
    is.null(col_var) ||
    length(col_var) == 0
  ) {
    ""
  } else {
    as.character(col_var[1])
  }

  facet_var <- if (
    is.null(facet_var) ||
    length(facet_var) == 0
  ) {
    ""
  } else {
    as.character(facet_var[1])
  }

  empty_plot <- function(message) {

    plotly::plot_ly() |>
      plotly::layout(
        xaxis = list(
          visible = FALSE,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          visible = FALSE,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        annotations = list(
          list(
            text = message,
            x = 0.5,
            y = 0.5,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 15)
          )
        )
      )
  }

  # ----------------------------------------------------------
  # Vérifier les données et les variables
  # ----------------------------------------------------------
  if (
    is.null(dat) ||
    !is.data.frame(dat) ||
    nrow(dat) == 0
  ) {
    return(
      empty_plot("Aucune donnée disponible.")
    )
  }

  if (!"Y" %in% names(dat)) {
    return(
      empty_plot("La variable de réponse Y est absente.")
    )
  }

  if (!nzchar(x_var)) {
    return(
      empty_plot("Sélectionnez un facteur pour l’axe X.")
    )
  }

  requested_vars <- unique(
    c(
      x_var,
      if (nzchar(col_var)) col_var else NULL,
      if (nzchar(facet_var)) facet_var else NULL
    )
  )

  missing_vars <- setdiff(
    requested_vars,
    names(dat)
  )

  if (length(missing_vars) > 0) {
    return(
      empty_plot(
        paste0(
          "Variable introuvable dans les données : ",
          paste(missing_vars, collapse = ", "),
          "."
        )
      )
    )
  }

  if (identical(col_var, x_var)) {
    col_var <- ""
  }

  if (
    facet_var %in% c(x_var, col_var)
  ) {
    facet_var <- ""
  }

  has_col   <- nzchar(col_var)
  has_facet <- nzchar(facet_var)

  dat <- as.data.frame(dat)

  dat[[x_var]] <- factor(dat[[x_var]])

  if (has_col) {
    dat[[col_var]] <- factor(dat[[col_var]])
  }

  if (has_facet) {
    dat[[facet_var]] <- factor(dat[[facet_var]])
  }

  # ----------------------------------------------------------
  # Variables de regroupement
  # ----------------------------------------------------------
  grp_vars <- unique(
    c(
      x_var,
      if (has_col) col_var else NULL,
      if (has_facet) facet_var else NULL
    )
  )

  grp_key <- do.call(
    paste,
    c(
      lapply(
        grp_vars,
        function(v) {
          as.character(dat[[v]])
        }
      ),
      sep = "__"
    )
  )

  keys_unique <- unique(grp_key)

  grid_rows <- lapply(
    keys_unique,
    function(key) {

      index <- grp_key == key
      values <- dat$Y[index]

      parts <- strsplit(
        key,
        "__",
        fixed = TRUE
      )[[1]]

      row <- as.data.frame(
        setNames(
          as.list(parts),
          grp_vars
        ),
        stringsAsFactors = FALSE
      )

      row$Y_mean <- mean(
        values,
        na.rm = TRUE
      )

      n_valid <- sum(
        is.finite(values)
      )

      row$se <- if (n_valid > 1) {
        stats::sd(
          values,
          na.rm = TRUE
        ) / sqrt(n_valid)
      } else {
        NA_real_
      }

      row
    }
  )

  grid <- do.call(
    rbind,
    grid_rows
  )

  for (v in grp_vars) {
    grid[[v]] <- factor(
      grid[[v]],
      levels = levels(dat[[v]])
    )
  }

  grid$Y_mean <- as.numeric(grid$Y_mean)
  grid$se     <- as.numeric(grid$se)

  grid <- grid[
    do.call(
      order,
      lapply(
        grp_vars,
        function(v) grid[[v]]
      )
    ),
    ,
    drop = FALSE
  ]

  # ----------------------------------------------------------
  # Construire le profil
  # ----------------------------------------------------------
  if (has_col) {

    p <- ggplot2::ggplot(
      grid,
      ggplot2::aes(
        x = .data[[x_var]],
        y = Y_mean,
        color = .data[[col_var]],
        group = .data[[col_var]]
      )
    ) +
      ggplot2::scale_color_brewer(
        palette = "Set1"
      )

  } else {

    p <- ggplot2::ggplot(
      grid,
      ggplot2::aes(
        x = .data[[x_var]],
        y = Y_mean,
        group = 1
      )
    )
  }

  p <- p +
    ggplot2::geom_line(
      linewidth = 1
    ) +
    ggplot2::geom_point(
      size = 3
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = Y_mean - se,
        ymax = Y_mean + se
      ),
      width = 0.15
    )

  if (has_facet) {
    p <- p +
      ggplot2::facet_wrap(
        stats::as.formula(
          paste("~", facet_var)
        ),
        labeller = ggplot2::label_both
      )
  }

  p <- p +
    ggplot2::labs(
      title = "Profil des moyennes (± 1 erreur-type)",
      x = x_var,
      y = "Moyenne de Y",
      color = if (has_col) col_var else NULL
    ) +
    ggplot2::theme_minimal(
      base_size = 13
    ) +
    ggplot2::theme(
      legend.position = "bottom"
    )

  plotly::ggplotly(p)
}

compute_quasi_f_numeric <- function(
    eff_name,
    quasi_info,
    all_ms,
    all_df
) {

  # ========================================================
  # Extraire une valeur numérique nommée
  # ========================================================
  get_numeric_value <- function(x, name) {

    value <- x[[name]]

    if (
      is.null(value) ||
      length(value) != 1L ||
      !is.numeric(value) ||
      !is.finite(value)
    ) {
      return(NA_real_)
    }

    as.numeric(value)
  }

  # ========================================================
  # Approximation de Satterthwaite pour une combinaison
  # positive de carrés moyens
  #
  # L = somme(w_i * MS_i)
  #
  # ddl(L) =
  #   L² / somme[(w_i * MS_i)² / ddl_i]
  # ========================================================
  satterthwaite_df <- function(
      weighted_ms,
      df_values
  ) {

    if (
      length(weighted_ms) == 0L ||
      length(weighted_ms) != length(df_values) ||
      any(!is.finite(weighted_ms)) ||
      any(!is.finite(df_values)) ||
      any(df_values <= 0)
    ) {
      return(NA_real_)
    }

    total_ms <- sum(weighted_ms)

    denominator <- sum(
      weighted_ms^2 / df_values
    )

    if (
      !is.finite(total_ms) ||
      total_ms <= 0 ||
      !is.finite(denominator) ||
      denominator <= 0
    ) {
      return(NA_real_)
    }

    total_ms^2 / denominator
  }

  # ========================================================
  # Carré moyen et ddl de l'effet testé
  # ========================================================
  ms_eff <- get_numeric_value(
    all_ms,
    eff_name
  )

  df_eff <- get_numeric_value(
    all_df,
    eff_name
  )

  if (
    !is.finite(ms_eff) ||
    !is.finite(df_eff) ||
    df_eff <= 0
  ) {
    return(
      list(
        ct = NULL,
        satt = NULL
      )
    )
  }

  result <- list(
    ct = NULL,
    satt = NULL
  )

  # ========================================================
  # Coefficients Cornfield-Tukey
  # ========================================================
  ct <- quasi_info$ct

  if (
    is.null(ct) ||
    is.null(ct$coefficients) ||
    length(ct$coefficients) == 0L
  ) {
    return(result)
  }

  coefficients <- ct$coefficients

  if (is.null(names(coefficients))) {

    result$ct <- list(
      ms_num = NA_real_,
      ms_den = NA_real_,
      ms_quasi = NA_real_,
      F = NA_real_,
      df_num = NA_real_,
      df_den = NA_real_,
      df_quasi = NA_real_,
      p = NA_real_,
      negative = NA,
      moved_negative = FALSE,
      error = paste(
        "Les coefficients Cornfield-Tukey",
        "ne possèdent pas de noms."
      )
    )

    return(result)
  }

  # Éliminer les coefficients exactement nuls
  coefficients <- coefficients[
    is.finite(coefficients) &
      coefficients != 0
  ]

  term_names <- names(coefficients)

  if (length(term_names) == 0L) {

    result$ct <- list(
      ms_num = NA_real_,
      ms_den = NA_real_,
      ms_quasi = NA_real_,
      F = NA_real_,
      df_num = NA_real_,
      df_den = NA_real_,
      df_quasi = NA_real_,
      p = NA_real_,
      negative = FALSE,
      moved_negative = FALSE,
      error = "Aucun terme Cornfield-Tukey non nul."
    )

    return(result)
  }

  ms_values <- vapply(
    term_names,
    function(term_name) {
      get_numeric_value(
        all_ms,
        term_name
      )
    },
    numeric(1)
  )

  df_values <- vapply(
    term_names,
    function(term_name) {
      get_numeric_value(
        all_df,
        term_name
      )
    },
    numeric(1)
  )

  missing_ms <- !is.finite(ms_values)

  missing_df <- !is.finite(df_values) |
    df_values <= 0

  if (
    any(missing_ms) ||
    any(missing_df)
  ) {

    invalid_terms <- unique(
      c(
        term_names[missing_ms],
        term_names[missing_df]
      )
    )

    result$ct <- list(
      ms_num = NA_real_,
      ms_den = NA_real_,
      ms_quasi = NA_real_,
      F = NA_real_,
      df_num = NA_real_,
      df_den = NA_real_,
      df_quasi = NA_real_,
      p = NA_real_,
      negative = any(coefficients < 0),
      moved_negative = FALSE,
      error = paste(
        "Carré moyen ou degré de liberté invalide pour :",
        paste(
          invalid_terms,
          collapse = ", "
        )
      )
    )

    return(result)
  }

  # ========================================================
  # Gaylor-Hopper / Howe
  #
  # Dénominateur CT initial :
  #
  #   somme(c_i * MS_i)
  #
  # Les termes c_i < 0 sont transférés au numérateur :
  #
  #   F' =
  #   [MS_effet + somme(|c_i| MS_i pour c_i < 0)]
  #   -------------------------------------------------------
  #   [somme(c_i MS_i pour c_i > 0)]
  # ========================================================
  positive_indices <- coefficients > 0
  negative_indices <- coefficients < 0

  if (!any(positive_indices)) {

    result$ct <- list(
      ms_num = NA_real_,
      ms_den = NA_real_,
      ms_quasi = NA_real_,
      F = NA_real_,
      df_num = NA_real_,
      df_den = NA_real_,
      df_quasi = NA_real_,
      p = NA_real_,
      negative = any(negative_indices),
      moved_negative = FALSE,
      error = paste(
        "La combinaison Cornfield-Tukey ne contient",
        "aucun terme positif au dénominateur."
      )
    )

    return(result)
  }

  # --------------------------------------------------------
  # Numérateur positif
  # --------------------------------------------------------
  numerator_weighted_ms <- c(
    ms_eff,
    abs(coefficients[negative_indices]) *
      ms_values[negative_indices]
  )

  numerator_df_values <- c(
    df_eff,
    df_values[negative_indices]
  )

  ms_num <- sum(
    numerator_weighted_ms
  )

  df_num <- satterthwaite_df(
    weighted_ms = numerator_weighted_ms,
    df_values = numerator_df_values
  )

  # --------------------------------------------------------
  # Dénominateur positif
  # --------------------------------------------------------
  denominator_weighted_ms <-
    coefficients[positive_indices] *
    ms_values[positive_indices]

  denominator_df_values <-
    df_values[positive_indices]

  ms_den <- sum(
    denominator_weighted_ms
  )

  df_den <- satterthwaite_df(
    weighted_ms = denominator_weighted_ms,
    df_values = denominator_df_values
  )

  # --------------------------------------------------------
  # Statistique et probabilité
  # --------------------------------------------------------
  quasi_f <- if (
    is.finite(ms_num) &&
    ms_num > 0 &&
    is.finite(ms_den) &&
    ms_den > 0
  ) {
    ms_num / ms_den
  } else {
    NA_real_
  }

  p_value <- if (
    is.finite(quasi_f) &&
    is.finite(df_num) &&
    df_num > 0 &&
    is.finite(df_den) &&
    df_den > 0
  ) {
    stats::pf(
      quasi_f,
      df1 = df_num,
      df2 = df_den,
      lower.tail = FALSE
    )
  } else {
    NA_real_
  }

  corrected_result <- list(
    ms_num = ms_num,
    ms_den = ms_den,

    # Conservé pour compatibilité avec l'ancien code :
    # il correspond maintenant au dénominateur positif.
    ms_quasi = ms_den,

    F = quasi_f,

    df_num = df_num,
    df_den = df_den,

    # Conservé temporairement pour compatibilité :
    df_quasi = df_den,

    p = p_value,

    negative = any(negative_indices),

    moved_negative = any(negative_indices),

    positive_terms = names(
      coefficients[positive_indices]
    ),

    negative_terms = names(
      coefficients[negative_indices]
    ),

    error = if (
      is.finite(quasi_f) &&
      is.finite(df_num) &&
      is.finite(df_den)
    ) {
      NULL
    } else {
      paste(
        "Le quasi-F ou ses degrés de liberté",
        "n'ont pas pu être calculés."
      )
    }
  )

  # La construction CT détermine les coefficients.
  # Satterthwaite fournit les ddl des deux combinaisons.
  result$ct <- corrected_result
  result$satt <- corrected_result

  result
}

quasi_story_intro <- function(effect_name) {

  card(
    fill = FALSE,
    class = "mb-3",

    card_header(
      tags$b(
        paste0(
          "1. Pourquoi un quasi-F pour l’effet ",
          effect_name,
          " ?"
        )
      )
    ),

    card_body(
      fill = FALSE,

      layout_columns(
        col_widths = c(5, 2, 5),

        div(
          class = "alert alert-light border mb-0",

          tags$h5("Le problème"),

          p(
            class = "mb-2",
            "L’effet ",
            tags$b(effect_name),
            " ne possède aucun terme d’erreur pur."
          ),

          p(
            class = "mb-0",
            "Aucun carré moyen unique du plan ne contient exactement ",
            "les composantes de variance nécessaires pour tester cet effet."
          )
        ),

        div(
          style = paste(
            "display:flex;",
            "align-items:center;",
            "justify-content:center;",
            "font-size:2rem;"
          ),
          HTML("&rarr;")
        ),

        div(
          class = "alert alert-success mb-0",

          tags$h5("La solution"),

          p(
            class = "mb-2",
            "Il faut construire un nouveau dénominateur en combinant ",
            "plusieurs carrés moyens."
          ),

          p(
            class = "mb-0",
            tags$b(
              "Ce dénominateur composite est appelé un quasi-F."
            )
          )
        )
      )
    )
  )
}

quasi_story_components <- function(
    effect_name,
    quasi_info,
    ems_table
) {

  # ----------------------------------------------------------
  # Composantes de variance à reproduire
  # ----------------------------------------------------------
  sigma_terms <- quasi_info$sigma_terms %||% character(0)

  if (length(sigma_terms) == 0) {
    return(
      div(
        class = "alert alert-info",
        "Aucune composante de variance à reconstruire n’a été identifiée."
      )
    )
  }

  if (
    is.null(ems_table) ||
    is.null(ems_table[[effect_name]])
  ) {
    return(
      div(
        class = "alert alert-warning",
        "Les EMS nécessaires à cette représentation ne sont pas disponibles."
      )
    )
  }

  # ----------------------------------------------------------
  # Palette stable : une couleur par composante
  # ----------------------------------------------------------
  palette <- c(
    "#cfe2ff",  # bleu
    "#d1e7dd",  # vert
    "#fff3cd",  # jaune
    "#f8d7da",  # rouge pâle
    "#e2d9f3",  # violet
    "#cff4fc",  # cyan
    "#ffe5d0",  # orange pâle
    "#e9ecef"   # gris
  )

  border_palette <- c(
    "#6ea8fe",
    "#75b798",
    "#ffda6a",
    "#ea868f",
    "#a98eda",
    "#6edff6",
    "#ffb77c",
    "#adb5bd"
  )

  text_palette <- c(
    "#084298",
    "#0f5132",
    "#664d03",
    "#842029",
    "#432874",
    "#055160",
    "#7a3e00",
    "#343a40"
  )

  component_style <- setNames(
    lapply(
      seq_along(sigma_terms),
      function(i) {

        j <- ((i - 1L) %% length(palette)) + 1L

        list(
          background = palette[j],
          border     = border_palette[j],
          text       = text_palette[j]
        )
      }
    ),
    sigma_terms
  )

  # ----------------------------------------------------------
  # EMS candidats
  # Conserver ceux qui contiennent au moins une composante
  # recherchée, en excluant l’effet testé
  # ----------------------------------------------------------
  candidate_names <- setdiff(
    names(ems_table),
    effect_name
  )

  candidate_names <- candidate_names[
    vapply(
      candidate_names,
      function(candidate_name) {

        candidate_ems <- ems_table[[candidate_name]]

        any(
          sigma_terms %in% names(candidate_ems)
        )
      },
      logical(1)
    )
  ]

  # Placer d’abord les termes employés par Cornfield-Tukey
  # lorsqu’une solution existe
  ct_terms <- if (
    !is.null(quasi_info$ct) &&
    !is.null(quasi_info$ct$coefficients)
  ) {
    names(quasi_info$ct$coefficients)
  } else {
    character(0)
  }

  candidate_names <- unique(
    c(
      intersect(ct_terms, candidate_names),
      setdiff(candidate_names, ct_terms)
    )
  )

  # ----------------------------------------------------------
  # Pastille représentant uniquement une composante de variance
  # Les coefficients sont volontairement omis à cette étape
  # ----------------------------------------------------------
  component_badge <- function(
      term_name,
      compact = FALSE
  ) {

    style_info <- component_style[[term_name]]

    latex_term <- paste0(
      "\\sigma^2_{",
      term_name,
      "}"
    )

    tags$span(
      style = paste(
        "display:inline-block;",
        "font-weight:700;",
        paste0("color:", style_info$text, ";"),
        paste0("background-color:", style_info$background, ";"),
        paste0("border:1.5px solid ", style_info$border, ";"),
        "border-radius:0.35rem;",
        if (compact) {
          "padding:0.08rem 0.28rem;"
        } else {
          "padding:0.14rem 0.42rem;"
        },
        "margin:0.08rem 0.12rem;",
        "white-space:nowrap;"
      ),

      k(latex_term)
    )
  }

  # ----------------------------------------------------------
  # Bloc 1 : composantes recherchées
  # ----------------------------------------------------------
  target_components <- tagList(
    lapply(
      sigma_terms,
      function(term_name) {
        component_badge(term_name)
      }
    )
  )

  # ----------------------------------------------------------
  # Bloc 2 : matrice composantes × EMS candidats
  # ----------------------------------------------------------
  matrix_header <- tags$tr(

    tags$th(
      style = paste(
        "min-width:160px;",
        "vertical-align:middle;"
      ),
      "Composante recherchée"
    ),

    lapply(
      candidate_names,
      function(candidate_name) {

        tags$th(
          class = "text-center",
          style = paste(
            "min-width:105px;",
            "vertical-align:middle;"
          ),

          paste0(
            "EMS(",
            candidate_name,
            ")"
          )
        )
      }
    )
  )

  matrix_rows <- lapply(
    sigma_terms,
    function(term_name) {

      candidate_cells <- lapply(
        candidate_names,
        function(candidate_name) {

          candidate_ems <- ems_table[[candidate_name]]

          if (term_name %in% names(candidate_ems)) {

            tags$td(
              class = "text-center align-middle",

              component_badge(
                term_name = term_name,
                compact = TRUE
              )
            )

          } else {

            tags$td(
              class = "text-center align-middle text-muted",
              style = "font-size:1.15rem;",
              "—"
            )
          }
        }
      )

      tags$tr(

        tags$td(
          style = "vertical-align:middle;",

          component_badge(
            term_name = term_name,
            compact = TRUE
          )
        ),

        candidate_cells
      )
    }
  )

  component_matrix <- if (length(candidate_names) > 0) {

    tags$div(
      style = "overflow-x:auto;",

      tags$table(
        class = paste(
          "table table-sm table-bordered",
          "table-hover align-middle mb-0"
        ),

        tags$thead(
          class = "table-light",
          matrix_header
        ),

        tags$tbody(
          matrix_rows
        )
      )
    )

  } else {

    div(
      class = "alert alert-warning mb-0",
      "Aucun autre EMS ne contient les composantes de variance recherchées."
    )
  }

  # ----------------------------------------------------------
  # Vérifier si un EMS contient simultanément toutes les
  # composantes recherchées
  # ----------------------------------------------------------
  complete_candidates <- candidate_names[
    vapply(
      candidate_names,
      function(candidate_name) {

        all(
          sigma_terms %in%
            names(ems_table[[candidate_name]])
        )
      },
      logical(1)
    )
  ]

  conclusion_block <- if (length(complete_candidates) == 0) {

    div(
      class = "alert alert-warning mb-0",

      tags$h5(
        class = "alert-heading",
        "Aucun EMS ne suffit à lui seul"
      ),

      p(
        class = "mb-1",
        "Aucun EMS ne contient simultanément toutes les composantes ",
        "de variance recherchées."
      ),

      p(
        class = "mb-0",
        "Ces composantes sont réparties entre plusieurs carrés moyens. ",
        "Il faut donc les combiner pour construire un dénominateur composite."
      )
    )

  } else {

    div(
      class = "alert alert-info mb-0",

      tags$h5(
        class = "alert-heading",
        "Vérification à effectuer"
      ),

      p(
        class = "mb-0",
        "Les EMS suivants contiennent toutes les composantes recherchées : ",

        tags$b(
          paste(
            complete_candidates,
            collapse = ", "
          )
        ),

        ". Leurs coefficients doivent cependant être comparés avant de ",
        "conclure qu’un terme d’erreur pur existe."
      )
    )
  }

  # ----------------------------------------------------------
  # Carte finale
  # ----------------------------------------------------------
  card(
    fill = FALSE,
    class = "mb-3",

    card_header(
      tags$b(
        "2. Où retrouver les composantes de variance ?"
      )
    ),

    card_body(
      fill = FALSE,

      div(
        class = "alert alert-light border mb-3",

        tags$h5(
          paste0(
            "Composantes à reproduire dans le dénominateur de ",
            effect_name
          )
        ),

        p(
          class = "mb-2",
          "Le dénominateur doit contenir les mêmes composantes de ",
          "variance que l’EMS de ",
          tags$b(effect_name),
          ", sans contenir sa contribution propre."
        ),

        div(
          target_components
        )
      ),

      tags$h5(
        class = "mb-1",
        "Répartition des composantes dans les EMS disponibles"
      ),

      p(
        class = "text-muted mb-2",
        "Les EMS disponibles dans le plan sont examinés un à un afin ",
        "de déterminer lesquels contiennent les composantes recherchées."
      ),

      component_matrix,

      div(
        class = "mt-3",
        conclusion_block
      )
    )
  )
}

quasi_story_methods <- function(
    effect_name,
    quasi_info
) {

  has_ct   <- !is.null(quasi_info$ct)
  has_satt <- !is.null(quasi_info$satterthwaite)

  intro <- div(
    class = "alert alert-light border mb-3",

    p(
      class = "mb-2",
      "Les composantes nécessaires au test de ",
      tags$b(effect_name),
      " ont maintenant été repérées dans plusieurs EMS."
    ),

    p(
      class = "mb-0",
      "Il faut les réunir dans un seul dénominateur composite. ",
      "Deux méthodes permettent d’effectuer cette reconstruction."
    )
  )

  ct_card <- div(
    class = if (has_ct) {
      "alert alert-primary mb-0 h-100"
    } else {
      "alert alert-secondary mb-0 h-100"
    },

    tags$h4("Cornfield–Tukey"),

    p(
      tags$b("Principe : "),
      "reconstruire exactement les composantes de variance nécessaires ",
      "en combinant plusieurs carrés moyens."
    ),

    tags$ul(
      tags$li(
        "La méthode recherche les coefficients permettant de reproduire ",
        "l’EMS voulu."
      ),
      tags$li(
        "Elle peut additionner ou soustraire certains carrés moyens."
      ),
      tags$li(
        "La reconstruction est algébriquement exacte lorsqu’une solution existe."
      )
    ),

    div(
      class = if (has_ct) {
        "alert alert-success mb-0"
      } else {
        "alert alert-secondary mb-0"
      },

      if (has_ct) {
        "Une reconstruction Cornfield–Tukey a été trouvée pour cet effet."
      } else {
        "Aucune reconstruction Cornfield–Tukey n’a été trouvée pour cet effet."
      }
    )
  )

  satt_card <- div(
    class = if (has_satt) {
      "alert alert-success mb-0 h-100"
    } else {
      "alert alert-secondary mb-0 h-100"
    },

    tags$h4("Satterthwaite"),

    p(
      tags$b("Principe : "),
      "construire un dénominateur approché à partir de contributions positives, ",
      "puis ajuster ses degrés de liberté."
    ),

    tags$ul(
      tags$li(
        "La méthode combine les carrés moyens associés aux composantes recherchées."
      ),
      tags$li(
        "Le dénominateur obtenu demeure positif lorsque les carrés moyens le sont."
      ),
      tags$li(
        "Ses degrés de liberté sont calculés par approximation."
      )
    ),

    div(
      class = if (has_satt) {
        "alert alert-success mb-0"
      } else {
        "alert alert-secondary mb-0"
      },

      if (has_satt) {
        "Une approximation de Satterthwaite est disponible pour cet effet."
      } else {
        "Aucune approximation de Satterthwaite n’est disponible pour cet effet."
      }
    )
  )

  card(
    fill = FALSE,
    class = "mb-3",

    card_header(
      tags$b(
        paste0(
          "3. Comment reconstruire le dénominateur de ",
          effect_name,
          " ?"
        )
      )
    ),

    card_body(
      fill = FALSE,

      intro,

      layout_columns(
        col_widths = c(6, 6),
        ct_card,
        satt_card
      ),

      div(
        class = "alert alert-info mt-3 mb-0",

        tags$b("Différence essentielle : "),

        "Cornfield–Tukey recherche une reconstruction exacte, tandis que ",
        "Satterthwaite construit une approximation et ajuste les degrés de liberté."
      )
    )
  )
}

quasi_formula_card <- function(
    effect_name,
    quasi_info,
    ems_table
) {

  ct          <- quasi_info$ct
  satt        <- quasi_info$satterthwaite
  sigma_terms <- quasi_info$sigma_terms %||% character(0)

  # ----------------------------------------------------------
  # Palette identique à celle de la carte 2
  # ----------------------------------------------------------
  palette <- c(
    "#cfe2ff",
    "#d1e7dd",
    "#fff3cd",
    "#f8d7da",
    "#e2d9f3",
    "#cff4fc",
    "#ffe5d0",
    "#e9ecef"
  )

  border_palette <- c(
    "#6ea8fe",
    "#75b798",
    "#ffda6a",
    "#ea868f",
    "#a98eda",
    "#6edff6",
    "#ffb77c",
    "#adb5bd"
  )

  text_palette <- c(
    "#084298",
    "#0f5132",
    "#664d03",
    "#842029",
    "#432874",
    "#055160",
    "#7a3e00",
    "#343a40"
  )

  component_style <- setNames(
    lapply(
      seq_along(sigma_terms),
      function(i) {

        j <- ((i - 1L) %% length(palette)) + 1L

        list(
          background = palette[j],
          border     = border_palette[j],
          text       = text_palette[j]
        )
      }
    ),
    sigma_terms
  )

  # ----------------------------------------------------------
  # Pastille d'une composante de variance
  # ----------------------------------------------------------
  component_badge <- function(term_name) {

    style_info <- component_style[[term_name]]

    if (is.null(style_info)) {
      return(NULL)
    }

    tags$span(
      style = paste(
        "display:inline-block;",
        "font-weight:700;",
        paste0("color:", style_info$text, ";"),
        paste0("background-color:", style_info$background, ";"),
        paste0("border:1.5px solid ", style_info$border, ";"),
        "border-radius:0.35rem;",
        "padding:0.06rem 0.25rem;",
        "margin:0.04rem 0.08rem;",
        "white-space:nowrap;"
      ),

      k(
        paste0(
          "\\sigma^2_{",
          term_name,
          "}"
        )
      )
    )
  }

  # ----------------------------------------------------------
  # Composantes recherchées présentes dans un EMS donné
  # ----------------------------------------------------------
  badges_for_ms <- function(ms_name) {

    if (
      is.null(ems_table) ||
      is.null(ems_table[[ms_name]])
    ) {
      return(
        tags$span(
          class = "text-muted",
          "Composantes non disponibles"
        )
      )
    }

    present_terms <- intersect(
      sigma_terms,
      names(ems_table[[ms_name]])
    )

    if (length(present_terms) == 0) {
      return(
        tags$span(
          class = "badge bg-secondary",
          "Terme d’ajustement"
        )
      )
    }

    tagList(
      lapply(
        present_terms,
        component_badge
      )
    )
  }

  # ----------------------------------------------------------
  # Formater un coefficient
  # ----------------------------------------------------------
  format_coefficient <- function(value) {

    if (is.null(value) || length(value) == 0 || !is.finite(value)) {
      return("—")
    }

    if (abs(value - round(value)) < 1e-8) {
      sprintf("%+.0f", value)
    } else {
      paste0(
        if (value > 0) "+" else "",
        format(
          round(value, 4),
          trim = TRUE,
          scientific = FALSE
        )
      )
    }
  }

  # ----------------------------------------------------------
  # Afficher un MS sous forme mathématique
  # ----------------------------------------------------------
  ms_math <- function(term_name) {
    k(
      paste0(
        "MS_{",
        term_name,
        "}"
      )
    )
  }

  # ==========================================================
  # BLOC CORNFIELD-TUKEY
  # ==========================================================
  ct_block <- if (!is.null(ct)) {

    coefficients <- ct$coefficients %||% numeric(0)
    ct_terms     <- names(coefficients)

    has_negative <- any(
      coefficients < 0,
      na.rm = TRUE
    )

    # --------------------------------------------------------
    # Tableau des EMS et coefficients retenus
    # --------------------------------------------------------
    coefficient_rows <- lapply(
      ct_terms,
      function(term_name) {

        coefficient <- coefficients[[term_name]]

        row_class <- if (
          !is.null(coefficient) &&
          is.finite(coefficient) &&
          coefficient < 0
        ) {
          "table-warning"
        } else {
          ""
        }

        tags$tr(
          class = row_class,

          tags$td(
            style = "font-weight:700;",
            ms_math(term_name)
          ),

          tags$td(
            badges_for_ms(term_name)
          ),

          tags$td(
            class = "text-center",
            style = paste(
              "font-size:1.05rem;",
              "font-weight:800;"
            ),

            format_coefficient(coefficient)
          )
        )
      }
    )

    coefficients_table <- tags$table(
      class = paste(
        "table table-sm table-bordered",
        "table-hover align-middle mb-0"
      ),

      tags$thead(
        class = "table-light",

        tags$tr(
          tags$th("Carré moyen retenu"),
          tags$th("Composantes recherchées présentes"),
          tags$th(
            class = "text-center",
            "Coefficient"
          )
        )
      ),

      tags$tbody(
        coefficient_rows
      )
    )

    # --------------------------------------------------------
    # Reconstruction visuelle ligne par ligne
    # --------------------------------------------------------
    reconstruction_rows <- lapply(
      seq_along(ct_terms),
      function(i) {

        term_name  <- ct_terms[i]
        coefficient <- coefficients[[term_name]]

        sign_text <- if (i == 1) {

          if (coefficient < 0) "−" else ""

        } else {

          if (coefficient < 0) "−" else "+"
        }

        absolute_coefficient <- abs(coefficient)

        coefficient_latex <- if (
          abs(absolute_coefficient - 1) < 1e-8
        ) {
          ""
        } else {
          paste0(
            format(
              round(absolute_coefficient, 4),
              trim = TRUE,
              scientific = FALSE
            ),
            "\\,"
          )
        }

        tags$div(
          style = paste(
            "display:flex;",
            "align-items:center;",
            "justify-content:center;",
            "gap:0.7rem;",
            "padding:0.30rem 0;"
          ),

          tags$span(
            style = paste(
              "display:inline-block;",
              "width:1.2rem;",
              "font-size:1.15rem;",
              "font-weight:800;",
              "text-align:center;"
            ),
            sign_text
          ),

          tags$span(
            style = paste(
              "display:inline-block;",
              "min-width:160px;",
              "font-size:1.05rem;"
            ),

            k(
              paste0(
                coefficient_latex,
                "MS_{",
                term_name,
                "}"
              )
            )
          ),

          tags$span(
            style = "min-width:240px;",
            badges_for_ms(term_name)
          )
        )
      }
    )

    reconstruction_visual <- div(
      class = "alert alert-light border mb-3",

      tags$h5(
        class = "text-center mb-3",
        "Reconstruction progressive"
      ),

      tagList(reconstruction_rows),

      tags$hr(
        style = paste(
          "max-width:470px;",
          "margin:0.6rem auto;"
        )
      ),

      div(
        class = "text-center",

        tags$div(
          style = paste(
            "font-size:1.5rem;",
            "line-height:1;",
            "margin-bottom:0.35rem;"
          ),
          "↓"
        ),

        k(
          "MS_{quasi}",
          display = TRUE
        )
      )
    )

    # --------------------------------------------------------
    # Formules finales
    # --------------------------------------------------------
    final_formulas <- div(
      class = "alert alert-primary mb-0",

      tags$h5(
        class = "alert-heading",
        "Formules finales"
      ),

      div(
        class = "mb-3",
        k(ct$formula, display = TRUE)
      ),

      div(
        class = "mb-3",
        k(ct$f_formula, display = TRUE)
      ),

      div(
        k(ct$df_formula, display = TRUE)
      )
    )

    negative_warning <- if (has_negative) {

      div(
        class = "alert alert-warning mt-3 mb-0",

        tags$b("Observation importante : "),

        "la reconstruction exacte comporte des coefficients négatifs. ",
        "Le dénominateur peut donc devenir nul ou négatif avec certaines ",
        "données observées. Dans cette situation, Satterthwaite est ",
        "généralement préférable."
      )

    } else {

      div(
        class = "alert alert-success mt-3 mb-0",

        tags$b("Reconstruction satisfaisante : "),

        "tous les coefficients de la combinaison Cornfield–Tukey sont positifs."
      )
    }

    div(
      class = "mb-4",

      tags$h4("Cornfield–Tukey"),

      p(
        "La méthode détermine une combinaison de carrés moyens dont les EMS
        reproduisent exactement les composantes de variance recherchées."
        ),

      coefficients_table,

      div(
        class = "mt-3",
        reconstruction_visual
      ),

      final_formulas,

      negative_warning
    )

  } else {

    div(
      class = "alert alert-secondary mb-4",

      tags$h4("Cornfield–Tukey"),

      p(
        class = "mb-0",
        "Aucune reconstruction algébrique exacte n’a été obtenue pour cet effet."
      )
    )
  }

  # ==========================================================
  # BLOC SATTERTHWAITE
  # ==========================================================
  satt_block <- if (!is.null(satt)) {

    satt_coefficients <- quasi_info$satt_coefficients %||% numeric(0)
    satt_terms        <- names(satt_coefficients)

    satt_rows <- lapply(
      satt_terms,
      function(term_name) {

        coefficient <- satt_coefficients[[term_name]]

        tags$tr(
          tags$td(
            style = "font-weight:700;",
            ms_math(term_name)
          ),

          tags$td(
            badges_for_ms(term_name)
          ),

          tags$td(
            class = "text-center",
            style = "font-weight:800;",
            format_coefficient(coefficient)
          )
        )
      }
    )

    satt_table <- if (length(satt_rows) > 0) {

      tags$table(
        class = paste(
          "table table-sm table-bordered",
          "table-hover align-middle mb-3"
        ),

        tags$thead(
          class = "table-light",

          tags$tr(
            tags$th("Carré moyen retenu"),
            tags$th("Composante représentée"),
            tags$th(
              class = "text-center",
              "Coefficient"
            )
          )
        ),

        tags$tbody(
          satt_rows
        )
      )

    } else {

      NULL
    }

    div(
      tags$h4("Satterthwaite"),

      p(
        "Satterthwaite construit un dénominateur positif à partir des ",
        "carrés moyens associés aux composantes recherchées, puis calcule ",
        "des degrés de liberté approximatifs."
      ),

      satt_table,

      div(
        class = "alert alert-success mb-0",

        div(
          class = "mb-3",
          k(satt$formula, display = TRUE)
        ),

        div(
          class = "mb-3",
          k(satt$f_formula, display = TRUE)
        ),

        div(
          k(satt$df_formula, display = TRUE)
        )
      )
    )

  } else {

    div(
      class = "alert alert-secondary mb-0",

      tags$h4("Satterthwaite"),

      p(
        class = "mb-0",
        "Aucune approximation de Satterthwaite n’est disponible pour cet effet."
      )
    )
  }

  # ==========================================================
  # CARTE FINALE
  # ==========================================================
  card(
    fill = FALSE,
    class = "mb-3",

    card_header(
      tags$b(
        "4. Reconstruction du dénominateur composite"
      )
    ),

    card_body(
      fill = FALSE,

      div(
        class = "alert alert-light border mb-3",

        p(
          class = "mb-0",

          "Les carrés moyens repérés à l’étape précédente sont maintenant ",
          "combinés afin de former un seul dénominateur possédant les ",
          "composantes de variance nécessaires au test de ",

          tags$b(effect_name),

          "."
        )
      ),

      ct_block,

      hr(),

      satt_block
    )
  )
}

quasi_interpretation <- function(
    effect_name,
    quasi_info
) {

  has_ct <- !is.null(quasi_info$ct)
  has_satt <- !is.null(quasi_info$satterthwaite)

  ct_negative <- has_ct &&
    length(quasi_info$ct$terms_neg %||% character(0)) > 0

  recommendation <- if (has_satt && ct_negative) {

    tagList(
      tags$b("Méthode recommandée : Satterthwaite. "),
      "La solution Cornfield–Tukey contient des coefficients négatifs; ",
      "Satterthwaite fournit donc une solution plus stable."
    )

  } else if (has_ct && !ct_negative) {

    tagList(
      tags$b("Méthode recommandée : Cornfield–Tukey. "),
      "Une combinaison algébrique sans coefficient négatif est disponible."
    )

  } else if (has_satt) {

    tagList(
      tags$b("Méthode disponible : Satterthwaite. "),
      "Elle permet de construire un dénominateur composite avec des degrés ",
      "de liberté approximés."
    )

  } else {

    tagList(
      tags$b("Aucune méthode complète n’est disponible automatiquement. "),
      "La structure du plan doit être examinée manuellement."
    )
  }

  card(
    fill = FALSE,

    card_header(
      tags$b("5. Que signifie le quasi-F obtenu ?")
    ),

    card_body(
      fill = FALSE,

      layout_columns(
        col_widths = c(7, 5),

        div(
          class = "alert alert-success mb-0",

          tags$h5("Interprétation"),

          p(
            "La combinaison construite reproduit les composantes de ",
            "variance nécessaires au test de l’effet ",
            tags$b(effect_name),
            "."
          ),

          p(
            class = "mb-0",
            "Le carré moyen composite peut donc remplacer le terme d’erreur ",
            "pur qui manque dans le plan."
          )
        ),

        div(
          class = "alert alert-primary mb-0",

          tags$h5("Décision pratique"),

          p(
            class = "mb-0",
            recommendation
          )
        )
      ),

     div(
       class = "alert alert-light mt-3 mb-0",

       tags$b("À retenir : "),

       "Le principe est exactement le même que pour un test F classique :
       l'effet est comparé à un dénominateur possédant les mêmes composantes
       de variance. Lorsqu'aucun terme d'erreur pur n'existe, ce dénominateur
       est reconstruit à partir de plusieurs carrés moyens."
       ) 
    )
  )
}

extract_ms_for_validation <- function(
    anova_result,
    factors,
    ems_table
) {

  effect_names <- names(ems_table)

  ms_out <- stats::setNames(
    rep(NA_real_, length(effect_names)),
    effect_names
  )

  if (
    is.null(anova_result) ||
    !is.null(anova_result$error) ||
    is.null(anova_result$summary)
  ) {
    return(ms_out)
  }

  summ <- anova_result$summary
  fnames <- names(factors)

  s_parents <- factors[["s"]]$nested_in %||% character(0)

  between_f <- intersect(
    s_parents,
    fnames
  )

  within_f <- setdiff(
    fnames,
    c("s", between_f)
  )

  # Correspondance entre les strates de aov() et les termes d'erreur
  error_names <- effect_names[
    vapply(
      effect_names,
      function(effect_name) {
        isTRUE(
          grepl("s", effect_name)
        )
      },
      logical(1)
    )
  ]

  stratum_map <- list()

  for (error_name in error_names) {

    letters_in_term <- unlist(
      strsplit(
        gsub(
          "[^A-Za-z]",
          "",
          error_name
        ),
        ""
      )
    )

    non_subject <- intersect(
      setdiff(fnames, "s"),
      letters_in_term
    )

    within_members <- intersect(
      non_subject,
      within_f
    )

    stratum_key <- if (length(within_members) == 0) {

      "subj_id"

    } else {

      paste(
        c(
          "subj_id",
          sort(within_members)
        ),
        collapse = ":"
      )
    }

    stratum_map[[stratum_key]] <- error_name
  }

  for (stratum_name in names(summ)) {

    stratum <- summ[[stratum_name]]

    tbl <- if (
      is.list(stratum) &&
      !is.data.frame(stratum)
    ) {
      stratum[[1]]
    } else {
      stratum
    }

    if (
      is.null(tbl) ||
      !is.data.frame(tbl) ||
      !"Mean Sq" %in% colnames(tbl)
    ) {
      next
    }

    stratum_key <- trimws(
      sub(
        "^Error:\\s*",
        "",
        stratum_name
      )
    )

    source_names <- trimws(
      rownames(tbl)
    )

    for (i in seq_len(nrow(tbl))) {

      source_name <- source_names[i]
      ms_value <- as.numeric(tbl[i, "Mean Sq"])

      if (identical(source_name, "Residuals")) {

        effect_name <- stratum_map[[stratum_key]]

      } else {

        effect_name <- gsub(
          ":",
          "",
          source_name
        )
      }

      if (
        !is.null(effect_name) &&
        effect_name %in% effect_names
      ) {
        ms_out[[effect_name]] <- ms_value
      }
    }
  }

  ms_out
}

validate_generator_mc <- function(
    design,
    n_levels,
    random_variances,
    residual_variance = 1,
    B = 100,
    seed = 12345,
    progress = TRUE
) {

  # ----------------------------------------------------------
  # Construire les objets du plan
  # ----------------------------------------------------------
  factors <- parse_design(design)
  effects <- generate_effects(factors)

  ems_table <- build_ems_table(
    factors = factors,
    effects = effects
  )

  df_list <- compute_df(
    factors = factors,
    effects = effects,
    n_levels = n_levels
  )

  effect_names <- names(ems_table)

  # Validation sous H0 : toutes les contributions fixes = 0
  fixed_effects <- effect_names[
    vapply(
      effect_names,
      function(effect_name) {

        effect_info <- effects[[effect_name]]

        non_subject <- setdiff(
          effect_info$factors,
          "s"
        )

        !isTRUE(effect_info$is_error) &&
          length(non_subject) > 0 &&
          all(
            vapply(
              non_subject,
              function(factor_name) {
                isTRUE(
                  factors[[factor_name]]$fixed
                )
              },
              logical(1)
            )
          )
      },
      logical(1)
    )
  ]

  effect_sizes <- stats::setNames(
    as.list(
      rep(0, length(fixed_effects))
    ),
    fixed_effects
  )

  # ----------------------------------------------------------
  # Répéter la simulation
  # ----------------------------------------------------------
  ms_matrix <- matrix(
    NA_real_,
    nrow = B,
    ncol = length(effect_names),
    dimnames = list(
      NULL,
      effect_names
    )
  )

  for (b in seq_len(B)) {

    dat <- sim_data(
      factors = factors,
      effects = effects,
      n_levels = n_levels,
      effect_sizes = effect_sizes,
      random_variances = random_variances,
      sigma2_error = residual_variance,
      seed = seed + b - 1L
    )

    anova_result <- compute_anova(
      dat = dat,
      factors = factors,
      effects = effects,
      df_list = df_list
    )

    ms_matrix[b, ] <- extract_ms_for_validation(
      anova_result = anova_result,
      factors = factors,
      ems_table = ems_table
    )[effect_names]

    if (
      isTRUE(progress) &&
      (
        b == 1L ||
        b %% max(1L, floor(B / 10L)) == 0L ||
        b == B
      )
    ) {
      message(
        "Simulation ",
        b,
        " / ",
        B
      )
    }
  }

  # ----------------------------------------------------------
  # Résultats descriptifs
  # ----------------------------------------------------------
  summary_table <- data.frame(
    Effet = effect_names,
    MS_moyen = colMeans(
      ms_matrix,
      na.rm = TRUE
    ),
    MS_ecart_type = apply(
      ms_matrix,
      2,
      stats::sd,
      na.rm = TRUE
    ),
    MS_mediane = apply(
      ms_matrix,
      2,
      stats::median,
      na.rm = TRUE
    ),
    N_valide = colSums(
      is.finite(ms_matrix)
    ),
    stringsAsFactors = FALSE
  )

  list(
    summary = summary_table,
    ms = ms_matrix,
    factors = factors,
    effects = effects,
    ems_table = ems_table,
    df_list = df_list,
    n_levels = n_levels,
    random_variances = random_variances,
    residual_variance = residual_variance,
    B = B
  )
}

compute_theoretical_ems <- function(
    ems_table,
    n_levels,
    random_variances,
    residual_term,
    residual_variance
) {

  variance_values <- random_variances
  variance_values[[residual_term]] <- residual_variance

  vapply(
    names(ems_table),
    function(effect_name) {

      ems_effect <- ems_table[[effect_name]]

      contributions <- vapply(
        names(ems_effect),
        function(term_name) {

          term_info <- ems_effect[[term_name]]

          # Validation sous H0 :
          # toutes les contributions fixes phi valent zéro.
          if (isTRUE(term_info$is_fixed)) {
            return(0)
          }

          variance_value <- variance_values[[term_name]]

          if (
            is.null(variance_value) ||
            length(variance_value) != 1L ||
            !is.finite(variance_value)
          ) {
            return(NA_real_)
          }

          coefficient <- parse_coef_numeric(
            term_info$coef_str,
            n_levels
          )

          coefficient * variance_value
        },
        numeric(1)
      )

      if (anyNA(contributions)) {
        NA_real_
      } else {
        sum(contributions)
      }
    },
    numeric(1)
  )
}

# ==========================================================
# MOTEUR EMS
#
# Génération, évaluation et validation des Expected Mean
# Squares (Cornfield-Tukey).
#
# Validation indépendante :
#   afex::ems()
#
# Fonction de régression :
#   validate_all_reference_plans()
#
# Dernière validation :
#   2026-08-02
#   8/8 plans de référence validés
# ==========================================================

# ----------------------------------------------------------
# 1. Génération des EMS
# ----------------------------------------------------------
build_ems_table <- function(factors, effects) {
  all_f     <- names(factors)
  eff_names <- names(effects)

  aux <- matrix(NA, nrow=length(all_f), ncol=length(eff_names),
                dimnames=list(all_f, eff_names))

for (f in all_f) {
    is_random <- f == "s" || isTRUE(factors[[f]]$random)
    for (e in eff_names) {
      f_in_factors <- f %in% effects[[e]]$factors
      f_in_parents <- f %in% effects[[e]]$nested_in
      aux[f, e]    <- if (f_in_factors) {
        if (is_random) "1" else "0"
      } else if (f_in_parents) {
        "1"
      } else {
        paste0("n_", f)
      }
    }
  }

  ems_table <- list()

  for (eff_name in eff_names) {
    eff   <- effects[[eff_name]]
    eff_f <- eff$factors

if (isTRUE(eff$is_error)) {
      ems_table[[eff_name]] <- list()

      for (cand_name in eff_names) {
        cand <- effects[[cand_name]]
        if (!isTRUE(cand$is_error)) next

        # E' inclut E si facteurs(E) ⊆ facteurs(E') ∪ parents(E')
        e_in_cand <- all(eff$factors %in%
                         c(cand$factors, cand$nested_in))
        if (!e_in_cand) next

        # Facteurs supplémentaires dans E' par rapport à E
        extra_f <- setdiff(cand$factors, c(eff$factors, "s"))

        # Un terme candidat différent du terme propre contribue
# uniquement si tous les facteurs supplémentaires sont
# aléatoires.
#
# Exemples :
#   s(A)  <- cs(A)   : extra = c       -> inclus
#   s(A)  <- Bs(A)   : extra = B       -> exclu
#   s(A)  <- Bcs(A)  : extra = B, c    -> exclu
#   Bs(A) <- Bcs(A)  : extra = c       -> inclus
#   cs(A) <- Bcs(A)  : extra = B       -> exclu
if (cand_name != eff_name) {

  if (length(extra_f) == 0L) {
    next
  }

  all_extra_random <- all(
    vapply(
      extra_f,
      function(f) {
        !is.null(factors[[f]]) &&
          isTRUE(factors[[f]]$random)
      },
      logical(1)
    )
  )

  if (!all_extra_random) {
    next
  }
}

        # Coefficient
        absent_f <- all_f[sapply(all_f, function(f) {
          f != "s" &&
          !f %in% cand$factors &&
          !f %in% cand$nested_in
        })]

        coef_str <- if (length(absent_f) == 0) "1"
                    else paste(sort(paste0("n_", absent_f)),
                               collapse="\u00b7")

        ems_table[[eff_name]][[cand_name]] <- list(
          coef_str = coef_str,
          symbol   = paste0("\u03c3\u00b2(", cand_name, ")"),
          is_fixed = FALSE
        )
      }
      next
    }

    ems <- list()

    for (cand_name in eff_names) {
      cand   <- effects[[cand_name]]
      cand_f <- cand$factors

      e_in_cand <- all(eff_f %in% c(cand_f, cand$nested_in))
      if (!e_in_cand) next

      rows_used  <- all_f[!all_f %in% eff_f]

      if (length(rows_used) == 0) {
        coef_parts <- character(0)
      } else {
        coef_parts <- aux[rows_used, cand_name, drop=TRUE]
      }

      if ("0" %in% coef_parts) next

      non_one  <- coef_parts[coef_parts != "1"]
      coef_str <- if (length(non_one) == 0) "1"
                  else paste(sort(non_one), collapse="\u00b7")

      cand_non_s   <- cand_f[cand_f != "s"]
      is_rand_term <- "s" %in% cand_f ||
        any(vapply(cand_non_s, function(f)
          isTRUE(factors[[f]]$random), logical(1)))

      ems[[cand_name]] <- list(
        coef_str = coef_str,
        symbol   = if (is_rand_term)
          paste0("\u03c3\u00b2(", cand_name, ")")
          else paste0("\u03c6(", cand_name, ")"),
        is_fixed = !is_rand_term
      )
    }

        ems_table[[eff_name]] <- ems
  }

  # ==========================================================
  # Ajouter la composante résiduelle terminale à toutes les EMS
  #
  # Dans sim_data(), le dernier terme d'erreur reçoit la
  # variance résiduelle sigma2_error et contribue à chaque
  # observation. Sa composante doit donc apparaître, avec un
  # coefficient égal à 1, dans chaque carré moyen attendu.
  # ==========================================================
  error_terms <- names(effects)[
    vapply(
      effects,
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

  if (
    length(residual_term) == 1L &&
    nzchar(residual_term)
  ) {

    for (effect_name in names(ems_table)) {

      if (
        is.null(
          ems_table[[effect_name]][[residual_term]]
        )
      ) {

        ems_table[[effect_name]][[residual_term]] <- list(
          coef_str = "1",
          symbol = paste0(
            "\u03c3\u00b2(",
            residual_term,
            ")"
          ),
          is_fixed = FALSE
        )
      }
    }
  }

  return(ems_table)
}

compute_fixed_phi <- function(
    effect_name,
    effect_info,
    effect_size,
    n_levels
) {

  effect_factors <- effect_info$factors

  if (
    length(effect_size) != 1L ||
    !is.finite(effect_size) ||
    effect_size == 0 ||
    length(effect_factors) == 0L
  ) {
    return(0)
  }

  effect_levels <- vapply(
    effect_factors,
    function(factor_name) {
      as.integer(n_levels[[factor_name]])
    },
    integer(1)
  )

  # ==========================================================
  # Effet principal
  # ==========================================================
  if (length(effect_factors) == 1L) {

    m <- effect_levels[1]

    level_effects <- seq(
      -effect_size * (m - 1) / 2,
       effect_size * (m - 1) / 2,
      length.out = m
    )

    return(
      sum(level_effects^2) / (m - 1)
    )
  }

  # ==========================================================
  # Interaction entre facteurs fixes
  #
  # On reproduit la règle utilisée dans sim_data(), puis on
  # centre successivement la composante selon chaque dimension.
  # Cela extrait la partie correspondant strictement à
  # l'interaction considérée.
  # ==========================================================
  grid <- expand.grid(
    lapply(
      effect_levels,
      seq_len
    ),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  names(grid) <- effect_factors

  interaction_values <- apply(
    grid,
    1,
    function(level_numbers) {

      signs <- ifelse(
        as.integer(level_numbers) %% 2L == 0L,
        1,
        -1
      )

      effect_size * prod(signs)
    }
  )

  component_array <- array(
    interaction_values,
    dim = effect_levels
  )

  # Centrage marginal selon chaque facteur :
  # projection dans l'espace propre de l'interaction.
  for (dimension in seq_along(effect_levels)) {

    marginal_mean <- apply(
      component_array,
      setdiff(
        seq_along(effect_levels),
        dimension
      ),
      mean
    )

    expanded_mean <- array(
      marginal_mean,
      dim = effect_levels[
        setdiff(
          seq_along(effect_levels),
          dimension
        )
      ]
    )

    permutation <- c(
      dimension,
      setdiff(
        seq_along(effect_levels),
        dimension
      )
    )

    component_permuted <- aperm(
      component_array,
      permutation
    )

    dimensions_permuted <- dim(
      component_permuted
    )

    mean_matrix <- matrix(
      rep(
        as.vector(expanded_mean),
        each = dimensions_permuted[1]
      ),
      nrow = dimensions_permuted[1]
    )

    component_permuted <- matrix(
      component_permuted,
      nrow = dimensions_permuted[1]
    ) - mean_matrix

    component_array <- aperm(
      array(
        component_permuted,
        dim = dimensions_permuted
      ),
      order(permutation)
    )
  }

  df_effect <- prod(
    effect_levels - 1L
  )

  if (df_effect <= 0) {
    return(0)
  }

  sum(component_array^2) / df_effect
}

# ----------------------------------------------------------
# 2. Évaluation numérique
# ----------------------------------------------------------
evaluate_ems <- function(
    ems_table,
    parameters,
    n_levels
) {

  details <- vector(
    mode = "list",
    length = length(ems_table)
  )

  names(details) <- names(ems_table)

  summary_rows <- vector(
    mode = "list",
    length = length(ems_table)
  )

  names(summary_rows) <- names(ems_table)

  for (effect_name in names(ems_table)) {

    ems <- ems_table[[effect_name]]

    term_rows <- vector(
      mode = "list",
      length = length(ems)
    )

    total <- 0

    for (i in seq_along(ems)) {

      term_name <- names(ems)[i]
      term_info <- ems[[term_name]]

      coefficient <- parse_coef_numeric(
        term_info$coef_str,
        n_levels
      )

      parameter_name <- if (isTRUE(term_info$is_fixed)) {
        paste0("phi", term_name)
      } else {
        term_name
      }

      parameter_value <- parameters[[parameter_name]]

      if (is.null(parameter_value)) {
        parameter_value <- 0
      }

      parameter_value <- as.numeric(parameter_value)

      if (
        length(parameter_value) != 1L ||
        !is.finite(parameter_value)
      ) {
        stop(
          paste0(
            "Valeur invalide pour le paramètre ",
            parameter_name,
            "."
          ),
          call. = FALSE
        )
      }

      contribution <- coefficient * parameter_value
      total <- total + contribution

      term_rows[[i]] <- data.frame(
        Terme = parameter_name,
        Type = if (isTRUE(term_info$is_fixed)) {
          "phi"
        } else {
          "sigma2"
        },
        Coefficient_symbolique = term_info$coef_str,
        Coefficient = coefficient,
        Valeur = parameter_value,
        Contribution = contribution,
        stringsAsFactors = FALSE
      )
    }

    effect_details <- if (length(term_rows) > 0) {

      do.call(
        rbind,
        term_rows
      )

    } else {

      data.frame(
        Terme = character(0),
        Type = character(0),
        Coefficient_symbolique = character(0),
        Coefficient = numeric(0),
        Valeur = numeric(0),
        Contribution = numeric(0),
        stringsAsFactors = FALSE
      )
    }

    details[[effect_name]] <- list(
      value = total,
      details = effect_details
    )

    summary_rows[[effect_name]] <- data.frame(
      Effet = effect_name,
      EMS_numerique = total,
      stringsAsFactors = FALSE
    )
  }

  summary_table <- do.call(
    rbind,
    summary_rows
  )

  rownames(summary_table) <- NULL

  result <- details
  result$summary <- summary_table

  result
}


diagnose_component_additivity <- function(
    design,
    n_levels,
    random_variances,
    residual_variance = 1,
    B = 1000,
    seed = 12345,
    progress = TRUE
) {

  # ==========================================================
  # Construire le plan
  # ==========================================================
  factors <- parse_design(design)
  effects <- generate_effects(factors)

  ems_table <- build_ems_table(
    factors = factors,
    effects = effects
  )

  df_list <- compute_df(
    factors = factors,
    effects = effects,
    n_levels = n_levels
  )

  effect_names <- names(ems_table)

  # Validation sous H0
  fixed_effect_names <- effect_names[
    vapply(
      effect_names,
      function(effect_name) {

        effect_info <- effects[[effect_name]]
        non_subject <- setdiff(effect_info$factors, "s")

        !isTRUE(effect_info$is_error) &&
          length(non_subject) > 0L &&
          all(
            vapply(
              non_subject,
              function(factor_name) {
                !is.null(factors[[factor_name]]) &&
                  isTRUE(factors[[factor_name]]$fixed)
              },
              logical(1)
            )
          )
      },
      logical(1)
    )
  ]

  effect_sizes <- stats::setNames(
    as.list(rep(0, length(fixed_effect_names))),
    fixed_effect_names
  )

  # ==========================================================
  # Matrices de stockage
  # ==========================================================
  ms_full <- matrix(
    NA_real_,
    nrow = B,
    ncol = length(effect_names),
    dimnames = list(NULL, effect_names)
  )

  ms_sum_components <- matrix(
    NA_real_,
    nrow = B,
    ncol = length(effect_names),
    dimnames = list(NULL, effect_names)
  )

  component_correlations <- vector(
    mode = "list",
    length = B
  )

  factor_columns <- names(factors)

  # ==========================================================
  # Répétitions Monte-Carlo
  # ==========================================================
  for (b in seq_len(B)) {

    dat_full <- sim_data(
      factors = factors,
      effects = effects,
      n_levels = n_levels,
      effect_sizes = effect_sizes,
      random_variances = random_variances,
      sigma2_error = residual_variance,
      seed = as.integer(seed) + b - 1L,
      keep_components = TRUE
    )

    components <- attr(
      dat_full,
      "components"
    )

    if (
      is.null(components) ||
      ncol(components) == 0L
    ) {
      stop(
        "Aucune composante n'a été conservée par sim_data().",
        call. = FALSE
      )
    }

    # --------------------------------------------------------
    # ANOVA avec toutes les composantes
    # --------------------------------------------------------
    fit_full <- compute_anova(
      dat = dat_full,
      factors = factors,
      effects = effects,
      df_list = df_list
    )

    ms_full[b, ] <- extract_ms_for_validation(
      anova_result = fit_full,
      factors = factors,
      ems_table = ems_table
    )[effect_names]

    # --------------------------------------------------------
    # Même ANOVA, une composante à la fois
    # --------------------------------------------------------
    ms_components_b <- matrix(
      0,
      nrow = ncol(components),
      ncol = length(effect_names),
      dimnames = list(
        names(components),
        effect_names
      )
    )

    for (component_name in names(components)) {

      dat_one <- dat_full[
        ,
        factor_columns,
        drop = FALSE
      ]

      dat_one$Y <- components[[component_name]]

      fit_one <- compute_anova(
        dat = dat_one,
        factors = factors,
        effects = effects,
        df_list = df_list
      )

      ms_components_b[component_name, ] <-
        extract_ms_for_validation(
          anova_result = fit_one,
          factors = factors,
          ems_table = ems_table
        )[effect_names]
    }

    ms_sum_components[b, ] <- colSums(
      ms_components_b,
      na.rm = TRUE
    )

    # Corrélations descriptives entre composantes au niveau
    # des observations. Elles sont secondaires; le diagnostic
    # principal repose sur l'additivité des MS.
    component_correlations[[b]] <- suppressWarnings(
      stats::cor(
        components,
        use = "pairwise.complete.obs"
      )
    )

    if (
      isTRUE(progress) &&
      (
        b == 1L ||
        b %% max(1L, floor(B / 10L)) == 0L ||
        b == B
      )
    ) {
      message(
        "Diagnostic d'additivité : ",
        b,
        " / ",
        B
      )
    }
  }

  # ==========================================================
  # Termes croisés
  # ==========================================================
  cross_terms <- ms_full -
    ms_sum_components

  mean_full <- colMeans(
    ms_full,
    na.rm = TRUE
  )

  mean_separate <- colMeans(
    ms_sum_components,
    na.rm = TRUE
  )

  mean_cross <- colMeans(
    cross_terms,
    na.rm = TRUE
  )

  sd_cross <- apply(
    cross_terms,
    2,
    stats::sd,
    na.rm = TRUE
  )

  n_valid <- colSums(
    is.finite(cross_terms)
  )

  se_cross <- sd_cross /
    sqrt(n_valid)

  z_cross <- ifelse(
    is.finite(se_cross) &
      se_cross > 0,
    mean_cross / se_cross,
    NA_real_
  )

  comparison <- data.frame(
    Effet = effect_names,
    MS_conjoint = unname(mean_full[effect_names]),
    Somme_MS_separes = unname(mean_separate[effect_names]),
    Terme_croise_moyen = unname(mean_cross[effect_names]),
    Erreur_MC = unname(se_cross[effect_names]),
    Ecart_standardise = unname(z_cross[effect_names]),
    Additivite_compatible = ifelse(
      is.finite(z_cross[effect_names]),
      abs(z_cross[effect_names]) <= 2,
      NA
    ),
    N_valide = unname(n_valid[effect_names]),
    stringsAsFactors = FALSE
  )

  # ==========================================================
  # Corrélation moyenne entre composantes
  # ==========================================================
  valid_correlations <- component_correlations[
    vapply(
      component_correlations,
      function(x) {
        is.matrix(x) &&
          all(dim(x) > 0L)
      },
      logical(1)
    )
  ]

  mean_correlation <- if (
    length(valid_correlations) > 0L
  ) {

    Reduce(
      `+`,
      valid_correlations
    ) / length(valid_correlations)

  } else {

    NULL
  }

  list(
    comparison = comparison,
    ms_full = ms_full,
    ms_sum_components = ms_sum_components,
    cross_terms = cross_terms,
    mean_component_correlation = mean_correlation,
    factors = factors,
    effects = effects,
    ems_table = ems_table,
    df_list = df_list,
    B = B,
    seed = seed
  )
}

diagnose_component_contributions <- function(
    design,
    n_levels,
    random_variances,
    residual_variance = 1,
    B = 500,
    seed = 12345,
    progress = TRUE
) {

  # ==========================================================
  # Construire les objets du plan
  # ==========================================================
  factors <- parse_design(design)
  effects <- generate_effects(factors)

  ems_table <- build_ems_table(
    factors = factors,
    effects = effects
  )

  df_list <- compute_df(
    factors = factors,
    effects = effects,
    n_levels = n_levels
  )

  effect_names <- names(ems_table)

  error_terms <- names(effects)[
    vapply(
      effects,
      function(effect_info) {
        isTRUE(effect_info$is_error)
      },
      logical(1)
    )
  ]

  if (length(error_terms) == 0L) {
    stop(
      "Aucun terme d'erreur n'a été identifié.",
      call. = FALSE
    )
  }

  residual_term <- tail(error_terms, 1)

  structural_components <- names(random_variances)

  all_components <- c(
    structural_components,
    residual_term
  )

  # ==========================================================
  # Construire la liste complète des paramètres EMS,
  # initialement tous fixés à zéro
  # ==========================================================
  parameter_names <- unique(
    unlist(
      lapply(
        ems_table,
        function(ems_effect) {

          vapply(
            names(ems_effect),
            function(term_name) {

              term_info <- ems_effect[[term_name]]

              if (isTRUE(term_info$is_fixed)) {
                paste0("phi", term_name)
              } else {
                term_name
              }
            },
            character(1)
          )
        }
      )
    )
  )

  zero_parameters <- stats::setNames(
    as.list(rep(0, length(parameter_names))),
    parameter_names
  )

  # ==========================================================
  # Stockage des résultats
  # ==========================================================
  result_rows <- list()
  row_index <- 0L

  # ==========================================================
  # Tester une composante à la fois
  # ==========================================================
  for (component_index in seq_along(all_components)) {

    component_name <- all_components[component_index]

    rv_one <- stats::setNames(
      as.list(rep(0, length(structural_components))),
      structural_components
    )

    residual_one <- 0

    if (identical(component_name, residual_term)) {

      residual_one <- 1

    } else {

      rv_one[[component_name]] <- 1
    }

    validation_one <- validate_generator_mc(
      design = design,
      n_levels = n_levels,
      random_variances = rv_one,
      residual_variance = residual_one,
      B = B,
      seed = seed,
      progress = FALSE
    )

    # --------------------------------------------------------
    # Contribution théorique de cette seule composante
    # --------------------------------------------------------
    parameters_one <- zero_parameters

    if (component_name %in% names(parameters_one)) {
      parameters_one[[component_name]] <- 1
    }

    theoretical_one <- evaluate_ems(
      ems_table = ems_table,
      parameters = parameters_one,
      n_levels = n_levels
    )$summary

    observed_one <- validation_one$summary

    comparison_one <- merge(
      theoretical_one,
      observed_one,
      by = "Effet",
      sort = FALSE
    )

    comparison_one <- comparison_one[
      match(effect_names, comparison_one$Effet),
      ,
      drop = FALSE
    ]

    comparison_one$Composante <- component_name

    comparison_one$Ecart <- with(
      comparison_one,
      MS_moyen - EMS_numerique
    )

    comparison_one$Erreur_MC <- with(
      comparison_one,
      MS_ecart_type / sqrt(N_valide)
    )

    comparison_one$Ecart_standardise <- with(
      comparison_one,
      ifelse(
        is.finite(Erreur_MC) & Erreur_MC > 0,
        Ecart / Erreur_MC,
        NA_real_
      )
    )

    comparison_one$Compatible_MC <- with(
      comparison_one,
      ifelse(
        is.finite(Ecart_standardise),
        abs(Ecart_standardise) <= 2,
        NA
      )
    )

    for (i in seq_len(nrow(comparison_one))) {

      row_index <- row_index + 1L

      result_rows[[row_index]] <- comparison_one[
        i,
        c(
          "Composante",
          "Effet",
          "EMS_numerique",
          "MS_moyen",
          "Ecart",
          "Erreur_MC",
          "Ecart_standardise",
          "Compatible_MC"
        ),
        drop = FALSE
      ]
    }

    if (isTRUE(progress)) {
      message(
        "Composante ",
        component_index,
        " / ",
        length(all_components),
        " : ",
        component_name
      )
    }
  }

  contribution_table <- do.call(
    rbind,
    result_rows
  )

  rownames(contribution_table) <- NULL

  list(
    contributions = contribution_table,
    factors = factors,
    effects = effects,
    ems_table = ems_table,
    df_list = df_list,
    residual_term = residual_term,
    B = B,
    seed = seed
  )
}

normalize_afex_effect_name <- function(x) {

  x <- gsub(":", "", x, fixed = TRUE)
  x <- gsub(" ", "", x, fixed = TRUE)

  # afex utilise S alors que l'application utilise s
  x <- gsub("S", "s", x, fixed = TRUE)

  # Cas d'imbrication attendu dans l'application
  x <- gsub("As", "s(A)", x, fixed = TRUE)
  x <- gsub("Bs", "s(B)", x, fixed = TRUE)
  x <- gsub("Cs", "s(C)", x, fixed = TRUE)

  x
}

# ----------------------------------------------------------
# 3. Conversion et comparaison
# ----------------------------------------------------------
convert_afex_ems <- function(
    afex_object,
    term_map,
    random_factors = c("C", "S"),
    replication_symbol = "r"
) {

  afex_matrix <- as.matrix(afex_object)

  if (
    is.null(rownames(afex_matrix)) ||
    is.null(colnames(afex_matrix))
  ) {
    stop(
      "La sortie afex doit posséder des noms de lignes et de colonnes.",
      call. = FALSE
    )
  }

  map_term <- function(x) {

    x <- trimws(x)

    if (x %in% names(term_map)) {
      return(unname(term_map[[x]]))
    }

    gsub(":", "", x, fixed = TRUE)
  }

  convert_multiplier <- function(x) {

    x <- trimws(as.character(x))

    if (is.na(x) || identical(x, "")) {
      return(NULL)
    }

    if (identical(x, "1")) {
      return("1")
    }

    symbols <- strsplit(
      x,
      "",
      fixed = TRUE
    )[[1]]

    # r représente la réplication au niveau le plus bas.
    symbols <- symbols[
      symbols != replication_symbol
    ]

    if (length(symbols) == 0L) {
      return("1")
    }

    # Correspondance avec la notation de l'application
    symbol_map <- c(
      a = "A",
      b = "B",
      c = "c",
      s = "s"
    )

    symbols <- unname(
      symbol_map[symbols]
    )

    symbols <- symbols[
      !is.na(symbols)
    ]

    if (length(symbols) == 0L) {
      return("1")
    }

    paste0(
      "n_",
      symbols,
      collapse = "\u00b7"
    )
  }

  component_is_fixed <- function(component_name) {

    if (identical(component_name, "e")) {
      return(FALSE)
    }

    component_factors <- strsplit(
      component_name,
      ":",
      fixed = TRUE
    )[[1]]

    !any(component_factors %in% random_factors)
  }

  result <- vector(
    "list",
    nrow(afex_matrix)
  )

  names(result) <- vapply(
    rownames(afex_matrix),
    map_term,
    character(1)
  )

  for (i in seq_len(nrow(afex_matrix))) {

    effect_terms <- list()

    for (j in seq_len(ncol(afex_matrix))) {

      cell_value <- afex_matrix[i, j]
      coefficient <- convert_multiplier(cell_value)

      if (is.null(coefficient)) {
        next
      }

      afex_component <- colnames(afex_matrix)[j]
      component_name <- map_term(afex_component)

      is_fixed <- component_is_fixed(
        afex_component
      )

      effect_terms[[component_name]] <- list(
        coef_str = coefficient,
        symbol = if (is_fixed) {
          paste0("\u03c6(", component_name, ")")
        } else {
          paste0("\u03c3\u00b2(", component_name, ")")
        },
        is_fixed = is_fixed
      )
    }

    result[[names(result)[i]]] <- effect_terms
  }

  result
}

compare_ems_tables <- function(
    app_ems,
    reference_ems,
    ignore_afex_residual = FALSE
) {

  normalize_coefficient <- function(x) {

    if (
      is.null(x) ||
      length(x) == 0L ||
      is.na(x)
    ) {
      return(NA_character_)
    }

    if (identical(x, "1")) {
      return("1")
    }

    parts <- strsplit(
      x,
      "\u00b7",
      fixed = TRUE
    )[[1]]

    parts <- trimws(parts)

    # Normaliser n_a/n_A, n_b/n_B, etc.
    parts <- sub(
      "^n_a$",
      "n_A",
      parts
    )

    parts <- sub(
      "^n_b$",
      "n_B",
      parts
    )

    parts <- sub(
      "^n_C$",
      "n_c",
      parts
    )

    parts <- sub(
      "^n_S$",
      "n_s",
      parts
    )

    paste(
      sort(parts),
      collapse = "\u00b7"
    )
  }

  all_effects <- union(
    names(app_ems),
    names(reference_ems)
  )

  rows <- list()
  row_id <- 0L

  for (effect_name in all_effects) {

    app_terms <- app_ems[[effect_name]] %||% list()
    ref_terms <- reference_ems[[effect_name]] %||% list()

    if (isTRUE(ignore_afex_residual)) {
      ref_terms[["e"]] <- NULL
    }

    all_terms <- union(
      names(app_terms),
      names(ref_terms)
    )

    for (term_name in all_terms) {

      app_term <- app_terms[[term_name]]
      ref_term <- ref_terms[[term_name]]

      app_coef <- if (is.null(app_term)) {
        NA_character_
      } else {
        normalize_coefficient(app_term$coef_str)
      }

      ref_coef <- if (is.null(ref_term)) {
        NA_character_
      } else {
        normalize_coefficient(ref_term$coef_str)
      }

      app_type <- if (is.null(app_term)) {
        NA_character_
      } else if (isTRUE(app_term$is_fixed)) {
        "phi"
      } else {
        "sigma2"
      }

      ref_type <- if (is.null(ref_term)) {
        NA_character_
      } else if (isTRUE(ref_term$is_fixed)) {
        "phi"
      } else {
        "sigma2"
      }

      same_coefficient <- (
        is.na(app_coef) &&
          is.na(ref_coef)
      ) || (
        !is.na(app_coef) &&
          !is.na(ref_coef) &&
          identical(app_coef, ref_coef)
      )

      same_type <- (
        is.na(app_type) &&
          is.na(ref_type)
      ) || (
        !is.na(app_type) &&
          !is.na(ref_type) &&
          identical(app_type, ref_type)
      )

      row_id <- row_id + 1L

      rows[[row_id]] <- data.frame(
        Effet = effect_name,
        Terme = term_name,
        Coef_application = app_coef,
        Coef_afex = ref_coef,
        Type_application = app_type,
        Type_afex = ref_type,
        Meme_coefficient = same_coefficient,
        Meme_type = same_type,
        Identique = same_coefficient && same_type,
        stringsAsFactors = FALSE
      )
    }
  }

  comparison <- do.call(
    rbind,
    rows
  )

  rownames(comparison) <- NULL
  comparison
}


# ----------------------------------------------------------
# 4. Validation
# ----------------------------------------------------------
validate_plan_ems <- function(
    design,
    ignore_afex_residual = TRUE,
    replication_symbol = "r",
    verbose = TRUE
) {

  # ==========================================================
  # Vérifier la disponibilité de afex
  # ==========================================================
  if (!requireNamespace("afex", quietly = TRUE)) {
    stop(
      paste(
        "Le package 'afex' est requis.",
        "Installez-le avec install.packages('afex')."
      ),
      call. = FALSE
    )
  }

  # ==========================================================
  # Construire les objets internes de l'application
  # ==========================================================
  factors <- parse_design(design)
  effects <- generate_effects(factors)

  if (is.null(factors) || length(factors) == 0L) {
    stop(
      "Le plan n'a produit aucun facteur.",
      call. = FALSE
    )
  }

  factor_names <- names(factors)

  # ==========================================================
  # Créer les alias à une lettre employés par afex
  #
  # Application :
  #   A, B : facteurs fixes
  #   b, c : facteurs aléatoires
  #   s    : sujets
  #
  # afex :
  #   A, B, C, S
  # ==========================================================
  afex_alias <- stats::setNames(
    toupper(factor_names),
    factor_names
  )

  if ("s" %in% factor_names) {
    afex_alias["s"] <- "S"
  }

  # afex exige des noms de facteurs à une seule lettre
  invalid_alias <- nchar(afex_alias) != 1L

  if (any(invalid_alias)) {
    stop(
      paste0(
        "afex::ems() exige des noms de facteurs à une lettre. ",
        "Facteurs problématiques : ",
        paste(
          names(afex_alias)[invalid_alias],
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  # Vérifier les collisions d'alias
  duplicated_alias <- duplicated(afex_alias) |
    duplicated(afex_alias, fromLast = TRUE)

  if (any(duplicated_alias)) {
    stop(
      paste0(
        "Deux facteurs du plan conduisent au même alias afex : ",
        paste(
          names(afex_alias)[duplicated_alias],
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  # ==========================================================
  # Construire la formule afex
  #
  # Exemple :
  #   r ~ A * B * C * S
  # ==========================================================
  afex_factor_order <- unname(
    afex_alias[factor_names]
  )

  afex_formula <- stats::as.formula(
    paste(
      replication_symbol,
      "~",
      paste(
        afex_factor_order,
        collapse = " * "
      )
    )
  )

  # ==========================================================
  # Identifier les facteurs aléatoires
  # ==========================================================
  random_factor_names <- factor_names[
    vapply(
      factor_names,
      function(factor_name) {

        identical(factor_name, "s") ||
          isTRUE(factors[[factor_name]]$random)
      },
      logical(1)
    )
  ]

  afex_random <- paste0(
    unname(
      afex_alias[random_factor_names]
    ),
    collapse = ""
  )

  # ==========================================================
  # Construire les relations d'imbrication
  #
  # Une relation distincte est transmise pour chaque parent :
  #
  #   s(A)  -> "A/S"
  #   s(AB) -> c("A/S", "B/S")
  # ==========================================================
  afex_nested <- character(0)

  for (child_name in factor_names) {

    parents <- factors[[child_name]]$nested_in

    if (
      is.null(parents) ||
      length(parents) == 0L
    ) {
      next
    }

    parents <- intersect(
      parents,
      factor_names
    )

    if (length(parents) == 0L) {
      next
    }

    parent_aliases <- unname(
      afex_alias[parents]
    )

    child_alias <- unname(
      afex_alias[[child_name]]
    )

    # IMPORTANT :
    # ne pas construire "A:B/S".
    # Pour s(AB), afex reçoit c("A/S", "B/S").
    afex_nested <- c(
      afex_nested,
      paste0(
        parent_aliases,
        "/",
        child_alias
      )
    )
  }

  afex_nested <- unique(afex_nested)

  if (length(afex_nested) == 0L) {
    afex_nested <- NULL
  }

  # ==========================================================
  # Obtenir la référence afex
  # ==========================================================
  afex_reference <- afex::ems(
    design = afex_formula,
    nested = afex_nested,
    random = afex_random
  )

# ==========================================================
# Construire automatiquement le dictionnaire de traduction
#
# Exemples :
#   A:B:C -> ABC
#   S     -> s(A) ou s(AB)
#   B:S   -> Bs(A)
#   C:S   -> Cs(AB)
#   e     -> dernier terme d'erreur du plan
# ==========================================================
error_terms <- names(effects)[
  vapply(
    effects,
    function(effect_info) {
      isTRUE(effect_info$is_error)
    },
    logical(1)
  )
]

residual_term <- if (length(error_terms) > 0L) {
  tail(error_terms, 1L)
} else {
  "e"
}

term_map <- c(
  "e" = residual_term
)

  factor_position <- stats::setNames(
    seq_along(afex_factor_order),
    afex_factor_order
  )

  for (effect_name in names(effects)) {

    effect_factors <- effects[[effect_name]]$factors

    if (
      is.null(effect_factors) ||
      length(effect_factors) == 0L
    ) {
      next
    }

    effect_factors <- intersect(
      effect_factors,
      factor_names
    )

    aliases <- unname(
      afex_alias[effect_factors]
    )

    aliases <- aliases[
      order(
        factor_position[aliases],
        na.last = TRUE
      )
    ]

    afex_effect_name <- paste(
      aliases,
      collapse = ":"
    )

    term_map[afex_effect_name] <- effect_name
  }

  # ==========================================================
  # Convertir la référence afex au format de l'application
  # ==========================================================
  afex_internal <- convert_afex_ems(
    afex_object = afex_reference,
    term_map = term_map,
    random_factors = unname(
      afex_alias[random_factor_names]
    ),
    replication_symbol = replication_symbol
  )

  # ==========================================================
  # Construire les EMS de l'application
  # ==========================================================
  application_ems <- build_ems_table(
    factors = factors,
    effects = effects
  )

  # ==========================================================
  # Comparer les deux générateurs
  # ==========================================================
  comparison <- compare_ems_tables(
    app_ems = application_ems,
    reference_ems = afex_internal,
    ignore_afex_residual = ignore_afex_residual
  )

  differences <- comparison[
    !comparison$Identique,
    ,
    drop = FALSE
  ]

  rownames(differences) <- NULL

  is_valid <- nrow(differences) == 0L

  # ==========================================================
  # Résumé dans la console
  # ==========================================================
  if (isTRUE(verbose)) {

    cat(
      "\n",
      "Validation des EMS\n",
      "==================\n",
      "Plan             : ", design, "\n",
      "Formule afex     : ", paste(deparse(afex_formula), collapse = ""), "\n",
      "Facteurs aléat.  : ",
      if (nzchar(afex_random)) afex_random else "aucun",
      "\n",
      "Imbrication      : ",
      if (is.null(afex_nested)) {
        "aucune"
      } else {
        paste(afex_nested, collapse = ", ")
      },
      "\n",
      "Nombre d'effets  : ", length(application_ems), "\n",
      "Divergences      : ", nrow(differences), "\n",
      "Verdict          : ",
      if (is_valid) {
        "EMS IDENTIQUES"
      } else {
        "EMS DIFFÉRENTS"
      },
      "\n\n",
      sep = ""
    )
  }

  # ==========================================================
  # Résultat complet
  # ==========================================================
  list(
    valid = is_valid,
    design = design,

    factors = factors,
    effects = effects,

    afex_formula = afex_formula,
    afex_random = afex_random,
    afex_nested = afex_nested,
    afex_alias = afex_alias,
    term_map = term_map,

    application_ems = application_ems,
    afex_raw = afex_reference,
    afex_internal = afex_internal,

    comparison = comparison,
    differences = differences
  )
}

latex_coef <- function(x) {

  x <- gsub(
    "·",
    " \\cdot ",
    x,
    fixed = TRUE
  )

  x <- gsub(
    "n_([A-Za-z0-9()]+)",
    "n_{\\1}",
    x,
    perl = TRUE
  )

  x
}


extract_anova_ms_df <- function(
    anova_res,
    factors,
    ems_table
) {

  if (
    is.null(anova_res) ||
    !is.null(anova_res$error) ||
    is.null(anova_res$summary)
  ) {
    return(
      list(
        ms = list(),
        df = list()
      )
    )
  }

  summ <- anova_res$summary
  fnames <- names(factors)

  s_nest <- if ("s" %in% fnames) {
    factors[["s"]]$nested_in %||% character(0)
  } else {
    character(0)
  }

  between_f <- intersect(
    s_nest,
    fnames
  )

  within_f <- setdiff(
    fnames,
    c("s", between_f)
  )

  # ----------------------------------------------------------
  # Correspondance entre les strates aov() et les termes EMS
  # ----------------------------------------------------------
  err_names <- names(ems_table)[
    vapply(
      names(ems_table),
      function(effect_name) {
        grepl(
          "s",
          effect_name,
          fixed = TRUE
        )
      },
      logical(1)
    )
  ]

  stratum_map <- list()

  for (error_name in err_names) {

    non_s_in_term <- intersect(
      setdiff(fnames, "s"),
      unlist(
        strsplit(
          gsub(
            "[^A-Za-z]",
            "",
            error_name
          ),
          ""
        )
      )
    )

    within_members <- intersect(
      non_s_in_term,
      within_f
    )

    stratum_suffix <- if (length(within_members) == 0L) {

      "subj_id"

    } else {

      paste(
        c(
          "subj_id",
          sort(within_members)
        ),
        collapse = ":"
      )
    }

    stratum_map[[stratum_suffix]] <- error_name
  }

  # ----------------------------------------------------------
  # Extraire les MS et les df
  # ----------------------------------------------------------
  all_ms <- list()
  all_df <- list()

  for (stratum_name in names(summ)) {

    stratum <- summ[[stratum_name]]

    tbl <- if (
      is.list(stratum) &&
      !is.data.frame(stratum)
    ) {
      stratum[[1]]
    } else {
      stratum
    }

    if (
      is.null(tbl) ||
      !is.data.frame(tbl)
    ) {
      next
    }

    stratum_key <- trimws(
      sub(
        "^Error:\\s*",
        "",
        stratum_name
      )
    )

    source_names <- trimws(
      rownames(tbl)
    )

    for (i in seq_len(nrow(tbl))) {

      source_name <- source_names[i]

      display_name <- if (
        identical(source_name, "Residuals")
      ) {

        ems_label <- stratum_map[[stratum_key]]

        if (!is.null(ems_label)) {
          ems_label
        } else {
          stratum_key
        }

      } else {

        source_name
      }

      all_ms[[display_name]] <- tbl[i, "Mean Sq"]
      all_df[[display_name]] <- tbl[i, "Df"]

      clean_name <- gsub(
        ":",
        "",
        source_name
      )

      if (!identical(clean_name, source_name)) {
        all_ms[[clean_name]] <- tbl[i, "Mean Sq"]
        all_df[[clean_name]] <- tbl[i, "Df"]
      }
    }
  }

  list(
    ms = all_ms,
    df = all_df,
    within_factors = within_f,
    between_factors = between_f,
    stratum_map = stratum_map
  )
}

# ==========================================================
# AJOUTER LES QUASI-F À UNE RÉPLICATION MONTE-CARLO
#
# La fonction reçoit le tableau extrait d'une seule réplication,
# calcule les quasi-F nécessaires, puis complète les colonnes
# F, p et degrés de liberté du dénominateur.
# ==========================================================
add_quasi_f_to_mc_replication <- function(
    extracted,
    r,
    method = c("auto", "ct", "satt")
) {

  method <- match.arg(method)

  if (
    is.null(extracted) ||
    !is.data.frame(extracted) ||
    nrow(extracted) == 0L
  ) {
    return(extracted)
  }

  required_columns <- c(
    "Effet",
    "df",
    "MS",
    "F",
    "p"
  )

  missing_columns <- setdiff(
    required_columns,
    names(extracted)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste(
        "Colonnes manquantes dans les résultats Monte-Carlo :",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # --------------------------------------------------------
  # Colonnes descriptives communes aux F classiques
  # et aux quasi-F
  # --------------------------------------------------------
  if (!"df_num" %in% names(extracted)) {
    extracted$df_num <- extracted$df
  }

  if (!"df_den" %in% names(extracted)) {
    extracted$df_den <- NA_real_
  }

  if (!"Type_test" %in% names(extracted)) {
    extracted$Type_test <- ifelse(
      is.finite(extracted$F),
      "F classique",
      NA_character_
    )
  }

  if (!"Methode" %in% names(extracted)) {
    extracted$Methode <- ifelse(
      is.finite(extracted$F),
      "Terme d'erreur pur",
      NA_character_
    )
  }

  # --------------------------------------------------------
  # Vecteurs nommés des MS et des degrés de liberté
  # --------------------------------------------------------
  all_ms <- stats::setNames(
    extracted$MS,
    extracted$Effet
  )

  all_df <- stats::setNames(
    extracted$df,
    extracted$Effet
  )

  # --------------------------------------------------------
  # Compléter les ddl des F classiques
  # --------------------------------------------------------
  pure_errors <- r$error_info$pure %||% list()

  for (effect_name in names(pure_errors)) {

    error_name <- pure_errors[[effect_name]]

    effect_index <- match(
      effect_name,
      extracted$Effet
    )

    error_index <- match(
      error_name,
      extracted$Effet
    )

    if (
      !is.na(effect_index) &&
      !is.na(error_index)
    ) {
      extracted$df_num[effect_index] <-
        extracted$df[effect_index]

      extracted$df_den[effect_index] <-
        extracted$df[error_index]

      if (is.finite(extracted$F[effect_index])) {
        extracted$Type_test[effect_index] <-
          "F classique"

        extracted$Methode[effect_index] <-
          paste0(
            "Erreur : ",
            error_name
          )
      }
    }
  }

  # --------------------------------------------------------
  # Aucun quasi-F dans ce plan
  # --------------------------------------------------------
  quasi_list <- r$quasi_list %||% list()

  if (length(quasi_list) == 0L) {
    return(extracted)
  }

  # --------------------------------------------------------
  # Calculer chaque quasi-F
  # --------------------------------------------------------
  for (effect_name in names(quasi_list)) {

    effect_index <- match(
      effect_name,
      extracted$Effet
    )

    if (is.na(effect_index)) {
      next
    }

    quasi_result <- compute_quasi_f_numeric(
      eff_name = effect_name,
      quasi_info = quasi_list[[effect_name]],
      all_ms = all_ms,
      all_df = all_df
    )

    ct_result <- quasi_result$ct
    satt_result <- quasi_result$satt

    # ------------------------------------------------------
    # Choix de la méthode
    #
    # auto :
    # - Cornfield-Tukey si la solution est valide et ne repose
    #   pas sur une combinaison signalée comme négative;
    # - sinon Satterthwaite;
    # - sinon CT si c'est la seule solution calculable.
    # ------------------------------------------------------
    selected_method <- method

    if (identical(method, "auto")) {

      ct_is_valid <- (
        !is.null(ct_result) &&
          is.finite(ct_result$F) &&
          is.finite(ct_result$p) &&
          is.finite(ct_result$df_quasi) &&
          ct_result$df_quasi > 0
      )

      # Le dénominateur Cornfield–Tukey est la reconstruction
# algébrique des composantes recherchées. Les ddl de ce
# dénominateur sont déjà approximés par Satterthwaite dans
# compute_quasi_f_numeric().
#
# La présence de coefficients négatifs n'invalide pas à elle
# seule le quasi-F. Le test est utilisable lorsque le
# dénominateur composite obtenu est positif et que ses ddl
# sont calculables.
selected_method <- if (ct_is_valid) {
  "ct"
} else {
  NA_character_
}
    }

    chosen_result <- if (
      identical(selected_method, "ct")
    ) {
      ct_result
    } else if (
      identical(selected_method, "satt")
    ) {
      satt_result
    } else {
      NULL
    }

    if (
  is.null(chosen_result) ||
  !is.finite(chosen_result$F) ||
  !is.finite(chosen_result$p) ||
  !is.finite(chosen_result$df_num) ||
  chosen_result$df_num <= 0 ||
  !is.finite(chosen_result$df_den) ||
  chosen_result$df_den <= 0
) {
      extracted$Type_test[effect_index] <-
        "Quasi-F non calculable"

      extracted$Methode[effect_index] <-
        if (is.na(selected_method)) {
          "Aucune solution numérique valide"
        } else if (identical(selected_method, "ct")) {
          "Cornfield–Tukey non calculable"
        } else {
          "Satterthwaite non calculable"
        }

      next
    }

    # ------------------------------------------------------
    # Remplacer le F absent par le quasi-F
    # ------------------------------------------------------
    extracted$F[effect_index] <-
      chosen_result$F

    extracted$p[effect_index] <-
      chosen_result$p

    extracted$df_num[effect_index] <-
  chosen_result$df_num

extracted$df_den[effect_index] <-
  chosen_result$df_den

    extracted$Methode[effect_index] <-
  "Cornfield–Tukey + ddl de Satterthwaite"
  }

  extracted
}

# ==========================================================
# MONTE-CARLO POUR UN PLAN EXPÉRIMENTAL
#
# Répète :
#   sim_data() -> compute_anova()
#
# Retourne :
#   - results : une ligne par réplication et par effet
#   - summary : résumé des MS, F, p et taux de rejet
#   - failures : réplications ayant échoué
#
# Première version :
#   les F classiques produits par compute_anova() sont recueillis;
#   les quasi-F seront calculés dans une étape ultérieure.
# ==========================================================
run_monte_carlo <- function(
    r,
    effect_sizes,
    random_variances,
    residual_variance,
    B = 100,
    seed = 20260802,
    alpha = 0.05,
    progress = TRUE,
    progress_callback = NULL
) {

  # --------------------------------------------------------
  # Validation des arguments
  # --------------------------------------------------------
  B <- as.integer(B)

  if (
    length(B) != 1L ||
    is.na(B) ||
    !is.finite(B) ||
    B < 1L
  ) {
    stop(
      "`B` doit être un entier positif.",
      call. = FALSE
    )
  }

  seed <- as.integer(seed)

  if (
    length(seed) != 1L ||
    is.na(seed) ||
    !is.finite(seed)
  ) {
    seed <- 1L
  }

  residual_variance <- as.numeric(
    residual_variance
  )

  if (
    length(residual_variance) != 1L ||
    is.na(residual_variance) ||
    !is.finite(residual_variance) ||
    residual_variance < 0
  ) {
    stop(
      "La variance résiduelle doit être un nombre positif ou nul.",
      call. = FALSE
    )
  }

  if (
    is.null(r$fcts) ||
    is.null(r$effects) ||
    is.null(r$n_levels) ||
    is.null(r$df_list) ||
    is.null(r$ems_table)
  ) {
    stop(
      "`r` ne contient pas tous les objets nécessaires.",
      call. = FALSE
    )
  }

  # --------------------------------------------------------
  # Structure du plan
  # --------------------------------------------------------
  factors <- r$fcts
  fnames  <- names(factors)

  s_nest <- if ("s" %in% fnames) {
    factors[["s"]]$nested_in %||% character(0)
  } else {
    character(0)
  }

  between_f <- intersect(
    s_nest,
    fnames
  )

  within_f <- setdiff(
    fnames,
    c("s", between_f)
  )

  # --------------------------------------------------------
  # Correspondance :
  # strate de compute_anova() -> terme EMS
  # --------------------------------------------------------
  error_names <- names(r$ems_table)[
    vapply(
      names(r$ems_table),
      function(effect_name) {
        grepl(
          "s",
          effect_name,
          fixed = TRUE
        )
      },
      logical(1)
    )
  ]

  stratum_map <- list()

  for (error_name in error_names) {

    non_subject_members <- intersect(
      setdiff(fnames, "s"),
      unlist(
        strsplit(
          gsub(
            "[^A-Za-z]",
            "",
            error_name
          ),
          ""
        )
      )
    )

    within_members <- intersect(
      non_subject_members,
      within_f
    )

    stratum_key <- if (
      length(within_members) == 0L
    ) {
      "subj_id"
    } else {
      paste(
        c(
          "subj_id",
          sort(within_members)
        ),
        collapse = ":"
      )
    }

    stratum_map[[stratum_key]] <-
      error_name
  }

  # --------------------------------------------------------
  # Extraire les statistiques d'une ANOVA
  # --------------------------------------------------------
  extract_one_anova <- function(
      anova_result,
      replication
  ) {

    if (
      is.null(anova_result) ||
      !is.null(anova_result$error) ||
      is.null(anova_result$summary)
    ) {
      return(NULL)
    }

    summ <- anova_result$summary
    rows <- list()

    for (stratum_name in names(summ)) {

      stratum <- summ[[stratum_name]]

      tbl <- if (
        is.list(stratum) &&
        !is.data.frame(stratum)
      ) {
        stratum[[1]]
      } else {
        stratum
      }

      if (
        is.null(tbl) ||
        !is.data.frame(tbl) ||
        nrow(tbl) == 0L
      ) {
        next
      }

      stratum_key <- trimws(
        sub(
          "^Error:\\s*",
          "",
          stratum_name
        )
      )

      source_names <- trimws(
        rownames(tbl)
      )

      for (i in seq_len(nrow(tbl))) {

        source_name <- source_names[i]

        effect_name <- if (
          identical(
            source_name,
            "Residuals"
          )
        ) {

          mapped_name <- stratum_map[[
            stratum_key
          ]]

          if (!is.null(mapped_name)) {
            mapped_name
          } else {
            stratum_key
          }

        } else {

          gsub(
            ":",
            "",
            source_name
          )
        }

        df_value <- if (
          "Df" %in% names(tbl)
        ) {
          as.numeric(tbl[i, "Df"])
        } else {
          NA_real_
        }

        ss_value <- if (
          "Sum Sq" %in% names(tbl)
        ) {
          as.numeric(tbl[i, "Sum Sq"])
        } else {
          NA_real_
        }

        ms_value <- if (
          "Mean Sq" %in% names(tbl)
        ) {
          as.numeric(tbl[i, "Mean Sq"])
        } else {
          NA_real_
        }

        f_value <- if (
          "F value" %in% names(tbl)
        ) {
          as.numeric(tbl[i, "F value"])
        } else {
          NA_real_
        }

        p_value <- if (
          "Pr(>F)" %in% names(tbl)
        ) {
          as.numeric(tbl[i, "Pr(>F)"])
        } else {
          NA_real_
        }

        rows[[length(rows) + 1L]] <-
          data.frame(
            Replication = replication,
            Effet = effect_name,
            Strate = stratum_key,
            df = df_value,
            SS = ss_value,
            MS = ms_value,
            F = f_value,
            p = p_value,
            stringsAsFactors = FALSE
          )
      }
    }

    if (length(rows) == 0L) {
      return(NULL)
    }

    result <- do.call(
      rbind,
      rows
    )

    rownames(result) <- NULL
    result
  }

  # --------------------------------------------------------
  # Boucle Monte-Carlo
  # --------------------------------------------------------
  simulation_results <- vector(
    mode = "list",
    length = B
  )

  failures <- vector(
    mode = "list",
    length = B
  )

  # Points d'affichage de la progression
  progress_steps <- unique(
    c(
      1L,
      seq(
        from = max(1L, floor(B / 10L)),
        to = B,
        by = max(1L, floor(B / 10L))
      ),
      B
    )
  )

  for (b in seq_len(B)) {

    replication_seed <- seed + b - 1L

    one_result <- tryCatch(
      {

        dat_b <- sim_data(
          factors = r$fcts,
          effects = r$effects,
          n_levels = r$n_levels,
          effect_sizes = effect_sizes,
          random_variances = random_variances,
          sigma2_error = residual_variance,
          seed = replication_seed
        )

        anova_b <- compute_anova(
          dat_b,
          r$fcts,
          r$effects,
          r$df_list
        )

        extracted <- extract_one_anova(
  anova_result = anova_b,
  replication = b
)

if (is.null(extracted)) {
  stop(
    "Aucun résultat ANOVA exploitable.",
    call. = FALSE
  )
}

# Ajouter les quasi-F lorsque l'effet ne possède
# pas de terme d'erreur pur.
extracted <- add_quasi_f_to_mc_replication(
  extracted = extracted,
  r = r,
  method = "auto"
)

extracted
      },
      error = function(e) {

        failures[[b]] <<- data.frame(
          Replication = b,
          Seed = replication_seed,
          Erreur = conditionMessage(e),
          stringsAsFactors = FALSE
        )

        NULL
      }
    )

    simulation_results[[b]] <- one_result

    if (
      isTRUE(progress) &&
      b %in% progress_steps
    ) {
      cat(
        sprintf(
          "Monte-Carlo %d / %d\n",
          b,
          B
        )
      )
    }
    if (
  is.function(progress_callback) &&
  (
    b == 1L ||
    b == B ||
    b %% max(1L, floor(B / 100L)) == 0L
  )
) {
  progress_callback(
    completed = b,
    total = B
  )
}
  }

  simulation_results <- Filter(
    Negate(is.null),
    simulation_results
  )

  failures <- Filter(
    Negate(is.null),
    failures
  )

  if (length(simulation_results) == 0L) {
    stop(
      "Toutes les réplications Monte-Carlo ont échoué.",
      call. = FALSE
    )
  }

  results <- do.call(
    rbind,
    simulation_results
  )

  rownames(results) <- NULL

  failure_table <- if (
    length(failures) > 0L
  ) {
    do.call(
      rbind,
      failures
    )
  } else {
    data.frame(
      Replication = integer(0),
      Seed = integer(0),
      Erreur = character(0),
      stringsAsFactors = FALSE
    )
  }

# --------------------------------------------------------
# EMS théoriques
# --------------------------------------------------------
ems_result <- compute_numeric_ems_result(
  r = r,
  effect_sizes = effect_sizes,
  random_variances = random_variances,
  residual_variance = residual_variance
)

ems_summary <- ems_result$ems$summary

theoretical_ems <- stats::setNames(
  ems_summary$EMS_numerique,
  ems_summary$Effet
)
  
  # --------------------------------------------------------
  # Résumé par effet
  # --------------------------------------------------------
  effect_names <- unique(
    results$Effet
  )

  summary_rows <- lapply(
    effect_names,
    function(effect_name) {

      dat_effect <- results[
        results$Effet == effect_name,
        ,
        drop = FALSE
      ]

      valid_ms <- is.finite(
        dat_effect$MS
      )

      valid_f <- is.finite(
        dat_effect$F
      )

      valid_p <- is.finite(
        dat_effect$p
      )

      ems_theoretical <- theoretical_ems[[effect_name]]

if (
  is.null(ems_theoretical) ||
  length(ems_theoretical) != 1L ||
  !is.finite(ems_theoretical)
) {
  ems_theoretical <- NA_real_
}

ms_mean <- if (any(valid_ms)) {
  mean(
    dat_effect$MS[valid_ms]
  )
} else {
  NA_real_
}

ms_sd <- if (sum(valid_ms) > 1L) {
  stats::sd(
    dat_effect$MS[valid_ms]
  )
} else {
  NA_real_
}

mc_standard_error <- if (
  sum(valid_ms) > 1L
) {
  ms_sd / sqrt(sum(valid_ms))
} else {
  NA_real_
}

difference <- if (
  is.finite(ms_mean) &&
  is.finite(ems_theoretical)
) {
  ms_mean - ems_theoretical
} else {
  NA_real_
}

standardized_difference <- if (
  is.finite(difference) &&
  is.finite(mc_standard_error) &&
  mc_standard_error > 0
) {
  difference / mc_standard_error
} else {
  NA_real_
}

data.frame(
  Effet = effect_name,

  EMS_theorique = ems_theoretical,

  N_MS = sum(valid_ms),

  MS_moyen = ms_mean,

  MS_ecart_type = ms_sd,

  Erreur_MC = mc_standard_error,

  Ecart = difference,

  Ecart_standardise = standardized_difference,

  MS_mediane = if (
    any(valid_ms)
  ) {
    stats::median(
      dat_effect$MS[valid_ms]
    )
  } else {
    NA_real_
  },

  F_moyen = if (
    any(valid_f)
  ) {
    mean(
      dat_effect$F[valid_f]
    )
  } else {
    NA_real_
  },

  Taux_rejet = if (
    any(valid_p)
  ) {
    mean(
      dat_effect$p[valid_p] < alpha
    )
  } else {
    NA_real_
  },

  N_F_valide = sum(valid_f),

  stringsAsFactors = FALSE
)
    }
  )

  summary_table <- do.call(
    rbind,
    summary_rows
  )

  rownames(summary_table) <- NULL

  # Respecter autant que possible l'ordre des EMS
  ems_order <- names(
    r$ems_table
  )

  summary_table$Ordre <- match(
    summary_table$Effet,
    ems_order
  )

  summary_table <- summary_table[
    order(
      is.na(summary_table$Ordre),
      summary_table$Ordre
    ),
    ,
    drop = FALSE
  ]

  summary_table$Ordre <- NULL
  rownames(summary_table) <- NULL

  # --------------------------------------------------------
  # Objet retourné
  # --------------------------------------------------------
  list(
    results = results,
    summary = summary_table,
    failures = failure_table,
    B_requested = B,
    B_completed = length(
      unique(results$Replication)
    ),
    alpha = alpha,
    seed = seed,
    effect_sizes = effect_sizes,
    random_variances = random_variances,
    residual_variance = residual_variance
  )
}

# ============================================================
# VALIDATION DU MOTEUR DE SIMULATION
#
# Vérifie :
#   1. le niveau nominal sous H0;
#   2. chaque effet fixe activé isolément;
#   3. l'absence de contamination entre effets fixes;
#   4. la concordance entre EMS théoriques et MS moyens;
#   5. facultativement, chaque composante aléatoire.
#
# Dépendances :
#   - run_monte_carlo()
#   - structure produite par prepare_design_result()
#
# OUTILS DE VALIDATION DU MOTEUR
#
# Développement uniquement
# Non utilisés par l'application Shiny
# ============================================================

validate_simulation_engine <- function(
    r,
    B = 1000L,
    seed = 1234L,
    alpha = 0.05,
    active_value = 1,
    residual_variance = 1,
    test_fixed = TRUE,
    test_random = FALSE,
    effects_to_test = NULL,
    random_terms_to_test = NULL,
    nominal_z = 3,
    power_moderate = 0.50,
    power_high = 0.80,
    ems_z_limit = 3,
    verbose = TRUE,
    progress = TRUE
) {

  # ==========================================================
  # 1. Vérifications générales
  # ==========================================================

  if (
    is.null(r) ||
    is.null(r$fcts) ||
    is.null(r$effects)
  ) {
    stop(
      paste(
        "`r` doit être un résultat valide produit par",
        "`prepare_design_result()`."
      ),
      call. = FALSE
    )
  }

  B <- as.integer(B)
  seed <- as.integer(seed)

  if (
    length(B) != 1L ||
    is.na(B) ||
    B < 10L
  ) {
    stop(
      "`B` doit être un entier d'au moins 10.",
      call. = FALSE
    )
  }

  if (
    length(seed) != 1L ||
    is.na(seed)
  ) {
    stop(
      "`seed` doit être un entier valide.",
      call. = FALSE
    )
  }

  if (
    length(alpha) != 1L ||
    !is.finite(alpha) ||
    alpha <= 0 ||
    alpha >= 1
  ) {
    stop(
      "`alpha` doit être compris strictement entre 0 et 1.",
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

  if (
    length(active_value) != 1L ||
    !is.finite(active_value) ||
    active_value < 0
  ) {
    stop(
      "`active_value` doit être positif ou nul.",
      call. = FALSE
    )
  }

  if (
    length(nominal_z) != 1L ||
    !is.finite(nominal_z) ||
    nominal_z <= 0
  ) {
    stop(
      "`nominal_z` doit être strictement positif.",
      call. = FALSE
    )
  }

  # ==========================================================
  # 2. Identifier les catégories d'effets
  # ==========================================================

  effect_names <- names(r$effects)

  error_terms <- effect_names[
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

  testable_effects <- setdiff(
    effect_names,
    error_terms
  )

  # ----------------------------------------------------------
  # Effets fixes simulables
  # ----------------------------------------------------------

  fixed_effects_all <- testable_effects[
    vapply(
      testable_effects,
      function(effect_name) {

        effect_info <- r$effects[[effect_name]]

        non_subject_factors <- setdiff(
          effect_info$factors,
          "s"
        )

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
  # Composantes aléatoires structurelles
  # ----------------------------------------------------------

  random_terms_all <- effect_names[
    vapply(
      effect_names,
      function(effect_name) {

        effect_info <- r$effects[[effect_name]]

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

  random_terms_non_residual <- setdiff(
    random_terms_all,
    residual_term
  )

  # ==========================================================
  # 3. Limiter facultativement les paramètres testés
  # ==========================================================

  fixed_effects <- fixed_effects_all

  if (!is.null(effects_to_test)) {

    unknown_fixed <- setdiff(
      effects_to_test,
      fixed_effects_all
    )

    if (length(unknown_fixed) > 0L) {
      stop(
        paste(
          "Effets fixes inconnus :",
          paste(unknown_fixed, collapse = ", "),
          "\nEffets fixes disponibles :",
          paste(fixed_effects_all, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    fixed_effects <- effects_to_test
  }

  random_terms <- random_terms_non_residual

  if (!is.null(random_terms_to_test)) {

    unknown_random <- setdiff(
      random_terms_to_test,
      random_terms_non_residual
    )

    if (length(unknown_random) > 0L) {
      stop(
        paste(
          "Composantes aléatoires inconnues :",
          paste(unknown_random, collapse = ", "),
          "\nComposantes disponibles :",
          paste(random_terms_non_residual, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    random_terms <- random_terms_to_test
  }

  # ==========================================================
  # 4. Vecteurs de paramètres nuls
  #
  # Tous les effets fixes simulables sont transmis, même si
  # seulement certains d'entre eux sont activés dans les tests.
  # ==========================================================

  zero_effect_sizes <- stats::setNames(
    rep(
      0,
      length(fixed_effects_all)
    ),
    fixed_effects_all
  )

  all_random_terms <- unique(
    c(
      random_terms_non_residual,
      residual_term
    )
  )

  zero_random_variances <- stats::setNames(
    rep(
      0,
      length(all_random_terms)
    ),
    all_random_terms
  )

  # ==========================================================
  # 5. Construire les scénarios
  # ==========================================================

  scenarios <- list()

  scenarios[["H0_complete"]] <- list(
    Scenario = "H0 complète",
    Type = "Nul",
    Parametre_actif = "Aucun",
    effect_sizes = zero_effect_sizes,
    random_variances = zero_random_variances
  )

  if (
    isTRUE(test_fixed) &&
    length(fixed_effects) > 0L
  ) {

    for (effect_name in fixed_effects) {

      effect_values <- zero_effect_sizes
      effect_values[[effect_name]] <- active_value

      scenario_id <- paste0(
        "fixe_",
        gsub(
          "[^A-Za-z0-9]",
          "_",
          effect_name
        )
      )

      scenarios[[scenario_id]] <- list(
        Scenario = paste0(
          "Effet fixe ",
          effect_name,
          " = ",
          active_value
        ),
        Type = "Effet fixe",
        Parametre_actif = effect_name,
        effect_sizes = effect_values,
        random_variances = zero_random_variances
      )
    }
  }

  if (
    isTRUE(test_random) &&
    length(random_terms) > 0L
  ) {

    for (term_name in random_terms) {

      variance_values <- zero_random_variances
      variance_values[[term_name]] <- active_value

      scenario_id <- paste0(
        "aleatoire_",
        gsub(
          "[^A-Za-z0-9]",
          "_",
          term_name
        )
      )

      scenarios[[scenario_id]] <- list(
        Scenario = paste0(
          "Variance ",
          term_name,
          " = ",
          active_value
        ),
        Type = "Composante aléatoire",
        Parametre_actif = term_name,
        effect_sizes = zero_effect_sizes,
        random_variances = variance_values
      )
    }
  }

  # ==========================================================
  # 6. Limites Monte-Carlo pour le niveau nominal
  # ==========================================================

  mc_se_alpha <- sqrt(
    alpha * (1 - alpha) / B
  )

  alpha_lower <- max(
    0,
    alpha - nominal_z * mc_se_alpha
  )

  alpha_upper <- min(
    1,
    alpha + nominal_z * mc_se_alpha
  )

  # ==========================================================
  # 7. Exécuter un scénario
  # ==========================================================

  run_one_scenario <- function(
      scenario,
      scenario_index
  ) {

    if (isTRUE(verbose)) {

      cat(
        "\n",
        paste(rep("=", 70), collapse = ""),
        "\n",
        scenario$Scenario,
        "\n",
        paste(rep("=", 70), collapse = ""),
        "\n",
        sep = ""
      )

      cat("\nEffets fixes :\n")
      print(scenario$effect_sizes)

      cat("\nComposantes aléatoires :\n")
      print(scenario$random_variances)

      cat(
        "\nVariance résiduelle : ",
        residual_variance,
        "\n\n",
        sep = ""
      )
    }

    scenario_seed <- seed +
      (scenario_index - 1L) * (B + 1000L)

    mc <- run_monte_carlo(
      r = r,
      effect_sizes = scenario$effect_sizes,
      random_variances = scenario$random_variances,
      residual_variance = residual_variance,
      B = B,
      seed = scenario_seed,
      alpha = alpha,
      progress = progress
    )

    if (
      is.null(mc) ||
      is.null(mc$summary) ||
      !is.data.frame(mc$summary)
    ) {
      stop(
        paste(
          "Le scénario",
          shQuote(scenario$Scenario),
          "n'a pas produit de résumé Monte-Carlo."
        ),
        call. = FALSE
      )
    }

    tbl <- mc$summary

    numeric_columns <- intersect(
      c(
        "EMS_theorique",
        "MS_moyen",
        "Ecart_standardise",
        "N_F_valide",
        "F_moyen",
        "Taux_rejet"
      ),
      names(tbl)
    )

    tbl[numeric_columns] <- lapply(
      tbl[numeric_columns],
      function(x) {
        suppressWarnings(
          as.numeric(
            as.character(x)
          )
        )
      }
    )

    tbl$Scenario <- scenario$Scenario
    tbl$Type_scenario <- scenario$Type
    tbl$Parametre_actif <- scenario$Parametre_actif
    tbl$B <- B
    tbl$Alpha <- alpha
    tbl$Borne_alpha_inf <- alpha_lower
    tbl$Borne_alpha_sup <- alpha_upper

    # --------------------------------------------------------
    # Catégorie structurelle de chaque ligne
    # --------------------------------------------------------

    tbl$Nature_effet <- ifelse(
      tbl$Effet %in% error_terms,
      "Terme d'erreur",
      ifelse(
        tbl$Effet %in% fixed_effects_all,
        "Effet fixe",
        "Effet aléatoire ou mixte"
      )
    )

    # --------------------------------------------------------
    # Rôle attendu dans le scénario
    # --------------------------------------------------------

    tbl$Role_attendu <- vapply(
      seq_len(nrow(tbl)),
      function(i) {

        effect_name <- tbl$Effet[i]
        nature <- tbl$Nature_effet[i]

        if (identical(nature, "Terme d'erreur")) {
          return("Terme d'erreur")
        }

        if (identical(scenario$Type, "Nul")) {
          return("Effet nul")
        }

        if (identical(scenario$Type, "Effet fixe")) {

          if (identical(
            effect_name,
            scenario$Parametre_actif
          )) {
            return("Effet actif")
          }

          return("Effet nul")
        }

        "Variance aléatoire active — inspection des EMS"
      },
      character(1)
    )

    # --------------------------------------------------------
    # Concordance EMS théorique / MS moyen
    # --------------------------------------------------------

    tbl$Diagnostic_EMS <- vapply(
      seq_len(nrow(tbl)),
      function(i) {

        z_value <- tbl$Ecart_standardise[i]

        if (!is.finite(z_value)) {
          return("Non disponible")
        }

        if (abs(z_value) <= ems_z_limit) {
          return("EMS reproduite")
        }

        "Écart EMS à examiner"
      },
      character(1)
    )

    # --------------------------------------------------------
    # Diagnostic statistique
    # --------------------------------------------------------

    tbl$Diagnostic <- vapply(
      seq_len(nrow(tbl)),
      function(i) {

        role <- tbl$Role_attendu[i]
        rejection_rate <- tbl$Taux_rejet[i]

        if (identical(role, "Terme d'erreur")) {
          return("Aucun test F attendu")
        }

        if (
          identical(
            role,
            "Variance aléatoire active — inspection des EMS"
          )
        ) {
          return("Inspection des EMS seulement")
        }

        if (!is.finite(rejection_rate)) {
          return("Test non calculable")
        }

        if (identical(role, "Effet nul")) {

          if (
            rejection_rate >= alpha_lower &&
            rejection_rate <= alpha_upper
          ) {
            return("Niveau nominal compatible")
          }

          if (rejection_rate > alpha_upper) {
            return("Taux de rejet trop élevé")
          }

          return("Test conservateur")
        }

        if (identical(role, "Effet actif")) {

          if (rejection_rate >= power_high) {
            return("Puissance élevée")
          }

          if (rejection_rate >= power_moderate) {
            return("Puissance modérée")
          }

          return("Puissance faible")
        }

        "Inspection requise"
      },
      character(1)
    )

    if (isTRUE(verbose)) {

      display_columns <- intersect(
        c(
          "Effet",
          "EMS_theorique",
          "MS_moyen",
          "Ecart_standardise",
          "N_F_valide",
          "F_moyen",
          "Taux_rejet",
          "Role_attendu",
          "Diagnostic_EMS",
          "Diagnostic"
        ),
        names(tbl)
      )

      print(
        tbl[
          ,
          display_columns,
          drop = FALSE
        ],
        row.names = FALSE
      )
    }

    list(
      scenario = scenario,
      seed = scenario_seed,
      mc = mc,
      summary = tbl
    )
  }

  # ==========================================================
  # 8. Exécuter tous les scénarios
  # ==========================================================

  scenario_results <- vector(
    mode = "list",
    length = length(scenarios)
  )

  names(scenario_results) <- names(scenarios)

  for (i in seq_along(scenarios)) {

    scenario_results[[i]] <- run_one_scenario(
      scenario = scenarios[[i]],
      scenario_index = i
    )
  }

  # ==========================================================
  # 9. Tableau long
  # ==========================================================

  summary_long <- do.call(
    rbind,
    lapply(
      scenario_results,
      function(x) {
        x$summary
      }
    )
  )

  rownames(summary_long) <- NULL

  preferred_columns <- c(
    "Scenario",
    "Type_scenario",
    "Parametre_actif",
    "Effet",
    "Nature_effet",
    "Role_attendu",
    "EMS_theorique",
    "MS_moyen",
    "Ecart_standardise",
    "Diagnostic_EMS",
    "N_F_valide",
    "F_moyen",
    "Taux_rejet",
    "Borne_alpha_inf",
    "Borne_alpha_sup",
    "Diagnostic",
    "B",
    "Alpha"
  )

  summary_long <- summary_long[
    ,
    c(
      intersect(
        preferred_columns,
        names(summary_long)
      ),
      setdiff(
        names(summary_long),
        preferred_columns
      )
    ),
    drop = FALSE
  ]

  # ==========================================================
  # 10. Tableau compact des taux de rejet
  # ==========================================================

  compact_rows <- lapply(
    scenario_results,
    function(result_item) {

      tbl <- result_item$summary

      rates <- stats::setNames(
        tbl$Taux_rejet,
        tbl$Effet
      )

      row <- data.frame(
        Scenario = tbl$Scenario[1],
        Type = tbl$Type_scenario[1],
        Parametre_actif = tbl$Parametre_actif[1],
        stringsAsFactors = FALSE
      )

      for (effect_name in names(rates)) {
        row[[effect_name]] <- rates[[effect_name]]
      }

      row
    }
  )

  summary_compact <- do.call(
    rbind,
    compact_rows
  )

  rownames(summary_compact) <- NULL

  # ==========================================================
  # 11. Anomalies réelles
  #
  # Les termes d'erreur et les scénarios aléatoires ne sont
  # pas considérés comme des anomalies statistiques.
  # ==========================================================

  statistical_anomalies <- summary_long[
    summary_long$Diagnostic %in% c(
      "Taux de rejet trop élevé",
      "Test conservateur",
      "Test non calculable"
    ) &
      summary_long$Nature_effet != "Terme d'erreur",
    ,
    drop = FALSE
  ]

  ems_anomalies <- summary_long[
    summary_long$Diagnostic_EMS ==
      "Écart EMS à examiner",
    ,
    drop = FALSE
  ]

  contamination <- summary_long[
    summary_long$Type_scenario == "Effet fixe" &
      summary_long$Role_attendu == "Effet nul" &
      summary_long$Diagnostic %in% c(
        "Taux de rejet trop élevé",
        "Test conservateur"
      ),
    ,
    drop = FALSE
  ]

  # ==========================================================
  # 12. Résumé global
  # ==========================================================

  h0_rows <- summary_long[
    summary_long$Type_scenario == "Nul" &
      summary_long$Role_attendu == "Effet nul",
    ,
    drop = FALSE
  ]

  h0_valid <- nrow(h0_rows) > 0L &&
    all(
      h0_rows$Diagnostic ==
        "Niveau nominal compatible"
    )

  fixed_null_rows <- summary_long[
    summary_long$Type_scenario == "Effet fixe" &
      summary_long$Role_attendu == "Effet nul",
    ,
    drop = FALSE
  ]

  no_fixed_contamination <-
    nrow(fixed_null_rows) == 0L ||
    all(
      fixed_null_rows$Diagnostic ==
        "Niveau nominal compatible"
    )

  ems_valid <- nrow(ems_anomalies) == 0L

  validation_passed <-
    h0_valid &&
    no_fixed_contamination &&
    ems_valid &&
    nrow(statistical_anomalies) == 0L

  result <- structure(
    list(
      design = r,

      settings = list(
        B = B,
        seed = seed,
        alpha = alpha,
        active_value = active_value,
        residual_variance = residual_variance,
        nominal_z = nominal_z,
        nominal_interval = c(
          lower = alpha_lower,
          upper = alpha_upper
        ),
        power_moderate = power_moderate,
        power_high = power_high,
        ems_z_limit = ems_z_limit
      ),

      fixed_effects = fixed_effects_all,
      random_terms = random_terms_non_residual,
      error_terms = error_terms,
      residual_term = residual_term,

      scenarios = scenarios,
      results = scenario_results,

      summary = summary_long,
      compact = summary_compact,

      statistical_anomalies = statistical_anomalies,
      ems_anomalies = ems_anomalies,
      contamination = contamination,

      checks = list(
        h0_level_valid = h0_valid,
        no_fixed_contamination =
          no_fixed_contamination,
        ems_valid = ems_valid
      ),

      passed = validation_passed
    ),
    class = "expdesign_validation"
  )

  if (isTRUE(verbose)) {
    print(result)
  }

  result
}


# ==========================================================
# Calculer les EMS numériques
# ==========================================================
compute_numeric_ems_result <- function(
    r,
    effect_sizes,
    random_variances,
    residual_variance
) {

  fixed_parameters <- list()

  for (effect_name in names(effect_sizes)) {

    effect_info <- r$effects[[effect_name]]

    if (is.null(effect_info)) {
      next
    }

    fixed_parameters[[paste0("phi", effect_name)]] <-
      compute_fixed_phi(
        effect_name = effect_name,
        effect_info = effect_info,
        effect_size = as.numeric(effect_sizes[[effect_name]]),
        n_levels = r$n_levels
      )
  }

  random_parameters <- lapply(
    random_variances,
    as.numeric
  )

  error_terms <- names(r$effects)[
    vapply(
      r$effects,
      function(x) isTRUE(x$is_error),
      logical(1)
    )
  ]

  residual_term <- if (length(error_terms) > 0L) {
    tail(error_terms, 1L)
  } else {
    character(0)
  }

  if (
    length(residual_term) == 1L &&
    nzchar(residual_term)
  ) {
    random_parameters[[residual_term]] <-
      as.numeric(residual_variance)
  }

  parameters <- c(
    fixed_parameters,
    random_parameters
  )

  ems_numeric <- evaluate_ems(
    ems_table = r$ems_table,
    parameters = parameters,
    n_levels = r$n_levels
  )

  list(
    parameters = parameters,
    ems = ems_numeric,
    residual_term = residual_term
  )
}

# ==========================================================
# Préparer tous les objets associés à un plan expérimental
# ==========================================================
prepare_design_result <- function(
    design_str,
    n_levels = NULL,
    default_n_levels = 3L
) {

  if (
    length(design_str) != 1L ||
    is.na(design_str) ||
    !nzchar(trimws(design_str))
  ) {
    stop(
      "Le plan expérimental doit être fourni.",
      call. = FALSE
    )
  }

  # --------------------------------------------------------
  # Facteurs du plan
  # --------------------------------------------------------
  fcts <- parse_design(
    design_str
  )

  factor_names <- names(fcts)

  # --------------------------------------------------------
  # Nombre de niveaux
  # --------------------------------------------------------
  if (is.null(n_levels)) {

    n_levels <- stats::setNames(
      rep(
        as.integer(default_n_levels),
        length(factor_names)
      ),
      factor_names
    )

  } else {

    n_levels <- unlist(
      n_levels,
      use.names = TRUE
    )

    missing_factors <- setdiff(
      factor_names,
      names(n_levels)
    )

    if (length(missing_factors) > 0L) {

      n_levels <- c(
        n_levels,
        stats::setNames(
          rep(
            as.integer(default_n_levels),
            length(missing_factors)
          ),
          missing_factors
        )
      )
    }

    n_levels <- n_levels[
      factor_names
    ]

    n_levels <- stats::setNames(
      lapply(
        n_levels,
        function(value) {

          value <- as.integer(value)

          if (
            length(value) != 1L ||
            is.na(value) ||
            value < 1L
          ) {
            as.integer(default_n_levels)
          } else {
            value
          }
        }
      ),
      factor_names
    )
  }

  # --------------------------------------------------------
  # Structure complète du plan
  # --------------------------------------------------------
  effects <- generate_effects(
    fcts
  )

  ems_table <- build_ems_table(
    fcts,
    effects
  )

  error_info <- find_error_terms(
    fcts,
    effects,
    ems_table
  )

  df_list <- compute_df(
    fcts,
    effects,
    n_levels
  )

  verify <- verify_df(
    fcts,
    effects,
    n_levels,
    df_list
  )

  quasi_list <- stats::setNames(
    lapply(
      names(error_info$quasi),
      function(effect_name) {

        build_quasi_f(
          effect_name,
          fcts,
          effects,
          ems_table,
          df_list,
          n_levels
        )
      }
    ),
    names(error_info$quasi)
  )
  
# ==========================================================
# Calculer les EMS numériques à partir des paramètres
# du modèle générateur
# ==========================================================
compute_numeric_ems_result <- function(
    r,
    effect_sizes,
    random_variances,
    residual_variance
) {

  if (
    is.null(r$effects) ||
    is.null(r$ems_table) ||
    is.null(r$n_levels)
  ) {
    stop(
      "`r` ne contient pas les éléments nécessaires au calcul des EMS.",
      call. = FALSE
    )
  }

  # --------------------------------------------------------
  # Contributions fixes phi(...)
  # --------------------------------------------------------
  fixed_parameters <- list()

  for (effect_name in names(effect_sizes)) {

    effect_info <- r$effects[[effect_name]]

    if (is.null(effect_info)) {
      next
    }

    phi_value <- compute_fixed_phi(
      effect_name = effect_name,
      effect_info = effect_info,
      effect_size = as.numeric(
        effect_sizes[[effect_name]]
      ),
      n_levels = r$n_levels
    )

    fixed_parameters[[
      paste0(
        "phi",
        effect_name
      )
    ]] <- phi_value
  }

  # --------------------------------------------------------
  # Composantes de variance
  # --------------------------------------------------------
  random_parameters <- lapply(
    random_variances,
    as.numeric
  )

  # Identifier le terme résiduel
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

  if (
    length(residual_term) == 1L &&
    nzchar(residual_term)
  ) {
    random_parameters[[
      residual_term
    ]] <- as.numeric(
      residual_variance
    )
  }

  parameters <- c(
    fixed_parameters,
    random_parameters
  )

  ems_numeric <- evaluate_ems(
    ems_table = r$ems_table,
    parameters = parameters,
    n_levels = r$n_levels
  )

  list(
    parameters = parameters,
    ems = ems_numeric,
    residual_term = residual_term
  )
}
  
  

  list(
    fcts = fcts,
    effects = effects,
    ems_table = ems_table,
    error_info = error_info,
    df_list = df_list,
    verify = verify,
    quasi_list = quasi_list,
    n_levels = n_levels
  )
}

# ==========================================================================
# VALIDATION INTERNE DES EMS
#
# Les fonctions détaillées de validation demeurent dans
# validation_plans.R. Elles sont chargées automatiquement dans
# l'environnement de global.R, quel que soit le sous-répertoire
# dans lequel le fichier est rangé.
# ==========================================================================

load_validation_functions <- function(
    filename = "validation_plans.R",
    verbose = TRUE
) {

  # Chercher d'abord dans le répertoire courant, puis récursivement.
  candidates <- unique(
    c(
      filename,
      list.files(
        path = ".",
        pattern = paste0(
          "^",
          gsub("\\.", "\\\\.", filename),
          "$"
        ),
        recursive = TRUE,
        full.names = TRUE
      )
    )
  )

  candidates <- candidates[
    file.exists(candidates)
  ]

  if (length(candidates) == 0L) {
    stop(
      paste0(
        "Le fichier `",
        filename,
        "` est introuvable dans le projet."
      ),
      call. = FALSE
    )
  }

  validation_file <- normalizePath(
    candidates[1],
    winslash = "/",
    mustWork = TRUE
  )

  # Charger les fonctions dans l'environnement global,
  # comme si elles figuraient directement dans global.R.
  sys.source(
    validation_file,
    envir = .GlobalEnv
  )

  required_functions <- c(
    "validate_plan_ems",
    "validate_all_reference_plans"
  )

  missing_functions <- required_functions[
    !vapply(
      required_functions,
      exists,
      logical(1),
      envir = .GlobalEnv,
      inherits = FALSE,
      mode = "function"
    )
  ]

  if (length(missing_functions) > 0L) {
    stop(
      paste0(
        "Le fichier de validation a été chargé, mais les fonctions ",
        "suivantes sont absentes : ",
        paste(missing_functions, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (isTRUE(verbose)) {
    message(
      "Fonctions de validation chargées depuis : ",
      validation_file
    )
  }

  invisible(validation_file)
}


# --------------------------------------------------------------------------
# Test interne général
# --------------------------------------------------------------------------
run_internal_validation <- function(
    verbose = TRUE,
    stop_on_failure = FALSE
) {

  # Charger les fonctions si elles ne sont pas encore disponibles.
  if (
    !exists(
      "validate_plan_ems",
      envir = .GlobalEnv,
      inherits = FALSE,
      mode = "function"
    ) ||
    !exists(
      "validate_all_reference_plans",
      envir = .GlobalEnv,
      inherits = FALSE,
      mode = "function"
    )
  ) {
    load_validation_functions(
      verbose = verbose
    )
  }

  if (isTRUE(verbose)) {
    cat(
      "\n",
      "=========================================\n",
      " Validation interne ExpDesign2\n",
      "=========================================\n\n",
      sep = ""
    )
  }

  validation <- validate_all_reference_plans(
    verbose = FALSE
  )

  if (
    !is.data.frame(validation) ||
    !"Valide" %in% names(validation)
  ) {
    stop(
      paste(
        "`validate_all_reference_plans()` n'a pas retourné",
        "le tableau attendu."
      ),
      call. = FALSE
    )
  }

  n_total <- nrow(validation)

  n_valides <- sum(
    validation$Valide %in% TRUE,
    na.rm = TRUE
  )

  failed <- validation[
    is.na(validation$Valide) |
      !validation$Valide,
    ,
    drop = FALSE
  ]

  all_valid <- nrow(failed) == 0L

  if (isTRUE(verbose)) {

    print(
      validation,
      row.names = FALSE
    )

    cat("\n")

    if (all_valid) {

      cat(
        sprintf(
          "✓ %d/%d plans de référence validés.\n",
          n_valides,
          n_total
        )
      )

      cat(
        "Aucune divergence EMS détectée.\n"
      )

    } else {

      cat(
        sprintf(
          "✗ %d/%d plans de référence validés.\n",
          n_valides,
          n_total
        )
      )

      cat(
        sprintf(
          "%d plan(s) présentent une divergence ou une erreur.\n",
          nrow(failed)
        )
      )
    }
  }

  if (
    !all_valid &&
    isTRUE(stop_on_failure)
  ) {
    stop(
      sprintf(
        "Validation interne échouée pour %d plan(s).",
        nrow(failed)
      ),
      call. = FALSE
    )
  }

  invisible(
    list(
      valid = all_valid,
      n_valides = n_valides,
      n_total = n_total,
      results = validation,
      failures = failed
    )
  )
}

#############################################################
# VALIDATION
#############################################################
# ==========================================================
# PLANS DE RÉFÉRENCE POUR LA VALIDATION DES EMS
# ==========================================================

ems_reference_plans <- data.frame(
  plan = c(
    "A, B",
    "A, b",
    "A, B, s(A)",
    "A, b, s(A)",
    "A, B, c, s(A)",
    "A, b, c, s(A)",
    "A, B, C, s(AB)",
    "A, b, C, s(A)"
  ),

  categorie = c(
    "Inter-sujets",
    "Inter-sujets aléatoire",
    "Mixte",
    "Mixte aléatoire",
    "Mixte + facteur aléatoire",
    "Deux facteurs aléatoires",
    "Sujet imbriqué dans AB",
    "Plan complexe"
  ),

  stringsAsFactors = FALSE
)

# ==========================================================
# VALIDATION INDÉPENDANTE DES EMS AVEC afex::ems()
#
# Requiert dans global.R :
#   convert_afex_ems()
#   compare_ems_tables()
#   validate_plan_ems()
# ==========================================================

validate_plans_with_afex <- function(
    plans = ems_reference_plans,
    verbose = TRUE
) {

  if (is.character(plans)) {

    plans <- data.frame(
      plan = plans,
      categorie = rep(
        "Non classé",
        length(plans)
      ),
      stringsAsFactors = FALSE
    )
  }

  if (
    !is.data.frame(plans) ||
    !"plan" %in% names(plans)
  ) {
    stop(
      "`plans` doit être un vecteur de plans ou un data.frame contenant une colonne `plan`.",
      call. = FALSE
    )
  }

  if (!"categorie" %in% names(plans)) {
    plans$categorie <- "Non classé"
  }

  details <- vector(
    mode = "list",
    length = nrow(plans)
  )

  summary_rows <- vector(
    mode = "list",
    length = nrow(plans)
  )

  for (i in seq_len(nrow(plans))) {

    current_plan <- plans$plan[i]

    validation <- tryCatch(
      validate_plan_ems(
        design = current_plan,
        verbose = FALSE
      ),
      error = function(e) e
    )

    if (inherits(validation, "error")) {

      details[[i]] <- validation

      summary_rows[[i]] <- data.frame(
        Plan = current_plan,
        Categorie = plans$categorie[i],
        Effets = NA_integer_,
        Divergences = NA_integer_,
        Valide = FALSE,
        Erreur = conditionMessage(validation),
        stringsAsFactors = FALSE
      )

    } else {

      details[[i]] <- validation

      summary_rows[[i]] <- data.frame(
        Plan = current_plan,
        Categorie = plans$categorie[i],
        Effets = length(validation$effects),
        Divergences = nrow(validation$differences),
        Valide = isTRUE(validation$valid),
        Erreur = "",
        stringsAsFactors = FALSE
      )
    }
  }

  names(details) <- plans$plan

  summary_table <- do.call(
    rbind,
    summary_rows
  )

  rownames(summary_table) <- NULL

  if (isTRUE(verbose)) {

    cat(
      "\n",
      "Validation indépendante des EMS avec afex\n",
      "=========================================\n\n",
      sep = ""
    )

    print(
      summary_table,
      row.names = FALSE
    )

    n_valid <- sum(
      summary_table$Valide,
      na.rm = TRUE
    )

    n_total <- nrow(summary_table)

    cat("\n")

    if (n_valid == n_total) {

      cat(
        sprintf(
          "✓ %d/%d plans validés.\n",
          n_valid,
          n_total
        )
      )

      cat("Aucune divergence détectée.\n")

    } else {

      cat(
        sprintf(
          "✗ %d/%d plans validés.\n",
          n_valid,
          n_total
        )
      )
    }
  }

  invisible(
    list(
      valid = all(summary_table$Valide),
      summary = summary_table,
      details = details
    )
  )
}


# ==========================================================
# VALIDATION MONTE-CARLO DU GÉNÉRATEUR DE DONNÉES
#
# Requiert dans global.R :
#   validate_generator_mc()
#   evaluate_ems()
# ==========================================================
# ==========================================================
# ANCIENNE VALIDATION MONTE-CARLO
# (conservée temporairement)
# ==========================================================
validate_plan_mc <- function(
    design,
    n_levels,
    random_variances,
    residual_variance = 1,
    B = 100,
    seed = 20260801,
    tolerance_z = 3,
    tolerance_absolute = 1e-8,
    verbose = TRUE
) {

  # --------------------------------------------------------
  # Simulation Monte-Carlo
  # --------------------------------------------------------
  mc <- validate_generator_mc(
    design = design,
    n_levels = n_levels,
    random_variances = random_variances,
    residual_variance = residual_variance,
    B = B,
    seed = seed
  )

  # --------------------------------------------------------
  # Paramètres théoriques sous H0
  #
  # Tous les effets fixes ont une contribution phi = 0.
  # --------------------------------------------------------
  parameters <- list()

  ems_terms <- unique(
    unlist(
      lapply(
        mc$ems_table,
        names
      ),
      use.names = FALSE
    )
  )

  fixed_terms <- ems_terms[
    vapply(
      ems_terms,
      function(term_name) {

        entries <- lapply(
          mc$ems_table,
          function(ems_row) {
            ems_row[[term_name]]
          }
        )

        entries <- Filter(
          Negate(is.null),
          entries
        )

        any(
          vapply(
            entries,
            function(entry) {
              isTRUE(entry$is_fixed)
            },
            logical(1)
          )
        )
      },
      logical(1)
    )
  ]

  for (term_name in fixed_terms) {
    parameters[[paste0("phi", term_name)]] <- 0
  }

  for (term_name in names(random_variances)) {
    parameters[[term_name]] <- as.numeric(
      random_variances[[term_name]]
    )
  }

  # Le terme résiduel est le dernier terme d'erreur.
  residual_candidates <- setdiff(
    names(mc$ems_table),
    c(
      names(random_variances),
      fixed_terms
    )
  )

  # Chercher le dernier terme ayant une EMS propre de coefficient 1.
  residual_term <- tail(
    names(mc$ems_table),
    1
  )

  parameters[[residual_term]] <- as.numeric(
    residual_variance
  )

  # --------------------------------------------------------
  # EMS numériques théoriques
  # --------------------------------------------------------
  theoretical <- evaluate_ems(
    ems_table = mc$ems_table,
    parameters = parameters,
    n_levels = n_levels
  )

  # --------------------------------------------------------
  # Comparaison
  # --------------------------------------------------------
  comparison <- merge(
    theoretical$summary,
    mc$summary,
    by = "Effet",
    sort = FALSE
  )

  comparison$Ecart <-
    comparison$MS_moyen -
    comparison$EMS_numerique

  comparison$Erreur_MC <-
    comparison$MS_ecart_type /
    sqrt(comparison$N_valide)

  comparison$Ecart_standardise <- ifelse(
    comparison$Erreur_MC > tolerance_absolute,
    comparison$Ecart / comparison$Erreur_MC,
    ifelse(
      abs(comparison$Ecart) <= tolerance_absolute,
      0,
      Inf
    )
  )

  comparison$Compatible_MC <- (
    abs(comparison$Ecart) <= tolerance_absolute
  ) | (
    abs(comparison$Ecart_standardise) <= tolerance_z
  )

  is_valid <- all(
    comparison$Compatible_MC,
    na.rm = TRUE
  )

  if (isTRUE(verbose)) {

    cat(
      "\n",
      "Validation Monte-Carlo du générateur\n",
      "====================================\n",
      "Plan             : ", design, "\n",
      "Répétitions      : ", B, "\n",
      "Graine           : ", seed, "\n",
      "Seuil |z|        : ", tolerance_z, "\n",
      "Divergences      : ",
      sum(!comparison$Compatible_MC, na.rm = TRUE),
      "\n",
      "Verdict          : ",
      if (is_valid) {
        "COMPATIBLE AVEC LES EMS"
      } else {
        "DIVERGENCES DÉTECTÉES"
      },
      "\n\n",
      sep = ""
    )

    print(
      comparison,
      row.names = FALSE
    )
  }

  invisible(
    list(
      valid = is_valid,
      design = design,
      parameters = parameters,
      theoretical = theoretical,
      monte_carlo = mc,
      comparison = comparison,
      differences = comparison[
        !comparison$Compatible_MC,
        ,
        drop = FALSE
      ]
    )
  )
}


validate_all_reference_plans <- function(verbose = TRUE) {

  validation <- validate_plans_with_afex(
    plans = ems_reference_plans,
    verbose = verbose
  )

  invisible(validation$summary)
}
