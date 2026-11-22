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
spss_syntax <- function(factors, wide_dat) {
  fnames    <- names(factors)
  s_nest    <- factors[["s"]]$nested_in
  between_f <- s_nest[s_nest %in% fnames]
  within_f  <- fnames[!fnames %in% c("s", between_f)]

  # Colonnes WS = toutes les colonnes sauf sujet, between et Y
  excl   <- c("sujet", between_f, "Y")
  y_cols <- names(wide_dat)[!names(wide_dat) %in% excl]

  lines <- c(
    "* ============================================================",
    "* Syntaxe SPSS — générée par ExpDesign",
    "* ============================================================",
    "",
    "* Lire les données (adapter le chemin).",
    "GET FILE='donnees_simulees.sav'.",
    ""
  )

  if(length(within_f) == 0) {
    # BS pur
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
    # Avec WS
    ws_vars <- paste(y_cols, collapse=" ")

    # Nombre de niveaux par facteur WS
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
stata_syntax <- function(factors, wide_dat) {
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
    'use "donnees_simulees.dta", clear',
    ""
  )

  if (length(within_f) == 0) {
    # ── Plan purement BS ──────────────────────────────────────────
    lines <- c(lines,
      "* Plan entièrement between-subjects :",
      paste0("anova Y ", paste(between_f, collapse="##"))
    )

  } else if (length(within_f) == 1) {
    # ── Un seul facteur within : wsanova ──────────────────────────
    ws <- within_f[1]
    bet_str <- if (length(between_f) > 0) {
      bet_terms <- c(between_f,
        if (length(between_f) > 1)
          apply(combn(between_f, 2), 2, paste, collapse="") else NULL)
      paste(bet_terms, collapse=" ")
    } else NULL

    cmd <- paste0("wsanova Y ", ws, ", id(sujet)",
      if (!is.null(bet_str)) paste0(" between(", bet_str, ")") else "",
      " epsilon")

    lines <- c(lines,
      "* Plan avec un facteur within (wsanova) :",
      cmd
    )

  } else {
    # ── Plusieurs facteurs within : anova strate par strate ───────
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

plot_boxplot_flex <- function(dat, x_var, col_var="", facet_var="") {
  dat[[x_var]] <- factor(dat[[x_var]])
  if(nchar(col_var)  > 0) dat[[col_var]]   <- factor(dat[[col_var]])
  if(nchar(facet_var)> 0) dat[[facet_var]] <- factor(dat[[facet_var]])

  has_col   <- nchar(col_var)   > 0
  has_facet <- nchar(facet_var) > 0

  if(has_col) {
    dat$x_combined <- interaction(dat[[x_var]], dat[[col_var]])
    p <- ggplot2::ggplot(dat,
      ggplot2::aes(x=x_combined, y=Y, fill=.data[[col_var]])) +
      ggplot2::geom_boxplot(alpha=0.7, outlier.shape=NA) +
      ggplot2::geom_jitter(
        ggplot2::aes(color=.data[[col_var]]),
        width=0.15, alpha=0.5, size=1.5, show.legend=FALSE) +
      ggplot2::scale_x_discrete(
        labels=function(l) gsub("\\.", "\n", l)) +
      ggplot2::labs(x=paste0(x_var, " : ", col_var),
                    fill=col_var)
  } else {
    p <- ggplot2::ggplot(dat,
      ggplot2::aes(x=.data[[x_var]], y=Y, fill=.data[[x_var]])) +
      ggplot2::geom_boxplot(alpha=0.7, outlier.shape=NA) +
      ggplot2::geom_jitter(width=0.15, alpha=0.5, size=1.5,
                           show.legend=FALSE) +
      ggplot2::labs(x=x_var, fill=x_var)
  }

  if(has_facet)
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]),
                                  labeller=ggplot2::label_both)

  p <- p +
    ggplot2::scale_fill_brewer(palette="Set2") +
    ggplot2::scale_color_brewer(palette="Set2") +
    ggplot2::labs(title="Distribution de Y par groupe", y="Y") +
    ggplot2::theme_minimal(base_size=13) +
    ggplot2::theme(legend.position="bottom")

  plotly::ggplotly(p)
}

plot_interaction_flex <- function(dat, x_var, col_var="", facet_var="") {
  dat[[x_var]] <- factor(dat[[x_var]])
  if(nchar(col_var)  > 0) dat[[col_var]]   <- factor(dat[[col_var]])
  if(nchar(facet_var)> 0) dat[[facet_var]] <- factor(dat[[facet_var]])

  has_col   <- nchar(col_var)   > 0
  has_facet <- nchar(facet_var) > 0

  grp_vars <- unique(c(x_var,
                       if(has_col)   col_var   else NULL,
                       if(has_facet) facet_var else NULL))

  # Grouper manuellement sans aggregate ni FUN
  grp_key <- do.call(paste, c(
    lapply(grp_vars, function(v) as.character(dat[[v]])),
    sep="__"))

  keys_unique <- unique(grp_key)

  grid <- do.call(rbind, lapply(keys_unique, function(k) {
    idx    <- grp_key == k
    vals   <- dat$Y[idx]
    parts  <- strsplit(k, "__")[[1]]
    row    <- as.data.frame(
      setNames(as.list(parts), grp_vars),
      stringsAsFactors=FALSE)
    row$Y_mean <- mean(vals)
    row$se     <- sd(vals) / sqrt(length(vals))
    row
  }))

  # Restaurer les niveaux facteurs pour le bon ordre
  for(v in grp_vars) grid[[v]] <- factor(grid[[v]],
    levels=levels(dat[[v]]))
  grid$Y_mean <- as.numeric(grid$Y_mean)
  grid$se     <- as.numeric(grid$se)
  grid <- grid[do.call(order, lapply(grp_vars, function(v) grid[[v]])),]

  if(has_col) {
    p <- ggplot2::ggplot(grid,
      ggplot2::aes(x=.data[[x_var]], y=Y_mean,
                   color=.data[[col_var]],
                   group=.data[[col_var]])) +
      ggplot2::scale_color_brewer(palette="Set1")
  } else {
    p <- ggplot2::ggplot(grid,
      ggplot2::aes(x=.data[[x_var]], y=Y_mean, group=1)) +
      ggplot2::scale_color_manual(values=c("1"="#2196F3"))
  }

  p <- p +
    ggplot2::geom_line(linewidth=1) +
    ggplot2::geom_point(size=3) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin=Y_mean - se, ymax=Y_mean + se),
      width=0.15)

  if(has_facet)
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(.data[[facet_var]]),
      labeller=ggplot2::label_both)

  p <- p +
    ggplot2::labs(
      title = "Profil des moyennes (+/- 1 ET)",
      x=x_var, y="Moyenne de Y",
      color=if(has_col) col_var else ""
    ) +
    ggplot2::theme_minimal(base_size=13) +
    ggplot2::theme(legend.position="bottom")

  plotly::ggplotly(p)
}