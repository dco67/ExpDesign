# ==========================================================
# Onglet EMS théoriques
#
# Statut : terminé (v1.0)
#
# Évolutions possibles :
# - Monte-Carlo (convergence des MS vers les EMS)
# - Distribution empirique des MS
# - Distribution empirique des F
# - Robustesse aux violations des postulats
# ==========================================================

source("global.R")
# source("sim_export.R")

ui <- page_sidebar(
  title = "Plans d'expérience — Tableau EMS",
  lang  = "fr",
  theme = bs_theme(bootswatch="flatly", base_font=font_google("Inter")),

  # KaTeX CSS local pour le rendu HTML produit par katex::katex_html()
tags$head(
  tags$link(rel="stylesheet", href="katex.min.css"),
  tags$style(HTML("
    .ems-tooltip {
      position: relative;
      cursor: help;
      text-decoration: underline dotted;
    }
    .ems-tooltip::after {
      content: attr(data-tooltip);
      position: absolute;
      left: 100%;
      top: 50%;
      transform: translateY(-50%);
      margin-left: 8px;
      background: #333;
      color: #fff;
      padding: 4px 8px;
      border-radius: 4px;
      font-size: 0.82em;
      white-space: nowrap;
      opacity: 0;
      pointer-events: none;
      transition: opacity 0.2s;
      z-index: 9999;
    }
    .ems-tooltip:hover::after {
      opacity: 1;
    }
  "))
),
  useShinyjs(),

  sidebar = sidebar(
  id = "main_sidebar",
  open = "desktop",
  width = 320,
  padding = "0.5rem",
    card(
      card_header("Specification du plan"),
      layout_columns(
        col_widths = 12,
        textInput("design_str", NULL,
            value="A, B, s(A)",
            placeholder="ex. A, B, s(AB)")
      ),

      h6("Niveaux :"),
      uiOutput("ui_levels"),
      hr(),
      actionButton("btn_run", "Analyser", class="btn-primary w-100")
    ),
    card(
      height = 500,
      card_header("Exemples classiques"),
selectInput("exemple", "Charger un exemple :",
  choices = c(
    "— Exemples —"                       = "",
    "BS pur"                             = "A, s(A)",
    "WS pur"                             = "A, s",
    "Mixte — A between, B within"        = "A, B, s(A)",
    "Deux facteurs between"              = "A, B, s(AB)",
    "Deux facteurs within"               = "A, B, s",
    "Mixte — A between, B,C within"      = "A, B, C, s(A)",
    "Mixte avec quasi-F"                 = "A, B, c, s(AB)",
    "Facteur aléatoire b"                = "A, b, C, s(Ab)"
  ),
  selected = ""
)
    )
  ),

  navset_card_underline(
    id = "tabs_main",

    source("intro_panel.R", local=TRUE)$value,

    nav_panel("Facteurs", icon=icon("list"),
      card_body(uiOutput("ui_facteurs"))
    ),

    nav_panel(
      title = "EMS et df", 
      value = "ems",
      icon=icon("table"),
      card_body(uiOutput("ui_ems_dl"))
    ),

    nav_panel(
      title = "Quasi-F",
      value = "quasi_f",
      icon = icon("divide"),
      card_body(uiOutput("ui_quasif"))
    ),

    nav_panel(
      title = "Repères",
      value = "reperes",
      icon = icon("map-signs"),
      card_body(
        div(
          class = "alert alert-primary mb-3",
          tags$b("Un itinéraire pour les plans complexes"),
          tags$p(
            class = "mb-0 mt-2",
            "L'objectif est d'identifier la structure du plan avant de calculer : ",
            "nature des facteurs, croisements ou emboîtements, puis terme d'erreur du test."
          )
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            card_header("1. Décrire le plan"),
            card_body(
              "Utilisez le panneau de gauche pour écrire les facteurs et les sujets. ",
              "Une majuscule désigne un facteur fixe; une minuscule, un facteur aléatoire. ",
              "Par exemple, ", tags$code("A, B, s(A)"),
              " décrit un facteur A entre sujets, un facteur B répété et des sujets emboîtés dans A."
            )
          ),
          card(
            card_header("2. Comprendre le test"),
            card_body(
              "L'onglet EMS et df affiche ce qu'estime chaque carré moyen et le terme d'erreur correspondant. ",
              "Lorsqu'aucun terme unique ne convient, l'onglet Quasi-F explique la combinaison nécessaire."
            )
          ),
          card(
            card_header("3. Passer aux données"),
            card_body(
              "La simulation illustre le plan équilibré. Pour des données réellement déséquilibrées ou incomplètes, ",
              "l'analyse se poursuit avec un modèle mixte (chapitre 22), plutôt qu'avec une règle mécanique sur les carrés moyens."
            )
          )
        ),
        card(
          class = "mt-3",
          card_header("Vérification avancée"),
          card_body(
            tags$p(
              class = "mb-2",
              "Une vérification indépendante des carrés moyens attendus est utile pour les plans très complexes. ",
              "Elle sert à contrôler la structure et les dénominateurs; elle ne remplace pas l'identification du plan ni l'interprétation statistique."
            ),
            tags$p(
              class = "mb-0 text-muted small",
              "Dans un plan mixte équilibré, cette vérification peut reposer sur un modèle non restreint, tandis que les tableaux pédagogiques de cette application suivent les règles de Cornfield–Tukey. ",
              "Les deux approches coïncident pour les tests des effets fixes."
            )
          )
        )
      )
    ),
    
nav_panel("Simulation", icon=icon("dice"),
  layout_sidebar(
    sidebar = sidebar(
  width = 280,
  title = "Paramètres de simulation",

  accordion(
    open = "Paramètres généraux",

    accordion_panel(
      title = "Paramètres généraux",

      numericInput(
        inputId = "sim_seed",
        label = "Graine aléatoire",
        value = default_seed(),
        min = 1,
        max = 999999,
        step = 1,
        width = "100%"
      ),

      actionButton(
        inputId = "new_seed",
        label = "Nouvelle graine",
        icon = icon("shuffle"),
        class = "btn-outline-secondary btn-sm w-100"
      )
    ),

    accordion_panel(
      title = "Contributions systématiques",

      tags$p(
        class = "text-muted small mb-2",
        paste(
          "Ces valeurs contrôlent l'intensité des effets fixes.",
          "Une valeur nulle correspond à l'hypothèse nulle."
        )
      ),

      uiOutput("ui_effect_sizes")
    ),

    accordion_panel(
      title = "Sources de variation aléatoire",

      tags$p(
        class = "text-muted small mb-2",
        paste(
          "Chaque valeur représente une composante de variance",
          "du modèle. Le dernier terme correspond à la variance",
          "résiduelle."
        )
      ),

      uiOutput("ui_random_variances")
    )
  ),

  hr(),

  actionButton(
  inputId = "btn_sim",
  label = "Simuler",
  icon = icon("dice"),
  class = "btn-success w-100 mb-2"
),

actionButton(
  inputId = "btn_resim",
  label = "Nouvel échantillon",
  icon = icon("rotate"),
  class = "btn-outline-success w-100")
),

    # Corps principal
    navset_tab(
      nav_panel("Données simulées",
        card_body(
          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header("Aperçu (format large)"),
              card_body(uiOutput("ui_data_preview"))
            ),
            card(
              card_header("Télécharger"),
              card_body(
                h6("Format des données :"),
                radioButtons("dl_format", NULL,
                  choices = c("CSV"="csv","XLSX"="xlsx",
                              "SPSS (.sav)"="sav","Stata (.dta)"="dta"),
                  selected = "csv"),
                downloadButton("dl_data", "Télécharger",
                               class="btn-primary w-100 mb-3")
              )
            )
          )
        )
      ),

nav_panel("Analyse",
  card_body(
    navset_tab(
      id = "analysis_tabs",
      nav_panel("Tableau ANOVA",
        card_body(
          uiOutput("ui_sphericite_controls"),
          hr(),
          uiOutput("ui_anova_table")
        )
      ),
      nav_panel(
        "EMS théoriques",
        card_body(
          uiOutput("ui_ems_theoriques")
          )
      ),
      
      nav_panel(
  "Quasi-F observés",
  value = "analysis_quasi_f",
  icon = icon("divide"),

  card_body(
    uiOutput("ui_quasi_f_observes")
  )
),
      
      nav_panel("Postulats",
        card_body(
          uiOutput("ui_postulats")
        )
      ),
      nav_panel("Graphiques",
        card_body(
          layout_columns(
            col_widths = c(3, 9),
            card(
              card_header("Options"),
              card_body(uiOutput("ui_plot_controls"))
            ),
            navset_tab(
              nav_panel("Distribution",
                plotly::plotlyOutput("plot_box", height="400px")
              ),
              nav_panel("Profil des moyennes",
                plotly::plotlyOutput("plot_inter", height="400px")
              )
            )
          )
        )
      ),
      nav_panel("Interpretation",
        card_body(
          uiOutput("ui_interpretation")
        )
      )
    )
  )
),

      nav_panel("Syntaxe",
        card_body(
          layout_columns(
            col_widths = c(2, 10),
            div(
              radioButtons("syn_preview", NULL,
                choices = c("SPSS"="spss", "Stata"="stata"),
                selected = "spss"),
              hr(),
              downloadButton("dl_syntax", "Telecharger",
                             class="btn-outline-secondary w-100")
            ),
            verbatimTextOutput("preview_syntax")
          )
        )
      )
    )
  )
),



nav_panel(
  "Monte-Carlo",
  value = "monte_carlo",
  icon = icon("repeat"),

  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      
      card(
  class = "mb-3",

  card_header(
    "🧪 Laboratoire Monte-Carlo"
  ),

  card_body(

    tags$p(
      class = "small",

      paste(
        "Les quatre activités suivantes illustrent",
        "expérimentalement les principaux concepts",
        "de l'analyse de variance.  Elles sont conçues pour être parcourues dans l'ordre."
      )
    ),

    uiOutput("mc_progress")
  )
),

      numericInput(
        "mc_B",
        "Nombre de réplications",
        value = 500,
        min = 10,
        max = 5000,
        step = 100
      ),

      numericInput(
        "mc_seed",
        "Graine",
        value = 20260802,
        min = 1,
        max = 999999
      ),

      actionButton(
        "btn_mc",
        "Lancer Monte-Carlo",
        icon = icon("play"),
        class = "btn-primary w-100"
      )
    ),

    navset_tab(
  id = "mc_step",
  selected = "mc_ms",

  nav_panel(
    "① Distribution du MS",
    value = "mc_ms",

  tagList(

    div(
      class = "alert alert-primary py-2 px-3 mb-3",

      div(
  class = "d-flex justify-content-between align-items-center flex-wrap gap-2",

  div(
    tags$div(
      class = "text-uppercase small fw-semibold",
      "Laboratoire Monte-Carlo — Étape 1 sur 4"
    ),

    tags$div(
      class = "mt-1",
      tags$b("Objectif : "),
      paste(
        "comprendre pourquoi un carré moyen observé fluctue",
        "autour de son EMS théorique."
      )
    )
  ),

  tags$span(
    class = "badge bg-primary",
    "MS → EMS"
  )
)
    ),

    layout_columns(
      col_widths = c(3, 9),

      card(
        fill = FALSE,

        card_header(
          class = "py-2",
          tags$b("Paramètres et résultats")
        ),

        card_body(
          class = "py-2",
          uiOutput("ui_mc_ms_controls")
        )
      ),

      card(
        fill = FALSE,

        card_header(
          class = "py-2",

          div(
            class = "d-flex justify-content-between align-items-center",

            tags$b("Distribution empirique du carré moyen"),

            tags$span(
              class = "text-muted small",
              "Un nouvel échantillon à chaque réplication"
            )
          )
        ),

        card_body(
          class = "py-2",

          plotly::plotlyOutput(
            "plot_mc_ms",
            height = "390px"
          )
        )
      )
    ),

    div(
      class = "text-muted small mt-2",

      tags$b("Parcours recommandé : "),

      paste(
        "après cette étape, examinez la distribution du rapport F",
        "pour comprendre comment les fluctuations des carrés moyens",
        "se transforment en décision statistique."
      )
    )
  )
),

      nav_panel(
  "② Distribution du F et puissance",
  value = "mc_f",

  tagList(

    div(
      class = "alert alert-primary py-2 px-3 mb-3",

      div(
        class = paste(
          "d-flex justify-content-between",
          "align-items-center flex-wrap gap-2"
        ),

        div(
          tags$div(
            class = "text-uppercase small fw-semibold",
            "Laboratoire Monte-Carlo — Étape 2 sur 4"
          ),

          tags$div(
            class = "mt-1",

            tags$b("Objectif : "),

            paste(
              "comprendre comment les fluctuations des carrés",
              "moyens produisent un rapport F et déterminent",
              "l'erreur de type I ou la puissance du test."
            )
          )
        ),

        tags$span(
          class = "badge bg-primary",
          "MS → F → décision"
        )
      )
    ),

    layout_columns(
      col_widths = c(3, 9),

      card(
        fill = FALSE,

        card_header(
          class = "py-2",
          tags$b("Paramètres et résultats")
        ),

        card_body(
          class = "py-2",
          uiOutput("ui_mc_f_controls")
        )
      ),

      card(
        fill = FALSE,

        card_header(
          class = "py-2",

          div(
            class = paste(
              "d-flex justify-content-between",
              "align-items-center flex-wrap gap-2"
            ),

            tags$b(
              "Distribution empirique du rapport F"
            ),

            tags$span(
              class = "text-muted small",
              "Comparaison avec les lois sous H₀ et H₁"
            )
          )
        ),

        card_body(
          class = "py-2",

          plotly::plotlyOutput(
            "plot_mc_f",
            height = "440px"
          )
        )
      )
    ),

    div(
      class = "text-muted small mt-2",

      tags$b("Parcours recommandé : "),

      paste(
        "après cette étape, examinez la validation des EMS",
        "pour vérifier que les moyennes Monte-Carlo reproduisent",
        "correctement les valeurs théoriques."
      )
    )
  )
),

nav_panel(
  
  "③ Validation des EMS",
  value = "mc_validation",

  tagList(

    div(
      class = "alert alert-primary py-2 px-3 mb-3",

      div(
        class = paste(
          "d-flex justify-content-between",
          "align-items-center flex-wrap gap-2"
        ),

        div(
          tags$div(
            class = "text-uppercase small fw-semibold",
            "Laboratoire Monte-Carlo — Étape 3 sur 4"
          ),

          tags$div(
            class = "mt-1",

            tags$b("Objectif : "),

            paste(
              "vérifier que les données simulées reproduisent",
              "fidèlement les EMS théoriques du plan d’expérience."
            )
          )
        ),

        tags$span(
          class = "badge bg-primary",
          "Théorie ↔ simulation"
        )
      )
    ),

    uiOutput("ui_mc_summary"),

    div(
      class = "text-muted small mt-2",

      tags$b("Parcours recommandé : "),

      paste(
        "terminez par l’étape de convergence cumulative afin",
        "d’observer comment la moyenne des carrés moyens se",
        "stabilise progressivement autour de l’EMS."
      )
    )
  )
),

      nav_panel(
  "④ Convergence Monte-Carlo",
  value = "mc_convergence",

  tagList(

    div(
      class = "alert alert-primary py-2 px-3 mb-3",

      div(
        class = paste(
          "d-flex justify-content-between",
          "align-items-center flex-wrap gap-2"
        ),

        div(
          tags$div(
            class = "text-uppercase small fw-semibold",
            "Laboratoire Monte-Carlo — Étape 4 sur 4"
          ),

          tags$div(
            class = "mt-1",

            tags$b("Objectif : "),

            paste(
              "observer comment la moyenne cumulative des carrés",
              "moyens se stabilise progressivement autour de",
              "l’EMS théorique."
            )
          )
        ),

        tags$span(
          class = "badge bg-primary",
          "Réplications → stabilité"
        )
      )
    ),

    layout_columns(
      col_widths = c(3, 9),

      card(
        fill = FALSE,

        card_header(
          class = "py-2",
          tags$b("Paramètres et résultats")
        ),

        card_body(
          class = "py-2",
          uiOutput("ui_mc_conv_controls")
        )
      ),

      card(
        fill = FALSE,

        card_header(
          class = "py-2",

          div(
            class = paste(
              "d-flex justify-content-between",
              "align-items-center flex-wrap gap-2"
            ),

            tags$b("Convergence cumulative du carré moyen"),

            tags$span(
              class = "text-muted small",
              "Moyenne calculée après chaque réplication"
            )
          )
        ),

        card_body(
          class = "py-2",

          plotly::plotlyOutput(
            "plot_mc_convergence",
            height = "430px"
          )
        )
      )
    ),

    div(
      class = "text-muted small mt-2",

      tags$b("Fin du parcours : "),

      paste(
        "les quatre étapes montrent successivement la fluctuation",
        "des MS, la formation du rapport F, la validation globale",
        "des EMS et leur convergence progressive."
      )
    )
  )
)
    )
  )
)
)
)

server <- function(input, output, session) {


observeEvent(
  input$tabs_main,
  {
    bslib::toggle_sidebar(
      id = "main_sidebar",
      open = !identical(input$tabs_main, "monte_carlo"),
      session = session
    )
  },
  ignoreInit = FALSE
)
  
  observe({

  r <- result()

  quasi_required <- (
    !is.null(r) &&
    !is.null(r$quasi_list) &&
    length(r$quasi_list) > 0L
  )

  if (isTRUE(quasi_required)) {

    bslib::nav_show(
      id = "tabs_main",
      target = "quasi_f",
      session = session
    )

  } else {

    # Si l'utilisateur se trouve actuellement dans Quasi-F,
    # sélectionner un autre onglet avant de le masquer.
    if (identical(input$tabs_main, "quasi_f")) {

      bslib::nav_select(
        id = "tabs_main",
        selected = "ems",
        session = session
      )
    }

    bslib::nav_hide(
      id = "tabs_main",
      target = "quasi_f",
      session = session
    )
  }
})
  
  
  output$mc_progress <- renderUI({

  current_step <- input$mc_step %||% "mc_ms"

  steps <- data.frame(
    value = c(
      "mc_ms",
      "mc_f",
      "mc_validation",
      "mc_convergence"
    ),

    number = c(
      "①",
      "②",
      "③",
      "④"
    ),

    label = c(
      "Distribution du carré moyen",
      "Distribution du rapport F et puissance",
      "Validation des EMS",
      "Convergence Monte-Carlo"
    ),

    stringsAsFactors = FALSE
  )

  current_index <- match(
    current_step,
    steps$value
  )

  if (
    length(current_index) != 1L ||
    is.na(current_index)
  ) {
    current_index <- 1L
  }

  step_rows <- lapply(
    seq_len(nrow(steps)),
    function(i) {

      if (i < current_index) {

        status_icon <- icon(
          "circle-check",
          class = "text-success"
        )

        row_class <- "text-success"

      } else if (i == current_index) {

        status_icon <- icon(
          "circle-dot",
          class = "text-primary"
        )

        row_class <- "text-primary fw-semibold"

      } else {

        status_icon <- icon(
          "circle",
          class = "text-muted"
        )

        row_class <- "text-muted"
      }

      div(
        class = paste(
          "d-flex align-items-start gap-2",
          "mb-2",
          row_class
        ),

        div(
          style = "width:1.1rem; flex:0 0 1.1rem;",
          status_icon
        ),

        div(
          tags$span(
            class = "me-1",
            steps$number[i]
          ),

          tags$span(
            steps$label[i]
          )
        )
      )
    }
  )

  tagList(

    tags$div(
      class = "small fw-semibold mb-2",
      "Parcours recommandé"
    ),

    tagList(step_rows),

    tags$div(
      class = "small text-muted mt-2 pt-2 border-top",

      paste0(
        "Étape ",
        current_index,
        " sur ",
        nrow(steps)
      )
    )
  )
})

# ==========================================================
# Diagnostic de la spécification du plan
# ==========================================================
plan_analysis_error <- reactiveVal(NULL)

validate_design_text <- function(design_text) {

  design_text <- trimws(design_text %||% "")

  if (!nzchar(design_text)) {
    return(
      "La spécification du plan est vide."
    )
  }

  # Virgule initiale, finale ou deux virgules successives
  if (
    grepl("^\\s*,", design_text) ||
    grepl(",\\s*$", design_text) ||
    grepl(",\\s*,", design_text)
  ) {
    return(
      paste(
        "Une virgule est en trop ou un facteur est manquant.",
        "Chaque virgule doit séparer deux facteurs valides."
      )
    )
  }

  # Vérification élémentaire des parenthèses
  n_open <- lengths(
    regmatches(
      design_text,
      gregexpr("\\(", design_text)
    )
  )

  n_close <- lengths(
    regmatches(
      design_text,
      gregexpr("\\)", design_text)
    )
  )

  if (!identical(n_open, n_close)) {
    return(
      "Le nombre de parenthèses ouvrantes et fermantes ne correspond pas."
    )
  }

  NULL
}

plan_error_panel <- function(message) {

  div(
    class = "alert alert-danger",

    tags$h5(
      class = "alert-heading mb-2",
      icon("triangle-exclamation"),
      tags$span(
        class = "ms-1",
        "Spécification erronée du plan d’expérience"
      )
    ),

    tags$p(
      class = "mb-2",
      "Impossible d’analyser le plan expérimental."
    ),

    tags$p(
      class = "mb-2",
      tags$b("Diagnostic : "),
      message
    ),

    tags$p(
      class = "mb-1",
      tags$b("Vérifiez notamment :")
    ),

    tags$ul(
      class = "mb-2",

      tags$li(
        "les virgules en trop ou manquantes;"
      ),

      tags$li(
        "les parenthèses ouvrantes et fermantes;"
      ),

      tags$li(
        "la notation des facteurs imbriqués, par exemple s(A);"
      ),

      tags$li(
        "les noms des facteurs."
      )
    ),

    tags$p(
      class = "mb-0",

      tags$b("Exemple valide : "),
      tags$code("A, B, s(A)")
    )
  )
}

# Effacer l’ancien diagnostic dès que la spécification est modifiée
observeEvent(
  input$design_str,
  {
    plan_analysis_error(NULL)
  },
  ignoreInit = TRUE
)

output$ui_levels <- renderUI({

  req(input$design_str)

  design_text <- trimws(input$design_str)

  syntax_error <- validate_design_text(
    design_text
  )

  if (!is.null(syntax_error)) {

    return(
      tags$div(
        class = "text-danger small",
        icon("triangle-exclamation"),
        tags$span(
          class = "ms-1",
          syntax_error
        )
      )
    )
  }

  tryCatch(
    {

      fcts <- parse_design(
        design_text
      )

      factor_names <- names(fcts)

      # Protection supplémentaire contre les facteurs sans nom
      if (
        length(factor_names) == 0L ||
        any(!nzchar(trimws(factor_names)))
      ) {
        stop(
          "Au moins un facteur est vide, probablement en raison d’une virgule en trop.",
          call. = FALSE
        )
      }

      tagList(
        lapply(
          factor_names,
          function(f) {

            div(
              style = paste(
                "display:flex;",
                "align-items:center;",
                "margin-bottom:4px;"
              ),

              tags$label(
                paste0("n(", f, ") :"),
                style = paste(
                  "width:80px;",
                  "margin:0;",
                  "font-size:0.9em;"
                )
              ),

              div(
                style = "flex:1;",

                numericInput(
                  inputId = paste0("n_", f),
                  label = NULL,
                  value = if (f == "s") 5 else 3,
                  min = 2,
                  max = 50
                )
              )
            )
          }
        )
      )
    },

    error = function(e) {

      tags$div(
        class = "text-danger small",

        icon("triangle-exclamation"),

        tags$span(
          class = "ms-1",
          conditionMessage(e)
        )
      )
    }
  )
})

result <- eventReactive(
  input$btn_run,
  {

    design_text <- trimws(
      input$design_str %||% ""
    )

    # ========================================================
    # Validation préliminaire
    # ========================================================
    syntax_error <- validate_design_text(
      design_text
    )

    if (!is.null(syntax_error)) {

      plan_analysis_error(
        syntax_error
      )

      showNotification(
        paste(
          "Spécification erronée du plan :",
          syntax_error
        ),
        type = "error",
        duration = 10
      )

      return(NULL)
    }

    # ========================================================
    # Analyse complète
    # ========================================================
    tryCatch(
      {

        fcts <- parse_design(
          design_text
        )

        factor_names <- names(fcts)

        if (
          length(factor_names) == 0L ||
          any(!nzchar(trimws(factor_names)))
        ) {
          stop(
            paste(
              "Au moins un facteur est vide.",
              "Vérifiez les virgules dans la spécification."
            ),
            call. = FALSE
          )
        }

        n_levels <- stats::setNames(
          lapply(
            factor_names,
            function(factor_name) {

              value <- input[[
                paste0(
                  "n_",
                  factor_name
                )
              ]]

              as.integer(
                if (is.null(value)) {
                  if (factor_name == "s") 5L else 3L
                } else {
                  value
                }
              )
            }
          ),
          factor_names
        )

        analysis_result <- prepare_design_result(
          design_str = design_text,
          n_levels = n_levels
        )

        # Conserver le plan réellement analysé
        analysis_result$design_str <-
          design_text

        # L’analyse a réussi : effacer tout diagnostic antérieur
        plan_analysis_error(NULL)

        analysis_result
      },

      error = function(e) {

        error_message <- conditionMessage(e)

        plan_analysis_error(
          error_message
        )

        showNotification(
          paste(
            "Spécification erronée du plan :",
            error_message
          ),
          type = "error",
          duration = 10
        )

        NULL
      }
    )
  }
)

sphericite_result <- reactive({
  s <- sim_result(); req(s)

  fnames    <- names(s$r$fcts)
  s_nest    <- s$r$fcts[["s"]]$nested_in
  between_f <- s_nest[s_nest %in% fnames]
  within_f  <- fnames[!fnames %in% c("s", between_f)]

  if (length(within_f) == 0) return(NULL)

  dat    <- s$anova_res$dat
  sujets <- levels(factor(dat$subj_id))

  # ── Calculer Mauchly et epsilon via car::Anova ──────────────────────────
  resultats <- setNames(lapply(within_f, function(wf) {
    nw <- length(levels(factor(dat[[wf]])))

    if (nw < 3) return(list(
      nw=nw, trivial=TRUE,
      W=NA, p_mauchly=NA,
      eps_gg=1, eps_hf=1, eps_box=1/(nw-1)
    ))

    tryCatch({
      # Restructurer en format large pour ce facteur within
      # Moyenner sur les autres facteurs within si nécessaire
      other_within <- within_f[within_f != wf]

      if (length(other_within) == 0) {
        dat_w <- reshape(dat[, c("subj_id", wf, "Y")],
          idvar     = "subj_id",
          timevar   = wf,
          direction = "wide")
      } else {
        # Moyenner Y sur les autres facteurs within
        dat_agg <- aggregate(
          Y ~ subj_id + get(wf),
          data = dat,
          FUN  = mean)
        names(dat_agg)[2] <- wf
        dat_w <- reshape(dat_agg,
          idvar     = "subj_id",
          timevar   = wf,
          direction = "wide")
      }

      y_cols <- names(dat_w)[grepl("^Y\\.", names(dat_w))]
      Y_wide <- as.matrix(dat_w[, y_cols])
      fit_lm <- lm(Y_wide ~ 1)
      idata  <- data.frame(wf_var = factor(seq_len(nw)))
      names(idata) <- wf
      av <- suppressWarnings(
        car::Anova(fit_lm,
                   idata   = idata,
                   idesign = as.formula(paste0("~", wf)),
                   type    = 3)
      )
      sp <- suppressWarnings(
        summary(av, multivariate=FALSE)
      )

      W         <- sp$sphericity.tests[1, "Test statistic"]
      p_mauchly <- sp$sphericity.tests[1, "p-value"]
      eps_gg    <- min(sp$pval.adjustments[1, "GG eps"], 1)
      eps_hf    <- min(sp$pval.adjustments[1, "HF eps"], 1)
      eps_box   <- 1/(nw-1)

      list(
        nw=nw, trivial=FALSE,
        W=W, p_mauchly=p_mauchly,
        eps_gg=eps_gg, eps_hf=eps_hf, eps_box=eps_box
      )
    }, error=function(e) {
      list(
        nw=nw, trivial=FALSE,
        W=NA, p_mauchly=NA,
        eps_gg=NA, eps_hf=NA, eps_box=1/(nw-1)
      )
    })
  }), within_f)

  list(
    within_f      = within_f,
    resultats     = resultats,
    any_violation = any(sapply(resultats, function(r)
      !isTRUE(r$trivial) && !is.na(r$p_mauchly) && r$p_mauchly < .05))
  )
})
  
output$ui_resume <- renderUI({

  analysis_error <- plan_analysis_error()

  if (!is.null(analysis_error)) {
    return(
      plan_error_panel(
        analysis_error
      )
    )
  }

  r <- result()

  if (is.null(r)) {
    return(
      div(
        class = "alert alert-info",
        paste(
          "Aucune analyse.",
          "Spécifiez un plan et cliquez sur Analyser."
        )
      )
    )
  }
    n_quasi <- length(r$quasi_list)
    div(class="alert alert-success",
      tags$b("Plan : "), code(input$design_str), tags$br(),
      tags$b("Niveaux : "),
      paste(names(r$n_levels), r$n_levels, sep="=", collapse=", "), tags$br(),
      tags$b("Effets du modèle: "), length(r$effects), tags$br(),
      tags$b("Termes d'erreur purs : "), length(r$error_info$pure), tags$br(),
      tags$b("Tests nécessitant un quasi-F : "),
      if(n_quasi==0) "aucun" else as.character(n_quasi)
    )
  })

  output$ui_facteurs <- renderUI({
  req(input$design_str)

  design <- trimws(input$design_str)
    analysis_error <- plan_analysis_error()

  if (!is.null(analysis_error)) {
    return(
      plan_error_panel(
        analysis_error
      )
    )
  }

  if (nchar(design) == 0) {
    return(
      div(
        class = "alert alert-info",
        "Saisissez un plan et cliquez sur Analyser."
      )
    )
  }

  tryCatch({

    # --------------------------------------------------------
    # Lire le plan
    # --------------------------------------------------------
    fcts <- parse_design(design)

    # Facteurs dans lesquels les sujets sont imbriqués :
    # ce sont les facteurs inter-sujets.
    subject_parents <- if ("s" %in% names(fcts)) {
      fcts[["s"]]$nested_in %||% character(0)
    } else {
      character(0)
    }

    # Les autres facteurs, sauf s, sont représentés
    # comme facteurs intra-sujets dans le diagramme.
    within_factors <- setdiff(
      names(fcts),
      c("s", subject_parents)
    )

    # --------------------------------------------------------
    # Nombre de niveaux
    # --------------------------------------------------------
    n_levels <- setNames(
      lapply(names(fcts), function(f) {
        v <- input[[paste0("n_", f)]]

        as.integer(
          if (is.null(v)) {
            ifelse(f == "s", 5, 3)
          } else {
            v
          }
        )
      }),
      names(fcts)
    )

    # --------------------------------------------------------
    # Tableau des facteurs
    # --------------------------------------------------------
    r <- result()

    tbl <- if (!is.null(r)) {

      subject_parents_result <- if ("s" %in% names(r$fcts)) {
        r$fcts[["s"]]$nested_in %||% character(0)
      } else {
        character(0)
      }

      rows <- lapply(names(r$fcts), function(f) {

        fct <- r$fcts[[f]]

        type <- if (f == "s") {
          "Aléatoire [sujet]"
        } else if (isTRUE(fct$fixed)) {
          "Fixe"
        } else {
          "Aléatoire"
        }

        ni <- if (length(fct$nested_in) == 0) {
          "—"
        } else {
          paste(fct$nested_in, collapse = ", ")
        }

        cw <- if (length(fct$crossed_with) == 0) {
          "—"
        } else {
          paste(fct$crossed_with, collapse = ", ")
        }

        role <- if (f == "s") {
          "Sujet"
        } else if (f %in% subject_parents_result) {
          "Facteur inter-sujets"
        } else if (isTRUE(fct$random)) {
          "Facteur aléatoire"
        } else {
          "Facteur intra-sujets"
        }

        tags$tr(
          tags$td(f),
          tags$td(type),
          tags$td(ni),
          tags$td(cw),
          tags$td(role)
        )
      })

      make_table(
        c(
          "Facteur",
          "Type",
          "Imbrication",
          "Croisements",
          "Rôle"
        ),
        rows
      )

    } else {

      div(
        class = "alert alert-info",
        "Cliquez sur Analyser pour voir le tableau des facteurs."
      )
    }

    # --------------------------------------------------------
    # Facteur affiché comme dimension principale des colonnes
    # --------------------------------------------------------
    selected_column_factor <- if (length(within_factors) > 1) {

      current_selection <- input$design_column_factor

      if (
        is.null(current_selection) ||
        !current_selection %in% within_factors
      ) {
        tail(within_factors, 1)
      } else {
        current_selection
      }

    } else {

      NULL
    }
    
    pure_bs_two_factor <- (
  length(within_factors) == 0 &&
  length(subject_parents) == 2
)

selected_row_factor <- if (pure_bs_two_factor) {

  current_row <- input$design_row_factor

  if (
    is.null(current_row) ||
    !current_row %in% subject_parents
  ) {
    subject_parents[1]
  } else {
    current_row
  }

} else {

  NULL
}

selected_col_factor <- if (pure_bs_two_factor) {
  setdiff(subject_parents, selected_row_factor)[1]
} else {
  NULL
}
    
# --------------------------------------------------------
# Interface finale
# --------------------------------------------------------
tagList(

  layout_columns(
    col_widths = c(4, 8),
    uiOutput("ui_resume"),
    tbl
  ),

  hr(),

  h5("Représentation du plan (style Kirk)"),

  p(
    class = "text-muted mb-2",
    paste(
      "Cette figure montre comment les observations sont organisées",
      "dans le plan expérimental. Chaque case représente une",
      "observation qui sera simulée."
    )
  ),

  if (length(within_factors) > 1) {
    selectInput(
      inputId = "design_column_factor",
      label = "Facteur variant dans les colonnes",
      choices = within_factors,
      selected = selected_column_factor,
      width = "280px"
    )
  },

  if (pure_bs_two_factor) {
    selectInput(
      inputId = "design_row_factor",
      label = "Facteur affiché en rangées",
      choices = subject_parents,
      selected = selected_row_factor,
      width = "280px"
    )
  },

  draw_design(
    factors = fcts,
    n_levels = n_levels,

    column_factor = if (length(within_factors) > 1) {
      input$design_column_factor
    } else {
      NULL
    },

    row_factor = selected_row_factor,
    col_factor = selected_col_factor
  )
)

  }, error = function(e) {

    div(
      class = "alert alert-info",
      "Saisissez un plan valide dans le panneau gauche."
    )
  })
})

output$ui_ems_dl <- renderUI({

  r <- result()
  req(r)

  # ----------------------------------------------------------
  # Effets disponibles et effet sélectionné
  # ----------------------------------------------------------
  effect_names <- names(r$ems_table)

  selected_effect <- input$ems_effect_selected

  if (
    is.null(selected_effect) ||
    !selected_effect %in% effect_names
  ) {
    selected_effect <- effect_names[1]
  }

  # ----------------------------------------------------------
  # Composantes communes avec un terme d'erreur pur
  # ----------------------------------------------------------
  common_info <- ems_common_variance_info(
    effect_name = selected_effect,
    ems_table   = r$ems_table,
    error_info  = r$error_info
  )

  pure_error   <- common_info$error
  common_terms <- common_info$terms

  # ----------------------------------------------------------
  # Tableau EMS et degrés de liberté
  # ----------------------------------------------------------
  rows <- lapply(
    effect_names,
    function(eff_name) {

      terms_to_highlight <- if (
        length(common_terms) > 0 &&
        eff_name %in% c(selected_effect, pure_error)
      ) {
        common_terms
      } else {
        character(0)
      }

      ems_html <- ems_row_to_html(
        ems = r$ems_table[[eff_name]],
        highlight_terms = terms_to_highlight
      )

      err <- r$error_info$pure[[eff_name]]

      err_str <- if (!is.null(err)) {
        paste0("Erreur : ", err)
      } else if (!is.null(r$error_info$quasi[[eff_name]])) {
        "Quasi-F"
      } else {
        ""
      }

      df <- r$df_list[[eff_name]]

      row_style <- if (identical(eff_name, selected_effect)) {
        "border-left:4px solid #0d6efd;"
      } else {
        NULL
      }

      tags$tr(
        style = row_style,

        tags$td(
          if (identical(eff_name, selected_effect)) {
            tags$b(eff_name)
          } else {
            eff_name
          }
        ),

        tags$td(ems_html),
        tags$td(err_str),
        tags$td(df_sym_to_html(df$symbolic)),
        tags$td(as.character(df$numeric))
      )
    }
  )

  # ----------------------------------------------------------
  # Bandeau expliquant le surlignage
  # ----------------------------------------------------------
  highlight_note <- if (
    !is.null(pure_error) &&
    length(common_terms) > 0
  ) {

    div(
      class = "alert alert-primary py-2 px-3 mt-3 mb-3",
      style = "font-size:0.92rem;",

      tags$span(
        style = paste(
          "display:inline-block;",
          "width:1.15rem;",
          "height:1.15rem;",
          "vertical-align:middle;",
          "background-color:#cfe2ff;",
          "border:1.5px solid #6ea8fe;",
          "border-radius:0.25rem;",
          "margin-right:0.4rem;"
        )
      ),

      "Les composantes surlignées sont communes aux EMS de ",

      tags$b(selected_effect),

      " et de ",

      tags$b(pure_error),

      ". Elles justifient le choix du terme d'erreur."
    )

  } else {

    NULL
  }

  # ----------------------------------------------------------
  # Interface finale
  # ----------------------------------------------------------
  tagList(

    ems_summary_card(
      effects = r$effects,
      error_info = r$error_info,
      df_verification = r$verify
    ),

    make_table(
      c(
        "Effet",
        "EMS",
        "Terme d'erreur",
        "Formule df",
        "df"
      ),
      rows
    ),

    # Le bandeau est maintenant placé après le tableau
    highlight_note,

    hr(),

    interpret_ems_effect(
      effect_name = selected_effect,
      factors = r$fcts,
      effects = r$effects,
      ems_table = r$ems_table,
      error_info = r$error_info,
      df_list = r$df_list,
      effect_names = effect_names
    )
  )
})

output$ui_quasif <- renderUI({
  r <- result()
  req(r)

  if (length(r$quasi_list) == 0) {
    return(
      div(
        class = "alert alert-info",
        "Aucun quasi-F n’est nécessaire pour ce plan."
      )
    )
  }

  # Un sous-onglet narratif par effet
  panels <- lapply(
    names(r$quasi_list),
    function(eff_name) {

      qf <- r$quasi_list[[eff_name]]

      nav_panel(
  title = paste("Effet", eff_name),

  value = paste0(
    "qf_effect_",
    gsub(
      "[^A-Za-z0-9]",
      "_",
      eff_name
    )
  ),

  navset_tab(

    nav_panel(

      "① Pourquoi ?",

      div(
        style = "padding:1em;",

        quasi_story_intro(
          effect_name = eff_name
        )
      )
    ),

    nav_panel(

      "② Composantes",

      div(
        style = "padding:1em;",

        quasi_story_components(
          effect_name = eff_name,
          quasi_info = qf,
          ems_table = r$ems_table
        )
      )
    ),

    nav_panel(

      "③ Méthodes",

      div(
        style = "padding:1em;",

        quasi_story_methods(
          effect_name = eff_name,
          quasi_info = qf
        )
      )
    ),

    nav_panel(

      "④ Reconstruction",

      div(
        style = "padding:1em;",

        quasi_formula_card(
          effect_name = eff_name,
          quasi_info = qf,
          ems_table = r$ems_table
        )
      )
    ),

    nav_panel(

      "⑤ Interprétation",

      div(
        style = "padding:1em;",

        quasi_interpretation(
          effect_name = eff_name,
          quasi_info = qf
        )
      )
    )

  )
)
    }
  )

do.call(
  navset_tab,
  c(
    list(
      id = "quasi_effect_tabs"
    ),
    panels
  )
)

})
  # ── Simulation ─────────────────────────────────────────────────────────
  output$ui_effect_sizes <- renderUI({
    r <- result(); req(r)
    fixed_effects <- names(r$effects)[sapply(names(r$effects), function(e) {
      eff     <- r$effects[[e]]
      non_s_f <- eff$factors[eff$factors != "s"]
      !isTRUE(eff$is_error) && length(non_s_f) > 0 &&
      all(sapply(non_s_f, function(f) isTRUE(r$fcts[[f]]$fixed)))
    })]
    if(length(fixed_effects) == 0)
      return(helpText("Aucun effet fixe."))

    tagList(lapply(fixed_effects, function(e) {
      layout_columns(
        col_widths = c(4, 8),
        tags$label(class="mt-2", paste0("d(", e, ") :")),
        numericInput(paste0("es_", gsub("[^A-Za-z]","_", e)),
                     NULL, value=0, min=0, max=5, step=0.1)
      )
    }))
  })

  output$ui_random_variances <- renderUI({

  r <- result()
  req(r)

  # ==========================================================
  # Identifier le terme résiduel
  # ==========================================================
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

  # ==========================================================
  # Identifier toutes les composantes aléatoires
  #
  # Cette fois, le terme résiduel est conservé dans la liste.
  # ==========================================================
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

  if (length(random_terms) == 0L) {

    return(
      tags$p(
        class = "text-muted small mb-0",
        "Aucune composante de variance n'est requise pour ce plan."
      )
    )
  }

  # ==========================================================
  # Construire les contrôles
  # ==========================================================
  controls <- lapply(
    random_terms,
    function(term_name) {

      is_residual <- (
        length(residual_term) == 1L &&
          identical(term_name, residual_term)
      )

      # Le résidu conserve l'identifiant sim_sigma2 utilisé
      # partout ailleurs dans l'application.
      input_id <- if (is_residual) {

        "sim_sigma2"

      } else {

        paste0(
          "var_",
          gsub(
            "[^A-Za-z0-9]",
            "_",
            term_name
          )
        )
      }

      # Présentation du libellé
      term_label <- if (is_residual) {

        tagList(
          tags$div(
            tags$b("Variance résiduelle")
          ),

          tags$div(
            class = "text-muted small",
            k(
              paste0(
                "\\sigma^2_{",
                term_name,
                "}"
              )
            )
          )
        )

      } else {

        k(
          paste0(
            "\\sigma^2_{",
            term_name,
            "}"
          )
        )
      }

default_value <- if (
  identical(term_name, residual_term)
) {
  1
} else {
  0
}

minimum_value <- if (
  identical(term_name, residual_term)
) {
  0.001
} else {
  0
}

layout_columns(
  col_widths = c(5, 7),

  div(
    class = "mt-2",
    term_label
  ),

  numericInput(
    inputId = input_id,
    label = NULL,
    value = default_value,
    min = minimum_value,
    max = 5,
    step = 0.05,
    width = "100%"
  )
      )
    }
  )

  tagList(controls)
})

  sim_result <- eventReactive(
  input$btn_sim + input$btn_resim,
  {

   # ========================================================
# Vérifier la spécification actuelle du plan
# ========================================================
current_design <- trimws(input$design_str %||% "")

if (!nzchar(current_design)) {

  showNotification(
    "Erreur de spécification : le plan est vide.",
    type = "error",
    duration = 8
  )

  return(NULL)
}

design_is_valid <- tryCatch(
  {
    parsed_design <- parse_design(current_design)

    length(parsed_design) > 0L
  },
  error = function(e) {

    showNotification(
      paste0(
        "Erreur de spécification du plan : ",
        conditionMessage(e),
        ". Vérifiez notamment les virgules, les parenthèses ",
        "et les noms de facteurs."
      ),
      type = "error",
      duration = 10
    )

    FALSE
  }
)

if (!isTRUE(design_is_valid)) {
  return(NULL)
}

# ========================================================
# Vérifier qu'une analyse valide existe
# ========================================================
r <- isolate(result())

if (is.null(r)) {

  showNotification(
    paste(
      "Le plan doit d'abord être analysé.",
      "Corrigez sa spécification, puis cliquez sur Analyser."
    ),
    type = "warning",
    duration = 8
  )

  return(NULL)
}

# ========================================================
# Vérifier que le plan n'a pas changé depuis l'analyse
# ========================================================
normalize_design <- function(x) {
  gsub(
    "\\s+",
    "",
    trimws(x %||% "")
  )
}

analyzed_design <- r$design_str %||% ""

if (
  !identical(
    normalize_design(current_design),
    normalize_design(analyzed_design)
  )
) {

  showNotification(
    paste(
      "La spécification du plan a changé depuis la dernière analyse.",
      "Cliquez de nouveau sur Analyser avant de simuler."
    ),
    type = "warning",
    duration = 10
  )

  return(NULL)
} 
    
    seed_used <- as.integer(input$sim_seed)

if (
  length(seed_used) != 1L ||
  is.na(seed_used) ||
  !is.finite(seed_used)
) {
  seed_used <- 1L
}
    # ========================================================
# Variance résiduelle
# ========================================================
residual_variance <- input$sim_sigma2

if (
  is.null(residual_variance) ||
  length(residual_variance) != 1L ||
  is.na(residual_variance) ||
  !is.finite(residual_variance) ||
  residual_variance < 0
) {
  residual_variance <- 1
}

residual_variance <- as.numeric(residual_variance)

    # ========================================================
    # Contributions des effets fixes
    # ========================================================
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
            length(non_subject_factors) > 0 &&
            all(
              vapply(
                non_subject_factors,
                function(factor_name) {
                  isTRUE(
                    r$fcts[[factor_name]]$fixed
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
      lapply(
        fixed_effects,
        function(effect_name) {

          input_id <- paste0(
            "es_",
            gsub(
              "[^A-Za-z0-9]",
              "_",
              effect_name
            )
          )

          value <- input[[input_id]]

          if (is.null(value)) {
            0
          } else {
            as.numeric(value)
          }
        }
      ),
      fixed_effects
    )

    # ========================================================
    # Composantes aléatoires
    # ========================================================
    error_terms <- names(r$effects)[
      vapply(
        r$effects,
        function(effect_info) {
          isTRUE(effect_info$is_error)
        },
        logical(1)
      )
    ]

    residual_term <- if (length(error_terms) > 0) {
      tail(error_terms, 1)
    } else {
      character(0)
    }

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

    random_variances <- stats::setNames(
      lapply(
        random_terms,
        function(effect_name) {

          input_id <- paste0(
            "var_",
            gsub(
              "[^A-Za-z0-9]",
              "_",
              effect_name
            )
          )

          value <- input[[input_id]]

          if (is.null(value)) {
            1
          } else {
            as.numeric(value)
          }
        }
      ),
      random_terms
    )

    # ========================================================
    # Simulation
    # ========================================================
    dat <- sim_data(
      factors = r$fcts,
      effects = r$effects,
      n_levels = r$n_levels,
      effect_sizes = effect_sizes,
      random_variances = random_variances,
      sigma2_error = residual_variance,
      seed = seed_used
    )

    wide <- sim_to_wide(
      dat,
      r$fcts
    )

    anova_res <- compute_anova(
      dat,
      r$fcts,
      r$effects,
      r$df_list
    )

    result_out <- list(
  dat = dat,
  wide = wide,
  anova_res = anova_res,
  r = r,
  effect_sizes = effect_sizes,
  random_variances = random_variances,
  residual_variance = residual_variance,
  residual_term = residual_term,
  seed = seed_used
)

updateNumericInput(
  session = session,
  inputId = "sim_seed",
  value = if (seed_used >= 999999L) {
    1L
  } else {
    seed_used + 1L
  }
)

result_out
  }
)
  
  anova_components <- reactive({

  s <- sim_result()
  req(s)

  extract_anova_ms_df(
    anova_res = s$anova_res,
    factors = s$r$fcts,
    ems_table = s$r$ems_table
  )
})
  
  
ems_numeric_result <- reactive({

  s <- sim_result()
  req(s)

  compute_numeric_ems_result(
    r = s$r,
    effect_sizes = s$effect_sizes,
    random_variances = s$random_variances,
    residual_variance = s$residual_variance
  )
})

  # Apercu données
  output$ui_data_preview <- renderUI({
    s <- sim_result(); req(s)
    wide <- s$wide
    dat_show <- head(wide, 15)
    rows <- lapply(seq_len(nrow(dat_show)), function(i) {
      tags$tr(lapply(dat_show[i,], function(v)
        tags$td(if(is.numeric(v)) round(v,3) else as.character(v))))
    })
    tagList(
      make_table(names(dat_show), rows),
      p(class="text-muted",
        paste0("Affichage de ", nrow(dat_show), " lignes sur ",
               nrow(wide), " sujets."))
    )
  })

output$ui_anova_table <- renderUI({
  s  <- sim_result(); req(s)
  sp <- sphericite_result()
  error_info <- s$r$error_info
  
  if (!is.null(s$anova_res$error))
    return(div(class="alert alert-danger", s$anova_res$error))
  
  method       <- input$quasi_method  %||% "ct"
  spher_method <- input$spher_method  %||% "none"
  
  components <- anova_components()

all_ms   <- components$ms
all_df   <- components$df
within_f <- components$within_factors
  
  # Quasi-F numériques
  quasi_numeric <- if (length(s$r$quasi_list) > 0) {
    setNames(
      lapply(names(s$r$quasi_list), function(e)
        compute_quasi_f_numeric(e, s$r$quasi_list[[e]], all_ms, all_df)),
      names(s$r$quasi_list)
    )
  } else NULL
  
  # Epsilon par facteur within pour correction sphéricité
  eps_by_factor <- if (!is.null(sp) && spher_method != "none") {
    setNames(lapply(sp$within_f, function(wf) {
      r <- sp$resultats[[wf]]
      switch(spher_method,
        gg  = r$eps_gg,
        hf  = r$eps_hf,
        box = r$eps_box,
        1)
    }), sp$within_f)
  } else NULL
  
  format_anova_table(s$anova_res, s$r$ems_table,
                     s$r$error_info, s$r$df_list, s$r$n_levels,
                     s$r$fcts, quasi_numeric, method,
                     eps_by_factor, spher_method, within_f)
})

  # Graphiques
output$plot_box <- plotly::renderPlotly({

  s <- sim_result()
  req(s, s$dat)

  x_var <- input$plot_x %||% ""

  plot_boxplot_flex(
    dat = s$dat,
    x_var = x_var,
    col_var = input$plot_color %||% "",
    facet_var = if (
  is.null(input$plot_facet) ||
  identical(input$plot_facet, "__AUCUNE__")
) {
  ""
} else {
  input$plot_facet
}
  )
})


output$plot_inter <- plotly::renderPlotly({

  s <- sim_result()
  req(s, s$dat)

  x_var <- input$plot_x %||% ""

  plot_interaction_flex(
    dat = s$dat,
    x_var = x_var,
    col_var = input$plot_color %||% "",
    facet_var = if (
  is.null(input$plot_facet) ||
  identical(input$plot_facet, "__AUCUNE__")
) {
  ""
} else {
  input$plot_facet
}
  )
})

  # Téléchargement données — format choisi par radioButton
  output$dl_data <- downloadHandler(
    filename = function() {
      ext <- input$dl_format
      paste0("sim_", gsub("[, ]","_", input$design_str), ".", ext)
    },
    content = function(file) {
      s <- sim_result(); req(s)
      switch(input$dl_format,
        csv  = write.csv(s$wide, file, row.names=FALSE),
        xlsx = writexl::write_xlsx(s$wide, file),
        sav  = haven::write_sav(s$wide, file),
        dta  = {
          long_dat <- prepare_stata_data(s$dat, s$r$fcts, s$wide)
          haven::write_dta(long_dat, file)
        }
      )
    }
  )

  output$ui_ems_theoriques <- renderUI({

  s <- sim_result()
  req(s)

  ems_result <- ems_numeric_result()
  req(ems_result)

  ems_numeric <- ems_result$ems

  effect_names <- names(
    s$r$ems_table
  )

  selected_effect <- input$ems_numeric_effect

  if (
    is.null(selected_effect) ||
    !selected_effect %in% effect_names
  ) {
    selected_effect <- effect_names[1]
  }

  # ==========================================================
  # Tableau général
  # ==========================================================
  summary_table <- ems_numeric$summary
  
  components <- anova_components()
all_ms <- components$ms

summary_table$MS_observe <- vapply(
  summary_table$Effet,
  function(effect_name) {

    value <- all_ms[[effect_name]]

    if (
      is.null(value) ||
      length(value) != 1L ||
      !is.finite(value)
    ) {
      NA_real_
    } else {
      as.numeric(value)
    }
  },
  numeric(1)
)

summary_table$Ecart <-
  summary_table$MS_observe -
  summary_table$EMS_numerique

rows_summary <- lapply(
  seq_len(nrow(summary_table)),
  function(i) {

    effect_name <- summary_table$Effet[i]

    symbolic_ems <- ems_row_to_html(
      s$r$ems_table[[effect_name]]
    )

    numeric_value <- summary_table$EMS_numerique[i]

    ms_observed <- if (
      "MS_observe" %in% names(summary_table)
    ) {
      summary_table$MS_observe[i]
    } else {
      NA_real_
    }

    difference <- if (
      "Ecart" %in% names(summary_table)
    ) {
      summary_table$Ecart[i]
    } else {
      NA_real_
    }

    tags$tr(
      style = if (
        identical(effect_name, selected_effect)
      ) {
        "border-left:4px solid #0d6efd;"
      } else {
        NULL
      },

      tags$td(
        if (identical(effect_name, selected_effect)) {
          tags$b(effect_name)
        } else {
          effect_name
        }
      ),

      tags$td(symbolic_ems),

      tags$td(
  if (
    length(numeric_value) == 0L ||
    is.na(numeric_value) ||
    !is.finite(numeric_value)
  ) {
    "—"
  } else {
    tags$b(
      sprintf("%.4f", numeric_value)
    )
  }
),

      tags$td(
        if (
          length(ms_observed) == 0L ||
          is.na(ms_observed) ||
          !is.finite(ms_observed)
        ) {
          "—"
        } else {
          sprintf("%.4f", ms_observed)
        }
      ),

      tags$td(
        if (
          length(difference) == 0L ||
          is.na(difference) ||
          !is.finite(difference)
        ) {
          "—"
        } else {
          sprintf("%+.4f", difference)
        }
      )
    )
  }
)

  # ==========================================================
  # Décomposition de l'effet sélectionné
  # ==========================================================
  selected_result <- ems_numeric[[
    selected_effect
  ]]

  detail_rows <- if (
    !is.null(selected_result) &&
    !is.null(selected_result$details) &&
    nrow(selected_result$details) > 0
  ) {

    details <- selected_result$details

    lapply(
      seq_len(nrow(details)),
      function(i) {

        tags$tr(
          tags$td(details$Terme[i]),

          tags$td(
            details$Type[i]
          ),

          tags$td(
            k(
              latex_coef(
                details$Coefficient_symbolique[i]
                )
              )
            ),

          tags$td(
            sprintf(
              "%.4f",
              details$Coefficient[i]
            )
          ),

          tags$td(
            sprintf(
              "%.4f",
              details$Valeur[i]
            )
          ),

          tags$td(
            sprintf(
              "%.4f",
              details$Contribution[i]
            )
          )
        )
      }
    )

  } else {

    list(
      tags$tr(
        tags$td(
          colspan = 6,
          class = "text-muted",
          "Aucune contribution disponible."
        )
      )
    )
  }

# ==========================================================
# Paramètres générateurs
# ==========================================================
parameter_rows <- lapply(
  names(ems_result$parameters),
  function(parameter_name) {

    parameter_value <- as.numeric(
      ems_result$parameters[[parameter_name]]
    )

    parameter_type <- if (
      grepl(
        "^phi",
        parameter_name
      )
    ) {
      "Fixe"
    } else {
      "Variance"
    }

    display_name <- if (
      grepl(
        "^phi",
        parameter_name
      )
    ) {
      paste0(
        "\u03c6(",
        sub(
          "^phi",
          "",
          parameter_name
        ),
        ")"
      )
    } else {
      paste0(
        "\u03c3\u00b2(",
        parameter_name,
        ")"
      )
    }

    tags$tr(
      tags$td(display_name),

      tags$td(
        if (identical(parameter_type, "Fixe")) {
          tags$span(
            class = "badge bg-primary",
            "Fixe"
          )
        } else {
          tags$span(
            class = "badge bg-success",
            "Variance"
          )
        }
      ),

      tags$td(
        sprintf(
          "%.4f",
          parameter_value
        )
      )
    )
  }
)
  # ==========================================================
  # Interface finale
  # ==========================================================
  tagList(

    div(
  class = "alert alert-info py-2",

      HTML(
        "Les EMS numériques sont les valeurs attendues des carrés",
        "moyens sous les paramètres qui ont servi à générer les",
        "données. Un MS observé peut s'en écarter dans une simulation",
        "particulière."
      )
    ),

    layout_columns(
      col_widths = c(3, 9),

      card(
        card_header(
          "Paramètres du modèle"
        ),

        card_body(
          make_table(
            c(
              "Paramètre",
              "Type",
              "Valeur"
            ),
            parameter_rows
          )
        )
      ),

      card(
        card_header(
          "Valeurs attendues des carrés moyens (EMS)"
        ),

        card_body(
          make_table(
  c(
    "Effet",
    "EMS symbolique",
    "EMS théorique",
    "MS observé",
    "Écart"
  ),
  rows_summary
)
        )
      )
    ),

    accordion(
  open = FALSE,

  accordion_panel(
    open = character(0),
    title = "Décomposition détaillée",
    value = "decomposition_ems",

    layout_columns(
      col_widths = c(8, 4),

      div(
        class = "text-muted small mt-2",
        paste(
          "Sélectionnez un effet pour examiner les termes qui",
          "composent son carré moyen attendu."
        )
      ),

      selectInput(
        inputId = "ems_numeric_effect",
        label = NULL,
        choices = effect_names,
        selected = selected_effect,
        width = "100%"
      )
    ),

    div(
      class = "alert alert-secondary py-2 mt-2",

      tags$b(
  paste0(
    "EMS(",
    selected_effect,
    ") = "
  )
),

      sprintf(
        "%.4f",
        selected_result$value
      )
    ),

    make_table(
      c(
        "Terme",
        "Type",
        "Coefficient symbolique",
        "Coefficient",
        "Valeur",
        "Contribution"
      ),
      detail_rows
    )
  )
)
  )
})
  
  # Téléchargement syntaxe
  output$dl_syntax <- downloadHandler(
    filename = function() {
      ext <- if(input$syn_preview == "spss") "sps" else "do"
      paste0("syntaxe_", input$syn_preview, "_",
             gsub("[, ]","_", input$design_str), ".", ext)
    },
content = function(file) {
  s <- sim_result(); req(s)
  nom <- gsub("[, ]", "_", input$design_str)
  syn <- if(input$syn_preview == "spss")
    spss_syntax(s$r$fcts, s$wide, nom)
  else
    stata_syntax(s$r$fcts, s$wide, nom)
  con <- file(file, open="wt", encoding="UTF-8")
  writeLines(syn, con)
  close(con)
},
    contentType = "application/octet-stream"
  )
  
  # Nouvelle graine aléatoire
  observeEvent(input$new_seed, {
    updateNumericInput(session, "sim_seed", value=default_seed())
  })
  
  observeEvent(
  input$new_seed,
  {
    updateNumericInput(
      session = session,
      inputId = "sim_seed",
      value = sample.int(
        n = 999999,
        size = 1
      )
    )
  }
)

  # Aperçu syntaxe
output$preview_syntax <- renderText({
  s <- sim_result()
  if(is.null(s)) return("Cliquez sur Simuler d'abord.")
  format <- input$syn_preview
  if(is.null(format) || length(format) == 0) return("")
  nom <- gsub("[, ]", "_", input$design_str)
  if(format == "spss")
    spss_syntax(s$r$fcts, s$wide, nom)
  else
    stata_syntax(s$r$fcts, s$wide, nom)
})
  
  # Charger un exemple prédéfini
  observeEvent(input$exemple, {
    req(input$exemple != "")

    # Niveaux par défaut selon le plan
defaults <- list(
  "A, s(A)"         = list(A=3, s=5),
  "A, s"            = list(A=4, s=6),
  "A, B, s(A)"      = list(A=2, B=3, s=5),
  "A, B, s(AB)"     = list(A=2, B=3, s=4),
  "A, B, s"         = list(A=2, B=3, s=8),
  "A, B, C, s(A)"   = list(A=3, B=2, C=4, s=6),
  "A, B, c, s(AB)"  = list(A=2, B=3, c=4, s=5),
  "A, b, C, s(Ab)"  = list(A=2, b=4, C=3, s=5)
)

    plan <- input$exemple
    niv  <- defaults[[plan]]

    # Mettre à jour la notation
    updateTextInput(session, "design_str", value=plan)

    # Mettre à jour les niveaux après un délai
    # (pour laisser ui_levels se reconstruire)
    shinyjs::delay(300, {
      if(!is.null(niv)) {
        for(f in names(niv)) {
          updateNumericInput(session, paste0("n_", f), value=niv[[f]])
        }
      }
    })

    # Remettre le sélecteur à vide
    updateSelectInput(session, "exemple", selected="")
  })
  
output$ui_plot_controls <- renderUI({

  r <- result()
  req(r)

  # Tous les facteurs expérimentaux peuvent être représentés,
  # qu’ils soient fixes ou aléatoires.
  plot_factors <- setdiff(
    names(r$fcts),
    "s"
  )

  if (length(plot_factors) == 0) {
    return(
      helpText(
        "Aucun facteur expérimental n’est disponible."
      )
    )
  }

  # Préserver les choix actuels seulement s’ils sont encore valides.
  selected_x <- if (
    !is.null(input$plot_x) &&
    input$plot_x %in% plot_factors
  ) {
    input$plot_x
  } else {
    plot_factors[1]
  }

  selected_color <- if (
    !is.null(input$plot_color) &&
    input$plot_color %in% plot_factors &&
    !identical(input$plot_color, selected_x)
  ) {
    input$plot_color
  } else if (length(plot_factors) > 1) {
    setdiff(plot_factors, selected_x)[1]
  } else {
    ""
  }

  selected_facet <- if (
    !is.null(input$plot_facet) &&
    input$plot_facet %in% plot_factors &&
    !input$plot_facet %in% c(selected_x, selected_color)
  ) {
    input$plot_facet
  } else {
    ""
  }

  tagList(

    selectInput(
      inputId = "plot_x",
      label = "Axe X :",
      choices = plot_factors,
      selected = selected_x
    ),

    selectInput(
      inputId = "plot_color",
      label = "Couleur :",
      choices = c(
        "Aucune" = "",
        plot_factors
      ),
      selected = selected_color
    ),

    if (length(plot_factors) >= 2) {
      selectInput(
  inputId = "plot_facet",
  label = "Facette :",
  choices = c(
    "Aucune" = "__AUCUNE__",
    stats::setNames(plot_factors, plot_factors)
  ),
  selected = if (
    !is.null(input$plot_facet) &&
    input$plot_facet %in% plot_factors
  ) {
    input$plot_facet
  } else {
    "__AUCUNE__"
  }
)
    }
  )
})

output$ui_sphericite_controls <- renderUI({
  s  <- sim_result(); req(s)
  sp <- sphericite_result()

  has_quasi  <- length(s$r$quasi_list) > 0
  has_within <- !is.null(sp)

  if (!has_quasi && !has_within) return(NULL)

  # ── Card 1 : Quasi-F ───────────────────────────────────────────────────
  card_quasi <- if (has_quasi) {
    has_negative <- any(sapply(names(s$r$quasi_list), function(e) {
      ct <- s$r$quasi_list[[e]]$ct
      !is.null(ct) && length(ct$terms_neg) > 0
    }))
    card(
      card_header(class="py-1", tags$b("Quasi-F")),
      card_body(class="py-1",
        radioButtons("quasi_method", NULL,
          choices  = c("Cornfield-Tukey" = "ct",
                       "Satterthwaite"   = "satt"),
          selected = if (has_negative) "satt" else "ct",
          inline   = TRUE),
        if (has_negative)
          tags$small(class="text-warning",
            "\u26a0 CT contrindiqué (coef. négatifs)")
      )
    )
  } else NULL

  # ── Card 2 : Correction sphéricité ─────────────────────────────────────
  card_spher <- if (has_within) {
    card(
      card_header(class="py-1", tags$b("Correction sphéricité")),
      card_body(class="py-1",
        radioButtons("spher_method", NULL,
          choices  = c("Aucune" = "none", "GG" = "gg",
                       "HF" = "hf", "Box" = "box"),
          selected = if (isTRUE(sp$any_violation)) "gg" else "none",
          inline   = TRUE),
        if (isTRUE(sp$any_violation))
          tags$small(class="text-warning",
            "\u26a0 Sphéricité violée (Mauchly p < .05)")
        else
          tags$small(class="text-success",
            "\u2713 Sphéricité acceptable")
      )
    )
  } else NULL

  # ── Mise en page 2 cards ────────────────────────────────────────────────
  cards <- Filter(Negate(is.null), list(card_quasi, card_spher))
  n     <- length(cards)
  col_widths <- rep(4, n)

  do.call(layout_columns,
    c(list(col_widths=col_widths, style="margin-bottom:0.5rem;"),
      cards))
})

output$ui_postulats <- renderUI({

  s <- sim_result()
  req(s)

  sp <- sphericite_result()

  fnames <- names(s$r$fcts)

  s_nest <- s$r$fcts[["s"]]$nested_in

  between_f <- s_nest[
    s_nest %in% fnames
  ]

  within_f <- fnames[
    !fnames %in% c("s", between_f)
  ]

  # ==========================================================
  # Fonctions de formatage
  # ==========================================================
  fmt_p <- function(p) {

    if (
      length(p) == 0L ||
      is.na(p)
    ) {
      "—"
    } else if (p < .001) {
      "< .001"
    } else {
      sprintf("%.3f", p)
    }
  }

  fmt_num <- function(x, digits = 3) {

    if (
      length(x) == 0L ||
      is.na(x) ||
      !is.finite(x)
    ) {
      "—"
    } else {
      sprintf(
        paste0("%.", digits, "f"),
        x
      )
    }
  }

  sections <- list()

  # ==========================================================
  # 1. Homogénéité des variances — Levene
  # ==========================================================
  if (length(between_f) > 0L) {

    dat <- s$anova_res$dat

    grp <- if (length(between_f) == 1L) {

      factor(
        dat[[between_f[1]]]
      )

    } else {

      factor(
        interaction(
          lapply(
            between_f,
            function(factor_name) {
              dat[[factor_name]]
            }
          ),
          drop = TRUE
        )
      )
    }

    levene_res <- tryCatch(
      {
        med_grp <- tapply(
          dat$Y,
          grp,
          median
        )

        dat$z <- abs(
          dat$Y -
            med_grp[grp]
        )

        fit_lev <- stats::lm(
          z ~ grp,
          data = dat
        )

        ftest <- summary(fit_lev)$fstatistic

        F_lev   <- unname(ftest[1])
        df1_lev <- unname(ftest[2])
        df2_lev <- unname(ftest[3])

        p_lev <- stats::pf(
          F_lev,
          df1_lev,
          df2_lev,
          lower.tail = FALSE
        )

        list(
          F = F_lev,
          df1 = df1_lev,
          df2 = df2_lev,
          p = p_lev
        )
      },
      error = function(e) {
        NULL
      }
    )

    if (!is.null(levene_res)) {

      levene_violation <- isTRUE(
        levene_res$p < .05
      )

      diagnostic_levene <- if (levene_violation) {

        div(
          class = "alert alert-warning mb-3",

          tags$b(
            "\u26a0 Homogénéité des variances mise en doute"
          ),

          tags$p(
            class = "mb-0 mt-2",

            paste0(
              "Le test de Levene est significatif, ",
              "F(",
              round(levene_res$df1),
              ", ",
              round(levene_res$df2),
              ") = ",
              fmt_num(levene_res$F, 2),
              ", p = ",
              fmt_p(levene_res$p),
              ". Les variances ne semblent donc pas homogènes ",
              "entre les groupes."
            )
          )
        )

      } else {

        div(
          class = "alert alert-success mb-3",

          tags$b(
            "\u2713 Homogénéité des variances acceptable"
          ),

          tags$p(
            class = "mb-0 mt-2",

            paste0(
              "Le test de Levene n'est pas significatif, ",
              "F(",
              round(levene_res$df1),
              ", ",
              round(levene_res$df2),
              ") = ",
              fmt_num(levene_res$F, 2),
              ", p = ",
              fmt_p(levene_res$p),
              ". Rien n'indique que les variances diffèrent ",
              "entre les groupes."
            )
          )
        )
      }

      recommandation_levene <- if (levene_violation) {

        div(
          class = "alert alert-light border mb-0",

          tags$b("Conséquences et précautions"),

          tags$p(
            class = "mb-1 mt-2",

            paste(
              "L'hétérogénéité des variances peut modifier la",
              "validité des tests F, particulièrement lorsque",
              "les tailles des groupes sont inégales."
            )
          ),

          tags$ul(
            class = "mb-0",

            tags$li(
              "Examiner les variances et les valeurs aberrantes dans chaque groupe."
            ),

            tags$li(
              "Interpréter prudemment les tests F classiques."
            ),

            tags$li(
              "Envisager une transformation ou une méthode robuste lorsque la violation est importante."
            )
          )
        )

      } else {

        NULL
      }

      section_levene <- card(
        card_header(
          "Homogénéité des variances — Test de Levene"
        ),

        card_body(

          tags$p(
            class = "text-muted",

            paste(
              "Le test de Levene vérifie si la dispersion de la",
              "variable dépendante est comparable entre les",
              "groupes définis par les facteurs inter-sujets."
            )
          ),

          tags$p(
            class = "small",

            "Facteurs inter-sujets : ",
            tags$b(
              paste(
                between_f,
                collapse = ", "
              )
            )
          ),

          tags$table(
            class = "table table-bordered table-sm",

            tags$thead(
              tags$tr(
                tags$th("F de Levene"),
                tags$th("dl1"),
                tags$th("dl2"),
                tags$th("p")
              )
            ),

            tags$tbody(
              tags$tr(
                class = if (levene_violation) {
                  "table-warning"
                } else {
                  "table-success"
                },

                tags$td(
                  sprintf(
                    "%.4f",
                    levene_res$F
                  )
                ),

                tags$td(
                  as.character(
                    levene_res$df1
                  )
                ),

                tags$td(
                  as.character(
                    levene_res$df2
                  )
                ),

                tags$td(
                  fmt_p(
                    levene_res$p
                  )
                )
              )
            )
          ),

          diagnostic_levene,

          recommandation_levene
        )
      )

    } else {

      section_levene <- card(
        card_header(
          "Homogénéité des variances — Test de Levene"
        ),

        card_body(
          div(
            class = "alert alert-warning mb-0",
            "Le test de Levene ne peut pas être calculé pour ces données."
          )
        )
      )
    }

    sections[[length(sections) + 1L]] <-
      section_levene
  }

  # ==========================================================
  # 2. Sphéricité — Mauchly
  # ==========================================================
  if (
    !is.null(sp) &&
    length(sp$within_f) > 0L
  ) {

    rows_m <- lapply(
      sp$within_f,
      function(wf) {

        r <- sp$resultats[[wf]]

        violation <- (
          !isTRUE(r$trivial) &&
            !is.na(r$p_mauchly) &&
            r$p_mauchly < .05
        )

        row_class <- if (isTRUE(r$trivial)) {

          ""

        } else if (violation) {

          "table-warning"

        } else {

          "table-success"
        }

        tags$tr(
          class = row_class,

          tags$td(wf),

          tags$td(
            if (isTRUE(r$trivial)) {
              "—"
            } else {
              sprintf(
                "%.4f",
                r$W
              )
            }
          ),

          tags$td(
            if (isTRUE(r$trivial)) {
              "Non requis"
            } else {
              fmt_p(
                r$p_mauchly
              )
            }
          ),

          tags$td(
            if (isTRUE(r$trivial)) {
              "1.0000"
            } else {
              sprintf(
                "%.4f",
                r$eps_gg
              )
            }
          ),

          tags$td(
            if (isTRUE(r$trivial)) {
              "1.0000"
            } else {
              sprintf(
                "%.4f",
                r$eps_hf
              )
            }
          ),

          tags$td(
            sprintf(
              "%.4f",
              r$eps_box
            )
          )
        )
      }
    )

    if (isTRUE(sp$any_violation)) {

      diagnostic_sphericite <- div(
        class = "alert alert-warning mb-3",

        tags$b(
          "\u26a0 Sphéricité violée pour au moins un facteur intra-sujets"
        ),

        tags$p(
          class = "mb-0 mt-2",

          paste(
            "Au moins un test de Mauchly est significatif.",
            "Les degrés de liberté non corrigés ne devraient donc",
            "pas être utilisés pour les effets concernés."
          )
        )
      )

      recommandation_sphericite <- div(
        class = "alert alert-light border mb-0",

        tags$b("Correction recommandée"),

        tags$p(
          class = "mb-1 mt-2",

          paste(
            "Utilisez une correction des degrés de liberté dans",
            "l'onglet Tableau ANOVA."
          )
        ),

        tags$ul(
          class = "mb-0",

          tags$li(
            "Greenhouse–Geisser est généralement privilégiée lorsque la violation est importante."
          ),

          tags$li(
            "Huynh–Feldt est moins conservatrice lorsque l'epsilon est relativement élevé."
          ),

          tags$li(
            "La correction modifie les degrés de liberté et la valeur p, mais pas le rapport F observé."
          )
        )
      )

    } else {

      diagnostic_sphericite <- div(
        class = "alert alert-success mb-0",

        tags$b(
          "\u2713 Sphéricité acceptable"
        ),

        tags$p(
          class = "mb-0 mt-2",

          paste(
            "Aucun test de Mauchly calculable n'est significatif.",
            "Les degrés de liberté non corrigés peuvent donc être",
            "utilisés pour les facteurs intra-sujets."
          )
        )
      )

      recommandation_sphericite <- NULL
    }

    section_sphericite <- card(
      card_header(
        "Sphéricité — Test de Mauchly"
      ),

      card_body(

        tags$p(
          class = "text-muted",

          paste(
            "La sphéricité suppose que les variances des différences",
            "entre les niveaux d'un facteur intra-sujets sont",
            "comparables."
          )
        ),

        tags$p(
          class = "small",

          "Facteurs intra-sujets : ",

          tags$b(
            paste(
              sp$within_f,
              collapse = ", "
            )
          )
        ),

        tags$table(
          class = "table table-bordered table-sm",

          tags$thead(
            tags$tr(
              tags$th("Facteur"),
              tags$th("W de Mauchly"),
              tags$th("p"),
              tags$th("\u03b5 GG"),
              tags$th("\u03b5 HF"),
              tags$th("\u03b5 Box")
            )
          ),

          tags$tbody(
            rows_m
          )
        ),

        diagnostic_sphericite,

        recommandation_sphericite
      )
    )

    sections[[length(sections) + 1L]] <-
      section_sphericite
  }

  # ==========================================================
  # Aucun postulat applicable
  # ==========================================================
  if (length(sections) == 0L) {

    return(
      div(
        class = "alert alert-info",

        paste(
          "Aucun test d'homogénéité ou de sphéricité n'est",
          "requis pour ce plan."
        )
      )
    )
  }

# ==========================================================
# Interface finale
# ==========================================================
tagList(

  div(
    class = "alert alert-info mb-3",

    tags$b("À propos de cette simulation : "),

    paste(
      "Les données sont actuellement générées de manière à respecter",
      "les postulats de l'ANOVA. Les tests présentés ici servent à",
      "illustrer leur vérification sur un échantillon particulier.",
      "Un résultat significatif peut donc occasionnellement apparaître",
      "uniquement en raison de la fluctuation d'échantillonnage."
    )
  ),

  tagList(sections)
)
})

output$ui_interpretation <- renderUI({

  s <- sim_result()
  req(s)

  sp <- sphericite_result()

  # ==========================================================
  # Objets nécessaires
  # ==========================================================
  factors    <- s$r$fcts
  effects    <- s$r$effects
  error_info <- s$r$error_info
  quasi_list <- s$r$quasi_list
  n_levels   <- s$r$n_levels
  ems_table  <- s$r$ems_table

  fnames <- names(factors)

  s_nest <- factors[["s"]]$nested_in %||% character(0)

  between_f <- intersect(
    s_nest,
    fnames
  )

  within_f <- setdiff(
    fnames,
    c("s", between_f)
  )

  spher_method <- input$spher_method %||% "none"
  quasi_method <- input$quasi_method %||% "ct"

  # ==========================================================
  # Fonctions utilitaires
  # ==========================================================
  qualifier_eta2p <- function(value) {

    if (
      is.na(value) ||
      !is.finite(value)
    ) {
      return("")
    }

    if (value >= 0.14) {
      "grand"
    } else if (value >= 0.06) {
      "modéré"
    } else if (value >= 0.01) {
      "petit"
    } else {
      "négligeable"
    }
  }

  fmt_p_txt <- function(value) {

    if (
      is.na(value) ||
      !is.finite(value)
    ) {
      return("p = —")
    }

    if (value < 0.001) {
      "p < .001"
    } else {
      sprintf("p = %.3f", value)
    }
  }

  fmt_df <- function(value) {

    if (
      is.na(value) ||
      !is.finite(value)
    ) {
      return("?")
    }

    if (abs(value - round(value)) < 1e-8) {
      sprintf("%g", value)
    } else {
      sprintf("%.1f", value)
    }
  }

  find_effect_info <- function(effect_name) {

    clean_name <- gsub(
      ":",
      "",
      effect_name
    )

    if (!is.null(effects[[effect_name]])) {
      effects[[effect_name]]
    } else if (!is.null(effects[[clean_name]])) {
      effects[[clean_name]]
    } else {
      NULL
    }
  }

  classify_effect <- function(effect_name) {

    effect_info <- find_effect_info(effect_name)

    if (is.null(effect_info)) {

      effect_parts <- strsplit(
        effect_name,
        ":",
        fixed = TRUE
      )[[1]]

      return(
        list(
          factors = effect_parts,
          fixed = character(0),
          random = character(0),
          is_random = FALSE,
          is_interaction = length(effect_parts) > 1,
          order = length(effect_parts)
        )
      )
    }

    effect_factors <- setdiff(
      effect_info$factors,
      "s"
    )

    fixed_members <- effect_factors[
      vapply(
        effect_factors,
        function(f) {
          !is.null(factors[[f]]) &&
            isTRUE(factors[[f]]$fixed)
        },
        logical(1)
      )
    ]

    random_members <- effect_factors[
      vapply(
        effect_factors,
        function(f) {
          !is.null(factors[[f]]) &&
            isTRUE(factors[[f]]$random)
        },
        logical(1)
      )
    ]

    list(
      factors = effect_factors,
      fixed = fixed_members,
      random = random_members,
      is_random = length(random_members) > 0,
      is_interaction = length(effect_factors) > 1,
      order = length(effect_factors)
    )
  }

  # ==========================================================
  # Description générale du plan
  # ==========================================================
  type_plan <- if (length(within_f) == 0) {

    "entièrement entre-sujets (between-subjects)"

  } else if (length(between_f) == 0) {

    "entièrement intra-sujets (within-subjects)"

  } else {

    "mixte (between-subjects et within-subjects)"
  }

  experimental_factors <- setdiff(
    fnames,
    "s"
  )

  niveaux_str <- paste(
    vapply(
      experimental_factors,
      function(f) {
        paste0(
          n_levels[[f]],
          " niveaux de ",
          f
        )
      },
      character(1)
    ),
    collapse = ", "
  )

  n_sujets <- if (length(between_f) > 0) {

    n_groups <- prod(
      vapply(
        between_f,
        function(f) {
          n_levels[[f]]
        },
        numeric(1)
      )
    )

    paste0(
      n_levels[["s"]],
      " sujets par groupe (",
      n_groups * n_levels[["s"]],
      " au total)"
    )

  } else {

    paste0(
      n_levels[["s"]],
      " sujets"
    )
  }

  correction_text <- if (!identical(spher_method, "none")) {

    paste0(
      " La correction de ",
      switch(
        spher_method,
        gg = "Greenhouse–Geisser",
        hf = "Huynh–Feldt",
        box = "Box (conservatrice)",
        ""
      ),
      " a été appliquée aux effets intra-sujets."
    )

  } else {

    ""
  }

  para_plan <- paste0(
    "Un plan d'expérience ",
    type_plan,
    " a été utilisé, avec ",
    niveaux_str,
    ". L'échantillon comprend ",
    n_sujets,
    ".",
    correction_text
  )

  # ==========================================================
  # Résultats ANOVA
  # ==========================================================
  summ <- s$anova_res$summary

  if (
    is.null(summ) ||
    length(summ) == 0
  ) {
    return(
      div(
        class = "alert alert-info",
        "Aucun résultat d'analyse n'est disponible."
      )
    )
  }

  # ==========================================================
  # Correspondance des strates aov() avec les termes EMS
  # ==========================================================
  build_stratum_map <- function() {

    error_names <- names(ems_table)[
      vapply(
        names(ems_table),
        function(effect_name) {
          grepl("s", effect_name)
        },
        logical(1)
      )
    ]

    stratum_map <- list()

    for (error_name in error_names) {

      non_s <- intersect(
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

      non_s <- setdiff(
        non_s,
        "s"
      )

      within_members <- intersect(
        non_s,
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

    stratum_map
  }

  stratum_map <- build_stratum_map()

  # ==========================================================
  # Première passe : SS, MS et df
  # ==========================================================
  all_ss <- list()
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

      display_name <- if (identical(source_name, "Residuals")) {

        ems_label <- stratum_map[[stratum_key]]

        if (!is.null(ems_label)) {
          ems_label
        } else {
          stratum_key
        }

      } else {

        source_name
      }

      all_ss[[display_name]] <- tbl[i, "Sum Sq"]
      all_ms[[display_name]] <- tbl[i, "Mean Sq"]
      all_df[[display_name]] <- tbl[i, "Df"]

      clean_name <- gsub(
        ":",
        "",
        source_name
      )

      if (!identical(clean_name, source_name)) {
        all_ss[[clean_name]] <- tbl[i, "Sum Sq"]
        all_ms[[clean_name]] <- tbl[i, "Mean Sq"]
        all_df[[clean_name]] <- tbl[i, "Df"]
      }
    }
  }

  # ==========================================================
  # Quasi-F numériques
  # ==========================================================
  quasi_numeric <- if (length(quasi_list) > 0) {

    stats::setNames(
      lapply(
        names(quasi_list),
        function(effect_name) {

          compute_quasi_f_numeric(
            effect_name,
            quasi_list[[effect_name]],
            all_ms,
            all_df
          )
        }
      ),
      names(quasi_list)
    )

  } else {

    NULL
  }

  # ==========================================================
  # Corrections de sphéricité
  # ==========================================================
  eps_by_factor <- if (
    !is.null(sp) &&
    !identical(spher_method, "none")
  ) {

    stats::setNames(
      lapply(
        sp$within_f,
        function(within_factor) {

          result_factor <- sp$resultats[[within_factor]]

          if (is.null(result_factor)) {
            return(1)
          }

          switch(
            spher_method,
            gg = result_factor$eps_gg,
            hf = result_factor$eps_hf,
            box = result_factor$eps_box,
            1
          )
        }
      ),
      sp$within_f
    )

  } else {

    NULL
  }

  # ==========================================================
  # Produire les paragraphes d'interprétation
  # ==========================================================
  interpretation_panels <- list()

significant_fixed_interactions <- character(0)
significant_effect_names       <- character(0)

n_effects_tested     <- 0L
n_significant        <- 0L
n_significant_fixed  <- 0L
n_significant_random <- 0L

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

    for (i in seq_len(nrow(tbl))) {

      effect_name <- trimws(
        rownames(tbl)[i]
      )

      if (identical(effect_name, "Residuals")) {
        next
      }

      clean_name <- gsub(
        ":",
        "",
        effect_name
      )

      F_value <- if ("F value" %in% colnames(tbl)) {
        tbl[i, "F value"]
      } else {
        NA_real_
      }

      p_value <- if ("Pr(>F)" %in% colnames(tbl)) {
        tbl[i, "Pr(>F)"]
      } else {
        NA_real_
      }

      df_effect <- tbl[i, "Df"]
      ss_effect <- tbl[i, "Sum Sq"]

      # ------------------------------------------------------
      # Terme d'erreur pur
      # ------------------------------------------------------
      error_name <- error_info$pure[[effect_name]]

      if (is.null(error_name)) {
        error_name <- error_info$pure[[clean_name]]
      }

      df_error <- if (!is.null(error_name)) {
        all_df[[error_name]]
      } else {
        NA_real_
      }

      ss_error <- if (!is.null(error_name)) {
        all_ss[[error_name]]
      } else {
        NA_real_
      }

      # ------------------------------------------------------
      # Quasi-F
      # ------------------------------------------------------
      is_quasi <- !is.null(
        error_info$quasi[[effect_name]]
      ) ||
        !is.null(
          error_info$quasi[[clean_name]]
        )

      F_effective <- F_value
      p_effective <- p_value
      df_denominator <- df_error

      quasi_note <- ""
      spher_note <- ""

      if (
        is_quasi &&
        !is.null(quasi_numeric)
      ) {

        quasi_key <- if (!is.null(quasi_numeric[[effect_name]])) {
          effect_name
        } else {
          clean_name
        }

        quasi_result <- quasi_numeric[[quasi_key]]

        if (!is.null(quasi_result)) {

          use_ct <- identical(quasi_method, "ct") &&
            !is.null(quasi_result$ct) &&
            !is.na(quasi_result$ct$F) &&
            is.finite(quasi_result$ct$F)

          selected_result <- if (use_ct) {

            quasi_result$ct

          } else if (
            !is.null(quasi_result$satt) &&
            !is.na(quasi_result$satt$F) &&
            is.finite(quasi_result$satt$F)
          ) {

            quasi_result$satt

          } else {

            NULL
          }

          if (!is.null(selected_result)) {

            F_effective <- selected_result$F
            p_effective <- selected_result$p
            df_denominator <- selected_result$df_quasi

            method_label <- if (use_ct) {
              "Cornfield–Tukey"
            } else {
              "Satterthwaite"
            }

            quasi_note <- paste0(
              " (quasi-F, ",
              method_label,
              ")"
            )
          }
        }

      } else if (
        !is.null(eps_by_factor) &&
        !is.na(F_value) &&
        is.finite(F_value)
      ) {

        effect_parts <- unlist(
          strsplit(
            clean_name,
            ""
          )
        )

        matching_within <- intersect(
          effect_parts,
          names(eps_by_factor)
        )

        if (
          length(matching_within) > 0 &&
          !is.na(df_error) &&
          is.finite(df_error)
        ) {

          epsilon <- min(
            vapply(
              matching_within,
              function(within_factor) {
                eps_by_factor[[within_factor]]
              },
              numeric(1)
            ),
            na.rm = TRUE
          )

          if (
            is.finite(epsilon) &&
            epsilon > 0
          ) {

            p_effective <- stats::pf(
              F_value,
              df_effect * epsilon,
              df_error * epsilon,
              lower.tail = FALSE
            )

            spher_note <- paste0(
              " [p corrigé ",
              switch(
                spher_method,
                gg = "GG",
                hf = "HF",
                box = "Box",
                ""
              ),
              "]"
            )
          }
        }
      }

      if (
        is.na(F_effective) ||
        !is.finite(F_effective)
      ) {
        next
      }

      effect_class <- classify_effect(effect_name)

      is_random_effect <- isTRUE(
        effect_class$is_random
      )

      is_interaction <- isTRUE(
        effect_class$is_interaction
      )

      is_fixed_effect <- !is_random_effect
      
      readable_name <- gsub(
        ":",
        " × ",
        effect_name
      )
      
      significant <- !is.na(p_effective) &&
        is.finite(p_effective) &&
        p_effective < 0.05
      
      # ------------------------------------------------------
# Compteurs pour le diagnostic automatique
# ------------------------------------------------------
n_effects_tested <- n_effects_tested + 1L

if (isTRUE(significant)) {

  n_significant <- n_significant + 1L

  significant_effect_names <- unique(
    c(
      significant_effect_names,
      readable_name
    )
  )

  if (is_random_effect) {

    n_significant_random <-
      n_significant_random + 1L

  } else {

    n_significant_fixed <-
      n_significant_fixed + 1L
  }
}

      # ------------------------------------------------------
      # Eta-carré partiel :
      # uniquement pour les effets entièrement fixes avec
      # un terme d'erreur pur disponible
      # ------------------------------------------------------
      eta2p <- if (
        is_fixed_effect &&
        !is.na(ss_effect) &&
        is.finite(ss_effect) &&
        !is.na(ss_error) &&
        is.finite(ss_error) &&
        (ss_effect + ss_error) > 0
      ) {

        ss_effect / (ss_effect + ss_error)

      } else {

        NA_real_
      }

      eta_text <- if (!is.na(eta2p)) {

        paste0(
          ", η²p = ",
          sprintf("%.3f", eta2p),
          " [effet ",
          qualifier_eta2p(eta2p),
          "]"
        )

      } else {

        ""
      }

      readable_name <- gsub(
        ":",
        " × ",
        effect_name
      )

      df_string <- paste0(
        "F(",
        fmt_df(df_effect),
        ", ",
        fmt_df(df_denominator),
        ")"
      )

      result_string <- paste0(
        df_string,
        " = ",
        sprintf("%.3f", F_effective),
        ", ",
        fmt_p_txt(p_effective),
        spher_note,
        eta_text,
        quasi_note
      )

      # ------------------------------------------------------
      # Formulation adaptée au type d'effet
      # ------------------------------------------------------
      if (!is_random_effect) {

        # Effet principal fixe
        if (!is_interaction) {

          texte <- if (significant) {

            paste0(
              "L'effet de ",
              readable_name,
              " est statistiquement significatif, ",
              result_string,
              ". Les moyennes diffèrent significativement entre les ",
              "niveaux de ",
              readable_name,
              "."
            )

          } else {

            paste0(
              "L'effet de ",
              readable_name,
              " n'est pas statistiquement significatif, ",
              result_string,
              ". Les données ne permettent pas de conclure à une ",
              "différence entre les niveaux de ",
              readable_name,
              "."
            )
          }

        } else {

          # Interaction composée uniquement de facteurs fixes
          texte <- if (significant) {

            significant_fixed_interactions <- unique(
              c(
                significant_fixed_interactions,
                readable_name
              )
            )

            paste0(
              "L'interaction ",
              readable_name,
              " est statistiquement significative, ",
              result_string,
              ". L'effet d'au moins un facteur dépend donc du niveau ",
              "de l'autre facteur ou des autres facteurs. Le graphique ",
              "d'interaction doit être examiné avant d'interpréter ",
              "isolément les effets d'ordre inférieur."
            )

          } else {

            paste0(
              "L'interaction ",
              readable_name,
              " n'est pas statistiquement significative, ",
              result_string,
              ". Les données ne montrent pas que l'effet d'un facteur ",
              "varie systématiquement selon les niveaux de l'autre ",
              "facteur ou des autres facteurs."
            )
          }
        }

      } else if (!is_interaction) {

        # Facteur aléatoire principal
        texte <- if (significant) {

          paste0(
            "La composante associée au facteur aléatoire ",
            readable_name,
            " est statistiquement significative, ",
            result_string,
            ". Les données indiquent une variabilité systématique entre ",
            "les niveaux de la population représentée par ce facteur."
          )

        } else {

          paste0(
            "La composante associée au facteur aléatoire ",
            readable_name,
            " n'est pas statistiquement significative, ",
            result_string,
            ". Les données ne fournissent pas d'indication suffisante ",
            "d'une variabilité systématique entre les niveaux de la ",
            "population représentée par ce facteur."
          )
        }

      } else {

        # Interaction contenant au moins un facteur aléatoire
        random_label <- paste(
          effect_class$random,
          collapse = ", "
        )

        texte <- if (significant) {

          paste0(
            "L'interaction ",
            readable_name,
            ", qui comprend le facteur aléatoire ",
            random_label,
            ", est statistiquement significative, ",
            result_string,
            ". La variabilité associée à cette interaction contribue ",
            "significativement au modèle. Elle doit être interprétée ",
            "comme une composante de variation plutôt que comme une ",
            "simple comparaison entre des niveaux particuliers."
          )

        } else {

          paste0(
            "L'interaction ",
            readable_name,
            ", qui comprend le facteur aléatoire ",
            random_label,
            ", n'est pas statistiquement significative, ",
            result_string,
            ". Les données ne fournissent pas d'indication suffisante ",
            "que la variabilité associée à cette interaction contribue ",
            "au modèle."
          )
        }
      }

      # ------------------------------------------------------
# Présentation de l'effet dans un panneau repliable
# ------------------------------------------------------
status_label <- if (significant) {
  "Significatif"
} else {
  "Non significatif"
}

status_class <- if (significant) {
  "badge bg-success"
} else {
  "badge bg-secondary"
}

effect_type_label <- if (!is_random_effect) {

  if (is_interaction) {
    "Interaction"
  } else {
    "Fixe"
  }

} else {

  if (is_interaction) {
    "Interaction aléatoire"
  } else {
    "Aléatoire"
  }
}

effect_type_class <- "badge bg-secondary"

panel_title <- tagList(

  tags$span(
    style="display:inline-block; min-width:90px;",
    tags$b(readable_name)
  ),

  tags$span(
    class=paste0(status_class," me-2"),
    status_label
  ),

  tags$span(
    class="text-muted fst-italic",
    effect_type_label
  )
)

interpretation_panels[[
  length(interpretation_panels) + 1L
]] <- accordion_panel(
  title = panel_title,
  value = paste0(
    "interpretation_",
    gsub(
      "[^A-Za-z0-9]",
      "_",
      clean_name
    )
  ),

  div(
    class = "mb-3",

    tags$div(
      class = if (significant) {
        "alert alert-success py-2"
      } else {
        "alert alert-secondary py-2"
      },

      tags$b(result_string)
    ),

    tags$p(
      class = "mb-0 mt-2",
      texte
    )
  )
)
    }
  }

  if (length(interpretation_panels) == 0L) {

  return(
    div(
      class = "alert alert-info",
      "Cliquez sur Simuler pour générer l'interprétation."
    )
  )
}

  # ==========================================================
  # Avertissement en cas d'interaction fixe significative
  # ==========================================================
  interaction_warning <- if (
    length(significant_fixed_interactions) > 0
  ) {

    div(
      class = "alert alert-warning mb-3",

      tags$h5(
        class = "alert-heading",
        "Interaction significative détectée"
      ),

      p(
        "Les interactions suivantes sont significatives : ",
        tags$b(
          paste(
            significant_fixed_interactions,
            collapse = ", "
          )
        ),
        "."
      ),

      p(
        class = "mb-0",
        "Les effets principaux et les interactions d'ordre inférieur ",
        "qui les composent ne doivent pas être interprétés isolément. ",
        "Examinez le graphique d'interaction pour comprendre la forme ",
        "de cette dépendance. Les analyses d'effets simples relèvent ",
        "d'une analyse factorielle complémentaire."
      )
    )

  } else {

    NULL
  }

  # ==========================================================
  # Bandeau de synthèse
  # ==========================================================
  correction_label <- switch(
    spher_method,
    none = "aucune",
    gg = "Greenhouse–Geisser",
    hf = "Huynh–Feldt",
    box = "Box",
    "aucune"
  )

  quasi_label <- if (identical(quasi_method, "ct")) {
    "Cornfield–Tukey"
  } else {
    "Satterthwaite"
  }
  
  # ==========================================================
# Diagnostic automatique
# ==========================================================
diagnostic_rows <- list(

  tags$tr(
    tags$td("Type de plan"),
    tags$td(tags$b(type_plan))
  ),

  tags$tr(
    tags$td("Effets testés"),
    tags$td(tags$b(n_effects_tested))
  ),

  tags$tr(
    tags$td("Effets significatifs"),
    tags$td(tags$b(n_significant))
  ),

  tags$tr(
    tags$td("Effets fixes significatifs"),
    tags$td(tags$b(n_significant_fixed))
  ),

  tags$tr(
    tags$td("Composantes aléatoires significatives"),
    tags$td(tags$b(n_significant_random))
  ),

  tags$tr(
    tags$td("Quasi-F utilisés"),
    tags$td(
      tags$b(
        if (length(quasi_list) > 0L) {
          paste0("Oui — ", quasi_label)
        } else {
          "Non"
        }
      )
    )
  ),

  tags$tr(
    tags$td("Correction de sphéricité"),
    tags$td(tags$b(correction_label))
  )
)

significant_summary <- if (n_significant > 0L) {

  div(
    class = "alert alert-success mb-0",

    tags$b("Effets significatifs : "),

    paste(
      significant_effect_names,
      collapse = ", "
    )
  )

} else {

  div(
    class = "alert alert-secondary mb-0",

    tags$b("Aucun effet statistiquement significatif.")
  )
}

diagnostic_card <- card(

  card_header(
    "Diagnostic automatique"
  ),

  card_body(

    layout_columns(
      col_widths = c(7, 5),

      tags$table(
        class = "table table-bordered table-sm mb-0",

        tags$thead(
          tags$tr(
            tags$th("Indicateur"),
            tags$th("Résultat")
          )
        ),

        tags$tbody(
          diagnostic_rows
        )
      ),

      significant_summary
    )
  )
)

# ==========================================================
# Interface finale
# ==========================================================
tagList(

  div(
    class = "alert alert-secondary py-2 mb-3",

    tags$b("Plan : "),
    tags$code(input$design_str),

    " — ",

    tags$b("Correction : "),
    correction_label,

    if (length(quasi_list) > 0L) {
      tagList(
        " — ",
        tags$b("Quasi-F : "),
        quasi_label
      )
    }
  ),

  diagnostic_card,

  interaction_warning,

  card(
    card_header(
      "Interprétation détaillée des effets"
    ),

    card_body(

      tags$p(
        class = "text-muted fst-italic mb-3",
        para_plan
      ),

      accordion(
        open = FALSE,
        !!!interpretation_panels
      ),

      tags$p(
        class = "text-muted fst-italic mt-3 mb-0",

        tags$small(
          paste0(
            "Note : texte généré automatiquement. ",
            "Les seuils descriptifs de Cohen pour η²p ",
            "(petit ≥ .01, modéré ≥ .06, grand ≥ .14) ",
            "sont appliqués uniquement aux effets entièrement fixes."
          )
        )
      )
    )
  )
)
})

mc_result <- eventReactive(
  input$btn_mc,
  {

    s <- sim_result()
    req(s)

    withProgress(
      message = "Simulation Monte-Carlo en cours...",
      value = 0,
      {

mc <- shiny::withProgress(
  message = "Simulation Monte-Carlo",
  detail = "Initialisation...",
  value = 0,
  {

    run_monte_carlo(
      r = s$r,
      effect_sizes = s$effect_sizes,
      random_variances = s$random_variances,
      residual_variance = s$residual_variance,
      B = as.integer(input$mc_B),
      seed = as.integer(input$mc_seed),
      progress = FALSE,

      progress_callback = function(completed, total) {

        p <- completed / total

        shiny::setProgress(
          value = p,
          detail = sprintf(
            "%d / %d réplications (%.0f%%)",
            completed,
            total,
            100 * p
          )
        )
      }
    )

  }
)
      }
    )
    mc
  }
)

output$ui_mc_summary <- renderUI({

  mc <- mc_result()
  req(mc)

  tbl <- mc$summary

  if (
    is.null(tbl) ||
    nrow(tbl) == 0L
  ) {
    return(
      div(
        class = "alert alert-info",
        "Aucun résultat Monte-Carlo disponible."
      )
    )
  }

  # Sécuriser les colonnes numériques
  numeric_columns <- intersect(
    c(
      "EMS_theorique",
      "N_MS",
      "MS_moyen",
      "MS_ecart_type",
      "Erreur_MC",
      "Ecart",
      "Ecart_standardise",
      "MS_mediane",
      "F_moyen",
      "Taux_rejet",
      "N_F_valide"
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

  # ========================================================
  # Diagnostic global
  # ========================================================
  z_values <- tbl$Ecart_standardise[
    is.finite(tbl$Ecart_standardise)
  ]

  max_abs_z <- if (length(z_values) > 0L) {
    max(abs(z_values))
  } else {
    NA_real_
  }

  n_green <- sum(
    is.finite(tbl$Ecart_standardise) &
      abs(tbl$Ecart_standardise) <= 2
  )

  n_yellow <- sum(
    is.finite(tbl$Ecart_standardise) &
      abs(tbl$Ecart_standardise) > 2 &
      abs(tbl$Ecart_standardise) <= 3
  )

  n_red <- sum(
    is.finite(tbl$Ecart_standardise) &
      abs(tbl$Ecart_standardise) > 3
  )

  mc_diagnostic <- if (
    is.finite(max_abs_z) &&
    max_abs_z <= 2
  ) {

    div(
      class = "alert alert-success mb-3",

      tags$b(
        "✓ Le générateur reproduit les EMS théoriques."
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "Aucun écart standardisé ne dépasse 2 en valeur absolue.",
          "Les différences observées sont compatibles avec la",
          "fluctuation Monte-Carlo."
        )
      )
    )

  } else if (
    is.finite(max_abs_z) &&
    max_abs_z <= 3
  ) {

    div(
      class = "alert alert-warning mb-3",

      tags$b(
        "⚠ La validation est généralement satisfaisante, avec quelques réserves."
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "Au moins une moyenne empirique se situe entre deux",
          "et trois erreurs Monte-Carlo de son EMS.",
          "Davantage de réplications permettraient de vérifier",
          "si cet écart se résorbe."
        )
      )
    )

  } else {

    div(
      class = "alert alert-danger mb-3",

      tags$b(
        "⚠ Une divergence importante doit être examinée."
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "Au moins une moyenne empirique se trouve à plus de",
          "trois erreurs Monte-Carlo de son EMS.",
          "Il faut vérifier le nombre de réplications, le modèle",
          "générateur et le calcul des carrés moyens."
        )
      )
    )
  }

  # ========================================================
  # Lignes du tableau
  # ========================================================
  rows <- lapply(
    seq_len(nrow(tbl)),
    function(i) {

      z_value <- tbl$Ecart_standardise[i]

      row_class <- if (
        is.finite(z_value) &&
        abs(z_value) <= 2
      ) {
        "table-success"
      } else if (
        is.finite(z_value) &&
        abs(z_value) <= 3
      ) {
        "table-warning"
      } else {
        "table-danger"
      }

      tags$tr(
        class = row_class,

        tags$td(
          tags$b(tbl$Effet[i])
        ),

        tags$td(
          sprintf(
            "%.4f",
            tbl$EMS_theorique[i]
          )
        ),

        tags$td(
          sprintf(
            "%.4f",
            tbl$MS_moyen[i]
          )
        ),

        tags$td(
          sprintf(
            "%.4f",
            tbl$Erreur_MC[i]
          )
        ),

        tags$td(
          sprintf(
            "%+.4f",
            tbl$Ecart[i]
          )
        ),

        tags$td(
          if (is.finite(z_value)) {
            sprintf(
              "%+.2f",
              z_value
            )
          } else {
            "—"
          }
        ),

        tags$td(
          if (
            is.finite(tbl$Taux_rejet[i])
          ) {
            sprintf(
              "%.3f",
              tbl$Taux_rejet[i]
            )
          } else {
            "—"
          }
        )
      )
    }
  )

  # ========================================================
  # Interface finale
  # ========================================================
  tagList(

    mc_diagnostic,

    layout_columns(
      col_widths = c(3, 3, 3, 3),

      value_box(
        title = "Réplications",
        value = mc$B_completed,
        showcase = icon("repeat")
      ),

      value_box(
        title = "Effets compatibles",
        value = paste0(
          n_green,
          " / ",
          nrow(tbl)
        ),
        showcase = icon("circle-check"),
        theme = "success"
      ),

      value_box(
        title = "À surveiller",
        value = n_yellow,
        showcase = icon("triangle-exclamation"),
        theme = if (n_yellow > 0) {
          "warning"
        } else {
          "light"
        }
      ),

      value_box(
        title = "Divergences",
        value = n_red,
        showcase = icon("circle-exclamation"),
        theme = if (n_red > 0) {
          "danger"
        } else {
          "light"
        }
      )
    ),

    card(
      fill = FALSE,

      card_header(
        class = "py-2",

        div(
          class = paste(
            "d-flex justify-content-between",
            "align-items-center flex-wrap gap-2"
          ),

          tags$b(
            "EMS théoriques et moyennes Monte-Carlo"
          ),

          actionButton(
            inputId = "btn_mc_summary_interpret",
            label = "Comprendre les résultats",
            icon = icon("book-open"),
            class = "btn-outline-primary btn-sm"
          )
        )
      ),

      card_body(
        class = "py-2",

        tags$table(
          class = paste(
            "table table-bordered table-sm",
            "align-middle mb-2"
          ),

          tags$thead(
            tags$tr(
              tags$th("Effet"),
              tags$th("EMS théorique"),
              tags$th("MS moyen"),
              tags$th("Erreur MC"),
              tags$th("Écart"),
              tags$th("Écart standardisé"),
              tags$th("Taux de rejet")
            )
          ),

          tags$tbody(
            rows
          )
        ),

        tags$p(
          class = "text-muted small mb-0",

          paste(
            "Repères : vert si |écart standardisé| ≤ 2;",
            "jaune entre 2 et 3; rouge au-delà de 3."
          )
        )
      )
    ),

    if (nrow(mc$failures) > 0L) {

      div(
        class = "alert alert-warning mt-3 mb-0",

        tags$b("Réplications ayant échoué : "),

        nrow(mc$failures),

        paste(
          "Les moyennes présentées sont calculées uniquement",
          "à partir des réplications complétées."
        )
      )

    } else {

      NULL
    }
  )
})

observeEvent(input$btn_mc_summary_interpret, {

  mc <- mc_result()
  req(mc)

  tbl <- mc$summary
  req(
    !is.null(tbl),
    nrow(tbl) > 0L
  )

  z_values <- tbl$Ecart_standardise[
    is.finite(tbl$Ecart_standardise)
  ]

  max_abs_z <- if (length(z_values) > 0L) {
    max(abs(z_values))
  } else {
    NA_real_
  }

  worst_index <- if (length(z_values) > 0L) {

    finite_indices <- which(
      is.finite(tbl$Ecart_standardise)
    )

    finite_indices[
      which.max(
        abs(
          tbl$Ecart_standardise[
            finite_indices
          ]
        )
      )
    ]

  } else {

    NA_integer_
  }

  worst_effect <- if (
    length(worst_index) == 1L &&
    is.finite(worst_index)
  ) {
    tbl$Effet[worst_index]
  } else {
    "—"
  }

  worst_z <- if (
    length(worst_index) == 1L &&
    is.finite(worst_index)
  ) {
    tbl$Ecart_standardise[worst_index]
  } else {
    NA_real_
  }

  global_interpretation <- if (
    is.finite(max_abs_z) &&
    max_abs_z <= 2
  ) {

    div(
      class = "alert alert-success",

      tags$b(
        "Validation globale satisfaisante."
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "Toutes les moyennes empiriques sont situées à moins",
          "de deux erreurs Monte-Carlo de leur EMS.",
          "Le générateur reproduit donc correctement les",
          "espérances théoriques du plan."
        )
      )
    )

  } else if (
    is.finite(max_abs_z) &&
    max_abs_z <= 3
  ) {

    div(
      class = "alert alert-warning",

      tags$b(
        "Validation généralement satisfaisante, mais encore instable."
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "La plupart des effets reproduisent correctement leur EMS,",
          "mais au moins un écart se situe entre deux et trois",
          "erreurs Monte-Carlo. Ce résultat demeure plausible;",
          "une nouvelle simulation ou davantage de réplications",
          "permettront de le vérifier."
        )
      )
    )

  } else {

    div(
      class = "alert alert-danger",

      tags$b(
        "Une divergence doit être examinée."
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "Au moins un effet présente un écart trop important",
          "pour être attribué aisément à la fluctuation",
          "Monte-Carlo. Une telle situation peut provenir d’un",
          "nombre insuffisant de réplications ou d’une incohérence",
          "entre les EMS, le générateur et l’analyse."
        )
      )
    )
  }

  showModal(
    modalDialog(

      title = "Comprendre les résultats — Validation des EMS",

      div(
        class = "alert alert-primary py-2",

        tags$b(
          "Étape 3 sur 4 — Validation du générateur"
        ),

        tags$div(
          class = "mt-1",

          paste(
            "Cette étape vérifie que les données simulées",
            "respectent réellement les propriétés théoriques",
            "du plan d’expérience."
          )
        )
      ),

      tags$h5(
        "1. Que compare ce tableau ?"
      ),

      tags$ul(

        tags$li(
          tags$b("L’EMS théorique "),

          paste(
            "est la valeur attendue du carré moyen, calculée",
            "à partir du plan et des paramètres générateurs."
          )
        ),

        tags$li(
          tags$b("Le MS moyen "),

          paste(
            "est la moyenne des carrés moyens observés dans",
            mc$B_completed,
            "réplications Monte-Carlo."
          )
        ),

        tags$li(
          tags$b("L’erreur Monte-Carlo "),

          paste(
            "mesure l’incertitude qui demeure sur la moyenne",
            "empirique en raison du nombre fini de réplications."
          )
        ),

        tags$li(
          tags$b("L’écart standardisé "),

          paste(
            "exprime la différence entre le MS moyen et l’EMS",
            "en unités d’erreur Monte-Carlo."
          )
        ),

        tags$li(
          tags$b("Le taux de rejet "),

          paste(
            "indique la proportion des réplications conduisant",
            "au rejet de l’hypothèse nulle lorsqu’un rapport F",
            "classique est disponible."
          )
        )
      ),

      tags$h5(
        "2. Comment lire les couleurs ?"
      ),

      tags$table(
        class = "table table-sm table-bordered",

        tags$thead(
          tags$tr(
            tags$th("Couleur"),
            tags$th("Écart standardisé"),
            tags$th("Interprétation")
          )
        ),

        tags$tbody(

          tags$tr(
            class = "table-success",

            tags$td(tags$b("Vert")),

            tags$td("|z| ≤ 2"),

            tags$td(
              paste(
                "La différence est compatible avec la",
                "fluctuation Monte-Carlo."
              )
            )
          ),

          tags$tr(
            class = "table-warning",

            tags$td(tags$b("Jaune")),

            tags$td("2 < |z| ≤ 3"),

            tags$td(
              paste(
                "L’écart est relativement rare, mais demeure",
                "plausible. Davantage de réplications sont utiles."
              )
            )
          ),

          tags$tr(
            class = "table-danger",

            tags$td(tags$b("Rouge")),

            tags$td("|z| > 3"),

            tags$td(
              paste(
                "La différence mérite une vérification du",
                "générateur ou du calcul des EMS."
              )
            )
          )
        )
      ),

      tags$h5(
        "3. Que montrent les résultats actuels ?"
      ),

      tags$table(
        class = "table table-sm table-bordered",

        tags$tbody(

          tags$tr(
            tags$td(tags$b("Réplications demandées")),
            tags$td(
              class = "text-end",
              mc$B_requested
            )
          ),

          tags$tr(
            tags$td(tags$b("Réplications complétées")),
            tags$td(
              class = "text-end",
              mc$B_completed
            )
          ),

          tags$tr(
            tags$td(tags$b("Échecs")),
            tags$td(
              class = "text-end",
              nrow(mc$failures)
            )
          ),

          tags$tr(
            tags$td(tags$b("Écart absolu maximal")),
            tags$td(
              class = "text-end",

              if (is.finite(max_abs_z)) {
                sprintf(
                  "%.2f",
                  max_abs_z
                )
              } else {
                "—"
              }
            )
          ),

          tags$tr(
            tags$td(tags$b("Effet le plus éloigné")),
            tags$td(
              class = "text-end",

              if (is.finite(worst_z)) {
                paste0(
                  worst_effect,
                  " (z = ",
                  sprintf("%+.2f", worst_z),
                  ")"
                )
              } else {
                "—"
              }
            )
          )
        )
      ),

      global_interpretation,

      tags$h5(
        "4. À retenir"
      ),

      div(
  class = "alert alert-light border mb-3",

  tags$ul(
    class = "mb-0",

    tags$li(
      paste(
        "Un générateur correct ne reproduit pas exactement",
        "l’EMS dans chaque échantillon."
      )
    ),

    tags$li(
      paste(
        "Il doit toutefois reproduire l’EMS en moyenne",
        "sur un grand nombre de réplications."
      )
    ),

    tags$li(
      paste(
        "L’erreur Monte-Carlo permet de juger si un écart",
        "est compatible avec le nombre de simulations réalisées."
      )
    ),

    tags$li(
      paste(
        "Une ligne rouge isolée peut disparaître lorsque",
        "le nombre de réplications augmente."
      )
    ),

    tags$li(
      paste(
        "Une divergence persistante constitue un signal",
        "utile pour repérer un problème dans le modèle",
        "générateur ou dans l’analyse."
      )
    ),

    tags$li(
      paste(
        "Le Monte-Carlo sert donc à la fois d’outil",
        "pédagogique et de banc de validation du logiciel."
      )
    )
  )
),

div(
  class = "alert alert-info mb-0",

  tags$b("Étape suivante →"),

  tags$p(
    class = "mb-0 mt-2",

    paste(
      "Vous avez vérifié que les moyennes Monte-Carlo",
      "reproduisent les EMS théoriques.",
      "Observez maintenant comment cette convergence",
      "se construit progressivement à mesure que",
      "les réplications s’accumulent."
    )
  )
),

      easyClose = TRUE,
      size = "l",

      footer = modalButton(
        "Fermer"
      )
    )
  )
})

output$ui_mc_ms_controls <- renderUI({

  mc <- mc_result()
  req(mc)

  effects <- mc$summary$Effet

  selected_effect <- input$mc_ms_effect

  if (
    is.null(selected_effect) ||
    !selected_effect %in% effects
  ) {
    selected_effect <- effects[1]
  }

  summary_row <- mc$summary[
    mc$summary$Effet == selected_effect,
    ,
    drop = FALSE
  ]

  standardized_gap <- summary_row$Ecart_standardise[1]

  diagnostic_class <- if (
    is.finite(standardized_gap) &&
    abs(standardized_gap) <= 2
  ) {
    "text-success"
  } else if (
    is.finite(standardized_gap) &&
    abs(standardized_gap) <= 3
  ) {
    "text-warning"
  } else {
    "text-danger"
  }

  diagnostic_label <- if (
    is.finite(standardized_gap) &&
    abs(standardized_gap) <= 2
  ) {
    "Convergence compatible"
  } else if (
    is.finite(standardized_gap) &&
    abs(standardized_gap) <= 3
  ) {
    "Écart à surveiller"
  } else {
    "Divergence possible"
  }

  tagList(

    selectInput(
      inputId = "mc_ms_effect",
      label = "Effet à examiner",
      choices = effects,
      selected = selected_effect,
      width = "100%"
    ),

    tags$table(
      class = "table table-sm table-bordered align-middle mb-2",
      style = "font-size:0.90rem;",

      tags$tbody(

        tags$tr(
          tags$td(tags$b("EMS théorique")),
          tags$td(
            class = "text-end",
            sprintf(
              "%.4f",
              summary_row$EMS_theorique[1]
            )
          )
        ),

        tags$tr(
          tags$td(tags$b("MS moyen")),
          tags$td(
            class = "text-end",
            sprintf(
              "%.4f",
              summary_row$MS_moyen[1]
            )
          )
        ),

        tags$tr(
          tags$td(tags$b("Erreur Monte-Carlo")),
          tags$td(
            class = "text-end",
            sprintf(
              "%.4f",
              summary_row$Erreur_MC[1]
            )
          )
        ),

        tags$tr(
          tags$td(tags$b("Écart")),
          tags$td(
            class = "text-end",
            sprintf(
              "%+.4f",
              summary_row$Ecart[1]
            )
          )
        ),

        tags$tr(
          tags$td(tags$b("Écart standardisé")),
          tags$td(
            class = paste(
              "text-end fw-semibold",
              diagnostic_class
            ),
            sprintf(
              "%+.2f",
              standardized_gap
            )
          )
        )
      )
    ),

    div(
      class = paste(
        "small fw-semibold mb-2",
        diagnostic_class
      ),

      icon(
        if (
          is.finite(standardized_gap) &&
          abs(standardized_gap) <= 2
        ) {
          "circle-check"
        } else if (
          is.finite(standardized_gap) &&
          abs(standardized_gap) <= 3
        ) {
          "triangle-exclamation"
        } else {
          "circle-exclamation"
        }
      ),

      tags$span(
        class = "ms-1",
        diagnostic_label
      )
    ),

    tags$p(
      class = "text-muted small mb-2",

      paste0(
        mc$B_completed,
        " réplications Monte-Carlo complétées."
      )
    ),

    actionButton(
      inputId = "btn_mc_ms_interpret",
      label = "Comprendre le graphique",
      icon = icon("book-open"),
      class = "btn-outline-primary btn-sm w-100"
    )
  )
})

output$plot_mc_ms <- plotly::renderPlotly({

  mc <- mc_result()
  req(mc)

  effect_name <- input$mc_ms_effect
  req(effect_name)

  dat <- mc$results[
    mc$results$Effet == effect_name &
      is.finite(mc$results$MS),
    ,
    drop = FALSE
  ]

  req(nrow(dat) > 0L)

  ems_value <- mc$summary$EMS_theorique[
    mc$summary$Effet == effect_name
  ]

  ms_mean <- mc$summary$MS_moyen[
    mc$summary$Effet == effect_name
  ]

  mc_error <- mc$summary$Erreur_MC[
    mc$summary$Effet == effect_name
  ]

  x_range <- range(dat$MS, finite = TRUE)
  x_pad <- max(diff(x_range) * 0.025, .Machine$double.eps)
  x_limits <- c(
    max(0, x_range[1] - x_pad),
    x_range[2] + x_pad
  )

  hist_info <- graphics::hist(
    dat$MS,
    breaks = 25,
    plot = FALSE
  )

  y_band_max <- max(hist_info$counts, na.rm = TRUE) * 1.08

  band_data <- if (
    length(mc_error) == 1L &&
    is.finite(mc_error) &&
    mc_error > 0
  ) {
    data.frame(
      xmin = c(
        max(0, ems_value - 3 * mc_error),
        max(0, ems_value - 2 * mc_error)
      ),
      xmax = c(
        ems_value + 3 * mc_error,
        ems_value + 2 * mc_error
      ),
      ymin = 0,
      ymax = y_band_max,
      Zone = c(
        "Entre ±2 et ±3 erreurs MC",
        "±2 erreurs MC"
      )
    )
  } else {
    NULL
  }

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(x = MS)
  )

  if (!is.null(band_data)) {
    p <- p +
      ggplot2::geom_rect(
        data = band_data,
        mapping = ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax,
          fill = Zone
        ),
        inherit.aes = FALSE,
        alpha = 0.46,
        color = NA,
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "Entre ±2 et ±3 erreurs MC" = "#f7c948",
          "±2 erreurs MC" = "#7db7ff"
        )
      )
  }

  p <- p +
    ggplot2::geom_histogram(
      bins = 25,
      fill = "#d9dee3",
      color = "white",
      linewidth = 0.35
    ) +
    ggplot2::geom_vline(
      xintercept = ems_value,
      color = "#0d6efd",
      linewidth = 1.25,
      linetype = "solid",
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(
      xintercept = ms_mean,
      color = "#fd7e14",
      linewidth = 1.25,
      linetype = "dashed",
      show.legend = FALSE
    ) +
    ggplot2::coord_cartesian(
      xlim = x_limits,
      clip = "off"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = paste0(
        "Distribution du carré moyen — ",
        effect_name
      ),
      x = "Carré moyen observé",
      y = "Fréquence"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(t = 18, r = 10, b = 8, l = 8)
    )

  fig <- plotly::ggplotly(
    p,
    tooltip = c("x", "y")
  ) |>
    plotly::layout(
      autosize = TRUE,
      showlegend = FALSE,
      margin = list(
        l = 55,
        r = 20,
        t = 58,
        b = 55
      )
    )

  fig <- fig |>
    plotly::add_annotations(
      x = ems_value,
      y = 1.01,
      xref = "x",
      yref = "paper",
      text = "EMS",
      showarrow = FALSE,
      xanchor = "right",
      xshift = -5,
      font = list(color = "#0d6efd", size = 11)
    ) |>
    plotly::add_annotations(
      x = ms_mean,
      y = 1.01,
      xref = "x",
      yref = "paper",
      text = "MS moyen",
      showarrow = FALSE,
      xanchor = "left",
      xshift = 5,
      font = list(color = "#fd7e14", size = 11)
    )

  fig
})

observeEvent(input$btn_mc_ms_interpret, {

  mc <- mc_result()
  req(mc)

  effect_name <- input$mc_ms_effect
  req(effect_name)

  dat <- mc$results[
    mc$results$Effet == effect_name &
      is.finite(mc$results$MS),
    ,
    drop = FALSE
  ]

  req(nrow(dat) > 0L)

  summary_row <- mc$summary[
    mc$summary$Effet == effect_name,
    ,
    drop = FALSE
  ]

  ems_value <- summary_row$EMS_theorique[1]
  ms_mean <- summary_row$MS_moyen[1]
  mc_error <- summary_row$Erreur_MC[1]
  raw_gap <- summary_row$Ecart[1]
  standardized_gap <- summary_row$Ecart_standardise[1]

  interpretation_class <- if (
    is.finite(standardized_gap) &&
    abs(standardized_gap) <= 2
  ) {
    "alert alert-success"
  } else if (
    is.finite(standardized_gap) &&
    abs(standardized_gap) <= 3
  ) {
    "alert alert-warning"
  } else {
    "alert alert-danger"
  }

  interpretation_text <- if (
    is.finite(standardized_gap) &&
    abs(standardized_gap) <= 2
  ) {

    paste(
      "La moyenne empirique des carrés moyens est compatible",
      "avec l’EMS théorique. L’écart observé est suffisamment petit",
      "pour être attribué à la fluctuation Monte-Carlo."
    )

  } else if (
    is.finite(standardized_gap) &&
    abs(standardized_gap) <= 3
  ) {

    paste(
      "La moyenne empirique demeure plausible, mais son écart",
      "par rapport à l’EMS est relativement important.",
      "Davantage de réplications permettraient de vérifier",
      "si cet écart se résorbe."
    )

  } else {

    paste(
      "La moyenne empirique s’écarte fortement de l’EMS théorique.",
      "Il faut vérifier le nombre de réplications ainsi que la",
      "cohérence entre le modèle théorique, les paramètres",
      "générateurs et le calcul des carrés moyens."
    )
  }

  showModal(
    modalDialog(

      title = paste(
        "Comprendre le graphique — effet",
        effect_name
      ),

      div(
        class = "alert alert-primary py-2",

        tags$b("Étape 1 sur 4 — Distribution du carré moyen"),

        tags$div(
          class = "mt-1",
          paste(
            "Cette étape montre pourquoi un carré moyen varie",
            "d’un échantillon à l’autre, même lorsque le plan",
            "et les paramètres générateurs restent identiques."
          )
        )
      ),

      tags$h5("1. Que montre ce graphique ?"),

      tags$ul(

        tags$li(
          tags$b("Les barres grises "),
          paste(
            "représentent la distribution empirique des carrés",
            "moyens obtenus dans",
            mc$B_completed,
            "échantillons simulés indépendamment."
          )
        ),

        tags$li(
          tags$b("La ligne bleue continue "),
          paste0(
            "indique l’EMS théorique, soit ",
            sprintf("%.4f", ems_value),
            ". L’EMS est la valeur moyenne attendue du carré moyen",
            "sur une infinité d’échantillons comparables."
          )
        ),

        tags$li(
          tags$b("La ligne orange pointillée "),
          paste0(
            "indique la moyenne des carrés moyens effectivement",
            "obtenus dans la simulation, soit ",
            sprintf("%.4f", ms_mean),
            "."
          )
        ),

        tags$li(
          tags$b("La bande bleue "),
          paste(
            "couvre l’intervalle situé à deux erreurs Monte-Carlo",
            "de part et d’autre de l’EMS. Une moyenne empirique",
            "située dans cette bande est compatible avec les",
            "fluctuations ordinaires de la simulation."
          )
        ),

        tags$li(
          tags$b("La bande jaune "),
          paste(
            "correspond à une distance comprise entre deux et trois",
            "erreurs Monte-Carlo. Elle signale un écart plus rare,",
            "mais encore plausible."
          )
        )
      ),

      tags$h5("2. Comment interpréter les résultats actuels ?"),

      tags$table(
        class = "table table-sm table-bordered",

        tags$tbody(

          tags$tr(
            tags$td(tags$b("EMS théorique")),
            tags$td(
              class = "text-end",
              sprintf("%.4f", ems_value)
            )
          ),

          tags$tr(
            tags$td(tags$b("MS moyen")),
            tags$td(
              class = "text-end",
              sprintf("%.4f", ms_mean)
            )
          ),

          tags$tr(
            tags$td(tags$b("Erreur Monte-Carlo")),
            tags$td(
              class = "text-end",
              sprintf("%.4f", mc_error)
            )
          ),

          tags$tr(
            tags$td(tags$b("Écart")),
            tags$td(
              class = "text-end",
              sprintf("%+.4f", raw_gap)
            )
          ),

          tags$tr(
            tags$td(tags$b("Écart standardisé")),
            tags$td(
              class = "text-end",
              sprintf("%+.2f", standardized_gap)
            )
          )
        )
      ),

      div(
        class = interpretation_class,
        interpretation_text
      ),

      tags$h5("3. À retenir"),

      div(
  class = "alert alert-light border mb-3",

  tags$ul(
    class = "mb-0",

    tags$li(
      "Un carré moyen observé est une variable aléatoire."
    ),

    tags$li(
      "Deux échantillons générés avec les mêmes paramètres ne produisent généralement pas le même MS."
    ),

    tags$li(
      "L’EMS n’est pas la valeur que doit prendre chaque MS; il s’agit de leur moyenne théorique."
    ),

    tags$li(
      "Lorsque le générateur et les calculs sont corrects, la moyenne des MS simulés se rapproche de l’EMS."
    ),

    tags$li(
      "L’erreur Monte-Carlo permet de distinguer une fluctuation ordinaire d’une divergence potentielle."
    )
  )
),

div(
  class = "alert alert-info mb-0",

  tags$b("Étape suivante →"),

  tags$p(
    class = "mb-0 mt-2",

    paste(
      "Vous avez vu pourquoi un carré moyen fluctue.",
      "Examinez maintenant comment ces fluctuations produisent",
      "un rapport F et déterminent la décision statistique."
    )
  )
),

      easyClose = TRUE,
      size = "l",

      footer = modalButton(
        "Fermer"
      )
    )
  )
})


# ── Distribution empirique du F, erreur de type I et puissance ──────────
mc_f_info <- reactive({

  mc <- mc_result()
  s  <- sim_result()

  req(mc, s)

  # ========================================================
  # Effets pouvant faire l'objet d'un test
  # ========================================================
  all_effects <- mc$summary$Effet

  testable_effects <- all_effects[
    vapply(
      all_effects,
      function(effect_name) {

        effect_info <- s$r$effects[[effect_name]]

        !is.null(effect_info) &&
          !isTRUE(effect_info$is_error)
      },
      logical(1)
    )
  ]

  if (length(testable_effects) == 0L) {

    return(
      list(
        available = FALSE,
        effects = character(0),
        effect = NULL,
        message = "Aucun effet testable n'est disponible pour ce plan."
      )
    )
  }

  # ========================================================
  # Effet sélectionné
  # ========================================================
  selected_effect <- input$mc_f_effect

  if (
    is.null(selected_effect) ||
    !selected_effect %in% testable_effects
  ) {
    selected_effect <- testable_effects[1]
  }

  # ========================================================
  # Résultats de l'effet sélectionné
  # ========================================================
  dat <- mc$results[
    mc$results$Effet == selected_effect &
      is.finite(mc$results$F),
    ,
    drop = FALSE
  ]

  if (nrow(dat) == 0L) {

    return(
      list(
        available = FALSE,
        effects = testable_effects,
        effect = selected_effect,
        message = paste(
          "Aucun rapport F ou quasi-F valide n'a été obtenu",
          "pour cet effet."
        )
      )
    )
  }

  # ========================================================
  # Type de test
  # ========================================================
  type_values <- if (
    "Type_test" %in% names(dat)
  ) {
    unique(
      dat$Type_test[
        !is.na(dat$Type_test) &
          nzchar(dat$Type_test)
      ]
    )
  } else {
    character(0)
  }

  test_type <- if (
    any(type_values == "Quasi-F")
  ) {
    "Quasi-F"
  } else {
    "F classique"
  }

  is_quasi_f <- identical(
    test_type,
    "Quasi-F"
  )

  method_values <- if (
    "Methode" %in% names(dat)
  ) {
    unique(
      dat$Methode[
        !is.na(dat$Methode) &
          nzchar(dat$Methode)
      ]
    )
  } else {
    character(0)
  }

  test_method <- if (length(method_values) > 0L) {
    paste(method_values, collapse = " / ")
  } else if (is_quasi_f) {
    "Méthode non précisée"
  } else {
    "Terme d'erreur pur"
  }

  # ========================================================
  # Degrés de liberté
  # ========================================================
  df_num_values <- if (
    "df_num" %in% names(dat)
  ) {
    suppressWarnings(
      as.numeric(dat$df_num)
    )
  } else {
    rep(
      as.numeric(
        s$r$df_list[[selected_effect]]$numeric
      ),
      nrow(dat)
    )
  }

  df_den_values <- if (
    "df_den" %in% names(dat)
  ) {
    suppressWarnings(
      as.numeric(dat$df_den)
    )
  } else {
    rep(NA_real_, nrow(dat))
  }

  valid_df_num <- is.finite(df_num_values) &
    df_num_values > 0

  valid_df_den <- is.finite(df_den_values) &
    df_den_values > 0

  if (
    !any(valid_df_num) ||
    !any(valid_df_den)
  ) {

    return(
      list(
        available = FALSE,
        effects = testable_effects,
        effect = selected_effect,
        message = paste(
          "Les degrés de liberté du test ne peuvent pas être",
          "déterminés pour cet effet."
        )
      )
    )
  }

  # Pour un F classique, les ddl sont constants.
  # Pour un quasi-F, les ddl approximatifs peuvent varier
  # légèrement d'une réplication à l'autre.
  df_num <- stats::median(
    df_num_values[valid_df_num]
  )

  df_den <- stats::median(
    df_den_values[valid_df_den]
  )

  # ========================================================
  # Seuil et taux de rejet
  # ========================================================
  alpha <- mc$alpha

  critical_f <- stats::qf(
    1 - alpha,
    df1 = df_num,
    df2 = df_den
  )

  valid_p <- is.finite(dat$p)

  rejection_rate <- if (any(valid_p)) {
    mean(
      dat$p[valid_p] < alpha
    )
  } else {
    NA_real_
  }

  # ========================================================
  # Nature de l'effet
  # ========================================================
  effect_size <- mc$effect_sizes[[selected_effect]]

  is_fixed_effect <-
    selected_effect %in% names(mc$effect_sizes)

  if (
    is.null(effect_size) ||
    length(effect_size) != 1L ||
    !is.finite(effect_size)
  ) {
    effect_size <- NA_real_
  }

  is_null_effect <-
    is_fixed_effect &&
    isTRUE(
      all.equal(
        effect_size,
        0
      )
    )

  # ========================================================
  # Terme d'erreur pur, s'il existe
  # ========================================================
  pure_error <-
    s$r$error_info$pure[[selected_effect]]

  # ========================================================
  # Courbe théorique sous H1
  #
  # Elle est conservée uniquement pour les F classiques.
  # L'approximation non centrale d'un quasi-F sera traitée
  # séparément après validation du moteur.
  # ========================================================
  ems_effect_row <- mc$summary[
    mc$summary$Effet == selected_effect,
    ,
    drop = FALSE
  ]

  ems_effect <- if (nrow(ems_effect_row) > 0L) {
    suppressWarnings(
      as.numeric(
        ems_effect_row$EMS_theorique[1]
      )
    )
  } else {
    NA_real_
  }

  ems_error <- NA_real_

  if (
    !is_quasi_f &&
    !is.null(pure_error)
  ) {

    ems_error_row <- mc$summary[
      mc$summary$Effet == pure_error,
      ,
      drop = FALSE
    ]

    if (nrow(ems_error_row) > 0L) {
      ems_error <- suppressWarnings(
        as.numeric(
          ems_error_row$EMS_theorique[1]
        )
      )
    }
  }

  ncp <- if (
    !is_quasi_f &&
    is_fixed_effect &&
    !is_null_effect &&
    is.finite(ems_effect) &&
    is.finite(ems_error) &&
    ems_error > 0
  ) {
    max(
      0,
      df_num *
        (
          ems_effect / ems_error - 1
        )
    )
  } else {
    NA_real_
  }

  # ========================================================
  # Libellé de la quantité empirique
  # ========================================================
  measure_label <- if (is_null_effect) {

    "Erreur de type I empirique"

  } else if (is_fixed_effect) {

    "Puissance empirique"

  } else {

    "Taux de rejet empirique"
  }

  # ========================================================
  # Résultat
  # ========================================================
  list(
    available = TRUE,

    mc = mc,
    s = s,

    effects = testable_effects,
    effect = selected_effect,

    dat = dat,

    test_type = test_type,
    is_quasi_f = is_quasi_f,
    test_method = test_method,

    pure_error = pure_error,

    df_num = df_num,
    df_den = df_den,

    df_num_values = df_num_values,
    df_den_values = df_den_values,

    alpha = alpha,
    critical_f = critical_f,

    effect_size = effect_size,
    is_fixed_effect = is_fixed_effect,
    is_null_effect = is_null_effect,

    rejection_rate = rejection_rate,
    measure_label = measure_label,

    ems_effect = ems_effect,
    ems_error = ems_error,
    ncp = ncp,

    n_valid = sum(valid_p),

    f_mean = mean(
      dat$F,
      na.rm = TRUE
    ),

    f_median = stats::median(
      dat$F,
      na.rm = TRUE
    )
  )
})

mc_f_interpretation <- reactive({

  info <- mc_f_info()

  if (!isTRUE(info$available)) {
    return(
      div(
        class = "alert alert-info mb-0",
        info$message
      )
    )
  }

  rejection_rate <- info$rejection_rate
  alpha          <- info$alpha
  n_valid        <- info$n_valid

  interpretation <- if (!is.finite(rejection_rate)) {

    div(
      class = "alert alert-secondary mb-0",
      "Le taux de rejet ne peut pas être calculé pour cet effet."
    )

  } else if (info$is_null_effect) {

    tolerance <- 1.96 * sqrt(
      alpha * (1 - alpha) / max(1, n_valid)
    )

    lower <- max(0, alpha - tolerance)
    upper <- min(1, alpha + tolerance)

    if (
      rejection_rate >= lower &&
      rejection_rate <= upper
    ) {
      div(
        class = "alert alert-success mb-0",
        tags$b("Niveau nominal bien reproduit."),
        tags$p(
          class = "mb-0 mt-2",
          paste0(
            "La contribution systématique de cet effet est nulle. ",
            "Le test rejette H₀ dans ",
            sprintf("%.1f %%", 100 * rejection_rate),
            " des réplications. Cette proportion est compatible avec ",
            "le seuil nominal de ",
            sprintf("%.1f %%", 100 * alpha),
            " compte tenu de l'erreur Monte-Carlo."
          )
        )
      )
    } else {
      div(
        class = "alert alert-warning mb-0",
        tags$b("Écart par rapport au niveau nominal."),
        tags$p(
          class = "mb-0 mt-2",
          paste0(
            "La contribution systématique est nulle, mais le test rejette ",
            "H₀ dans ",
            sprintf("%.1f %%", 100 * rejection_rate),
            " des réplications, contre un seuil nominal de ",
            sprintf("%.1f %%", 100 * alpha),
            ". Une augmentation du nombre de réplications permettra de ",
            "déterminer si cet écart persiste."
          )
        )
      )
    }

  } else if (info$is_fixed_effect) {

    if (rejection_rate >= .95) {
      alert_class <- "alert-success"
      heading <- "Puissance excellente."
      conclusion <- paste(
        "Le test détecte presque toujours l'effet.",
        "Dans cette configuration, une augmentation supplémentaire de la",
        "taille d'échantillon procurerait probablement peu de bénéfice."
      )
    } else if (rejection_rate >= .80) {
      alert_class <- "alert-success"
      heading <- "Puissance satisfaisante."
      conclusion <- paste(
        "Le test détecte généralement l'effet présent.",
        "La puissance atteint le repère usuel de 80 %."
      )
    } else if (rejection_rate >= .60) {
      alert_class <- "alert-warning"
      heading <- "Puissance intermédiaire."
      conclusion <- paste(
        "L'effet est détecté dans une majorité de réplications,",
        "mais il demeure fréquemment non détecté."
      )
    } else {
      alert_class <- "alert-danger"
      heading <- "Puissance faible."
      conclusion <- paste(
        "Dans la configuration actuelle, le test ne détecte pas l'effet",
        "dans une proportion importante des réplications."
      )
    }

    div(
      class = paste("alert", alert_class, "mb-0"),
      tags$b(heading),
      tags$p(
        class = "mb-0 mt-2",
        paste0(
          "Pour d(", info$effect, ") = ",
          sprintf("%.3f", info$effect_size),
          ", le test détecte l'effet dans ",
          sprintf("%.1f %%", 100 * rejection_rate),
          " des réplications. ",
          conclusion
        )
      )
    )

  } else {

    div(
      class = "alert alert-info mb-0",
      tags$b("Taux de rejet descriptif."),
      tags$p(
        class = "mb-0 mt-2",
        paste0(
          "Cet effet n'est pas contrôlé par une contribution fixe réglable. ",
          "La proportion de rejets observée est de ",
          sprintf("%.1f %%", 100 * rejection_rate),
          "."
        )
      )
    )
  }


  interpretation
})

output$ui_mc_f_controls <- renderUI({

  info <- mc_f_info()

  effect_selector <- selectInput(
    inputId = "mc_f_effect",
    label = "Effet à examiner",
    choices = info$effects,
    selected = info$effect,
    width = "100%"
  )

  if (!isTRUE(info$available)) {

    return(
      tagList(

        effect_selector,

        div(
          class = "alert alert-info mb-0",

          tags$b(
            paste0(
              "Aucun F classique disponible pour l’effet ",
              info$effect,
              "."
            )
          ),

          tags$p(
            class = "mb-0 mt-2",
            info$message
          )
        )
      )
    )
  }

  rejection_rate <- info$rejection_rate
  alpha          <- info$alpha
  n_valid        <- info$n_valid

  progress_value <- if (is.finite(rejection_rate)) {
    max(0, min(100, 100 * rejection_rate))
  } else {
    0
  }

  progress_color <- if (info$is_null_effect) {
    if (abs(rejection_rate - alpha) <=
        1.96 * sqrt(alpha * (1 - alpha) / max(1, n_valid))) {
      "#198754"
    } else {
      "#fd7e14"
    }
  } else if (is.finite(rejection_rate) && rejection_rate >= .80) {
    "#198754"
  } else if (is.finite(rejection_rate) && rejection_rate >= .60) {
    "#fd7e14"
  } else {
    "#dc3545"
  }

  
  test_identification <- div(
  class = if (isTRUE(info$is_quasi_f)) {
    "alert alert-warning py-2 mb-2"
  } else {
    "alert alert-secondary py-2 mb-2"
  },

  tags$b(
    if (isTRUE(info$is_quasi_f)) {
      "Quasi-F"
    } else {
      "F classique"
    }
  ),

  tags$div(
    class = "small mt-1",

    if (isTRUE(info$is_quasi_f)) {
      paste(
        "Méthode :",
        info$test_method
      )
    } else {
      paste(
        "Terme d'erreur :",
        info$pure_error
      )
    }
  )
)
  
  statistics_table <- tags$table(
    class = "table table-sm table-bordered mb-2",
    tags$tbody(
      tags$tr(
        tags$th(
  if (isTRUE(info$is_quasi_f)) {
    "ddl numérateur"
  } else {
    "ddl numérateur"
  }
),
        tags$td(sprintf("%.0f", info$df_num))
      ),
      tags$tr(
        tags$th(
  if (isTRUE(info$is_quasi_f)) {
    "ddl dénominateur approximatif"
  } else {
    "ddl dénominateur"
  }
),
        tags$td(sprintf("%.0f", info$df_den))
      ),
      tags$tr(
        tags$th(
  if (isTRUE(info$is_quasi_f)) {
    "Quasi-F critique"
  } else {
    "F critique"
  }
),
        tags$td(sprintf("%.3f", info$critical_f))
      ),
      tags$tr(
        tags$th(
  if (isTRUE(info$is_quasi_f)) {
    "Moyenne des quasi-F"
  } else {
    "Moyenne des F"
  }
),
        tags$td(sprintf("%.3f", info$f_mean))
      ),
      tags$tr(
        tags$th(
  if (isTRUE(info$is_quasi_f)) {
    "Médiane des quasi-F"
  } else {
    "Médiane des F"
  }
),
        tags$td(sprintf("%.3f", info$f_median))
      ),
      tags$tr(
        tags$th("Taux de rejet"),
        tags$td(
          if (is.finite(rejection_rate)) {
            sprintf("%.1f %%", 100 * rejection_rate)
          } else {
            "—"
          }
        )
      )
    )
  )

  tagList(

  effect_selector,

  test_identification,

  checkboxInput(
      inputId = "mc_f_density",
      label = "Superposer la loi F sous H₀",
      value = TRUE
    ),

    if (
      isTRUE(info$is_fixed_effect) &&
      !isTRUE(info$is_null_effect) &&
      is.finite(info$ncp) &&
      info$ncp > 0
    ) {
      checkboxInput(
        inputId = "mc_f_density_h1",
        label = "Superposer la loi F non centrale sous H₁",
        value = TRUE
      )
    },

    checkboxInput(
      inputId = "mc_f_rejection_zone",
      label = "Afficher la zone de rejet",
      value = TRUE
    ),

    hr(),

    statistics_table,

    # La barre représente uniquement la puissance. Sous H0, le pourcentage
    # affiché est une erreur de type I et une barre serait trompeuse autant
    # que redondante.
    if (isTRUE(info$is_fixed_effect) && !isTRUE(info$is_null_effect)) {
      tags$div(
        style = paste(
          "height:1.15rem;",
          "background:#e9ecef;",
          "border-radius:0.4rem;",
          "overflow:hidden;",
          "margin-bottom:0.55rem;"
        ),
        tags$div(
          style = paste0(
            "height:100%;",
            "width:", sprintf("%.2f", progress_value), "%;",
            "background:", progress_color, ";",
            "transition:width 0.3s ease;"
          )
        )
      )
    },

    tags$p(
  class = "text-muted small mb-2",

  paste0(
    info$mc$B_completed,
    " réplications Monte-Carlo; ",
    n_valid,
    " rapports F valides; α = ",
    sprintf("%.2f", alpha),
    "."
  )
),

actionButton(
  inputId = "btn_mc_f_interpret",
  label = "Comprendre le graphique",
  icon = icon("book-open"),
  class = "btn-outline-primary btn-sm w-100"
)
  )
})

observeEvent(input$btn_mc_f_interpret, {

  info <- mc_f_info()
  req(isTRUE(info$available))

  # ========================================================
  # Interprétation numérique actuelle
  # ========================================================
  current_interpretation <- if (
    !is.finite(info$rejection_rate)
  ) {

    div(
      class = "alert alert-secondary",

      paste(
        "Le taux de rejet ne peut pas être calculé",
        "pour cet effet."
      )
    )

  } else if (isTRUE(info$is_null_effect)) {

    tolerance <- 1.96 * sqrt(
      info$alpha *
        (1 - info$alpha) /
        max(1, info$n_valid)
    )

    lower_limit <- max(
      0,
      info$alpha - tolerance
    )

    upper_limit <- min(
      1,
      info$alpha + tolerance
    )

    if (
      info$rejection_rate >= lower_limit &&
      info$rejection_rate <= upper_limit
    ) {

      div(
        class = "alert alert-success",

        tags$b(
          "Le niveau nominal est correctement reproduit."
        ),

        tags$p(
          class = "mb-0 mt-2",

          paste0(
            "La contribution systématique de l'effet ",
            info$effect,
            " est nulle. Le test rejette H₀ dans ",
            sprintf(
              "%.1f %%",
              100 * info$rejection_rate
            ),
            " des réplications. Cette proportion est compatible ",
            "avec le seuil nominal de ",
            sprintf(
              "%.1f %%",
              100 * info$alpha
            ),
            ", compte tenu de l'erreur Monte-Carlo."
          )
        )
      )

    } else {

      div(
        class = "alert alert-warning",

        tags$b(
          "Le taux de rejet s'écarte du niveau nominal."
        ),

        tags$p(
          class = "mb-0 mt-2",

          paste0(
            "Sous H₀, le taux de rejet observé est de ",
            sprintf(
              "%.1f %%",
              100 * info$rejection_rate
            ),
            ", alors que le seuil nominal est de ",
            sprintf(
              "%.1f %%",
              100 * info$alpha
            ),
            ". Davantage de réplications permettraient de ",
            "vérifier si cet écart persiste."
          )
        )
      )
    }

  } else if (isTRUE(info$is_fixed_effect)) {

    power_class <- if (
      info$rejection_rate >= .80
    ) {
      "alert alert-success"
    } else if (
      info$rejection_rate >= .60
    ) {
      "alert alert-warning"
    } else {
      "alert alert-danger"
    }

    power_label <- if (
      info$rejection_rate >= .90
    ) {
      "Puissance très élevée."
    } else if (
      info$rejection_rate >= .80
    ) {
      "Puissance généralement satisfaisante."
    } else if (
      info$rejection_rate >= .60
    ) {
      "Puissance intermédiaire."
    } else {
      "Puissance faible."
    }

    div(
      class = power_class,

      tags$b(power_label),

      tags$p(
        class = "mb-0 mt-2",

        paste0(
          "Pour d(",
          info$effect,
          ") = ",
          sprintf(
            "%.3f",
            info$effect_size
          ),
          ", le test détecte l'effet dans ",
          sprintf(
            "%.1f %%",
            100 * info$rejection_rate
          ),
          " des réplications."
        )
      )
    )

  } else {

    div(
      class = "alert alert-info",

      tags$b("Taux de rejet descriptif."),

      tags$p(
        class = "mb-0 mt-2",

        paste0(
          "Cet effet n'est pas contrôlé par une contribution ",
          "systématique fixe réglable. Il est rejeté dans ",
          sprintf(
            "%.1f %%",
            100 * info$rejection_rate
          ),
          " des réplications."
        )
      )
    )
  }

  # ========================================================
  # Fenêtre modale
  # ========================================================
  showModal(
    modalDialog(

      title = paste(
        "Comprendre le graphique — effet",
        info$effect
      ),

      div(
        class = "alert alert-primary py-2",

        tags$b(
          "Étape 2 sur 4 — Distribution du rapport F"
        ),

        tags$div(
          class = "mt-1",

          paste(
            "Cette étape relie les carrés moyens à la décision",
            "statistique. Elle montre comment la distribution",
            "du rapport F change lorsqu'un effet est absent",
            "ou présent."
          )
        )
      ),

      tags$h5(
        "1. Que montre ce graphique ?"
      ),

      tags$ul(

        tags$li(
          tags$b("Les barres grises "),

          paste(
            "représentent la distribution empirique des rapports",
            "F obtenus dans",
            info$mc$B_completed,
            "échantillons simulés indépendamment."
          )
        ),

        tags$li(
          tags$b("La courbe bleue "),

          paste(
            "représente la loi F centrale attendue sous H₀.",
            "Elle décrit les rapports F que l'on devrait observer",
            "lorsque l'effet étudié est réellement nul."
          )
        ),

        if (
          isTRUE(info$is_fixed_effect) &&
          !isTRUE(info$is_null_effect) &&
          is.finite(info$ncp) &&
          info$ncp > 0
        ) {

          tags$li(
            tags$b("La courbe verte "),

            paste0(
              "représente la loi F non centrale attendue sous H₁ ",
              "pour les paramètres générateurs actuels. Son ",
              "paramètre de non-centralité est λ = ",
              sprintf(
                "%.3f",
                info$ncp
              ),
              "."
            )
          )
        },

        tags$li(
          tags$b("La ligne rouge pointillée "),

          paste0(
            "indique la valeur critique, F = ",
            sprintf(
              "%.3f",
              info$critical_f
            ),
            ", au seuil α = ",
            sprintf(
              "%.2f",
              info$alpha
            ),
            "."
          )
        ),

        tags$li(
          tags$b("La zone rose "),

          paste(
            "est la région de rejet. Toute réplication située",
            "à droite de la valeur critique conduit au rejet de H₀."
          )
        )
      ),

      tags$h5(
        "2. Comment l'interpréter ?"
      ),

      div(
        class = "alert alert-light border",

        if (isTRUE(info$is_null_effect)) {

          paste(
            "Lorsque H₀ est vraie, l'histogramme empirique doit",
            "ressembler à la courbe bleue. Seule une petite",
            "proportion des rapports F devrait dépasser la valeur",
            "critique. Cette proportion estime l'erreur de type I."
          )

        } else if (
          isTRUE(info$is_fixed_effect)
        ) {

          paste(
            "Lorsqu'un effet est présent, la distribution des",
            "rapports F se déplace vers la droite. Plus une grande",
            "partie de cette distribution se trouve dans la zone",
            "de rejet, plus la puissance du test est élevée."
          )

        } else {

          paste(
            "La proportion de rapports F qui dépassent la valeur",
            "critique constitue ici un taux de rejet descriptif."
          )
        }
      ),

      tags$table(
        class = "table table-sm table-bordered",

        tags$tbody(

          tags$tr(
            tags$td(tags$b("Effet examiné")),
            tags$td(
              class = "text-end",
              info$effect
            )
          ),

          tags$tr(
            tags$td(tags$b("Terme d'erreur")),
            tags$td(
              class = "text-end",
              info$pure_error
            )
          ),

          tags$tr(
            tags$td(tags$b("Degrés de liberté")),
            tags$td(
              class = "text-end",

              paste0(
                "df₁ = ",
                sprintf(
                  "%g",
                  info$df_num
                ),
                "; df₂ = ",
                sprintf(
                  "%g",
                  info$df_den
                )
              )
            )
          ),

          tags$tr(
            tags$td(tags$b("Valeur critique")),
            tags$td(
              class = "text-end",

              sprintf(
                "%.4f",
                info$critical_f
              )
            )
          ),

          tags$tr(
            tags$td(
              tags$b(info$measure_label)
            ),

            tags$td(
              class = "text-end",

              if (
                is.finite(info$rejection_rate)
              ) {
                sprintf(
                  "%.1f %%",
                  100 * info$rejection_rate
                )
              } else {
                "—"
              }
            )
          )
        )
      ),

      current_interpretation,

      tags$h5(
        "3. À retenir"
      ),

      div(
  class = "alert alert-light border mb-3",

  tags$ul(
    class = "mb-0",

    tags$li(
      paste(
        "Le rapport F compare un carré moyen associé",
        "à l'effet avec un carré moyen servant d'erreur."
      )
    ),

    tags$li(
      paste(
        "Sous H₀, les rapports F suivent une loi F centrale."
      )
    ),

    tags$li(
      paste(
        "Sous H₁, la distribution se déplace généralement",
        "vers la droite et devient non centrale."
      )
    ),

    tags$li(
      paste(
        "La région située au-delà de la valeur critique",
        "correspond aux décisions de rejet de H₀."
      )
    ),

    tags$li(
      paste(
        "Sous H₀, la proportion de rejets estime l'erreur",
        "de type I; sous H₁, elle estime la puissance."
      )
    ),

    tags$li(
      paste(
        "Plus la distribution sous H₁ se trouve à droite",
        "de la valeur critique, plus la puissance est élevée."
      )
    )
  )
),

div(
  class = "alert alert-info mb-0",

  tags$b("Étape suivante →"),

  tags$p(
    class = "mb-0 mt-2",

    paste(
      "Vous avez vu comment les carrés moyens produisent",
      "le rapport F et la décision statistique.",
      "Vérifiez maintenant si les simulations reproduisent",
      "fidèlement les EMS théoriques."
    )
  )
),

      easyClose = TRUE,
      size = "l",

      footer = modalButton(
        "Fermer"
      )
    )
  )
})

output$plot_mc_f <- plotly::renderPlotly({

  info <- mc_f_info()

  if (!isTRUE(info$available)) {
    return(
      plotly::plot_ly() |>
        plotly::layout(
          title = info$message
        )
    )
  }

  dat        <- info$dat
  critical_f <- info$critical_f
  df_num     <- info$df_num
  df_den     <- info$df_den

  # Éviter qu'une poignée de valeurs extrêmes écrase l'histogramme.
  upper_limit <- as.numeric(
    stats::quantile(
      dat$F,
      probs = .995,
      na.rm = TRUE,
      names = FALSE
    )
  )

  upper_limit <- max(
    upper_limit,
    critical_f * 1.15,
    1e-6
  )

  plot_dat <- dat[
    dat$F <= upper_limit,
    ,
    drop = FALSE
  ]

  bin_count <- max(
    15L,
    min(50L, round(sqrt(nrow(plot_dat))))
  )

  f_grid <- seq(
    from = 0,
    to = upper_limit,
    length.out = 600L
  )

  theoretical_f <- data.frame(
    F = f_grid,
    density = stats::df(
      f_grid,
      df1 = df_num,
      df2 = df_den
    )
  )

  theoretical_f_h1 <- if (
    is.finite(info$ncp) &&
    info$ncp > 0
  ) {
    data.frame(
      F = f_grid,
      density = stats::df(
        f_grid,
        df1 = df_num,
        df2 = df_den,
        ncp = info$ncp
      )
    )
  } else {
    NULL
  }

  hist_info <- graphics::hist(
    plot_dat$F,
    breaks = bin_count,
    plot = FALSE
  )

  y_max <- max(
    hist_info$density,
    theoretical_f$density,
    if (!is.null(theoretical_f_h1)) theoretical_f_h1$density else NA_real_,
    na.rm = TRUE
  )

  p <- ggplot2::ggplot(
    plot_dat,
    ggplot2::aes(x = F)
  ) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = after_stat(density)),
      bins = bin_count,
      boundary = 0,
      fill = "#adb5bd",
      color = "white",
      alpha = .85
    )

  # Ajouter la zone après l'histogramme afin qu'elle reste visible.
  # Des bornes numériques sont utilisées, car ggplotly() gère mal
  # les rectangles définis avec -Inf et Inf.
  if (isTRUE(input$mc_f_rejection_zone)) {
    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = critical_f,
        xmax = upper_limit,
        ymin = 0,
        ymax = y_max * 1.05,
        fill = "#dc3545",
        alpha = .22
      )
  }

  if (isTRUE(input$mc_f_density)) {
    p <- p +
      ggplot2::geom_line(
        data = theoretical_f,
        mapping = ggplot2::aes(
          x = F,
          y = density
        ),
        inherit.aes = FALSE,
        color = "#0d6efd",
        linewidth = 1.1
      )
  }

  if (
    isTRUE(input$mc_f_density_h1) &&
    !is.null(theoretical_f_h1)
  ) {
    p <- p +
      ggplot2::geom_line(
        data = theoretical_f_h1,
        mapping = ggplot2::aes(
          x = F,
          y = density
        ),
        inherit.aes = FALSE,
        color = "#198754",
        linewidth = 1.1,
        linetype = "longdash"
      )
  }

  p <- p +
    ggplot2::geom_vline(
      xintercept = critical_f,
      color = "#dc3545",
      linewidth = 1.05,
      linetype = "dashed"
    )

  n_hidden <- nrow(dat) - nrow(plot_dat)

 subtitle_text <- paste0(
  if (isTRUE(info$is_quasi_f)) {
    paste0(
      "Quasi-F — ",
      info$test_method,
      " | df₁ = "
    )
  } else {
    "H₀ : F centrale, df₁ = "
  },
  df_num,
  ", df₂ = ",
  df_den,
    if (
  !isTRUE(info$is_quasi_f) &&
  is.finite(info$ncp) &&
  info$ncp > 0
) {
      paste0(" | H₁ : F non centrale, λ = ", sprintf("%.3f", info$ncp))
    } else {
      ""
    },
    " | F critique = ", sprintf("%.3f", critical_f),
    " | ", info$mc$B_completed,
    " réplications"
  )

  if (n_hidden > 0L) {
    subtitle_text <- paste0(
      subtitle_text,
      " | ", n_hidden,
      " valeur(s) extrême(s) masquée(s) au-delà du 99,5e percentile"
    )
  }

  p <- p +
    ggplot2::coord_cartesian(
      xlim = c(0, upper_limit),
      ylim = c(0, y_max * 1.05)
    ) +
    ggplot2::labs(
      title = paste0(
  if (isTRUE(info$is_quasi_f)) {
    "Distribution empirique du quasi-F — effet "
  } else {
    "Distribution empirique du rapport F — effet "
  },
  info$effect
),
      subtitle = subtitle_text,
      x = if (isTRUE(info$is_quasi_f)) {
  "Quasi-F observé"
} else {
  "Rapport F observé"
},
      y = "Densité"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )

  plotly::ggplotly(
    p,
    tooltip = c("x", "y")
  )
})


# ── Troisième sous-onglet : convergence cumulative des MS ────────────────
output$ui_mc_conv_controls <- renderUI({

  mc <- mc_result()
  req(mc)

  effects <- mc$summary$Effet

  if (length(effects) == 0L) {
    return(
      div(
        class = "alert alert-info",
        "Aucun effet n'est disponible."
      )
    )
  }

  selected_effect <- input$mc_conv_effect

  if (
    is.null(selected_effect) ||
    !selected_effect %in% effects
  ) {
    selected_effect <- effects[1]
  }

  summary_row <- mc$summary[
    mc$summary$Effet == selected_effect,
    ,
    drop = FALSE
  ]

  final_z <- summary_row$Ecart_standardise[1]

  diagnostic_class <- if (
    is.finite(final_z) &&
    abs(final_z) <= 2
  ) {
    "text-success"
  } else if (
    is.finite(final_z) &&
    abs(final_z) <= 3
  ) {
    "text-warning"
  } else {
    "text-danger"
  }

  diagnostic_label <- if (
    is.finite(final_z) &&
    abs(final_z) <= 2
  ) {
    "Convergence compatible"
  } else if (
    is.finite(final_z) &&
    abs(final_z) <= 3
  ) {
    "Convergence encore instable"
  } else {
    "Convergence à vérifier"
  }

  tagList(

    selectInput(
      inputId = "mc_conv_effect",
      label = "Effet à examiner",
      choices = effects,
      selected = selected_effect,
      width = "100%"
    ),

    numericInput(
      inputId = "mc_conv_every",
      label = "Afficher un point toutes les",
      value = max(
        1L,
        floor(mc$B_completed / 100L)
      ),
      min = 1,
      max = max(1L, mc$B_completed),
      step = 1,
      width = "100%"
    ),

    checkboxInput(
      inputId = "mc_conv_band",
      label = "Afficher l'intervalle Monte-Carlo à 95 %",
      value = TRUE,
      width = "100%"
    ),

    hr(class = "my-2"),

    tags$table(
      class = "table table-sm table-bordered align-middle mb-2",
      style = "font-size:0.90rem;",

      tags$tbody(

        tags$tr(
          tags$td(tags$b("EMS théorique")),
          tags$td(
            class = "text-end",
            sprintf(
              "%.4f",
              summary_row$EMS_theorique[1]
            )
          )
        ),

        tags$tr(
          tags$td(tags$b("Moyenne finale")),
          tags$td(
            class = "text-end",
            sprintf(
              "%.4f",
              summary_row$MS_moyen[1]
            )
          )
        ),

        tags$tr(
          tags$td(tags$b("Erreur MC finale")),
          tags$td(
            class = "text-end",
            sprintf(
              "%.4f",
              summary_row$Erreur_MC[1]
            )
          )
        ),

        tags$tr(
          tags$td(tags$b("Écart standardisé")),
          tags$td(
            class = paste(
              "text-end fw-semibold",
              diagnostic_class
            ),
            sprintf(
              "%+.2f",
              final_z
            )
          )
        )
      )
    ),

    div(
      class = paste(
        "small fw-semibold mb-2",
        diagnostic_class
      ),

      icon(
        if (
          is.finite(final_z) &&
          abs(final_z) <= 2
        ) {
          "circle-check"
        } else if (
          is.finite(final_z) &&
          abs(final_z) <= 3
        ) {
          "triangle-exclamation"
        } else {
          "circle-exclamation"
        }
      ),

      tags$span(
        class = "ms-1",
        diagnostic_label
      )
    ),

    tags$p(
      class = "text-muted small mb-2",

      paste0(
        mc$B_completed,
        " réplications Monte-Carlo complétées."
      )
    ),

    actionButton(
      inputId = "btn_mc_conv_interpret",
      label = "Comprendre le graphique",
      icon = icon("book-open"),
      class = "btn-outline-primary btn-sm w-100"
    )
  )
})

observeEvent(input$btn_mc_conv_interpret, {

  mc <- mc_result()
  req(mc)

  effect_name <- input$mc_conv_effect
  req(effect_name)

  dat <- mc$results[
    mc$results$Effet == effect_name &
      is.finite(mc$results$MS),
    c("Replication", "MS"),
    drop = FALSE
  ]

  req(nrow(dat) > 0L)

  dat <- dat[
    order(dat$Replication),
    ,
    drop = FALSE
  ]

  if (anyDuplicated(dat$Replication)) {

    dat <- stats::aggregate(
      MS ~ Replication,
      data = dat,
      FUN = mean
    )

    dat <- dat[
      order(dat$Replication),
      ,
      drop = FALSE
    ]
  }

  n <- seq_len(nrow(dat))

  dat$MS_cumulatif <-
    cumsum(dat$MS) / n

  cumulative_sum_sq <-
    cumsum(dat$MS^2)

  cumulative_variance <-
    rep(
      NA_real_,
      length(n)
    )

  if (length(n) >= 2L) {

    cumulative_variance[-1] <-
      (
        cumulative_sum_sq[-1] -
          cumsum(dat$MS)[-1]^2 / n[-1]
      ) /
      (n[-1] - 1)
  }

  cumulative_variance[
    is.finite(cumulative_variance) &
      cumulative_variance < 0
  ] <- 0

  dat$Erreur_MC <-
    sqrt(cumulative_variance / n)

  ems_value <- mc$summary$EMS_theorique[
    mc$summary$Effet == effect_name
  ][1]

  final_mean <- tail(
    dat$MS_cumulatif,
    1
  )

  final_error <- tail(
    dat$Erreur_MC,
    1
  )

  final_gap <-
    final_mean - ems_value

  final_z <- if (
    is.finite(final_error) &&
    final_error > 0
  ) {
    final_gap / final_error
  } else {
    NA_real_
  }

  convergence_interpretation <- if (
    is.finite(final_z) &&
    abs(final_z) <= 2
  ) {

    div(
      class = "alert alert-success",

      tags$b(
        "La convergence finale est compatible avec l’EMS."
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "Après",
          nrow(dat),
          "réplications, la moyenne cumulative se trouve",
          "à moins de deux erreurs Monte-Carlo de l’EMS.",
          "La différence résiduelle est donc compatible avec",
          "la fluctuation de simulation."
        )
      )
    )

  } else if (
    is.finite(final_z) &&
    abs(final_z) <= 3
  ) {

    div(
      class = "alert alert-warning",

      tags$b(
        "La convergence est plausible, mais encore instable."
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "La moyenne cumulative finale se trouve entre deux",
          "et trois erreurs Monte-Carlo de l’EMS.",
          "Davantage de réplications permettraient probablement",
          "de stabiliser davantage la trajectoire."
        )
      )
    )

  } else {

    div(
      class = "alert alert-danger",

      tags$b(
        "La convergence finale mérite une vérification."
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "La moyenne cumulative demeure éloignée de l’EMS",
          "au regard de son erreur Monte-Carlo.",
          "Il faut vérifier si le nombre de réplications est",
          "suffisant ou si une divergence structurelle subsiste."
        )
      )
    )
  }

  showModal(
    modalDialog(

      title = paste(
        "Comprendre le graphique — effet",
        effect_name
      ),

      div(
        class = "alert alert-primary py-2",

        tags$b(
          "Étape 4 sur 4 — Convergence Monte-Carlo"
        ),

        tags$div(
          class = "mt-1",

          paste(
            "Cette dernière étape montre comment l’estimation",
            "du carré moyen attendu se construit progressivement",
            "à mesure que les réplications s’accumulent."
          )
        )
      ),

      tags$h5(
        "1. Que montre ce graphique ?"
      ),

      tags$ul(

        tags$li(
          tags$b("L’axe horizontal "),

          paste(
            "indique le nombre de réplications déjà prises",
            "en compte dans le calcul."
          )
        ),

        tags$li(
          tags$b("La courbe principale "),

          paste(
            "représente la moyenne cumulative des carrés moyens.",
            "À chaque point, elle utilise toutes les réplications",
            "réalisées jusque-là."
          )
        ),

        tags$li(
          tags$b("La ligne horizontale pointillée "),

          paste0(
            "représente l’EMS théorique, soit ",
            sprintf("%.4f", ems_value),
            "."
          )
        ),

        tags$li(
          tags$b("La bande autour de la courbe "),

          paste(
            "représente un intervalle Monte-Carlo approximatif",
            "à 95 % pour la moyenne cumulative."
          )
        ),

        tags$li(
          tags$b("Les premières réplications "),

          paste(
            "produisent souvent une trajectoire irrégulière,",
            "car chaque nouveau carré moyen influence fortement",
            "la moyenne cumulative."
          )
        ),

        tags$li(
          tags$b("À mesure que le nombre de réplications augmente, "),

          paste(
            "chaque nouvelle observation a moins d’influence",
            "et la trajectoire devrait se stabiliser."
          )
        )
      ),

      tags$h5(
        "2. Comment interpréter les résultats actuels ?"
      ),

      tags$table(
        class = "table table-sm table-bordered",

        tags$tbody(

          tags$tr(
            tags$td(tags$b("Réplications utilisées")),
            tags$td(
              class = "text-end",
              nrow(dat)
            )
          ),

          tags$tr(
            tags$td(tags$b("EMS théorique")),
            tags$td(
              class = "text-end",
              sprintf("%.4f", ems_value)
            )
          ),

          tags$tr(
            tags$td(tags$b("Moyenne cumulative finale")),
            tags$td(
              class = "text-end",
              sprintf("%.4f", final_mean)
            )
          ),

          tags$tr(
            tags$td(tags$b("Erreur MC finale")),
            tags$td(
              class = "text-end",
              if (is.finite(final_error)) {
                sprintf("%.4f", final_error)
              } else {
                "—"
              }
            )
          ),

          tags$tr(
            tags$td(tags$b("Écart final")),
            tags$td(
              class = "text-end",
              sprintf("%+.4f", final_gap)
            )
          ),

          tags$tr(
            tags$td(tags$b("Écart standardisé")),
            tags$td(
              class = "text-end",
              if (is.finite(final_z)) {
                sprintf("%+.2f", final_z)
              } else {
                "—"
              }
            )
          )
        )
      ),

      convergence_interpretation,

      tags$h5(
        "3. À retenir"
      ),

      div(
  class = "alert alert-light border mb-3",

  tags$ul(
    class = "mb-0",

    tags$li(
      paste(
        "La moyenne cumulative peut être très instable",
        "au début de la simulation."
      )
    ),

    tags$li(
      paste(
        "La stabilisation progressive ne signifie pas",
        "que chaque carré moyen individuel devient moins variable."
      )
    ),

    tags$li(
      paste(
        "C’est la moyenne de ces carrés moyens qui devient",
        "plus précise lorsque les réplications s’accumulent."
      )
    ),

    tags$li(
      paste(
        "L’intervalle Monte-Carlo se resserre généralement",
        "avec l’augmentation du nombre de réplications."
      )
    ),

    tags$li(
      paste(
        "Une trajectoire correcte n’est pas nécessairement",
        "monotone : elle peut passer plusieurs fois",
        "au-dessus et au-dessous de l’EMS."
      )
    ),

    tags$li(
      paste(
        "La convergence cumulative complète la validation",
        "globale présentée à l’étape 3."
      )
    )
  )
),

div(
  class = "alert alert-success mb-0",

  tags$h4(
    class = "alert-heading",
    "🎓 Laboratoire complété"
  ),

  tags$p(
    paste(
      "Vous venez de terminer le laboratoire Monte-Carlo."
    )
  ),

  tags$p(
    class = "mb-2",
    tags$b(
      "Vous avez observé expérimentalement que :"
    )
  ),

  tags$ul(
    tags$li(
      "les carrés moyens fluctuent d’un échantillon à l’autre;"
    ),

    tags$li(
      "les EMS représentent des espérances théoriques;"
    ),

    tags$li(
      "les rapports F découlent directement des carrés moyens;"
    ),

    tags$li(
      "la puissance dépend du déplacement de la distribution du rapport F;"
    ),

    tags$li(
      "les moyennes Monte-Carlo convergent vers les EMS;"
    ),

    tags$li(
      "la simulation permet de vérifier la cohérence entre la théorie et le générateur."
    )
  ),

  tags$hr(),

  tags$p(
    class = "mb-0 fw-semibold",

    paste(
      "Vous êtes maintenant prêt à explorer les tests quasi-F."
    )
  )
),

      easyClose = TRUE,
      size = "l",

      footer = modalButton(
        "Fermer"
      )
    )
  )
})

output$plot_mc_convergence <- plotly::renderPlotly({

  mc <- mc_result()
  req(mc)

  effect_name <- input$mc_conv_effect
  req(effect_name)

  dat <- mc$results[
    mc$results$Effet == effect_name &
      is.finite(mc$results$MS),
    c("Replication", "MS"),
    drop = FALSE
  ]

  req(nrow(dat) > 0L)

  dat <- dat[
    order(dat$Replication),
    ,
    drop = FALSE
  ]

  # Une seule valeur par réplication et par effet est attendue.
  # En cas de doublon imprévu, on moyenne les valeurs de MS.
  if (anyDuplicated(dat$Replication)) {
    dat <- stats::aggregate(
      MS ~ Replication,
      data = dat,
      FUN = mean
    )

    dat <- dat[
      order(dat$Replication),
      ,
      drop = FALSE
    ]
  }

  n <- seq_len(nrow(dat))
  cumulative_sum <- cumsum(dat$MS)
  cumulative_sum_sq <- cumsum(dat$MS^2)

  dat$N <- n
  dat$MS_cumulatif <- cumulative_sum / n

  cumulative_variance <- rep(NA_real_, length(n))

  if (length(n) >= 2L) {
    cumulative_variance[-1] <-
      (
        cumulative_sum_sq[-1] -
          cumulative_sum[-1]^2 / n[-1]
      ) /
      (n[-1] - 1)
  }

  cumulative_variance[
    is.finite(cumulative_variance) &
      cumulative_variance < 0
  ] <- 0

  dat$Erreur_MC <- sqrt(cumulative_variance / n)
  dat$Borne_inf <- dat$MS_cumulatif - 1.96 * dat$Erreur_MC
  dat$Borne_sup <- dat$MS_cumulatif + 1.96 * dat$Erreur_MC

  ems_value <- mc$summary$EMS_theorique[
    mc$summary$Effet == effect_name
  ]

  if (
    length(ems_value) != 1L ||
    !is.finite(ems_value)
  ) {
    ems_value <- NA_real_
  }

  every <- input$mc_conv_every

  if (
    is.null(every) ||
    length(every) != 1L ||
    !is.finite(every) ||
    every < 1
  ) {
    every <- 1L
  }

  every <- as.integer(every)

  keep <- unique(
    c(
      1L,
      seq.int(
        from = every,
        to = nrow(dat),
        by = every
      ),
      nrow(dat)
    )
  )

  keep <- keep[
    keep >= 1L &
      keep <= nrow(dat)
  ]

  plot_dat <- dat[
    keep,
    ,
    drop = FALSE
  ]

  p <- ggplot2::ggplot(
    plot_dat,
    ggplot2::aes(
      x = N,
      y = MS_cumulatif
    )
  )

  if (isTRUE(input$mc_conv_band)) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = Borne_inf,
          ymax = Borne_sup
        ),
        alpha = 0.18,
        na.rm = TRUE
      )
  }

  p <- p +
    ggplot2::geom_line(
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      size = 1.5
    )

  if (is.finite(ems_value)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = ems_value,
        linewidth = 1.0,
        linetype = "dashed"
      )
  }

  final_mean <- tail(dat$MS_cumulatif, 1)
  final_error <- tail(dat$Erreur_MC, 1)

  subtitle_text <- paste0(
    "Moyenne cumulative finale = ",
    sprintf("%.4f", final_mean),
    if (is.finite(ems_value)) {
      paste0(
        " | EMS théorique = ",
        sprintf("%.4f", ems_value)
      )
    } else {
      ""
    },
    if (is.finite(final_error)) {
      paste0(
        " | Erreur MC finale = ",
        sprintf("%.4f", final_error)
      )
    } else {
      ""
    }
  )

  p <- p +
    ggplot2::labs(
      title = paste0(
        "Convergence du carré moyen — ",
        effect_name
      ),
      subtitle = subtitle_text,
      x = "Nombre de réplications complétées",
      y = "Moyenne cumulative du MS"
    ) +
    ggplot2::theme_minimal(
  base_size = 12
) +
ggplot2::theme(
  panel.grid.minor = ggplot2::element_blank(),
  plot.title = ggplot2::element_text(
    face = "bold"
  )
)

  plotly::ggplotly(
    p,
    tooltip = c(
      "x",
      "y",
      "ymin",
      "ymax"
    )
  )
})

output$ui_quasi_f_observes <- renderUI({

  s <- sim_result()
  req(s)

  components <- anova_components()
  req(components)

  quasi_effects <- names(
    s$r$quasi_list %||% list()
  )

  if (length(quasi_effects) == 0L) {

    return(
      div(
        class = "alert alert-success mb-0",

        tags$b(
          icon("circle-check"),
          tags$span(
            class = "ms-1",
            "Aucun quasi-F n’est nécessaire"
          )
        ),

        tags$p(
          class = "mb-0 mt-2",

          paste(
            "Tous les effets testables de ce plan possèdent",
            "un terme d’erreur pur. Les tests présentés dans",
            "le tableau ANOVA sont donc des rapports F classiques."
          )
        )
      )
    )
  }

  all_ms <- components$ms
  all_df <- components$df

  panels <- lapply(
    quasi_effects,
    function(effect_name) {

      quasi_info <- s$r$quasi_list[[effect_name]]

ems_effect_html <- ems_row_to_html(
  ems = s$r$ems_table[[effect_name]]
)

ems_h0_html <- ems_under_h0_to_html(
  ems = s$r$ems_table[[effect_name]]
)

theory_tab_value <- paste0(
  "qf_effect_",
  gsub(
    "[^A-Za-z0-9]",
    "_",
    effect_name
  )
)

quasi_result <- compute_quasi_f_numeric(
        eff_name = effect_name,
        quasi_info = quasi_info,
        all_ms = all_ms,
        all_df = all_df
      )

      qf <- quasi_result$ct

      # ======================================================
      # Quasi-F non calculable
      # ======================================================
      if (
        is.null(qf) ||
        !is.null(qf$error) ||
        !is.finite(qf$F) ||
        !is.finite(qf$df_num) ||
        !is.finite(qf$df_den) ||
        !is.finite(qf$p)
      ) {

        return(
          nav_panel(
            title = paste("Effet", effect_name),

            div(
              class = "alert alert-danger m-3",

              tags$b(
                icon("triangle-exclamation"),
                tags$span(
                  class = "ms-1",
                  paste(
                    "Le quasi-F de l’effet",
                    effect_name,
                    "n’a pas pu être calculé."
                  )
                )
              ),

              tags$p(
                class = "mb-0 mt-2",

                qf$error %||%
                  paste(
                    "Vérifiez les carrés moyens observés",
                    "et les degrés de liberté."
                  )
              )
            )
          )
        )
      }

      # ======================================================
      # Coefficients Cornfield–Tukey
      # ======================================================
      coefficients <- quasi_info$ct$coefficients

      coefficients <- coefficients[
        is.finite(coefficients) &
          coefficients != 0
      ]

      positive_terms <- names(
        coefficients[
          coefficients > 0
        ]
      )

      negative_terms <- names(
        coefficients[
          coefficients < 0
        ]
      )

      # ======================================================
      # Tableau des carrés moyens utilisés
      # ======================================================
      term_names <- unique(
        c(
          effect_name,
          positive_terms,
          negative_terms
        )
      )

      term_rows <- lapply(
        term_names,
        function(term_name) {

          coefficient_label <- if (
            identical(term_name, effect_name)
          ) {

            "Effet testé"

          } else {

            coefficient_value <- coefficients[[term_name]]

            if (coefficient_value > 0) {
              sprintf(
                "+ %.3f",
                coefficient_value
              )
            } else {
              sprintf(
                "− %.3f",
                abs(coefficient_value)
              )
            }
          }

          destination <- if (
            identical(term_name, effect_name)
          ) {

            "Numérateur"

          } else if (
            coefficients[[term_name]] < 0
          ) {

            "Transféré au numérateur"

          } else {

            "Dénominateur"
          }

          tags$tr(
            tags$td(
              tags$b(term_name)
            ),

            tags$td(
              class = "text-end",
              sprintf(
                "%.4f",
                all_ms[[term_name]]
              )
            ),

            tags$td(
              class = "text-end",
              sprintf(
                "%.3f",
                all_df[[term_name]]
              )
            ),

            tags$td(
              class = "text-end",
              coefficient_label
            ),

            tags$td(destination)
          )
        }
      )

      # ======================================================
      # Formules numériques lisibles
      # ======================================================
      numerator_parts <- c(
        sprintf(
          "MS(%s)",
          effect_name
        ),

        vapply(
          negative_terms,
          function(term_name) {

            coefficient_value <- abs(
              coefficients[[term_name]]
            )

            if (isTRUE(all.equal(coefficient_value, 1))) {
              sprintf(
                "MS(%s)",
                term_name
              )
            } else {
              sprintf(
                "%.3f × MS(%s)",
                coefficient_value,
                term_name
              )
            }
          },
          character(1)
        )
      )

      denominator_parts <- vapply(
        positive_terms,
        function(term_name) {

          coefficient_value <- coefficients[[term_name]]

          if (isTRUE(all.equal(coefficient_value, 1))) {
            sprintf(
              "MS(%s)",
              term_name
            )
          } else {
            sprintf(
              "%.3f × MS(%s)",
              coefficient_value,
              term_name
            )
          }
        },
        character(1)
      )

      numerator_formula <- paste(
        numerator_parts,
        collapse = " + "
      )

      denominator_formula <- paste(
        denominator_parts,
        collapse = " + "
      )
      
      # ======================================================
# Combinaisons théoriques correspondantes
# ======================================================

theoretical_numerator_parts <- c(
  paste0(
    "EMS(",
    effect_name,
    ")"
  ),

  vapply(
    negative_terms,
    function(term_name) {

      coefficient_value <- abs(
        coefficients[[term_name]]
      )

      if (isTRUE(all.equal(coefficient_value, 1))) {

        paste0(
          "EMS(",
          term_name,
          ")"
        )

      } else {

        paste0(
          sprintf(
            "%.3f",
            coefficient_value
          ),
          " × EMS(",
          term_name,
          ")"
        )
      }
    },
    character(1)
  )
)

theoretical_denominator_parts <- vapply(
  positive_terms,
  function(term_name) {

    coefficient_value <- coefficients[[term_name]]

    if (isTRUE(all.equal(coefficient_value, 1))) {

      paste0(
        "EMS(",
        term_name,
        ")"
      )

    } else {

      paste0(
        sprintf(
          "%.3f",
          coefficient_value
        ),
        " × EMS(",
        term_name,
        ")"
      )
    }
  },
  character(1)
)

theoretical_numerator_formula <- paste(
  theoretical_numerator_parts,
  collapse = " + "
)

theoretical_denominator_formula <- paste(
  theoretical_denominator_parts,
  collapse = " + "
)

      
      theoretical_numerator_parts <- c(
  paste0(
    "EMS(",
    effect_name,
    ")"
  ),

  vapply(
    negative_terms,
    function(term_name) {

      coefficient_value <- abs(
        coefficients[[term_name]]
      )

      if (isTRUE(all.equal(coefficient_value, 1))) {
        paste0(
          "EMS(",
          term_name,
          ")"
        )
      } else {
        paste0(
          sprintf("%.3f", coefficient_value),
          " × EMS(",
          term_name,
          ")"
        )
      }
    },
    character(1)
  )
)

theoretical_denominator_parts <- vapply(
  positive_terms,
  function(term_name) {

    coefficient_value <- coefficients[[term_name]]

    if (isTRUE(all.equal(coefficient_value, 1))) {
      paste0(
        "EMS(",
        term_name,
        ")"
      )
    } else {
      paste0(
        sprintf("%.3f", coefficient_value),
        " × EMS(",
        term_name,
        ")"
      )
    }
  },
  character(1)
)

theoretical_numerator_formula <- paste(
  theoretical_numerator_parts,
  collapse = " + "
)

theoretical_denominator_formula <- paste(
  theoretical_denominator_parts,
  collapse = " + "
)

      decision_significant <- qf$p < 0.05

      decision_block <- if (decision_significant) {

        div(
          class = "alert alert-success mb-0",

          tags$b(
            icon("circle-check"),
            tags$span(
              class = "ms-1",
              "Effet statistiquement significatif"
            )
          ),

          tags$p(
            class = "mb-0 mt-2",

            paste0(
              "Le quasi-F de l’effet ",
              effect_name,
              " est significatif au seuil de 5 % : ",
              "F′(",
              sprintf("%.2f", qf$df_num),
              ", ",
              sprintf("%.2f", qf$df_den),
              ") = ",
              sprintf("%.3f", qf$F),
              ", p = ",
              format.pval(
                qf$p,
                digits = 3,
                eps = .001
              ),
              "."
            )
          )
        )

      } else {

        div(
          class = "alert alert-secondary mb-0",

          tags$b(
            icon("circle-xmark"),
            tags$span(
              class = "ms-1",
              "Effet non significatif"
            )
          ),

          tags$p(
            class = "mb-0 mt-2",

            paste0(
              "Le quasi-F de l’effet ",
              effect_name,
              " n’est pas significatif au seuil de 5 % : ",
              "F′(",
              sprintf("%.2f", qf$df_num),
              ", ",
              sprintf("%.2f", qf$df_den),
              ") = ",
              sprintf("%.3f", qf$F),
              ", p = ",
              format.pval(
                qf$p,
                digits = 3,
                eps = .001
              ),
              "."
            )
          )
        )
      }

      # ======================================================
      # Panneau de l’effet
      # ======================================================
      nav_panel(
        title = paste("Effet", effect_name),

        tagList(

          div(
            class = "alert alert-primary py-2 px-3 mb-3",

            div(
              class = paste(
                "d-flex justify-content-between",
                "align-items-center flex-wrap gap-2"
              ),

              div(
                tags$div(
                  class = "text-uppercase small fw-semibold",

                  paste(
                    "Quasi-F observé — effet",
                    effect_name
                  )
                ),

                tags$div(
                  class = "mt-1",

                  tags$b("Objectif : "),

                  paste(
                    "reconstruire le test à partir des carrés",
                    "moyens réellement observés dans l’échantillon."
                  )
                )
              ),

              tags$span(
                class = "badge bg-primary",
                "MS observés → décision"
              )
            )
          ),

          navset_tab(
            id = paste0(
              "observed_qf_",
              gsub(
                "[^A-Za-z0-9]",
                "_",
                effect_name
              )
            ),

            # ------------------------------------------------
            # Étape 1
            # ------------------------------------------------
            nav_panel(
              "① Carrés moyens",

              div(
                class = "pt-3",

                card(
                  fill = FALSE,

                  card_header(
                    tags$b(
                      "Carrés moyens utilisés dans le calcul"
                    )
                  ),

                  card_body(
                    tags$table(
                      class = paste(
                        "table table-sm table-bordered",
                        "align-middle mb-0"
                      ),

                      tags$thead(
                        tags$tr(
                          tags$th("Source"),
                          tags$th(
                            class = "text-end",
                            "MS observé"
                          ),
                          tags$th(
                            class = "text-end",
                            "ddl"
                          ),
                          tags$th(
                            class = "text-end",
                            "Coefficient CT"
                          ),
                          tags$th("Rôle")
                        )
                      ),

                      tags$tbody(
                        term_rows
                      )
                    )
                  )
                )
              )
            ),

            # ------------------------------------------------
            # Étape 2
            # ------------------------------------------------
            nav_panel(
  "② Reconstruction",

  div(
    class = "pt-3",

    card(
      fill = FALSE,

      card_header(
        class = "py-2",

        tags$b(
          paste0(
            "EMS théorique de l’effet ",
            effect_name
          )
        )
      ),

      card_body(

        # ====================================================
        # Rappel de l'EMS théorique
        # ====================================================
        div(
          class = paste(
            "border rounded bg-light",
            "p-3 mb-2"
          ),
          style = paste(
            "font-size:1.05rem;",
            "text-align:center;",
            "overflow-x:auto;"
          ),

          tags$span(
            class = "fw-semibold me-1",

            paste0(
              "EMS(",
              effect_name,
              ") = "
            )
          ),

          ems_effect_html
        ),

        tags$p(
  class = "text-muted small mb-3",

  paste(
    "L’EMS complète contient la contribution propre de l’effet",
    "ainsi que les composantes de variance associées au plan."
  )
),

div(
  class = "card border-warning mb-3",

  div(
    class = "card-header py-2 fw-semibold",
    HTML(
      paste0(
        "Passage sous H<sub>0</sub> : ",
        "&phi;(",
        effect_name,
        ") = 0"
      )
    )
  ),

  div(
    class = "card-body",

    tags$p(
      class = "mb-2",

      paste(
        "Sous l’hypothèse nulle, la contribution propre",
        "de l’effet testé disparaît de son EMS."
      )
    ),

    div(
      class = paste(
        "border rounded bg-light",
        "p-3 text-center mb-2"
      ),
      style = paste(
        "font-size:1.05rem;",
        "overflow-x:auto;"
      ),

      tags$span(
        class = "fw-semibold me-1",

        paste0(
          "EMS(",
          effect_name,
          " | H₀) = "
        )
      ),

      ems_h0_html
    ),

    tags$p(
      class = "small text-muted mb-0",

      paste(
        "Ce sont exactement ces composantes de variance",
        "que les combinaisons de carrés moyens doivent",
        "reproduire pour construire le test."
      )
    )
  )
),

# ====================================================
# Équivalence des combinaisons sous H0
# ====================================================

div(
  class = "card border-primary mb-3",

  div(
    class = "card-header py-2 fw-semibold",

    HTML(
      paste0(
        "Combinaisons équivalentes sous H<sub>0</sub>"
      )
    )
  ),

  div(
    class = "card-body",

    tags$p(
      class = "mb-3",

      paste(
        "Cornfield–Tukey recherche des combinaisons",
        "dont les espérances contiennent exactement",
        "les mêmes composantes de variance."
      )
    ),

    div(
      class = paste(
        "border rounded bg-light",
        "p-3 text-center mb-3"
      ),
      style = paste(
        "font-family:monospace;",
        "font-size:1.05rem;",
        "overflow-x:auto;"
      ),

      tags$div(
        class = "mb-2",

        tags$b(
          "Espérance du numérateur composite"
        )
      ),

      tags$div(
        theoretical_numerator_formula
      ),

      tags$div(
        class = "my-3 fw-bold",
        style = "font-size:1.25rem;",
        "="
      ),

      tags$div(
        class = "mb-2",

        tags$b(
          "Espérance du dénominateur composite"
        )
      ),

      tags$div(
        theoretical_denominator_formula
      )
    ),

    div(
      class = "alert alert-primary py-2 mb-0",

      tags$b(
        "Pourquoi le test est-il possible ?"
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "Sous H₀, ces deux combinaisons possèdent",
          "la même espérance. Les EMS sont ensuite",
          "remplacées par les carrés moyens observés",
          "pour calculer le quasi-F."
        )
      )
    )
  )
),

        # ====================================================
        # Pont vers le tutoriel théorique
        # ====================================================
        div(
          class = "alert alert-secondary py-2 mb-3",

          tags$b(
            icon("book"),

            tags$span(
              class = "ms-1",
              "Pourquoi cette reconstruction ?"
            )
          ),

          tags$p(
            class = "mb-2 mt-2",

            paste(
              "Le dénominateur du quasi-F doit reproduire",
              "les composantes de variance de",
              paste0("EMS(", effect_name, ")"),
              "sous H₀."
            )
          ),

          tags$p(
            class = "mb-2",

            paste(
              "La méthode de Cornfield–Tukey détermine",
              "la combinaison de carrés moyens qui possède",
              "cette même espérance."
            )
          ),

          tags$p(
            class = "mb-0 small",

            HTML(
              paste0(
                "Pour les justifications théoriques complètes, ",
                "consultez l’onglet principal <b>Quasi-F</b>."
              )
            )
          )
        ),

        # ====================================================
        # Reconstruction numérique
        # ====================================================
        # ====================================================
# Correspondance entre les EMS et les MS observés
# ====================================================
div(
  class = "card border-primary mb-3",

  div(
    class = "card-header py-2 fw-semibold",
    "Correspondance théorique sous H₀"
  ),

  div(
    class = "card-body",

    tags$p(
      class = "mb-2",

      HTML(
        paste0(
          "Sous H<sub>0</sub>, la contribution propre de l’effet ",
          "<b>",
          effect_name,
          "</b> est nulle : ",
          "<b>φ(",
          effect_name,
          ") = 0</b>."
        )
      )
    ),

    tags$p(
      class = "mb-3",

      paste(
        "La méthode de Cornfield–Tukey construit alors deux",
        "combinaisons dont les espérances sont égales."
      )
    ),

    div(
      class = paste(
        "border rounded bg-light",
        "p-3 text-center mb-3"
      ),
      style = paste(
        "font-family:monospace;",
        "font-size:1.05rem;",
        "overflow-x:auto;"
      ),

      tags$div(
        theoretical_numerator_formula
      ),

      tags$div(
        class = "my-2 fw-bold",
        "="
      ),

      tags$div(
        theoretical_denominator_formula
      )
    ),

    div(
      class = "alert alert-primary py-2 mb-0",

      tags$b("Passage aux données observées"),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "Les espérances théoriques sont remplacées par",
          "les carrés moyens observés correspondants.",
          "On obtient ainsi le quasi-F calculé ci-dessous."
        )
      )
    )
  )
),
        
        tags$h6(
          class = "mt-3 mb-2 border-bottom pb-2",
          "Reconstruction numérique"
        ),

        tags$p(
          paste(
            "Les termes possédant un coefficient négatif",
            "dans le dénominateur théorique sont transférés",
            "au numérateur afin d’obtenir deux combinaisons",
            "positives de carrés moyens."
          )
        ),

        tags$h6(
          class = "mt-3",
          "Numérateur composite"
        ),

        div(
          class = "border rounded bg-light p-3 mb-3",
          style = paste(
            "font-family:monospace;",
            "font-size:1.05rem;"
          ),

          numerator_formula,

          tags$hr(class = "my-2"),

          tags$b(
            paste0(
              "Valeur observée = ",
              sprintf(
                "%.4f",
                qf$ms_num
              )
            )
          )
        ),

        tags$h6(
          "Dénominateur composite"
        ),

        div(
          class = "border rounded bg-light p-3 mb-0",
          style = paste(
            "font-family:monospace;",
            "font-size:1.05rem;"
          ),

          denominator_formula,

          tags$hr(class = "my-2"),

          tags$b(
            paste0(
              "Valeur observée = ",
              sprintf(
                "%.4f",
                qf$ms_den
              )
            )
          )
        )
      )
    )
  )
),

            # ------------------------------------------------
            # Étape 3
            # ------------------------------------------------
            nav_panel(
  "③ Quasi-F",

  div(
    class = "pt-3",

    div(
      class = "alert alert-primary py-2 px-3 mb-3",

      tags$b(
        paste0(
          "Calcul du quasi-F pour l’effet ",
          effect_name
        )
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "Le quasi-F est le rapport entre les deux",
          "combinaisons positives construites à l’étape précédente."
        )
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      value_box(
        title = "Numérateur composite",
        value = sprintf(
          "%.4f",
          qf$ms_num
        ),
        showcase = icon("arrow-up")
      ),

      value_box(
        title = "Dénominateur composite",
        value = sprintf(
          "%.4f",
          qf$ms_den
        ),
        showcase = icon("arrow-down")
      )
    ),

    card(
      fill = FALSE,

      card_header(
        class = "py-2",
        tags$b("Calcul de la statistique observée")
      ),

      card_body(

        div(
          class = paste(
            "border rounded bg-light",
            "p-3 text-center mb-3"
          ),
          style = paste(
            "font-size:1.15rem;",
            "overflow-x:auto;"
          ),

          tags$div(
            class = "fw-semibold mb-2",

            HTML(
              paste0(
                "F&prime; = ",
                "<span style='display:inline-block;",
                "vertical-align:middle; text-align:center;'>",
                "<span style='display:block;",
                "border-bottom:1px solid;",
                "padding:0 0.45rem 0.18rem;'>",
                "Numérateur composite",
                "</span>",
                "<span style='display:block;",
                "padding:0.18rem 0.45rem 0;'>",
                "Dénominateur composite",
                "</span>",
                "</span>"
              )
            )
          ),

          tags$div(
            class = "my-3",
            style = "font-size:1.25rem;",

            HTML(
              paste0(
                "F&prime; = ",
                "<span style='display:inline-block;",
                "vertical-align:middle; text-align:center;'>",
                "<span style='display:block;",
                "border-bottom:1px solid;",
                "padding:0 0.45rem 0.18rem;'>",
                sprintf("%.4f", qf$ms_num),
                "</span>",
                "<span style='display:block;",
                "padding:0.18rem 0.45rem 0;'>",
                sprintf("%.4f", qf$ms_den),
                "</span>",
                "</span>",
                " = ",
                "<strong>",
                sprintf("%.4f", qf$F),
                "</strong>"
              )
            )
          )
        ),

        div(
          class = "alert alert-light border mb-0",

          tags$b("Lecture"),

          tags$p(
            class = "mb-0 mt-2",

            paste(
              "La statistique compare le carré moyen composite",
              "associé à l’effet au carré moyen composite servant",
              "de terme d’erreur. Sous H₀, leurs espérances sont égales."
            )
          )
        )
      )
    )
  )
),

            # ------------------------------------------------
            # Étape 4
            # ------------------------------------------------
            nav_panel(
  "④ Degrés de liberté",

  div(
    class = "pt-3",

    div(
      class = "alert alert-primary py-2 px-3 mb-3",

      tags$b(
        paste0(
          "Approximation des degrés de liberté — effet ",
          effect_name
        )
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "Le numérateur et le dénominateur du quasi-F sont",
          "des combinaisons de plusieurs carrés moyens.",
          "Ils ne possèdent donc pas les degrés de liberté",
          "d’un carré moyen unique."
        )
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        fill = FALSE,

        card_header(
          class = "py-2",
          tags$b("Numérateur composite")
        ),

        card_body(

          div(
            class = "border rounded bg-light p-3 mb-3",
            style = paste(
              "font-family:monospace;",
              "font-size:1.02rem;",
              "overflow-x:auto;"
            ),

            numerator_formula
          ),

          tags$p(
            class = "small text-muted mb-2",

            paste(
              "Cette combinaison comprend le carré moyen de l’effet",
              "et, s’il y en a, les termes négatifs transférés",
              "depuis le dénominateur théorique."
            )
          ),

          div(
            class = "alert alert-light border text-center mb-0",

            tags$div(
              class = "small text-uppercase fw-semibold text-muted",
              "ddl approximatif du numérateur"
            ),

            tags$div(
              class = "fs-3 fw-bold mt-1",
              sprintf(
                "%.3f",
                qf$df_num
              )
            )
          )
        )
      ),

      card(
        fill = FALSE,

        card_header(
          class = "py-2",
          tags$b("Dénominateur composite")
        ),

        card_body(

          div(
            class = "border rounded bg-light p-3 mb-3",
            style = paste(
              "font-family:monospace;",
              "font-size:1.02rem;",
              "overflow-x:auto;"
            ),

            denominator_formula
          ),

          tags$p(
            class = "small text-muted mb-2",

            paste(
              "Cette combinaison contient les termes positifs",
              "retenus pour reproduire les composantes de variance",
              "de l’EMS sous H₀."
            )
          ),

          div(
            class = "alert alert-light border text-center mb-0",

            tags$div(
              class = "small text-uppercase fw-semibold text-muted",
              "ddl approximatif du dénominateur"
            ),

            tags$div(
              class = "fs-3 fw-bold mt-1",
              sprintf(
                "%.3f",
                qf$df_den
              )
            )
          )
        )
      )
    ),

    card(
      fill = FALSE,
      class = "mt-3",

      card_header(
        class = "py-2",
        tags$b("Principe de Satterthwaite")
      ),

      card_body(

        tags$p(
          paste(
            "Pour chaque combinaison, l’approximation de",
            "Satterthwaite produit un nombre de degrés de liberté",
            "effectifs à partir des carrés moyens observés, de leurs",
            "coefficients et de leurs degrés de liberté respectifs."
          )
        ),

        div(
          class = "border rounded bg-light p-3 text-center mb-3",
          style = paste(
            "font-size:1.05rem;",
            "overflow-x:auto;"
          ),

          HTML(
            paste0(
              "&nu;<sub>eff</sub> = ",
              "<span style='display:inline-block;",
              "vertical-align:middle; text-align:center;'>",
              "<span style='display:block;",
              "border-bottom:1px solid;",
              "padding:0 0.45rem 0.18rem;'>",
              "(&Sigma; c<sub>i</sub>MS<sub>i</sub>)<sup>2</sup>",
              "</span>",
              "<span style='display:block;",
              "padding:0.18rem 0.45rem 0;'>",
              "&Sigma;[(c<sub>i</sub>MS<sub>i</sub>)<sup>2</sup>/&nu;<sub>i</sub>]",
              "</span>",
              "</span>"
            )
          )
        ),

        div(
          class = "alert alert-info mb-0",

          tags$b("À retenir"),

          tags$p(
            class = "mb-0 mt-2",

            paste(
              "Ces degrés de liberté peuvent être non entiers.",
              "Ce n’est pas une erreur : ils représentent les degrés",
              "de liberté équivalents des deux combinaisons de",
              "carrés moyens utilisées dans le quasi-F."
            )
          )
        )
      )
    )
  )
),

            # ------------------------------------------------
            # Étape 5
            # ------------------------------------------------
            nav_panel(
  "⑤ Décision",

  div(
    class = "pt-3",

    div(
      class = "alert alert-primary py-2 px-3 mb-3",

      tags$b(
        paste0(
          "Décision statistique — effet ",
          effect_name
        )
      ),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "La statistique observée est comparée à une loi F",
          "dont les deux degrés de liberté ont été approximés",
          "à l’étape précédente."
        )
      )
    ),

    layout_columns(
      col_widths = c(4, 4, 4),

      value_box(
        title = "Quasi-F observé",
        value = sprintf(
          "%.4f",
          qf$F
        ),
        showcase = icon("divide")
      ),

      value_box(
        title = "Degrés de liberté",
        value = paste0(
          sprintf("%.2f", qf$df_num),
          " ; ",
          sprintf("%.2f", qf$df_den)
        ),
        showcase = icon("superscript")
      ),

      value_box(
        title = "Valeur p",
        value = format.pval(
          qf$p,
          digits = 3,
          eps = .001
        ),
        showcase = icon("p")
      )
    ),

    card(
      fill = FALSE,

      card_header(
        class = "py-2",
        tags$b("Résultat du test")
      ),

      card_body(

        div(
          class = paste(
            "border rounded bg-light",
            "p-3 text-center mb-3"
          ),
          style = paste(
            "font-size:1.15rem;",
            "overflow-x:auto;"
          ),

          HTML(
            paste0(
              "F&prime;(",
              sprintf("%.2f", qf$df_num),
              ", ",
              sprintf("%.2f", qf$df_den),
              ") = ",
              "<strong>",
              sprintf("%.4f", qf$F),
              "</strong>",
              ", &nbsp; p = ",
              "<strong>",
              format.pval(
                qf$p,
                digits = 3,
                eps = .001
              ),
              "</strong>"
            )
          )
        ),

        if (decision_significant) {

          div(
            class = "alert alert-success mb-3",

            tags$b(
              icon("circle-check"),

              tags$span(
                class = "ms-1",
                "Rejet de l’hypothèse nulle"
              )
            ),

            tags$p(
              class = "mb-0 mt-2",

              paste0(
                "La valeur p est inférieure à 0,05. ",
                "L’effet ",
                effect_name,
                " est statistiquement significatif au seuil de 5 %."
              )
            )
          )

        } else {

          div(
            class = "alert alert-secondary mb-3",

            tags$b(
              icon("circle-xmark"),

              tags$span(
                class = "ms-1",
                "Non-rejet de l’hypothèse nulle"
              )
            ),

            tags$p(
              class = "mb-0 mt-2",

              paste0(
                "La valeur p est supérieure ou égale à 0,05. ",
                "L’effet ",
                effect_name,
                " n’est pas statistiquement significatif au seuil de 5 %."
              )
            )
          )
        },

        div(
          class = "alert alert-light border mb-0",

          tags$b("Interprétation"),

          tags$p(
            class = "mb-2 mt-2",

            if (decision_significant) {

              paste(
                "Les différences observées pour cet effet sont",
                "trop importantes pour être attribuées uniquement",
                "aux composantes d’erreur représentées par le",
                "dénominateur composite."
              )

            } else {

              paste(
                "Les différences observées pour cet effet demeurent",
                "compatibles avec les fluctuations attendues d’après",
                "les composantes d’erreur représentées par le",
                "dénominateur composite."
              )
            }
          ),

          tags$p(
            class = "small text-muted mb-0",

            paste(
              "La conclusion porte uniquement sur l’existence",
              "statistique de l’effet. Son importance pratique doit",
              "être appréciée séparément à partir des moyennes,",
              "des contrastes ou d’une mesure de taille d’effet."
            )
          )
        )
      )
    ),

    div(
      class = "alert alert-info mt-3 mb-0",

      tags$b("Ce que vous venez de faire"),

      tags$p(
        class = "mb-0 mt-2",

        paste(
          "Vous avez relié l’EMS théorique de l’effet aux carrés",
          "moyens observés, construit deux combinaisons positives,",
          "approximé leurs degrés de liberté et obtenu une décision",
          "statistique à partir du quasi-F."
        )
      )
    )
  )
),
          )
        )
      )
    }
  )

  do.call(
    navset_tab,
    panels
  )
})

# ==========================================================
# Lien entre le quasi-F observé et le tutoriel théorique
# ==========================================================

observeEvent(
  input$qf_theory_jump,
  {

    target_effect <- input$qf_theory_jump

    req(
      is.character(target_effect),
      length(target_effect) == 1L,
      nzchar(target_effect)
    )

    # Ouvrir l’onglet principal Quasi-F
    bslib::nav_select(
      id = "tabs_main",
      selected = "quasi_f",
      session = session
    )

    # Sélectionner le même effet dans le tutoriel
    bslib::nav_select(
      id = "quasi_effect_tabs",
      selected = target_effect,
      session = session
    )
  },
  ignoreInit = TRUE
)

}

shinyApp(ui, server)

