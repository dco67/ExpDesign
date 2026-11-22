intro_panel <- nav_panel("Introduction", icon = icon("book-open"),
  card_body(
    style = "max-width: 900px; margin: auto;",

    # ── Titre ──────────────────────────────────────────────
    h2("Espérances des carrés moyens (EMS)"),
    p(class="lead",
      "Les EMS sont au cœur de l'inférence dans les plans d'expérience
       avec facteurs aléatoires ou mesures répétées. Ils déterminent
       quel terme d'erreur utiliser pour tester chaque effet, et
       révèlent quand un quasi-F est nécessaire."),
    hr(),

    # ── 1. Pourquoi les EMS ────────────────────────────────
    h3("1. Pourquoi calculer les EMS ?"),
    p("Dans une ANOVA simple à un facteur fixe, le rapport F est :"),
    div(style="text-align:center; margin: 1em 0;",
      k(
  "F = \\frac{MS_A}{MS_{\\mathrm{erreur}}}",
  display = TRUE
)
    ),
    p("Ce rapport est valide parce que, sous H₀ :"),
    div(style="text-align:center; margin: 1em 0;",
      k(
  "E[MS_A] = E[MS_{\\mathrm{erreur}}] = \\sigma^2",
  display = TRUE
)
    ),
    p("Dès qu'on introduit des facteurs aléatoires ou des mesures répétées,
       les EMS deviennent plus complexes. La règle générale est :"),
    div(class="alert alert-primary",
      tags$b("Principe fondamental : "),
      "Le rapport F est valide si et seulement si ",
      k(
  "E[MS_{\\mathrm{numerateur}}] -
   E[MS_{\\mathrm{denominateur}}] =
   \\phi_{\\mathrm{effet}}"
),
      " sous H₁, et les deux espérances sont égales sous H₀."
    ),
    hr(),

    # ── 2. Notation ────────────────────────────────────────
    h3("2. Notation des plans"),
    p("Ce programme utilise la notation de Kirk (2013) :"),
    tags$table(
      class="table table-bordered table-sm",
      style="max-width:600px",
      tags$thead(tags$tr(
        tags$th("Notation"), tags$th("Signification"), tags$th("Exemple")
      )),
      tags$tbody(
        tags$tr(tags$td(tags$code("A")),
                tags$td("Facteur fixe"),
                tags$td("Traitement, condition")),
        tags$tr(tags$td(tags$code("b")),
                tags$td("Facteur aléatoire"),
                tags$td("Stimulus, item")),
        tags$tr(tags$td(tags$code("s")),
                tags$td("Sujets (toujours aléatoire)"),
                tags$td("Participants")),
        tags$tr(tags$td(tags$code("s(A)")),
                tags$td("Sujets imbriqués dans A"),
                tags$td("Groupes indépendants")),
        tags$tr(tags$td(tags$code("s(AB)")),
                tags$td("Sujets imbriqués dans A×B"),
                tags$td("Plan factoriel between"))
      )
    ),
    hr(),

    # ── 3. Algorithme Cornfield-Tukey ──────────────────────
    source("intro_section3.R", local=TRUE)$value,
    hr(),

    # ── 4. Exemples ────────────────────────────────────────
    h3("4. Exemples détaillés"),

    # Exemple T1
    tags$div(class="card mb-4",
      tags$div(class="card-header",
        tags$b("Exemple 1 — Plan BS pur : "),
        tags$code("A, s(A)")
      ),
      tags$div(class="card-body",
        p("A est un facteur fixe between-subjects. Les sujets sont
           imbriqués dans A (groupes indépendants)."),
        tags$table(class="table table-sm table-bordered",
          tags$thead(tags$tr(
            tags$th("Source"), tags$th("EMS"), tags$th("Test F")
          )),
          tags$tbody(
            tags$tr(
              tags$td("A"),
              tags$td(k("\\phi_A + n_s\\,\\sigma^2_{s(A)}")),
              tags$td(k("F = MS_A / MS_{s(A)}"))
            ),
            tags$tr(
              tags$td("s(A)"),
              tags$td(k("\\sigma^2_{s(A)}")),
              tags$td("— (terme d'erreur)")
            )
          )
        ),
        div(class="alert alert-info mt-2",
          tags$b("Interprétation : "),
          "Sous H₀ (pas d'effet A), ",
          k("E[MS_A] = \\sigma^2_{s(A)} = E[MS_{s(A)}]"),
          " — le rapport F est centré sur 1."
        )
      )
    ),

    # Exemple T3
    tags$div(class="card mb-4",
      tags$div(class="card-header",
        tags$b("Exemple 2 — Plan mixte : "),
        tags$code("A, B, s(A)")
      ),
      tags$div(class="card-body",
        p("A est between-subjects, B est within-subjects (mesures répétées)."),
        tags$table(class="table table-sm table-bordered",
          tags$thead(tags$tr(
            tags$th("Source"), tags$th("EMS"), tags$th("Erreur")
          )),
          tags$tbody(
            tags$tr(
              tags$td("A"),
              tags$td(k("\\phi_A + n_s\\,\\sigma^2_{s(A)}")),
              tags$td(k("MS_{s(A)}"))
            ),
            tags$tr(
              tags$td("B"),
              tags$td(k("\\phi_B + n_s\\,\\sigma^2_{Bs(A)}")),
              tags$td(k("MS_{Bs(A)}"))
            ),
            tags$tr(
              tags$td("AB"),
              tags$td(k("\\phi_{AB} + n_s\\,\\sigma^2_{Bs(A)}")),
              tags$td(k("MS_{Bs(A)}"))
            ),
            tags$tr(
              tags$td("s(A)"),
              tags$td(k("\\sigma^2_{s(A)}")),
              tags$td("— erreur between")
            ),
            tags$tr(
              tags$td("Bs(A)"),
              tags$td(k("\\sigma^2_{Bs(A)}")),
              tags$td("— erreur within")
            )
          )
        ),
        div(class="alert alert-warning mt-2",
          tags$b("Point clé : "),
          "A et B n'utilisent pas le même terme d'erreur ! ",
          "A est testé contre ", k("MS_{s(A)}"),
          " (variance between), tandis que B et AB sont testés contre ",
          k("MS_{Bs(A)}"), " (variance within)."
        )
      )
    ),

    # ── 5. Quasi-F ─────────────────────────────────────────
    h3("5. Quasi-F — quand il n'y a pas de terme d'erreur pur"),
    p("Parfois, aucun terme d'erreur disponible ne correspond exactement
       à EMS(E) moins φ(E). On doit alors construire un terme d'erreur
       composite."),

    tags$div(class="card mb-3",
      tags$div(class="card-header bg-warning text-dark",
        tags$b("Approximation de Satterthwaite")
      ),
      tags$div(class="card-body",
        p("On construit :"),
        div(style="text-align:center;",
          k("MS_{quasi} = \\sum_i c_i \\cdot MS_i", display=TRUE)
        ),
        p("Les degrés de liberté approchés sont :"),
        div(style="text-align:center;",
          k("df_{quasi} = \\frac{MS_{quasi}^2}{\\sum_i \\frac{(c_i \\cdot MS_i)^2}{df_i}}",
            display=TRUE)
        ),
        p("Et le test F approximatif est :"),
        div(style="text-align:center;",
          k("F' = \\frac{MS_{effet}}{MS_{quasi}} \\sim F(df_{effet},\\, df_{quasi})",
            display=TRUE)
        )
      )
    ),
    hr(),

    # ── 6. Références ──────────────────────────────────────
    h3("6. Références"),
    tags$ul(
      tags$li("Kirk, R.E. (2013). ",
        tags$em("Experimental Design: Procedures for the Behavioral Sciences"),
        " (4e éd.). SAGE Publications."),
      tags$li("Winer, B.J., Brown, D.R., & Michels, K.M. (1991). ",
        tags$em("Statistical Principles in Experimental Design"),
        " (3e éd.). McGraw-Hill."),
      tags$li("Cornfield, J., & Tukey, J.W. (1956). Average values of mean
        squares in factorials. ",
        tags$em("Annals of Mathematical Statistics"), ", 27, 907–949.")
    )
  )
)
