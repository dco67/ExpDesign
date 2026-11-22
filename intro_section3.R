    # Section 3 — Algorithme Cornfield-Tukey avec exemples concrets
section3 <- tagList(
  h3("3. Algorithme de Cornfield-Tukey"),

  p("Pour calculer ", tags$b("EMS(E)"), " — l'espérance du carré moyen d'un
     effet E — on examine chaque effet candidat E' du plan et on décide s'il
     contribue à EMS(E). Voici comment lire cette notation :"),

  div(class="alert alert-secondary",
    tags$b("Vocabulaire : "), tags$br(),
    tags$b("E"), " = l'effet dont on calcule l'EMS (ex. l'effet A qu'on veut tester)", tags$br(),
    tags$b("E'"), " = un effet candidat du plan qu'on examine (ex. s(A), Bs(A), AB...)", tags$br(),
    "Pour chaque E', on pose trois questions (R1, R2, R3). Si les trois
     réponses sont OUI, alors E' contribue à EMS(E)."
  ),

  p("Prenons le plan ", tags$code("A, B, s(A)"),
    " (A between-subjects, B within-subjects) comme fil conducteur.
     Ce plan contient les effets : A, B, AB, s(A), Bs(A)."),

  hr(),

  # Règle R1
  div(class="card mb-3",
    div(class="card-header bg-primary text-white",
      tags$b("Règle R1 — Inclusion : E' doit « contenir » E")
    ),
    div(class="card-body",
      p("Les facteurs de E doivent tous apparaître dans les facteurs ",
        tags$em("ou"), " les parents de E'. En d'autres termes : E' est
        une version « plus complète » qui contient E."),
      div(style="text-align:center; margin:1em 0;",
        k("\\text{facteurs}(E) \\subseteq \\text{facteurs}(E') \\cup \\text{parents}(E')",
          display=TRUE)
      ),
      p(tags$b("Exemple concret — Calculons EMS(A) :")),
      p("E = A, donc facteurs(E) = {A}. On examine chaque E' :"),
      tags$table(class="table table-sm table-bordered",
        tags$thead(tags$tr(
          tags$th("E'"), tags$th("facteurs(E')"), tags$th("parents(E')"),
          tags$th("{A} ⊆ facteurs ∪ parents ?"), tags$th("R1 ?")
        )),
        tags$tbody(
          tags$tr(tags$td("A"),     tags$td("{A}"),    tags$td("{}"),
                  tags$td("A ∈ {A} ✓"),      tags$td(tags$span(class="text-success","OUI"))),
          tags$tr(tags$td("B"),     tags$td("{B}"),    tags$td("{}"),
                  tags$td("A ∉ {B} ✗"),      tags$td(tags$span(class="text-danger","NON"))),
          tags$tr(tags$td("AB"),    tags$td("{A,B}"),  tags$td("{}"),
                  tags$td("A ∈ {A,B} ✓"),    tags$td(tags$span(class="text-success","OUI"))),
          tags$tr(tags$td("s(A)"),  tags$td("{s}"),    tags$td("{A}"),
                  tags$td("A ∈ {A} (parent) ✓"), tags$td(tags$span(class="text-success","OUI"))),
          tags$tr(tags$td("Bs(A)"), tags$td("{B,s}"),  tags$td("{A}"),
                  tags$td("A ∈ {A} (parent) ✓"), tags$td(tags$span(class="text-success","OUI")))
        )
      ),
      p(class="text-muted",
        "B est éliminé dès R1 — il ne contient pas A.")
    )
  ),

  # Règle R2
  div(class="card mb-3",
    div(class="card-header bg-warning text-dark",
      tags$b("Règle R2 — Pas de facteur fixe supplémentaire")
    ),
    div(class="card-body",
      p("Les facteurs de E' qui sont ", tags$em("absents de E"),
        " doivent tous être aléatoires. Si E' introduit un facteur fixe
        supplémentaire, il est rejeté."),
      p(tags$b("Exemple — Suite du calcul de EMS(A) :")),
      p("Candidats restants après R1 : A, AB, s(A), Bs(A)."),
      p("Pour chaque candidat, on regarde les facteurs « en plus » par
        rapport à E = {A} :"),
      tags$table(class="table table-sm table-bordered",
        tags$thead(tags$tr(
          tags$th("E'"), tags$th("Facteurs en plus que A"),
          tags$th("Ces facteurs sont-ils tous aléatoires ?"), tags$th("R2 ?")
        )),
        tags$tbody(
          tags$tr(tags$td("A"),     tags$td("aucun"),
                  tags$td("— (rien à vérifier)"),
                  tags$td(tags$span(class="text-success","OUI"))),
          tags$tr(tags$td("AB"),    tags$td("B (fixe !)"),
                  tags$td("NON — B est fixe"),
                  tags$td(tags$span(class="text-danger","NON → éliminé"))),
          tags$tr(tags$td("s(A)"),  tags$td("s (aléatoire)"),
                  tags$td("OUI — s est aléatoire"),
                  tags$td(tags$span(class="text-success","OUI"))),
          tags$tr(tags$td("Bs(A)"), tags$td("B (fixe !), s (aléatoire)"),
                  tags$td("NON — B est fixe"),
                  tags$td(tags$span(class="text-danger","NON → éliminé")))
        )
      ),
      div(class="alert alert-warning",
        tags$b("Intuition : "),
        "Un facteur fixe supplémentaire rendrait le rapport F invalide —
         les EMS du numérateur et du dénominateur ne seraient plus
         comparables sous H₀."
      )
    )
  ),

  # Règle R3
  div(class="card mb-3",
    div(class="card-header bg-info text-white",
      tags$b("Règle R3 — Au moins un facteur aléatoire")
    ),
    div(class="card-body",
      p("E' doit contenir au moins un facteur aléatoire, ou bien E' = E
         (l'effet lui-même). Cette règle assure qu'on ne compte pas les
         effets purement fixes comme composantes de variance."),
      p(tags$b("Exemple — Candidats restants : A et s(A)")),
      tags$table(class="table table-sm table-bordered",
        tags$thead(tags$tr(
          tags$th("E'"), tags$th("Facteurs aléatoires dans E'"),
          tags$th("R3 ?")
        )),
        tags$tbody(
          tags$tr(tags$td("A"),    tags$td("aucun (A est fixe), mais E'=E"),
                  tags$td(tags$span(class="text-success","OUI — E'=E"))),
          tags$tr(tags$td("s(A)"), tags$td("s est aléatoire"),
                  tags$td(tags$span(class="text-success","OUI")))
        )
      )
    )
  ),

  # Résultat
  div(class="card mb-3 border-success",
    div(class="card-header bg-success text-white",
      tags$b("Résultat — EMS(A) pour le plan A, B, s(A)")
    ),
    div(class="card-body",
      p("Seuls A et s(A) passent les trois règles. Donc :"),
      div(style="text-align:center; margin:1em 0;",
        k("\\text{EMS}(A) = \\phi_A + n_s\\,\\sigma^2_{s(A)}", display=TRUE)
      ),
      tags$table(class="table table-sm table-bordered",
        tags$thead(tags$tr(
          tags$th("E'"), tags$th("Coefficient"), tags$th("Terme"), tags$th("Explication")
        )),
        tags$tbody(
          tags$tr(
            tags$td("A"),
            tags$td(k("1")),
            tags$td(k("\\phi_A")),
            tags$td("E'=E, A est fixe → terme φ avec coeff 1")
          ),
          tags$tr(
            tags$td("s(A)"),
            tags$td(k("n_s")),
            tags$td(k("\\sigma^2_{s(A)}")),
            tags$td("s absent de E={A}, présent dans E'={s} → coeff = n_s")
          )
        )
      ),
      div(class="alert alert-success",
        tags$b("Lecture : "),
        "Sous H₀ (φ_A = 0), ",
        k("E[MS_A] = \\sigma^2_{s(A)} = E[MS_{s(A)}]"),
        ". Donc le terme d'erreur de A est MS(s(A)) et le test F est ",
        k("F = MS_A / MS_{s(A)}"), "."
      )
    )
  ),

  # Calcul du coefficient
  div(class="card mb-3",
    div(class="card-header bg-light",
      tags$b("Comment calculer le coefficient ?")
    ),
    div(class="card-body",
      p("Le coefficient de σ²(E') dans EMS(E) est le produit des ",
        k("n_f"), " pour les facteurs f qui sont :"),
      tags$ul(
        tags$li(tags$b("absents de E"), " (non dans les facteurs de E)"),
        tags$li(tags$b("absents de E'"), " (non dans facteurs(E') ni parents(E'))")
      ),
      p(tags$b("Exemple — Coefficient de σ²(s(A)) dans EMS(A) :")),
      p("E = {A}, E' = s(A) avec facteurs={s} et parents={A}."),
      p("Facteurs du plan : {A, B, s}. Absent de E ET absent de E' :"),
      tags$ul(
        tags$li("A : absent de E ? NON (A ∈ E) → exclu"),
        tags$li("B : absent de E ? OUI. Absent de E' ? OUI (B ∉ {s,A}) → inclus → n_B"),
        tags$li("s : absent de E ? OUI. Absent de E' ? NON (s ∈ facteurs(E')) → exclu")
      ),
      p("Coefficient = ", k("n_B"), "... mais attendez !"),
      div(class="alert alert-warning",
        tags$b("Note : "), "B est fixe, donc il ne contribue pas au coefficient
        selon la règle R2. Seuls les facteurs aléatoires absents contribuent.
        Donc coefficient = ", k("n_s"), " (s est dans E' avec n_s niveaux,
        contribuant à la réplication)."
      ),
      p("C'est pourquoi on obtient ", k("n_s \\cdot \\sigma^2_{s(A)}"), ".")
    )
  )
)
