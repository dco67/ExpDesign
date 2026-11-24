# ExpDesign — Plans expérimentaux et carrés moyens attendus

**Décrire un plan d'expérience, trouver ce qu'estime chaque carré moyen, choisir le bon terme d'erreur, puis le vérifier par simulation.**

ExpDesign est une application pédagogique **R/Shiny**, en français, consacrée aux plans d'analyse de variance à facteurs fixes, aléatoires, croisés ou emboîtés. Elle calcule les valeurs attendues des carrés moyens (EMS) par l'algorithme de Cornfield-Tukey, en déduit les degrés de liberté et le dénominateur de chaque test F, et construit un quasi-F lorsqu'aucun terme d'erreur unique ne convient. Un laboratoire Monte-Carlo montre ensuite ces résultats à l'œuvre sur des données simulées.

- [Utiliser l'application en ligne](https://shiny.dcstats.org/ExpDesign/)
- [Consulter le manuel pédagogique](https://dcstats.org/ExpDesign/)

## Objectifs pédagogiques

- spécifier un plan : facteurs fixes ou aléatoires, croisés ou emboîtés, mesures répétées;
- comprendre ce qu'estime chaque carré moyen et pourquoi le terme d'erreur change d'un effet à l'autre;
- construire et interpréter un quasi-F (Satterthwaite) quand il n'existe pas de terme d'erreur pur;
- comprendre qu'un EMS est une moyenne théorique, pas la valeur de chaque carré moyen;
- passer du plan aux données : analyse, postulats, profils des moyennes et puissance.

## Fonctionnalités

| Volet | Contenu |
| --- | --- |
| Plan | Spécification des facteurs, exemples classiques, EMS et degrés de liberté, quasi-F, repères de lecture. |
| Simulation | Paramètres du modèle, données simulées (aperçu et téléchargement). |
| Analyse | Tableau de l'ANOVA, EMS théoriques, quasi-F observés, postulats (Levene, Mauchly, diagnostic automatique), graphiques, profils des moyennes, interprétation détaillée, syntaxe. |
| Laboratoire Monte-Carlo | Distribution des carrés moyens, distribution de F et puissance, validation des EMS, convergence. |
| Théorie | Pourquoi calculer les EMS, notation des plans, algorithme de Cornfield-Tukey, exemples détaillés, quasi-F, références. |

## Exécuter l'application localement

```r
install.packages(c("shiny", "bslib", "shinyjs", "plotly", "car", "katex", "haven",
                   "writexl", "httr", "jsonlite"))
shiny::runApp("ExpDesign")
```

## Licence

GPL-3.0. Voir le fichier `LICENSE`.

## Auteur

Daniel Coulombe, Ph.D. — <https://dcstats.org>
