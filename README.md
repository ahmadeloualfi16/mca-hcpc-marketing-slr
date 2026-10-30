# MCA and HCPC of a systematic literature review on data science for marketing measurement

Replication code, data and figures for the article:

> **Can Data Science Improve Marketing Measurement? Assessing the Challenges**
> Ahmad EL OUALFI (2026). ORCID: [0009-0008-5756-9050](https://orcid.org/0009-0008-5756-9050)

This repository contains everything needed to reproduce the **Multiple Correspondence Analysis (MCA)**
and the **Hierarchical Clustering on Principal Components (HCPC)** reported in the article, together
with all figures. The analysis follows the approach of Saura, Skare & Dosen (2024), *Journal of
Innovation & Knowledge*, 9(1), 100597.

## Contents

```
.
├── ACM_HCPC_figures.R            # single script: MCA + HCPC + all figures
├── data/
│   ├── Classeur4.csv             # coding matrix: 41 studies x 16 binary variables (0/1)
│   └── variables_codebook.csv    # variable codes, names and dimensions
├── FIGURES/                      # figures produced by the script (PNG, 300 dpi)
├── LICENSE                       # MIT licence (code)
└── README.md
```

## Data

`data/Classeur4.csv` is semicolon-separated (`;`). Each row is one of the **41 studies retained in the
systematic review**; the first column is the study identifier. Each of the **16 columns** is a binary
variable: `1` = the variable is explicitly addressed or applied in the study, `0` = it is not.

| Dimension | Codes | Variables |
|---|---|---|
| Performance indicators | P1–P4 | CTR, conversion rate, ROI/ROAS, CLV |
| Analytical techniques | T1–T5 | machine learning, predictive analytics, deep learning, NLP, recommendation systems |
| Measurement challenges | C1–C4, C6–C8 | data quality, privacy, algorithmic bias, explainability, data silos, attribution complexity, cross-channel tracking |

The coding framework defines 17 variables. **C5 (model drift)** was not identified in any of the
41 studies; a variable without variation cannot enter an MCA, so the analysis uses **16 active
variables**. See `data/variables_codebook.csv`.

In the figures, studies are numbered **1 to 41 in the order of the rows of `Classeur4.csv`**.
The correspondence between these numbers and the original identifiers is written by the script to
`FIGURES/Correspondance_numero_ID.csv`.

## How to reproduce

### Requirements
* [R](https://cran.r-project.org/) (version 4.1 or later) and, optionally, [RStudio](https://posit.co/download/rstudio-desktop/).
* The R packages `FactoMineR`, `factoextra`, `ggplot2`, `ggrepel` and `reshape2`
  (installed automatically by the script if missing).

### Steps
1. Download or clone this repository.
2. Open the repository folder in RStudio (*File > Open Project…* or *Session > Set Working Directory > Choose Directory…*), so that the working directory is the folder that contains `ACM_HCPC_figures.R`.
3. Open `ACM_HCPC_figures.R` and click **Source** (or run `source("ACM_HCPC_figures.R")` in the console).
4. The figures appear in the *Plots* pane and are saved in the `FIGURES/` folder.

If `data/Classeur4.csv` is not found, the script opens a file-selection window so that you can choose
`Classeur4.csv` manually.

### Main settings (top of the script)
| Setting | Default | Meaning |
|---|---|---|
| `NB_CLUSTERS` | `5` | number of HCPC clusters (`-1` = automatic choice, which gives 3) |
| `K_VAR` | `3` | number of groups of variables drawn as ellipses in Figures 4 and 6 |
| `ARTICLES_NUMEROTES` | `TRUE` | label studies 1–41 (`FALSE` keeps the original identifiers) |

## Results at a glance

* The first two dimensions explain 21.7% and 13.1% of the inertia (34.8% together).
* HCPC (with k-means consolidation) yields five clusters:

| Cluster | Studies | Proposed name |
|---|---|---|
| K1 | 23 | The Predictive Analytics Core |
| K2 | 7 | The Campaign Performance Measurement Cluster |
| K3 | 6 | The Data Governance and Infrastructure Cluster |
| K4 | 2 | The Long-Term Value and Financial Return Cluster |
| K5 | 3 | The Advanced AI and Explainability Cluster |

Clusters K4 and K5 are very small; their profiles are descriptive and should be interpreted with caution.

## Figures

| File | Content |
|---|---|
| `Figure1_Matrice_codage.png` | coding matrix, studies sorted by cluster |
| `Figure2_Frequence_Variables.png` | frequency of each variable |
| `Figure3_Eboulis.png` | eigenvalues / explained variance |
| `Figure4_Carte_variables.png` | MCA variables map (colour = cos2) |
| `Figure5_Carte_articles.png` | MCA studies map (colour = contribution) |
| `Figure6_Biplot.png` | MCA biplot (studies and variables) |
| `Figure7_Biplot_fleches.png` | biplot with arrows (distance to the centre) |
| `Figure8_Clusters_HCPC.png` | HCPC clusters on the factor plane |
| `Figure9_Dendrogramme.png` | HCPC tree, labels coloured by final cluster |
| `Figure10_Profil_clusters.png` | share of studies addressing each variable, by cluster |
| `Figure11_Cooccurrence_variables.png` | co-occurrence of variables (phi coefficient) |
| `Figure12_Contribution_variables.png` | contribution of variables to dimensions 1 and 2 |
| `Figure13_Etendue_thematique.png` | number of variables addressed per study, by cluster |
| `Figure14_vtest_clusters.png` | over- and under-represented variables by cluster (v-test) |
| `Figure15_Dimensions_par_cluster.png` | mean number of variables per dimension, by cluster |

Figure numbering in this repository follows the script and may differ from the numbering used in the article.
Note that in Figure 9 the tree is drawn before k-means consolidation; the colours show the final clusters.

## Citation

If you use this code or data, please cite the article above and, if applicable, this repository:

```
[AUTHOR NAME(S)] (2026). MCA and HCPC of a systematic literature review on data science for
marketing measurement [Source code and data]. GitHub. https://github.com/[USERNAME]/[REPOSITORY]
```

## Licence

Code: [MIT](LICENSE). The coding matrix (`data/`) and the figures (`FIGURES/`) may be reused with
attribution to the article (CC BY 4.0).

## For future collaboration contact me

[Ahmad EL OUALFI,PhD] — [eloualfi.ahmad@ucd.ac.ma]
