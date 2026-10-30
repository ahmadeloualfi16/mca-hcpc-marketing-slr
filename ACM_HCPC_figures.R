# ======================================================================
#  ACM (MCA) + HCPC  -  FIGURES  (style Saura et al., 2024)
#  Données : Classeur4.csv  (41 articles x 16 variables binaires 0/1)
#
#  COMMENT UTILISER (RStudio) :
#   1. File > Open File...  -> ouvrir ce fichier  ACM_HCPC_figures.R
#   2. Cliquer sur le bouton "Source" (en haut à droite de l'éditeur)
#   3. Une fenêtre s'ouvre : choisir Classeur4.csv
#   4. Attendre : les graphiques apparaissent dans l'onglet "Plots"
#      et sont AUSSI enregistrés en PNG (haute résolution) dans le
#      dossier  FIGURES  (créé à côté du fichier csv).
#  La 1ère fois, l'installation des packages peut prendre 2-5 minutes.
# ======================================================================

# ---------- 0. PACKAGES (installés automatiquement si absents) -------
pkgs <- c("FactoMineR", "factoextra", "ggplot2", "ggrepel", "reshape2")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggrepel)

# ---------- 1. LECTURE DES DONNÉES -----------------------------------
# Mode 1 (GitHub) : si le dossier du projet contient data/Classeur4.csv, il est utilisé
#                   automatiquement (ouvrir le dossier du projet dans RStudio).
# Mode 2 (simple) : sinon, une fenêtre s'ouvre pour choisir Classeur4.csv.
chemin_depot <- file.path("data", "Classeur4.csv")
if (file.exists(chemin_depot)) {
  fichier <- chemin_depot
  dossier <- getwd()
} else {
  cat("Choisis le fichier Classeur4.csv dans la fenêtre qui s'ouvre...\n")
  fichier <- file.choose()
  dossier <- dirname(fichier)
  setwd(dossier)                     # tout sera enregistré à côté du csv
}
dir.create("FIGURES", showWarnings = FALSE)

# le csv est séparé par des ";" et la 1ère colonne = ID de l'article
donnees_filtrees <- read.csv2(fichier, row.names = 1,
                              fileEncoding = "UTF-8-BOM")
cat("Articles :", nrow(donnees_filtrees),
    " | Variables :", ncol(donnees_filtrees), "\n")
stopifnot(all(as.matrix(donnees_filtrees) %in% c(0, 1)))   # vérification 0/1

# ---- noms complets des variables (d'après coding_framework_variables_codes.docx)
# (C5 = Model Drift n'existe pas dans le fichier : variable non incluse dans l'ACM)
noms_variables <- c(
  P1 = "CTR", P2 = "Conversion rate", P3 = "ROI/ROAS", P4 = "CLV",
  T1 = "Machine learning", T2 = "Predictive analytics", T3 = "Deep learning",
  T4 = "NLP", T5 = "Recommendation systems",
  C1 = "Data quality", C2 = "Privacy", C3 = "Algorithmic bias",
  C4 = "Explainability", C6 = "Data silos", C7 = "Attribution complexity",
  C8 = "Cross-channel tracking")
nom_var <- function(codes) unname(ifelse(codes %in% names(noms_variables),
                                         noms_variables[codes], codes))

# ---- numérotation des articles 1..41 (ordre du fichier csv)
# TRUE  : les graphiques montrent 1, 2, 3 ... 41 (comme Saura)
# FALSE : les graphiques gardent les ID d'origine (S1, W3, ...)
ARTICLES_NUMEROTES <- TRUE
correspondance <- data.frame(Numero = seq_len(nrow(donnees_filtrees)),
                             ID_origine = rownames(donnees_filtrees))
if (ARTICLES_NUMEROTES) {
  rownames(donnees_filtrees) <- correspondance$Numero
}
dir.create("FIGURES", showWarnings = FALSE)
write.csv(correspondance, file.path("FIGURES", "Correspondance_numero_ID.csv"),
          row.names = FALSE)

# ---------- 2. PRÉPARATION POUR L'ACM --------------------------------
# chaque colonne devient un facteur à 2 modalités : P1_0 (absent), P1_1 (présent)
df_mca <- as.data.frame(lapply(names(donnees_filtrees), function(v) {
  factor(paste0(v, "_", donnees_filtrees[[v]]),
         levels = c(paste0(v, "_0"), paste0(v, "_1")))
}))
names(df_mca)     <- names(donnees_filtrees)
rownames(df_mca)  <- rownames(donnees_filtrees)

# ---------- 3. ACM + HCPC --------------------------------------------
res.mca  <- MCA(df_mca, ncp = 5, graph = FALSE)

# nb.clust = nombre de clusters (5 = tableau Cluster.docx ; 3 = choix automatique
# de R ; -1 = R choisit tout seul)
NB_CLUSTERS <- 5
res.hcpc <- HCPC(res.mca, nb.clust = NB_CLUSTERS, graph = FALSE)
cat("Nombre de clusters retenu :",
    length(unique(res.hcpc$data.clust$clust)), "\n")

# petite fonction pour enregistrer + afficher
sauver <- function(graphique, nom, l = 10, h = 7) {
  print(graphique)
  ggsave(file.path("FIGURES", nom), plot = graphique,
         width = l, height = h, dpi = 300, bg = "white")
}

# on ne garde que les modalités "présent" (_1), comme dans l'article de Saura
mod_presentes <- grep("_1$", rownames(res.mca$var$coord), value = TRUE)

# etiquettes de dimensions avec le % de variance
pc <- round(res.mca$eig[, 2], 1)
lab1 <- paste0("Dim1 (", pc[1], "%)")
lab2 <- paste0("Dim2 (", pc[2], "%)")

# ======================================================================
# FIGURE 1 : Matrice de codage (articles x variables), articles triés par cluster
# ======================================================================
clust_art <- res.hcpc$data.clust$clust
ordre <- order(as.integer(as.character(clust_art)),
               suppressWarnings(as.numeric(rownames(donnees_filtrees))))
etiq  <- paste0(rownames(donnees_filtrees), "  [K", clust_art, "]")[ordre]
mat <- reshape2::melt(as.matrix(donnees_filtrees[ordre, ]))
names(mat) <- c("Article", "Variable", "Valeur")
mat$Article  <- factor(rep(etiq, times = ncol(donnees_filtrees)), levels = rev(etiq))
mat$Variable <- factor(nom_var(as.character(mat$Variable)),
                       levels = nom_var(names(donnees_filtrees)))
fig1 <- ggplot(mat, aes(Variable, Article, fill = factor(Valeur))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "#EEEEEE", "1" = "#2E86AB"),
                    labels = c("0 = absent", "1 = present"), name = "") +
  labs(title = "Figure 1. Coding matrix for the 16 variables (articles sorted by cluster K)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7),
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 11))
sauver(fig1, "Figure1_Matrice_codage.png", 8, 10)

# ======================================================================
# FIGURE 2 : Fréquence des variables  (ton code)
# ======================================================================
freq_vars <- colSums(donnees_filtrees)
freq_df <- data.frame(Variable = nom_var(names(freq_vars)), Frequence = freq_vars)
freq_df$Etiquette <- paste0(freq_df$Frequence, "\n(",
                            round(100 * freq_df$Frequence / nrow(donnees_filtrees), 1), "%)")
freq_df <- freq_df[order(freq_df$Frequence, decreasing = TRUE), ]
fig2 <- ggplot(freq_df, aes(x = reorder(Variable, -Frequence), y = Frequence)) +
  geom_bar(stat = "identity", fill = "#2E86AB", color = "black", width = 0.7) +
  geom_text(aes(label = Etiquette), vjust = -0.3, size = 3.3, fontface = "bold", lineheight = 0.9) +
  labs(title = "Figure 2. Frequency of variables in the literature",
       x = "Variables", y = "Number of articles mentioning the variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(10, 10, 10, 60)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
sauver(fig2, "Figure2_Frequence_Variables.png", 12, 7.5)

# ======================================================================
# FIGURE 3 : Éboulis des valeurs propres (Table 4 en graphique)
# ======================================================================
fig3 <- fviz_screeplot(res.mca, addlabels = TRUE, ncp = 5,
                       barfill = "#2E86AB", barcolor = "black") +
  labs(title = "Figure 3. Eigenvalues / explained variance (MCA)",
       x = "Dimensions", y = "% of explained variance")
sauver(fig3, "Figure3_Eboulis.png")

# ======================================================================
#  STYLE "SAURA" : couleurs et groupes utilisés dans les figures 4 à 7
# ======================================================================
grad <- c("#00AFBB", "#E7B800", "#FC4E07")      # bleu-vert -> jaune -> orange

vc <- as.data.frame(res.mca$var$coord[mod_presentes, 1:2])
names(vc) <- c("Dim1", "Dim2")
vc$Variable <- nom_var(sub("_1$", "", rownames(vc)))
vc$cos2     <- rowSums(res.mca$var$cos2[mod_presentes, 1:2])
set.seed(123)
K_VAR <- 3                                      # nb de groupes de variables (C1..C4)
vc$Groupe <- factor(paste0("C", kmeans(vc[, c("Dim1", "Dim2")], K_VAR,
                                       nstart = 25)$cluster))
couleurs_groupes <- setNames(c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02", "#E7298A")[1:K_VAR],
                             levels(vc$Groupe))

ic <- as.data.frame(res.mca$ind$coord[, 1:2])
names(ic) <- c("Dim1", "Dim2")
ic$Article <- rownames(ic)
ic$contrib <- rowSums(res.mca$ind$contrib[, 1:2])

# ellipses (une couche par groupe ayant au moins 3 variables)
ellipses_groupes <- function(couleurs = couleurs_groupes, lt = "solid") {
  lapply(levels(vc$Groupe), function(g) {
    d <- vc[vc$Groupe == g, ]
    if (nrow(d) < 3) return(NULL)
    stat_ellipse(data = d, aes(Dim1, Dim2), inherit.aes = FALSE, type = "norm",
                 level = 0.8, color = couleurs[[g]], linetype = lt, linewidth = 0.6)
  })
}
theme_saura <- theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold"))
axes <- list(geom_hline(yintercept = 0, linetype = "dashed"),
             geom_vline(xintercept = 0, linetype = "dashed"),
             labs(x = lab1, y = lab2))

# ======================================================================
# FIGURE 4 : Carte des variables (= Fig.1 de Saura)
#   triangles + noms colorés selon cos2, ellipses = groupes de variables
# ======================================================================
fig4 <- ggplot(vc, aes(Dim1, Dim2, color = cos2)) +
  axes + ellipses_groupes() +
  geom_point(shape = 17, size = 2.5) +
  geom_text_repel(aes(label = Variable), size = 3.8, max.overlaps = Inf) +
  scale_color_gradientn(colours = grad, name = "cos2") +
  labs(title = "Figure 4. Variables factor map (MCA)") +
  theme_saura
sauver(fig4, "Figure4_Carte_variables.png")

# ======================================================================
# FIGURE 5 : Carte des articles (= Fig.2 de Saura)
#   les articles sont écrits (leur ID), colorés selon leur contribution
# ======================================================================
fig5 <- ggplot(ic, aes(Dim1, Dim2, color = contrib)) +
  axes +
  geom_text_repel(aes(label = Article), size = 4, max.overlaps = Inf) +
  scale_color_gradientn(colours = grad, name = "contrib") +
  labs(title = "Figure 5. Contributions of row points (articles) in MCA") +
  theme_saura
sauver(fig5, "Figure5_Carte_articles.png")

# ======================================================================
# FIGURE 6 : Biplot (= Fig.3 de Saura)
#   articles en bleu, variables en rouge avec ellipses rouges
# ======================================================================
fig6 <- ggplot() +
  axes + ellipses_groupes(rep("red", K_VAR) |> setNames(levels(vc$Groupe)), "dashed") +
  geom_text_repel(data = ic, aes(Dim1, Dim2, label = Article),
                  color = "blue", size = 3.5, max.overlaps = Inf) +
  geom_point(data = vc, aes(Dim1, Dim2), color = "red", shape = 17, size = 2) +
  geom_text_repel(data = vc, aes(Dim1, Dim2, label = Variable),
                  color = "red", size = 3.8, max.overlaps = Inf) +
  labs(title = "Figure 6. Multiple Correspondence Analysis biplot") +
  theme_saura
sauver(fig6, "Figure6_Biplot.png")

# ======================================================================
# FIGURE 7 : Biplot avec flèches (= Fig.4 de Saura)
#   plus la flèche est longue, plus la variable est loin du centre
# ======================================================================
fig7 <- ggplot() +
  axes +
  geom_segment(data = vc, aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.25, "cm")), color = "red") +
  geom_text_repel(data = ic, aes(Dim1, Dim2, label = Article),
                  color = "blue", size = 3, max.overlaps = Inf) +
  geom_text_repel(data = vc, aes(Dim1, Dim2, label = Variable),
                  color = "red", size = 3.8, max.overlaps = Inf) +
  labs(title = "Figure 7. MCA biplot - distances to the centre of the X and Y axes") +
  theme_saura
sauver(fig7, "Figure7_Biplot_fleches.png")

# ======================================================================
# FIGURE 8 : Clusters HCPC sur le plan factoriel
# ======================================================================
# couleurs des clusters = celles du tableau Cluster.docx (rouge, jaune, vert, bleu, violet)
pal_clusters <- c("#E74C3C", "#F1C40F", "#2ECC71", "#3498DB", "#9B59B6", "#E67E22", "#7F8C8D")
K_CLUST <- length(unique(res.hcpc$data.clust$clust))
fig8 <- fviz_cluster(res.hcpc, repel = TRUE, show.clust.cent = TRUE,
                     palette = pal_clusters[1:K_CLUST], ggtheme = theme_minimal(),
                     main = "Figure 8. Clusters of articles on the factor plane (HCPC)")
sauver(fig8, "Figure8_Clusters_HCPC.png")

# ======================================================================
# FIGURE 9 : Dendrogramme (arbre de Ward) - feuilles colorées par cluster FINAL
#   L'arbre est celui de la classification hiérarchique ; la couleur de chaque
#   article est celle de son cluster final (Figure 8, tableau Cluster.docx).
#   => la figure est cohérente avec la Figure 8.
# ======================================================================
arbre <- res.hcpc$call$t$tree                       # arbre hiérarchique
cl_final <- setNames(res.hcpc$data.clust$clust, rownames(res.hcpc$data.clust))  # cluster final de chaque article
feuilles <- arbre$labels[arbre$order]               # articles dans l'ordre de l'arbre
couleurs_feuilles <- pal_clusters[as.integer(as.character(cl_final[feuilles]))]
fig9 <- fviz_dend(arbre, k = 1, k_colors = "grey30", rect = FALSE, cex = 0.8,
                  label_cols = couleurs_feuilles, lwd = 0.7,
                  main = "Figure 9. HCPC dendrogram (labels coloured by final cluster)",
                  sub = paste0("Colours: ",
                               paste0("K", seq_len(K_CLUST), " = ",
                                      c("red", "yellow", "green", "blue", "purple", "orange", "grey")[seq_len(K_CLUST)],
                                      collapse = "; ")))
sauver(fig9, "Figure9_Dendrogramme.png")

# ======================================================================
# ======================================================================
#   FIGURES SUPPLÉMENTAIRES POUR L'ARTICLE (10 à 15)
# ======================================================================
# ======================================================================
# noms des clusters (d'après Cluster.docx ; valables si 5 clusters)
noms_clusters <- c("K1 Predictive Analytics Core",
                   "K2 Campaign Performance Measurement",
                   "K3 Data Governance & Infrastructure",
                   "K4 Long-Term Value & Financial Return",
                   "K5 Advanced AI & Explainability")
if (K_CLUST != 5) noms_clusters <- paste0("K", seq_len(K_CLUST))
cl_f <- setNames(res.hcpc$data.clust$clust, rownames(res.hcpc$data.clust))
cl_f <- as.integer(as.character(cl_f[rownames(donnees_filtrees)]))
n_clust <- table(factor(cl_f, levels = seq_len(K_CLUST)))
noms_avec_n <- paste0(noms_clusters, " (n=", n_clust, ")")
type_var <- c(P = "Performance indicators", T = "Analytical techniques",
              C = "Measurement challenges")[substr(names(donnees_filtrees), 1, 1)]

# ----------------------------------------------------------------------
# FIGURE 10 : Profil des clusters (% d'articles de chaque cluster
#             qui traitent chaque variable)  -> la figure la plus utile
# ----------------------------------------------------------------------
prof <- aggregate(donnees_filtrees, by = list(Cluster = cl_f), FUN = mean)
prof_l <- reshape2::melt(prof, id.vars = "Cluster")
prof_l$Pct <- 100 * prof_l$value
prof_l$Cluster  <- factor(noms_avec_n[prof_l$Cluster], levels = rev(noms_avec_n))
prof_l$Variable <- factor(nom_var(as.character(prof_l$variable)),
                          levels = nom_var(names(donnees_filtrees)))
fig10 <- ggplot(prof_l, aes(Variable, Cluster, fill = Pct)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = round(Pct), color = Pct > 60), size = 3.6, fontface = "bold",
            show.legend = FALSE) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "white")) +
  scale_fill_gradientn(colours = c("#F7FBFF", "#9ECAE1", "#2171B5", "#08306B"),
                       limits = c(0, 100), name = "% of articles\nin the cluster") +
  labs(title = "Figure 10. Cluster profiles: share of articles addressing each variable (%)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 12))
sauver(fig10, "Figure10_Profil_clusters.png", 12, 5.5)

# ----------------------------------------------------------------------
# FIGURE 11 : Co-occurrence des variables (coefficient phi = corrélation 0/1)
# ----------------------------------------------------------------------
phi <- cor(donnees_filtrees)
ordv <- names(donnees_filtrees)
phi[upper.tri(phi)] <- NA
phi_l <- reshape2::melt(phi, na.rm = TRUE)
names(phi_l) <- c("V1", "V2", "phi")
phi_l$V1 <- factor(nom_var(as.character(phi_l$V1)), levels = nom_var(ordv))
phi_l$V2 <- factor(nom_var(as.character(phi_l$V2)), levels = rev(nom_var(ordv)))
phi_l <- phi_l[as.character(phi_l$V1) != as.character(phi_l$V2), ]
fig11 <- ggplot(phi_l, aes(V1, V2, fill = phi)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", phi)), size = 2.8) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-0.6, 0.8), name = "phi") +
  labs(title = "Figure 11. Co-occurrence of variables (phi coefficient between binary variables)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 11))
sauver(fig11, "Figure11_Cooccurrence_variables.png", 10, 8)

# ----------------------------------------------------------------------
# FIGURE 12 : Contribution des variables aux dimensions 1 et 2
# ----------------------------------------------------------------------
ctr <- res.mca$var$contrib[mod_presentes, 1:2]
ctr_l <- data.frame(Variable = rep(nom_var(sub("_1$", "", rownames(ctr))), 2),
                    Dim = rep(c(lab1, lab2), each = nrow(ctr)),
                    Contribution = c(ctr[, 1], ctr[, 2]))
ctr_l$Dim <- factor(ctr_l$Dim, levels = c(lab1, lab2))
ref <- 100 / nrow(res.mca$var$contrib)       # contribution moyenne attendue
fig12 <- ggplot(ctr_l, aes(reorder(Variable, Contribution), Contribution)) +
  geom_col(fill = "#2E86AB", color = "black", width = 0.7) +
  geom_hline(yintercept = ref, linetype = "dashed", color = "red") +
  coord_flip() + facet_wrap(~ Dim) +
  labs(title = "Figure 12. Contribution of each variable to the first two dimensions (%)",
       subtitle = "Red dashed line = average contribution expected under uniformity",
       x = NULL, y = "Contribution (%)") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 12), panel.grid.minor = element_blank())
sauver(fig12, "Figure12_Contribution_variables.png", 11, 6)

# ----------------------------------------------------------------------
# FIGURE 13 : Étendue thématique : nb de variables traitées par article, par cluster
# ----------------------------------------------------------------------
rich <- data.frame(Cluster = factor(noms_avec_n[cl_f], levels = noms_avec_n),
                   K = factor(paste0("K", cl_f)),
                   Nb = rowSums(donnees_filtrees))
fig13 <- ggplot(rich, aes(Cluster, Nb, fill = K)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = 0.12, height = 0.05, size = 2, show.legend = FALSE) +
  scale_fill_manual(values = setNames(pal_clusters[1:K_CLUST], paste0("K", 1:K_CLUST))) +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x, fixed = TRUE)) +
  labs(title = "Figure 13. Thematic breadth: number of variables addressed per article",
       x = NULL, y = "Number of variables (out of 16)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7.5, face = "bold"),
        plot.title = element_text(face = "bold", size = 12))
sauver(fig13, "Figure13_Etendue_thematique.png", 11, 6)

# ----------------------------------------------------------------------
# FIGURE 14 : Variables sur- / sous-représentées par cluster (v.test, HCPC)
# ----------------------------------------------------------------------
desc <- res.hcpc$desc.var$category
vt <- do.call(rbind, lapply(seq_along(desc), function(k) {
  d <- desc[[k]]
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d <- as.data.frame(d)
  d$modalite <- rownames(d)
  d$K <- k
  d
}))
vt <- vt[grepl("_1$", vt$modalite), ]              # on garde "variable présente"
vt$code <- sub("^.*=", "", sub("_1$", "", vt$modalite))
vt$code <- sub("^(.*)=.*$", "\\1", vt$modalite)
vt$code <- sub("=.*$", "", vt$modalite)
vt$Variable <- nom_var(vt$code)
vt <- vt[abs(vt$v.test) > 2, ]
if (nrow(vt) > 0) {
  vt$Cluster <- factor(noms_clusters[vt$K], levels = noms_clusters)
  fig14 <- ggplot(vt, aes(reorder(Variable, v.test), v.test, fill = v.test > 0)) +
    geom_col(color = "black", width = 0.7, show.legend = FALSE) +
    coord_flip() + facet_wrap(~ Cluster, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = c("TRUE" = "#2E86AB", "FALSE" = "#D95F02")) +
    labs(title = "Figure 14. Variables over- (blue) and under-represented (orange) in each cluster",
         subtitle = "v-test from HCPC; only |v-test| > 2 (variable present)",
         x = NULL, y = "v-test") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", size = 12),
          strip.text = element_text(face = "bold", size = 8))
  sauver(fig14, "Figure14_vtest_clusters.png", 12, 9)
}

# ----------------------------------------------------------------------
# FIGURE 15 : Répartition par dimension conceptuelle (performance /
#             techniques / défis) : nb moyen de variables par article et par cluster
# ----------------------------------------------------------------------
types <- unique(type_var)
moy <- sapply(types, function(t) rowSums(donnees_filtrees[, type_var == t, drop = FALSE]))
moy_c <- aggregate(as.data.frame(moy), by = list(K = cl_f), FUN = mean)
moy_l <- reshape2::melt(moy_c, id.vars = "K")
names(moy_l) <- c("K", "Dimension", "Mean")
moy_l$Dimension <- factor(gsub("\\.", " ", moy_l$Dimension),
                          levels = gsub("\\.", " ", make.names(types)))
moy_l$Cluster <- factor(noms_avec_n[moy_l$K], levels = noms_avec_n)
fig15 <- ggplot(moy_l, aes(Cluster, Mean, fill = Dimension)) +
  geom_col(position = "dodge", color = "black", width = 0.75) +
  geom_text(aes(label = round(Mean, 1)), position = position_dodge(width = 0.75),
            vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("#2E86AB", "#E7B800", "#D95F02")) +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x, fixed = TRUE)) +
  labs(title = "Figure 15. Average number of variables per article by conceptual dimension and cluster",
       x = NULL, y = "Mean number of variables per article", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7.5, face = "bold"),
        plot.title = element_text(face = "bold", size = 11),
        legend.position = "top")
sauver(fig15, "Figure15_Dimensions_par_cluster.png", 12, 6.5)

writeLines(capture.output(sessionInfo()), file.path("FIGURES", "sessionInfo.txt"))
cat("\nTERMINÉ. Les images sont dans :", file.path(dossier, "FIGURES"), "\n")
print(list.files("FIGURES"))
