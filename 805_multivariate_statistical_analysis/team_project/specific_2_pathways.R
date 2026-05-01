library(car)
library(ggplot2)
library(patchwork) 
setwd('/Users/fangsiyu/Desktop/sdu-2026-code/805_multivariate_statistical_analysis/team_project')

source('hotelling.R')
source('GSEA_functions.R')
source('dsk805utils.R')
source('lottery.R')
expr_gene <- readRDS("gene_expressions.RDS") 
group_inds <- readRDS("group.RDS")
hml_all <- readRDS("HML.RDS")

# 'HALLMARK_DNA_REPAIR' , significant in the T^2 test but not in GSEA
# 'HALLMARK_COMPLEMENT', significant in GSEA but not in the T^2 test

# ==============================================================================
# STEP 0: do GSEA plot and visualization
# ==============================================================================
group_inds_logical <- (group_inds == "relapse")
ranked_result <- rank_genes(expr_gene, group_inds_logical)
diff_scores <- ranked_result[["diff"]]
ordered_expr <- ranked_result[["genes"]]

pathway_name <- 'HALLMARK_COMPLEMENT'
pathway_genes <- hml_all[[pathway_name]]

es_result <- compute_enrichment_score(diff_scores, pathway_genes)
print(paste(pathway_name, "- Max ES:", es_result$ES_max))


# ==============================================================================
# 1. GSEA Plot
# ==============================================================================
gsea_data <- data.frame(
  GeneRank = 1:length(es_result$ES),
  ES = es_result$ES
)

gsea_plot <- ggplot(gsea_data, aes(x = GeneRank, y = ES)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_hline(yintercept = es_result$ES_max, color = "red", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = max(gsea_data$GeneRank), y = es_result$ES_max, 
           label = paste("Max ES:", round(es_result$ES_max, 3)), 
           color = "red", fontface = "bold", hjust = 1, vjust = -0.5) +
  labs(
    title = paste("GSEA:", pathway_name),
    x = "Gene Rank",
    y = "Enrichment Score"
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), 
    text = element_text(size = 12)
  )

# ==============================================================================
# 2. PCA
# ==============================================================================
target_expr <- expr_gene[rownames(expr_gene) %in% pathway_genes, ]
pca_input <- t(target_expr)
pca_result <- prcomp(pca_input, scale. = TRUE) 

pca_data <- data.frame(
  Patient = rownames(pca_input),
  PC1 = pca_result$x[, 1], 
  PC2 = pca_result$x[, 2], 
  Group = group_inds
) 

pc1_var <- round(pca_result$sdev[1]^2 / sum(pca_result$sdev^2) * 100, 1) 
pc2_var <- round(pca_result$sdev[2]^2 / sum(pca_result$sdev^2) * 100, 1) 

pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Group, fill = Group)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(geom = "polygon", alpha = 0.2, type = "norm") +
  scale_color_manual(values = c("relapse" = "#E41A1C", "no_relapse" = "#377EB8")) + 
  scale_fill_manual(values = c("relapse" = "#E41A1C", "no_relapse" = "#377EB8")) +
  labs(
    title = paste("PCA:", pathway_name),
    x = paste0("PC1 (", pc1_var, "%)"),
    y = paste0("PC2 (", pc2_var, "%)")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), 
    text = element_text(size = 12),
    legend.position = "right"
  )

# ==============================================================================
# 3. combine and get the PLOT
# ==============================================================================
combined_plot <- gsea_plot + pca_plot

ggsave("all the plot/HALLMARK_COMPLEMENT_Combined_Plot.png", 
       plot = combined_plot, 
       width = 14, height = 6, units = "in", dpi = 300, bg = "white")