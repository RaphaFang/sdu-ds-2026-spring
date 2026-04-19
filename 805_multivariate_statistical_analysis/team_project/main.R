# ==============================================================================
# PREPARATION: Setup Parallel Processing
# ==============================================================================
library(parallel)

num_cores <- detectCores() - 2 
print(paste("Using", num_cores, "cores for parallel processing!"))

approximate_null_distribution_fast <- function(expr_gene, inds, geneset, iters=1000, p=1) {
  evids_list <- mclapply(1:iters, function(i) {
    p1 = sample(inds)
    res = rank_genes(expr_gene, p1)
    res2 = compute_enrichment_score(res$diff, geneset)
    return(res2$ES_max)
  }, mc.cores = num_cores)
  
  evids <- unlist(evids_list)
  return(evids)
}

# ==============================================================================
# MAIN SETUP
# ==============================================================================
setwd('/Users/fangsiyu/Desktop/sdu-2026-code/805_multivariate_statistical_analysis/team_project')
library(car)

source('hotelling.R')
source('GSEA_functions.R')
source('dsk805utils.R')
source('lottery.R')

genetic_lottery(c("ossch25", "ajash25", "sifan25"))

lottery_result <- c(
  "HALLMARK_ESTROGEN_RESPONSE_EARLY",
  "HALLMARK_ALLOGRAFT_REJECTION",
  "HALLMARK_DNA_REPAIR",
  "HALLMARK_PI3K_AKT_MTOR_SIGNALING",
  "HALLMARK_HEME_METABOLISM"
)

# ==============================================================================
# STEP 1: Load Data & Prepare Gene Ranking
# ==============================================================================
expr_gene <- readRDS("gene_expressions.RDS")
group_inds <- readRDS("group.RDS")
hml_all <- readRDS("HML.RDS")

group_inds_logical <- (group_inds == "relapse") 
ranked_result <- rank_genes(expr_gene, group_inds_logical)

diff_scores <- ranked_result$diff
ordered_expr <- ranked_result$genes

my_genesets <- hml_all[lottery_result]

# ==============================================================================
# STEP 2: Calculate Enrichment Score (ES) & Null Distribution
# ==============================================================================
current_geneset_genes <- as.character(unlist(my_genesets[[lottery_result[5]]]))

es_result <- compute_enrichment_score(diff_scores, current_geneset_genes)
print(paste("Pathway:", lottery_result[5], "- Max ES:", es_result$ES_max))

null_dist <- approximate_null_distribution_fast(expr_gene, group_inds_logical, current_geneset_genes, iters = 1000)

# Plot GSEA heatmap
plot_gsea_heatmap(ordered_expr, group_inds_logical)

# ==============================================================================
# STEP 3: Hotelling's T^2 Test
# ==============================================================================
# Filter valid genes present in the expression matrix
valid_genes <- intersect(current_geneset_genes, rownames(expr_gene))

# Transpose expression matrix for valid genes
pathway_data <- t(expr_gene[valid_genes, ])

# Split patients into two groups (relapse vs. no-relapse)
group1_data <- pathway_data[group_inds_logical, ]
group2_data <- pathway_data[!group_inds_logical, ]

# Run two-sample Hotelling's T^2 test
t2_result <- two_sample_test(group1_data, group2_data)

print(paste("Hotelling's T2 p-value for", lottery_result[5], ":", t2_result$pvalue))

# Previous p-value results:
# HALLMARK_ESTROGEN_RESPONSE_EARLY : 0.000751325129620284
# HALLMARK_ALLOGRAFT_REJECTION     : 0.0109568685852781
# HALLMARK_DNA_REPAIR              : 0.00219744831903657
# HALLMARK_PI3K_AKT_MTOR_SIGNALING : 0.00219532700196945
# HALLMARK_HEME_METABOLISM         : 0.0640582794842217