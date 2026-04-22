# ==============================================================================
# Parallel Processing, it should work on windows env as well
# ==============================================================================
install.packages("future.apply")
library(future.apply)
library(parallel)

num_cores <- detectCores() - 2 
plan(multisession, workers = num_cores) 
print(paste("Using", nbrOfWorkers(), "cores for parallel processing."))

approximate_null_distribution_fast <- function(expr_gene, inds, geneset, iters=1000, p=1) {
  evids_list <- future_lapply(1:iters, function(i) {
    p1 = sample(inds)
    res = rank_genes(expr_gene, p1)
    res2 = compute_enrichment_score(res$diff, geneset)
    return(res2$ES_max)
  }, future.seed = TRUE)
  
  evids <- unlist(evids_list)
  return(evids)
}

# ==============================================================================
# STEP 0: MAIN SETUP, and load data
# ==============================================================================
setwd('/Users/fangsiyu/Desktop/sdu-2026-code/805_multivariate_statistical_analysis/team_project')
library(car)

source('hotelling.R')
source('GSEA_functions.R')
source('dsk805utils.R')
source('lottery.R')
expr_gene <- readRDS("gene_expressions.RDS") # col -> patient, row -> all genes' score
group_inds <- readRDS("group.RDS") # the no_relapse / relapse result
hml_all <- readRDS("HML.RDS") # all the pathway and which genes build the pathway

genetic_lottery(c("ossch25", "ajash25", "sifan25"))
lottery_result <- c("HALLMARK_ESTROGEN_RESPONSE_EARLY", "HALLMARK_ALLOGRAFT_REJECTION", "HALLMARK_DNA_REPAIR", "HALLMARK_PI3K_AKT_MTOR_SIGNALING", "HALLMARK_HEME_METABOLISM")
my_5_pathway <- hml_all[lottery_result] # extract the 5 pathway we need from hml_all


# ==============================================================================
# STEP 1: GSEA_functions part
# ==============================================================================
group_inds_logical <- (group_inds == "relapse") # we have to turn it into T/F list
ranked_result <- rank_genes(expr_gene, group_inds_logical) # re-order base on the activeness they have in "relapse group"
diff_scores <- ranked_result[["diff"]] # weird R, u need to use [[]] to un-wrap and "only get the value"
ordered_expr <- ranked_result[["genes"]]

current_geneset_genes <- my_5_pathway[[lottery_result[4]]] 

es_result <- compute_enrichment_score(diff_scores, current_geneset_genes) # calculate how the pathway we pick is matched with all the genes order score
print(paste("Pathway:", lottery_result[4], "- Max ES:", es_result$ES_max))

null_dist <- approximate_null_distribution_fast(expr_gene, group_inds_logical, current_geneset_genes, iters = 1000) # generate 1000 random es score
actual_es <- es_result[["ES_max"]]
gsea_pvalue <- sum(abs(null_dist) >= abs(actual_es)) / length(null_dist) 
# compare random es with actual_es, find the ratio that's bigger than the score we actual got, how significant the actual_es is.

print(paste("GSEA p-value:", gsea_pvalue))


png(paste0(lottery_result[4], "_heatmap.png"), width = 1000, height = 800, res = 100)
plot_gsea_heatmap(ordered_expr, group_inds_logical)
dev.off()

# ==============================================================================
# STEP 2: T2 Test, 2 group, muilti measurement
# ==============================================================================
valid_genes <- intersect(current_geneset_genes, rownames(expr_gene)) # find the intersection of both, and filter-out the genes not in current pathway 
pathway_data_table <- t(expr_gene[valid_genes, ]) # base on valid_genes, find the row in the expr_gene table, and T
pathway_data_transformed <- apply(pathway_data_table, 2, yj_transform) # cuz we're doing T2, we have to make sure force the data to be normally distributed.

group1_data <- pathway_data_transformed[group_inds_logical, ]
group2_data <- pathway_data_transformed[!group_inds_logical, ]

t2_result <- two_sample_test(group1_data, group2_data)
print(paste("T2 p-value  :", t2_result$pvalue))


# ==============================================================================
# ==============================================================================
# Pathway: HALLMARK_ESTROGEN_RESPONSE_EARLY - Max ES: 0.24259005075277
# GSEA p-value: 0.592
# T2 p-value  : 0.000751325129620284

# Pathway: HALLMARK_ALLOGRAFT_REJECTION - Max ES: -0.639618114384972
# GSEA p-value: 0.004
# T2 p-value  : 0.0109568685852781

# Pathway: HALLMARK_DNA_REPAIR - Max ES: 0.354455074572819
# GSEA p-value: 0.081
# T2 p-value  : 0.00219744831903657

# Pathway: HALLMARK_PI3K_AKT_MTOR_SIGNALING - Max ES: -0.335329068599293
# GSEA p-value: 0.055
# T2 p-value  : 0.00335721145328016

# HALLMARK_HEME_METABOLISM - Max ES: -0.234261609394692
# GSEA p-value: 0.237
# T2 p-value  : 0.0640582794842217
