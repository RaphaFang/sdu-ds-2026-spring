setwd('/Users/fangsiyu/Desktop/sdu-2026-code/805_multivariate_statistical_analysis/team_project')

library(car)

source('hotelling.R')
source('GSEA_functions.R')
source('dsk805utils.R')
source('lottery.R')
genetic_lottery(c("ossch25", "ajash25", "sifan25"))

# "HALLMARK_ESTROGEN_RESPONSE_EARLY"
# "HALLMARK_ALLOGRAFT_REJECTION"
# "HALLMARK_DNA_REPAIR"
# "HALLMARK_PI3K_AKT_MTOR_SIGNALING"
# "HALLMARK_HEME_METABOLISM"   

# ========================================================================================
expr_gene = readRDS("gene_expressions.RDS")
group_inds = readRDS("group.RDS") # only have relapse / no-relapse
hml_all = readRDS("HML.RDS")


my_5_pathways <- c("HALLMARK_GLYCOLYSIS", 
                   "HALLMARK_HYPOXIA", 
                   "HALLMARK_KRAS_SIGNALING_DN", 
                   "HALLMARK_COAGULATION", 
                   "HALLMARK_APOPTOSIS")
my_genesets <- hml_all[my_5_pathways]

# ranked_result <- rank_genes(expr_gene, group_inds)
# 假設其中一個類別是 "Relapse"，我們把等於 "Relapse" 的設為 TRUE
group_inds_logical <- (group_inds == "Relapse") 
# 然後再把這個新的邏輯向量丟給 rank_genes
ranked_result <- rank_genes(expr_gene, group_inds_logical)


diff_scores <- ranked_result$diff

ordered_expr <- ranked_result$genes



# 抓取第一個通路的基因列表

current_geneset_name <- my_5_pathways[1]

current_geneset_genes <- my_genesets[[current_geneset_name]]



# 步驟 2：計算 Enrichment Score (ES)

es_result <- compute_enrichment_score(diff_scores, current_geneset_genes)

print(paste("Pathway:", current_geneset_name, "- Max ES:", es_result$ES_max))



# 步驟 3：計算 Null distribution 

# 注意：iters=1000 會跑很久，如果你只是想先測試程式會不會動，可以先設 iters=10，確認沒報錯再設回 1000 放著讓它跑。

null_dist <- approximate_null_distribution(expr_gene, group_inds, current_geneset_genes, iters = 1000)



# (可選) 畫出 GSEA 熱圖來看看

plot_gsea_heatmap(ordered_expr, group_inds)



# 1. 找出該通路中有實際出現在表達矩陣裡的基因

# (因為老師說可能有未登錄的基因，要過濾掉)

valid_genes <- intersect(current_geneset_genes, rownames(expr_gene))



# 2. 從表達矩陣中，只抽出這些有效基因的資料 (注意：要把矩陣轉置，讓病人變成 row)

pathway_data <- t(expr_gene[valid_genes, ])



# 3. 根據 group_inds 將病人分成兩組

# (假設 group_inds 裡 TRUE 代表復發組，FALSE 代表未復發組)

group1_data <- pathway_data[group_inds, ]

group2_data <- pathway_data[!group_inds, ]



# 4. 執行 two_sample_test

t2_result <- two_sample_test(group1_data, group2_data)



# 印出 p-value

print(paste("Hotelling's T2 p-value for", current_geneset_name, ":", t2_result$pvalue))