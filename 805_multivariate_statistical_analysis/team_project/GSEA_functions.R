rank_genes = function(expr_gene, inds){
  '
  A function for ranking the genes by how well they separate the phenotype
  This implements just the Signal-to-Noise Ratio
  In:
    gene_expression_data: an n by p dataframe, n observations of p genes
    phenotype: A binary vector of ones and zeros of length n
  Out:
    res$genes: list of columns of Gene_expression_data, in the descending order
      of their evidence
    res$diff: the evidence, i.e. the difference between the phenotypes
  '
  mu1=rowMeans(expr_gene[,inds])
  mu2=rowMeans(expr_gene[,!inds])
  sd1=apply(expr_gene[,inds], 1, FUN=sd)
  sd2=apply(expr_gene[,!inds], 1, FUN=sd)
  diff=(mu1-mu2)/(sd1+sd2)
  genes=expr_gene[order(diff,decreasing = TRUE),]
  diff=diff[order(diff,decreasing=TRUE)]
  res=NULL
  res$diff=diff
  res$genes=genes
  return(res)
  
}

compute_enrichment_score = function(diff, geneset, p = 1) {
  '
  Given a geneset and the individual evidences, compute the enrichsment score
  for the geneset
  in:
    diff: A sorted and named p-vector that describes the evidence for each of the columns
      of the gene_expression_data
    geneset: A named subset of the columns of the gene_expression_data
      The geneset and the diff have to have compatible names
    p: The weighting factor. p=1 is the default, p=0 is the KS stat. Higher
    p puts more emphasis on the extreme entries (makes the method less robust)
  
  '
  diff = sort(diff, decreasing = TRUE)
  genes = names(diff)
  
  hits = genes %in% geneset
  
  Nh = sum(hits)
  N = length(genes)
  
  if (Nh == 0 || Nh == N) {
    stop("Invalid gene set overlap")
  }
  
  weights = abs(diff)^p
  
  hit_weights = weights[hits]
  P_hit  = cumsum(hits * weights / sum(hit_weights))
  
  misses = as.numeric(!hits)
  P_miss = cumsum(misses / (N - Nh))
  
  running_ES = P_hit - P_miss
  
  
  hit_steps = hit_weights / sum(hit_weights)
  miss_step = 1 / (N - Nh)
  
  return(list(
    ES = running_ES,
    ES_max = running_ES[which.max(abs(running_ES))]
  ))
}


approximate_null_distribution=function(expr_gene,inds,geneset,iters=1000,p=1){
  '
  Approximates null distribution of the enrichment score under the hypothesis
  that the geneset is not any more differentially expressed than other genesets
  of similar size. This is done by permuting the phenotypes labels (expensive)
  The results can be used to compute approximate p-values and other similar
  quantities
  In:
    expr_gene: the gene expression data
    inds: boolean vector describing phenotype
    geneset: The geneset to be studied
    iters: Number of iterations (higher is better, but slower)
    p: the weight factor (defaults to 1)
  Out:
    evids: a vector of length iter, each describing the ES_max computed for a
    single permutation.
  '
  evids=rep(0,iters)
  for (i in 1:iters) {
    p1=sample(inds)
    res=rank_genes(expr_gene, p1)
    res2=compute_enrichment_score(res$diff,geneset)
    evids[i]=res2$ES_max
  }
  return(evids)
}


plot_gsea_heatmap <- function(expr_ordered, inds, 
                              n = 100, 
                              scale_genes = TRUE,
                              use_bins = FALSE,
                              n_bins = 50) {
  '
  A picture for drawing the GSEA Heatmap. Automatically generates the plot with
  no return. Useful for looking at the definition of evidence
  In: expr_ordered: The gene expression matrix ordered by evidence
      inds: Boolean vector denoting the groups
  '
  
  # --- checks ---
  if (length(inds) != ncol(expr_ordered)) {
    stop("inds must match number of samples (columns)")
  }
  
  # --- gene selection ---
  if (!use_bins) {
    n = min(n, floor(nrow(expr_ordered)/2))
    genes_use = c(
      rownames(expr_ordered)[1:n],
      rownames(expr_ordered)[(nrow(expr_ordered)-n+1):nrow(expr_ordered)]
    )
    expr_sub = expr_ordered[genes_use, , drop=FALSE]
  } else {
    bins = cut(1:nrow(expr_ordered), breaks = n_bins, labels = FALSE)
    expr_sub = sapply(1:n_bins, function(i) {
      colMeans(expr_ordered[bins == i, , drop=FALSE])
    })
    expr_sub = t(expr_sub)
  }
  
  # --- scale ---
  if (scale_genes) {
    expr_sub = log2(expr_sub + 1)
    expr_sub = t(scale(t(expr_sub)))
    expr_sub[is.na(expr_sub)] = 0
  }
  gene_means = rowMeans(expr_sub)
  gene_sds   = apply(expr_sub, 1, sd)
  
  ord = order(inds)
  expr_sub = expr_sub[, ord, drop=FALSE]
  inds_ord = inds[ord]
  
  # color scale (mimicking the original)
  cols = colorRampPalette(c("blue", "white", "red"))(100)
  
  # plot layout
  par(mfrow = c(2,1), mar = c(0,4,2,2))
  
  # The phenotype bar
  z_bar = matrix(as.numeric(inds_ord), nrow = length(inds_ord), ncol = 1)
  
  image(
    x = 1:nrow(z_bar),
    y = 1,
    z = z_bar,
    col = c("blue", "red"),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  
  sep = sum(inds_ord == 0)
  abline(v = sep, col = "black", lwd = 2)
  
  # The heat map
  par(mar = c(5, 4, 0, 2))
  
  mat = expr_sub[nrow(expr_sub):1, , drop = FALSE]  # flip top genes on top
  
  image(
    1:ncol(mat),
    1:nrow(mat),
    t(mat),
    col = cols,
    axes = FALSE,
    xlab = "Patients",
    ylab = "Genes"
  )
  
  axis(1, at = seq(1, ncol(mat), by = 10), labels = colnames(mat)[seq(1, ncol(mat), by = 10)])
  axis(2, at = seq(1, nrow(mat), by = 5), labels = rev(rownames(mat))[seq(1, nrow(mat), by = 5)])
}
