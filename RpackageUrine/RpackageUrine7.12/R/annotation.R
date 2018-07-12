annotate <- function(protein.data,gene.description = T,tissue.specificity = T){
  protein.data = data.frame(GeneSymbol = rownames(protein.data),protein.data)
  data("annotationData")
  annotated.data = data.frame(GeneSymbol = anno$GeneSymbol)
  if(gene.description == T){
    annotated.data = data.frame(annotated.data,
                                Gene.description = anno$description,
                                Gene.summary = anno$ncbi_gene_summary)
  }
  if(tissue.specificity == T){
    annotated.data = data.frame(annotated.data,
                      RNA.tissue.category = anno$category,
                      Number.of.detected.tissues = anno$no..detected.tissues,
                      RNA.TS = anno$elevated.tissue.s.,
                      RNA.tissue.specific.score = anno$tissue.specific.score,
                      RNA.group.specific.score = anno$group.specific.score,
                      RNA.enhanced.score = anno$enhanced.score
                      )
  }
  annotated.data = merge(annotated.data,protein.data,by = 'GeneSymbol',all.y = T,sort = F)
  x = annotated.data$GeneSymbol
  annotated.data = annotated.data[!duplicated(x),]
  rownames(annotated.data) = annotated.data$GeneSymbol
  annotated.data = annotated.data[sort(rownames(annotated.data))[rank(rownames(protein.data))],]
  return(annotated.data)
}

