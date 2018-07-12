build_ri <- function(male,female,upper.ri = 0.975){
  male_ri = data.frame(GeneSymbol = rownames(male),
                       ri.male = apply(male,1,function(x){return(quantile(x,upper.ri))}))
  female_ri = data.frame(GeneSymbol = rownames(female),
                         ri.female = apply(female,1,function(x){return(quantile(x,upper.ri))}))
  ri = merge(male_ri,female_ri,by = 'GeneSymbol',all = T)
  rownames(ri) = ri$GeneSymbol
  ri = ri[,-1]
  ri = round(ri,2)
  return(ri)
}

