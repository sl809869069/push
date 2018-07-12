# load('~/Desktop/learnFunction/normal_male_female.R')
# ri = build_ri(male,female)
# data("A166")
# data("A166_clinical")
# x = calc_outliers(protein.data,clinical.info,ri)$result_info
# rownames(x) = x$GeneSymbol
# x = x[,-1]
# y = annotate(x)

calc_outliers <- function(protein.data,clinical.info,ri,imputed_value = 0,ri.freq.cutoff = 0.25){
  if(length(clinical.info$Gender[!duplicated(clinical.info$Gender)]) > 1){
    cat('Error: There are two genders.')
    return(0)
  }
  gender = clinical.info$Gender[1]
  if(gender == '男'){
    ri_4_calc = data.frame(GeneSymbol = rownames(ri),RI = ri$ri.male)
  }else if(gender == '女'){
    ri_4_calc = data.frame(GeneSymbol = rownames(ri),RI = ri$ri.female)
  }else{
    cat('Error: Gender error')
    return(0)
  }
  p_data = data.frame(GeneSymbol = rownames(protein.data),protein.data)
  merged_data = merge(ri_4_calc,p_data,by = 'GeneSymbol',all.y = T)
  merged_data[is.na(merged_data)] = imputed_value

  # ==== calc diff matrix ====
  diff_matrix = merged_data[,-1:-2]
  rownames(diff_matrix) = merged_data$GeneSymbol
  diff_matrix = diff_matrix - merged_data$RI
  diff_matrix[diff_matrix < 0] = 0
  diff_matrix[diff_matrix > 0] = 1

  outliers_of_each_sample = data.frame(Sample = colnames(diff_matrix),RI.count = apply(diff_matrix,2,sum))

  outlier_freq = data.frame(GeneSymbol = rownames(diff_matrix),RI.freq = apply(diff_matrix,1,sum))
  outlier_freq_table = merge(outlier_freq,merged_data,by = 'GeneSymbol',all = T)
  outlier_freq_table$GeneSymbol = as.character(outlier_freq_table$GeneSymbol)

  x = outliers_of_each_sample$RI.count
  x = c('Outlier.count','','',x)
  result_info = rbind(x,outlier_freq_table)

  x = as.vector(clinical.info$Date)
  x = c('Date','','',x)
  result_info = rbind(x,result_info)

  result = list(diff_matrix = diff_matrix,
                outliers_of_each_sample = outliers_of_each_sample,
                outlier_freq_table = outlier_freq_table,
                result_info = result_info)
  return(result)
}

