filterUS <- function(targetDIR,u=0,s=0,us=2,GPs.count.cutoff = 0,type){
  setwd(targetDIR)
  filelist = list.files(targetDIR)

  if(type == 'ifot'){
    index = 9
  }else if(type == 'ibaq'){
    index = 8
  }else{
    cat('Error: No such data type.')
    return(0)
  }

  totalProteins = c()
  totalFiles = c()
  for(file in filelist){
    if(substr(file,nchar(file)-3,nchar(file)) == '.txt'){
      totalFiles = c(totalFiles,file)

      data = read.delim(file,header = T)
      tmp_data = data[,c(1,4,5,6,index)]

      # tmp_data[,2] = upep, tmp_data[,3] = spep, tmp_data[,4] = uspep
      tmp_data = tmp_data[which(tmp_data[,2] >= u & tmp_data[,3] >= s & tmp_data[,4] >= us),]
      totalProteins = union(totalProteins,tmp_data$GeneSymbol)
    }
  }

  totalProteins = totalProteins[order(totalProteins)]
  data_all = data.frame(GeneSymbol = totalProteins)
  data_result = data.frame(GeneSymbol = totalProteins)

  cat('File list: ','\n')
  for(file in filelist){
    if(substr(file,nchar(file)-3,nchar(file)) == '.txt'){
      cat(file,'\n')

      data = read.delim(file,header = T)
      tmp_data = data[,c(1,4,5,6,index)]
      # tmp_data[,2] = upep, tmp_data[,3] = spep, tmp_data[,4] = uspep
      tmp_data = tmp_data[which(tmp_data[,2] >= u & tmp_data[,3] >= s & tmp_data[,4] >= us),]
      tmp_data = merge(data_all,tmp_data,by = 'GeneSymbol',all = T)
      tmp_data = tmp_data[order(tmp_data$GeneSymbol),]
      data_result = cbind(data_result,tmp_data[,5])
      colnames(data_result)[ncol(data_result)] = substr(file,1,nchar(file)-8)
    }
  }
  count = apply(data_result,2,function(x){return(length(x[!is.na(x) & x > 0]))})
  result = data_result[,which(count > GPs.count.cutoff)]
  cat('Total files: ',length(totalFiles),'\n')
  cat('Total GPs: ',length(totalProteins),'\n')
  return(result)
}

