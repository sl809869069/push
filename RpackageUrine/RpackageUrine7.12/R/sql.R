library(RMySQL)
sqlResult <- function(sql){
  #尝试连接数据库
  conn<-dbConnect(MySQL(),dbname='Urine_Survey',user='nixiaotian',password='nixiaotian',host='192.168.99.4')

  #解决乱码关键
  dbSendQuery(conn,'SET NAMES utf8')#设置数据库读取编码格式为utf-8

  # 查询
  result = dbGetQuery(conn, sql)

  #断开当前连接
  dbDisconnect(conn)

  return(result)
}

