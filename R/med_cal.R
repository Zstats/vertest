#' My Function
#'
#' This function calculate mediation effection.
#'
#' @param beta1, se1 beta2,se2,beta_total,n.
#' @return data.frame.
#' @export
#'
#'
#beta1、se1：XM的回归系数和标准误，beta2、se2：MY的回归系数和标准误，beta_total:XY总效应回归系数
.med_cal <- function(beta1,se1,beta2,se2,beta_total,n=1000){
  # 采用compiler 方式加载
  # fun_env <- new.env()
  # compiler::loadcmp("inst/bytecode/utils.rds", envir = fun_env)
  # utilTool <-fun_env$UtilsTool$new()

  #utilTool <-UtilsTool$new()

  # return(utilTool$med_cal(beta1,se1,beta2,se2,beta_total,n))
  args <- list(beta1=beta1, se1=se1, beta2=beta2, se2=se2, beta_total=beta_total, n=n)

  # 使用加密文件路径
  enc_path <- system.file("bytecode", "utilsList.enc", package = "zstats1")
  result <-.Call("call_bytecode_func", as.character("med_cal"), args, enc_path)
  return(result)
}

is_numeric<- function(str){
  args <- list(x=str)

  # 使用加密文件路径
  enc_path <- system.file("bytecode", "utilsList.enc", package = "zstats1")
  result <-.Call("call_bytecode_func", as.character("is_numeric"), args, enc_path)
  return(result)
}
