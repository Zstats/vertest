#' @keywords internal
#' @importFrom utils readline
#' @importFrom utils packageVersion
#' @importFrom utils packageStartupMessage

.onLoad <- function(libname, pkgname) {
  forbidden <- c("trace", "debug", "browser", "edit", "get", "recover")
  for (f in forbidden) {
    if (exists(f, envir = .GlobalEnv, inherits = FALSE)) {
      stop(sprintf("检测到调试函数 %s，操作被禁止！", f))
    }
  }

  packageStartupMessage('正在加载包的依赖项...')

  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("需要安装 'utils' 包")
  }
  checkManage()
  #suppressWarnings({
  #  untrace("med_cal", where = asNamespace(pkgname))
  #})
  #f <- get("med_cal", envir =  asNamespace(pkgname))
  #lockEnvironment(environment(f), bindings = TRUE)

  packageStartupMessage("授权成功！请继续访问")
}

#' @keywords internal
.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)
  cat("version")
  packageStartupMessage(sprintf("欢迎zhp使用 %s (版本 %s)", pkgname, version))
  validate_environment()
}

checkManage <- function(){
    # 检查是否存在授权文件
    verify <- VerifyManage$new()
    license_key <- verify$read_license()
    unHave_license <- is.null(license_key)
    if (unHave_license) {
      # packageStartupMessage("未找到授权文件，请根据机器码向管理员获取授权")
      # 提示用户输入
      machine_id <- verify$get_machine_id()
      packageStartupMessage(sprintf("您的机器码是：%s", machine_id))
      license_key <- readline(prompt = "请输入您的授权码后按回车：")
    }
    # 验证授权
    chekcResult <- verify$verify_license(license_key)
    print(chekcResult)
    if (!chekcResult$success) {
      stop("未授权，禁止使用本包。请联系管理员获取授权，并替换授权文件。")
    }else{
      #授权成功再进行license存储
      if(unHave_license){
        verify$save_license(license_key)
      }
    }
}

#' 验证运行环境
#' @keywords internal
validate_environment <- function() {
  # 示例：检查R版本
  if (getRversion() < "4.0.0") {
    warning("建议使用 R 4.0.0 或更高版本")
  }

  # 示例：检查必要的系统环境变量
  required_env_vars <- c("R_HOME")
  missing_vars <- required_env_vars[!required_env_vars %in% names(Sys.getenv())]

  if (length(missing_vars) > 0) {
    warning(sprintf("缺少必要的环境变量: %s", paste(missing_vars, collapse = ", ")))
  }

  # 这里可以添加更多自定义的验证逻辑
  TRUE
}
