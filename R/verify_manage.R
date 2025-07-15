#' @import Rcpp
#' @import openssl
#' @import httr2
#' @import jsonlite
#' @import uuid
#' @useDynLib protectRule

#' @export
# VerifyManage R6类定义
VerifyManage <- R6::R6Class( "VerifyManage",
  public = list(
    dll_name = "protectRule",

    # 初始化
    initialize = function(dll_path = "src/protectRule.dll") {

      private$license_file <- file.path(getwd(), "config", "license")

      tryCatch({
        if (!is.null(dll_path) && file.exists(dll_path)) {
          dll_info <- dyn.load(dll_path)
          self$dll_name <- dll_info[["name"]]
        } else {
          self$dll_name <- "protectRule"
        }
      }, error = function(e) {
        warning("无法加载: ", e$message, "\n", "某些功能可能不可用。")
      })
    },

    # 读取文件
    read_license = function() {
      tryCatch({
        if (!file.exists(private$license_file)) {
          #cat("授权文件不存在:", private$license_file, "\n")
          return(NULL)
        }

        # 直接读取文本授权码，而不是二进制数据
        license_key <- readLines(private$license_file, warn = FALSE)[1]
        #cat("读取授权文件:", license_key, "\n")
        return(license_key)
      }, error = function(e) {
        private$log_error(paste("读取授权文件失败:", e$message))
        return(NULL)
      })
    },

    # 保存授权码
    save_license = function(license_key) {
      dir.create(dirname(private$license_file), recursive = TRUE, showWarnings = FALSE)
      # 直接写入文本授权码，而不是加密后的二进制数据
      writeLines(license_key, private$license_file)
      cat("已保存授权码到文件:", private$license_file, "\n")
    },

    # 获取机器码
    get_machine_id = function() {
      tryCatch({
        if (!is.null(self$dll_name)) {
          machine_id <- base::.Call("r_generate_machine_id", NULL, PACKAGE = self$dll_name)
          return(machine_id)
        } else {
          # 使用模拟函数
          #machine_id <- private$mock_generate_machine_id()
          # cat('使用模拟函数生成机器码:', machine_id, '\n')
          # 调用不成功，返回机器码失败，不进行返回
          return(NULL)
        }
      }, error = function(e) {
        cat('获取机器码失败:', e$message, '\n')
        # machine_id <- private$mock_generate_machine_id()
        # cat('使用模拟函数生成机器码:', machine_id, '\n')
        return(NULL)
      })
    },

    # 加密数据
    encrypt_data = function(data) {
      raw_data <- charToRaw(data)
      # cat("加密数据",openssl::base64_encode(data),"\n")

      # 调用动态库中的加密函数（使用内置密钥和IV）
      result <- base::.Call("r_encrypt_data", raw_data, PACKAGE = "protectRule")
      base64Result <- openssl::base64_encode(result)
      # cat("加密结果",base64Result)
      return(list( data = base64Result))
    },

    # 解密数据
    decrypt_data = function(data) {
      # 将base64数据转换回raw格式
      raw_data <- openssl::base64_decode(data)
      # cat("解密数据", data, "\n")

      # 调用动态库中的解密函数（使用内置密钥和IV）
      result <- base::.Call("r_decrypt_data", raw_data, PACKAGE = "protectRule")

      # 将解密结果转换为字符串
      decrypted_text <- rawToChar(result)
      # cat("解密结果", decrypted_text, "\n")

      return(decrypted_text)
    },

    # 验证许可证
    verify_license = function(license_key = NULL) {
      tryCatch({
        if (is.null(license_key)) {
          # 尝试从文件读取授权码
          stored_license <- self$read_license()
          if (is.null(stored_license)) {
            # 这里如果没有授权文件，可以先提示机器码，并生成对应的机器码文件
            machine_id <- self$get_machine_id()
            cat("未找到授权文件。您的机器码是:", machine_id, "\n")

            # 确保config目录存在
            config_dir <- file.path(getwd(), "config")
            if (!dir.exists(config_dir)) {
              dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
            }

            # 生成机器码文件
            machine_id_file <- file.path(config_dir, "machine_id.txt")
            writeLines(machine_id, machine_id_file)
            cat("已将机器码保存到文件:", machine_id_file, "\n")
            cat("请将此机器码提供给管理员以获取授权\n")

            private$log_error("未找到授权文件，请根据机器码向管理员获取授权")
            return(list(success = FALSE, machine_id = machine_id, message = "需要授权"))
          }
          license_key <- stored_license
        }

        # 组合机器码和授权码
        machine_id <- self$get_machine_id()
        combined <- paste(machine_id, license_key, sep = ":")

        # 加密数据
        encrypted <- self$encrypt_data(combined)
        # cat("加密数据",encrypted$data,"\n")

        # 发送到服务器验证 - 使用tryCatch专门处理HTTP请求错误
        tryCatch({
          # 获取操作系统信息
          os_info <- paste(Sys.info()["sysname"], Sys.info()["release"], Sys.info()["machine"])
          # 当前时间戳
          timestamp <- as.character(round(as.numeric(Sys.time()) * 1000))
          # 客户端版本,这里自动获取包版本
          client_version <- "1.0.0"

          # 生成签名（简单示例，实际应用中可能需要更复杂的签名算法）
          signature_data <- paste(machine_id, license_key, client_version, timestamp, sep = "|")
          signature <- openssl::sha256(charToRaw(signature_data))
          signature_hex <- openssl::base64_encode(signature)

          # 创建符合Java Bean的请求体
          request_body <- list(
            machineId = machine_id,
            licenseKey = license_key,
            clientVersion = client_version,
            osInfo = os_info,
            timestamp = timestamp,
            signature = signature_hex
          )

          # 打印请求体（仅用于调试）
          # cat("发送请求体：\n")
          # print(request_body)

          # 将请求体转换为JSON字符串
          json_body <- jsonlite::toJSON(request_body, auto_unbox = TRUE)
          # cat("JSON格式：\n", json_body, "\n")
          # 调用加密函数
          encrypted <- self$encrypt_data(json_body)
          # cat("加密后的请求体：\n", encrypted$data, "\n")

          # 创建请求并发送
          req <- httr2::request(private$verify_url) |>
            httr2::req_headers("Content-Type" = "text/plain") |>
            httr2::req_body_raw(encrypted$data, "text/plain") |>
            httr2::req_retry(max_tries = 3) |> # 重试3次
            httr2::req_error(is_error = function(resp) FALSE) # 禁用自动错误，手动处理

          response <- httr2::req_perform(req)

          # 检查HTTP状态码
          status_code <- httr2::resp_status(response)
          # cat("HTTP响应状态码:", status_code, "\n")

          if (status_code == 200) {
            content <- httr2::resp_body_json(response)
            # print(content)
            if (!is.null(content$success) && content$success) {
              if (!is.null(license_key)) {
                self$save_license(license_key)
              }
              return(list(success = TRUE, machine_id = machine_id, message = "授权验证成功"))
            } else {
              return(list(success = FALSE, machine_id = machine_id,
                         message = paste0("授权验证失败：", ifelse(is.null(content$message), "未知错误", content$message))))
            }
          } else {
            # 处理非200状态码
            return(list(success = FALSE, machine_id = machine_id,
                       message = paste0("HTTP错误: ", status_code),
                       http_status = status_code))
          }
        }, error = function(e) {
          # 专门处理HTTP请求错误
          error_msg <- paste("HTTP请求失败:", e$message)
          # cat(error_msg, "\n")
          private$log_error(error_msg)
          return(list(success = FALSE, machine_id = machine_id,
                     message = error_msg,
                     error = e))
        })
      }, error = function(e) {
        # 处理其他一般错误
        error_msg <- paste("授权验证过程出错:", e$message)
        private$log_error(error_msg)
        return(list(success = FALSE, message = error_msg))
      })
    }
  ),

  private = list(
    verify_url = "https://www.medsta.cn/apis/rpackage.halo.run/v1alpha1/rpackagelicenses/verify",
    license_file = NULL,

    mock_generate_machine_id = function() {
      # 生成模拟的机器码
      paste0("TEST-MACHINE-", paste0(sample(c(0:9, LETTERS[1:6]), 16, replace = TRUE), collapse = ""))
    },

    log_error = function(message) {
      # 创建日志目录在当前项目中
      log_dir <- file.path(getwd(), "logs")
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
      }
      log_file <- file.path(log_dir, "license.log")
      cat(format(Sys.time()), message, "\n", file = log_file, append = TRUE)
    }
  )
)
