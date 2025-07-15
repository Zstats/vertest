// src/callfunc.c
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <openssl/evp.h>
#include <openssl/aes.h>
#include <stdio.h>
#include <stdlib.h>

// 解密函数
unsigned char* aes_decrypt(const unsigned char* ciphertext, int ciphertext_len,
                          const unsigned char* key, const unsigned char* iv,
                          int* outlen) {
  EVP_CIPHER_CTX *ctx = EVP_CIPHER_CTX_new();
  if(!ctx) return NULL;
  
  // 初始化解密操作
  if(1 != EVP_DecryptInit_ex(ctx, EVP_aes_256_cbc(), NULL, key, iv)) {
    EVP_CIPHER_CTX_free(ctx);
    return NULL;
  }
  
  // 分配输出缓冲区 - 保证足够大
  unsigned char* plaintext = (unsigned char*)malloc(ciphertext_len);
  if(!plaintext) {
    EVP_CIPHER_CTX_free(ctx);
    return NULL;
  }
  
  int len = 0;
  
  // 解密
  if(1 != EVP_DecryptUpdate(ctx, plaintext, &len, ciphertext, ciphertext_len)) {
    free(plaintext);
    EVP_CIPHER_CTX_free(ctx);
    return NULL;
  }
  
  int plaintext_len = len;
  
  // 完成解密
  if(1 != EVP_DecryptFinal_ex(ctx, plaintext + len, &len)) {
    free(plaintext);
    EVP_CIPHER_CTX_free(ctx);
    return NULL;
  }
  
  plaintext_len += len;
  *outlen = plaintext_len;
  
  EVP_CIPHER_CTX_free(ctx);
  return plaintext;
}

SEXP call_bytecode_func(SEXP fname, SEXP args, SEXP enc_path) {
  SEXP expr, fexpr, env, val;
  
  // AES密钥（32字节，与R端一致）
  const unsigned char key[] = "1234567890abcdef1234567890abcdef";
  FILE *file;
  unsigned char iv[16]; // 初始化向量
  long file_size;
  unsigned char *encrypted_data, *decrypted_data;
  int decrypted_size;

  // 打开加密文件
  const char* path = CHAR(STRING_ELT(enc_path, 0));
  file = fopen(path, "rb");
  if (!file) {
    error("Cannot open encrypted file: %s", path);
    return R_NilValue;
  }
  
  // 读取IV（前16字节）
  if (fread(iv, 1, 16, file) != 16) {
    fclose(file);
    error("Failed to read IV from encrypted file");
    return R_NilValue;
  }
  
  // 获取文件大小
  fseek(file, 0, SEEK_END);
  file_size = ftell(file);
  fseek(file, 16, SEEK_SET); // 跳过IV
  
  // 分配内存并读取加密数据
  encrypted_data = (unsigned char*)malloc(file_size - 16);
  if (!encrypted_data) {
    fclose(file);
    error("Memory allocation failed");
    return R_NilValue;
  }
  
  if (fread(encrypted_data, 1, file_size - 16, file) != file_size - 16) {
    free(encrypted_data);
    fclose(file);
    error("Failed to read encrypted data");
    return R_NilValue;
  }
  fclose(file);
  
  // 解密数据
  decrypted_data = aes_decrypt(encrypted_data, file_size - 16, key, iv, &decrypted_size);
  free(encrypted_data);
  
  if (!decrypted_data) {
    error("Decryption failed");
    return R_NilValue;
  }
  
  // 创建R原始向量存放解密后的数据
  SEXP raw_vec = PROTECT(Rf_allocVector(RAWSXP, decrypted_size));
  memcpy(RAW(raw_vec), decrypted_data, decrypted_size);
  free(decrypted_data);
  
  // 反序列化
  SEXP unserialize_call = PROTECT(Rf_lang2(Rf_install("unserialize"), raw_vec));
  expr = PROTECT(Rf_eval(unserialize_call, R_GlobalEnv));
  
  // 创建临时环境用于执行
  PROTECT(env = R_NewEnv(R_GlobalEnv, 1, 29));
  
  // 执行字节码定义的所有表达式（定义函数等）
  for (int i = 0; i < Rf_length(expr); i++) {
    Rf_eval(VECTOR_ELT(expr, i), env);
  }
  
  // 从环境中获取目标函数
  PROTECT(fexpr = Rf_findVar(Rf_install(CHAR(STRING_ELT(fname, 0))), env));
  
  // 用do.call调用目标函数，args为参数list
  SEXP do_call = PROTECT(Rf_lang3(Rf_install("do.call"), fexpr, args));
  val = Rf_eval(do_call, env);
  
  UNPROTECT(6);
  return val;
}
