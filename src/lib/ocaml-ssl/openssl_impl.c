#define KEYSPATH "../keys/"

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <stdio.h>
#include <string.h>

#include <openssl/sha.h>
#include <openssl/hmac.h>
#include <openssl/aes.h>
#include <openssl/rsa.h>
#include <openssl/hmac.h>
#include <openssl/evp.h>
#include <openssl/bio.h>
#include <openssl/buffer.h>

#define UString_val(v) ((unsigned char *) (v))

CAMLprim base64(value ml_input, value ml_output)
{
  CAMLparam2(ml_input,ml_output);
  unsigned char *input, *output, *outbuf;
  int outlen;

  input=UString_val(ml_input);
  output=UString_val(ml_output);

  BIO *bmem, *b64, *bio;

  b64 = BIO_new(BIO_f_base64());
  bmem = BIO_new(BIO_s_mem());
  BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
  bio = BIO_push(b64, bmem);
  if (BIO_write(bio, input, strlen(input))!=strlen(input)) 
    { fprintf (stderr, "error b64enc\n"); }
  BIO_flush(bio);
  outlen = BIO_get_mem_data(bio, &outbuf);
  output = malloc(outlen+1);
  memcpy(output, outbuf, outlen);
  (output)[outlen] = '\0';
  BIO_free_all(bio);
  CAMLreturn(Val_unit);
}

CAMLprim ibase64(value ml_input, value ml_output)
{
  CAMLparam2(ml_input,ml_output);
  unsigned char *input, *output;
  long inlen, outlen;
  BIO *mbio, *b64bio, *bio;

  input=UString_val(ml_input);
  output=UString_val(ml_output);

  mbio = BIO_new_mem_buf((void *)input, -1);
  b64bio = BIO_new(BIO_f_base64());
  BIO_set_flags(b64bio, BIO_FLAGS_BASE64_NO_NL);
  bio = BIO_push(b64bio, mbio);

  inlen = strlen(input);
  outlen = inlen*2;

  output = malloc(outlen+1);

  if (BIO_read(bio, output, inlen*2) <= 0) {
    fprintf(stderr,"error in BIO_read when base64 encoding");
  }
  BIO_free_all(bio);
  
  return 0;
  CAMLreturn(Val_unit);
}

CAMLprim sha1(value ml_input, value ml_output)
{
  CAMLparam2(ml_input,ml_output);
  unsigned char *input, *output;
  input=UString_val(ml_input);
  output=UString_val(ml_output);
  SHA1(input, strlen(input), output);
  CAMLreturn(Val_unit);
}

CAMLprim rsa_sha1(value ml_key, value ml_input, value ml_output)
{
  CAMLparam3(ml_key,ml_input,ml_output);
  unsigned char *input, *key, *output;
  char keyfile[80];
  strcpy(keyfile, KEYSPATH);
  strcat(keyfile,UString_val(ml_key));
  input=UString_val(ml_input);
  output=String_val(ml_output);
  
  FILE *fp;

  fp = fopen(keyfile, "r");
  RSA *rsa = RSA_new();
  PEM_read_RSAPrivateKey(fp, &rsa, NULL, NULL);
  unsigned int siglen = RSA_size(rsa);
  
  RSA_sign(NID_sha1, input, strlen(input), output, &siglen, rsa);

  RSA_free(rsa);
  fclose(fp);
  CAMLreturn(Val_unit);
  
}

CAMLprim rsa_verify(value ml_key, value ml_input, value ml_sig)
{
  CAMLparam3(ml_key,ml_input,ml_sig);
  unsigned char *input, *key, *sig;
  char keyfile[80];
  strcpy(keyfile, KEYSPATH);
  strcat(keyfile,UString_val(ml_key));
  input=UString_val(ml_input);
  sig=UString_val(ml_sig);
 
  RSA *rsa = RSA_new();
  FILE *fp;

  fp = fopen(keyfile, "r");
  PEM_read_RSA_PUBKEY(fp, &rsa, NULL, NULL); 
  fclose(fp);
  if (RSA_verify(NID_sha1, input, strlen(input), sig, 128, rsa)) { 
      RSA_free(rsa);
      CAMLreturn(Val_true); }
  else
    { RSA_free(rsa);
      CAMLreturn(Val_false); }
}

CAMLprim mkNonce(value ml_unit)
{
  CAMLparam1(ml_unit);
 unsigned char *buffer = (unsigned char *)malloc(16);
 RAND_bytes(buffer, 16);
 CAMLreturn(copy_string(buffer));
}



CAMLprim rsa_enc(value ml_key, value ml_input, value ml_output)
{
  CAMLparam3(ml_key,ml_input,ml_output);
  unsigned char *input, *key, *output;
  char keyfile[80];
  strcpy(keyfile, KEYSPATH);
  strcat(keyfile,UString_val(ml_key));
  input=UString_val(ml_input);
  output=UString_val(ml_output);
  
  RSA *rsa = RSA_new();
  FILE *fp;

  //printf ("reading %s\n",keyfile);

  fp = fopen(keyfile, "r");
  PEM_read_RSA_PUBKEY(fp, &rsa, NULL, NULL); 
  fclose(fp);

  //printf ("done with %s\n",keyfile);
  
  int d = RSA_public_encrypt(strlen(input), input, output, rsa, RSA_PKCS1_PADDING);
  //printf("encrypted %d bytes successfully, strlen(out)=%d\n",d,strlen(output));

  RSA_free(rsa);
  //printf("freeing");
  //fclose(fp);
  //printf("freeing");
  CAMLreturn(Val_unit);  
}


CAMLprim rsa_dec(value ml_key, value ml_input, value ml_output)
{
  CAMLparam3(ml_key,ml_input,ml_output);
  unsigned char *input, *key, *output;
  char keyfile[80];
  strcpy(keyfile, KEYSPATH);
  strcat(keyfile,UString_val(ml_key));
  
  input=UString_val(ml_input);
  output=UString_val(ml_output);
  RSA *rsa = RSA_new();
  FILE *fp;
  //printf ("reading %s\n",keyfile);

  fp = fopen(keyfile, "r");
  if (PEM_read_RSAPrivateKey(fp, &rsa, NULL, NULL)==NULL)
    {
      printf("error loading private key\n");
    }

  //printf("will decr(%d):\n%s\n",strlen(input),input);
  
  int bytes = RSA_private_decrypt(128, input, output, rsa, RSA_PKCS1_PADDING);
  if (bytes==-1)
    {
      printf("error!\n");
    }
  //printf("decrypted(%d): \n%s\n",d,output);

  RSA_free(rsa);
  fclose(fp);
  CAMLreturn(Val_int(strlen(output)));
  
}

CAMLprim aes_enc(value ml_key, value ml_input, value ml_output)
{
  CAMLparam3(ml_key,ml_input,ml_output);
  unsigned char *input, *key, *output;
  unsigned char iv[] = {0,1,2,3,4,5,6,7,8};
  int outlen,tmplen;

  key=UString_val(ml_key);
  //printf("encr with key: %s\n",key);
  input=UString_val(ml_input);
  //printf("message length: %d\n",strlen(input));
  // printf("message: %s\n",input);
  output=UString_val(ml_output);

  EVP_CIPHER_CTX x;
  EVP_CIPHER_CTX_init(&x);
  if(!EVP_EncryptInit_ex(&x, EVP_aes_128_cbc(), NULL, key, iv))
    printf("\n ERROR!! \n");
  if(!EVP_EncryptUpdate(&x, output, &outlen, input, strlen(input)))
    printf("\n ERROR!! \n");
  if(!EVP_EncryptFinal_ex(&x,output+outlen,&tmplen))
    printf("\n ERROR!! \n");
   outlen += tmplen;
   EVP_CIPHER_CTX_cleanup(&x);
   // printf("length output ENC: %d\n",outlen);  
     CAMLreturn(Val_int(outlen));
  
}


CAMLprim aes_dec(value ml_key, value ml_input, value ml_output)
{
  CAMLparam3(ml_key,ml_input,ml_output);

  unsigned char *input, *key, *output;
  unsigned char iv[] = {0,1,2,3,4,5,6,7,8};
  int outlen,tmplen;
  key=UString_val(ml_key);
  //printf("decr with key: %s\n",key);
  input=UString_val(ml_input);
  output=UString_val(ml_output);
  //printf("length input: %d\n",strlen(input));
  // printf("input: %s\n",input);

  EVP_CIPHER_CTX x;
  EVP_CIPHER_CTX_init(&x);
  EVP_DecryptInit_ex(&x, EVP_aes_128_cbc(), NULL, key, iv);
  if(!EVP_DecryptUpdate(&x, output, &outlen, input, strlen(input)))
    printf("\n ERROR!! \n");
  if(!EVP_DecryptFinal_ex(&x, output + outlen, &tmplen)){
     printf("ERROR aes decryption\n");
  }
  outlen += tmplen;
  //printf("length output: %d\n",outlen);
    // printf("output: %s\n",output);
   EVP_CIPHER_CTX_cleanup(&x);
   CAMLreturn(Val_int(outlen));
  
}




CAMLprim mac(value ml_key, value ml_input, value ml_output)
{
  CAMLparam3(ml_key,ml_input,ml_output);

  unsigned char *input, *key, *output;
  int outlen;
  key=UString_val(ml_key);
  input=UString_val(ml_input);
  output=UString_val(ml_output);

  HMAC_CTX x;
  HMAC_CTX_init(&x);
  HMAC_Init_ex(&x, key, strlen(key), EVP_sha1(), NULL);
  HMAC_Update(&x, input, strlen(input));
  HMAC_Final(&x, output, &outlen);
  //printf("length output: %d\n",outlen);
    // printf("output: %s\n",output);
   HMAC_CTX_cleanup(&x);
   CAMLreturn(Val_int(outlen));
  
}


