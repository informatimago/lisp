#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include <dlfcn.h>
#include <sysexits.h>
#include <pkcs11.h>



const char * TAG = "test-find-objects";
CK_FUNCTION_LIST_PTR p11;

/* ==================== */

void error(const char * format, ...)
{
    va_list args;
    fprintf(stderr, "[%s] ", TAG);
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    exit(EX_SOFTWARE);
}

void *  check_memory(void * memory)
{
    if (memory)
    {
        return memory;
    }
    fprintf(stderr, "[%s] out of memory\n", TAG);
    exit(EX_OSERR);
}

typedef enum
{
    tag_null,
    tag_unsigned_long,
    tag_signed_long,
    tag_string,
    tag_cons,
}typetag;

struct string
{
    unsigned long size;
    char *  value;
};

struct cons
{
    struct object *  car;
    struct object *  cdr;
};

typedef struct object
{
    union
    {
        unsigned long u;
        long l;
        struct string s;
        struct cons k;
    } value;
    typetag tag;
} * object;

typetag type_of(object k)
{
    return k?k->tag:tag_null;
}

const char *  label_type_of(object k)
{
    switch(type_of(k))
    {
      case tag_null:          return "null";
      case tag_unsigned_long: return "unsigned";
      case tag_signed_long:   return "long";
      case tag_string:        return "string";
      case tag_cons:          return "cons";
      default:                return "unknown";
    }
}

object ul(unsigned long v)
{
    object result = check_memory(malloc(sizeof(*result)));
    result->tag = tag_unsigned_long;
    result->value.u = v;
    return result;
}

unsigned long ulvalue(object ul)
{
    if (!ul)
    {
        error("type error: expected a unsigned long, got a NIL");
    }
    if (ul->tag != tag_unsigned_long)
    {
        error("type error: expected a unsigned long, got a %s", label_type_of(ul));
    }
    return ul->value.u;
}

object sl(signed long v)
{
    object result = check_memory(malloc(sizeof(*result)));
    result->tag = tag_signed_long;
    result->value.l = v;
    return result;
}

object string(const char * string)
{
    object result = check_memory(malloc(sizeof(*result)));
    result->tag = tag_string;
    result->value.s.value = check_memory(strdup(string));
    result->value.s.size  = strlen(string);
    return result;
}

object cons(object car, object cdr)
{
    object result = check_memory(malloc(sizeof(*result)));
    result->tag = tag_cons;
    result->value.k.car = car;
    result->value.k.cdr = cdr;
    return result;
}

object car(object k)
{
    if (!k) return k;
    if (k->tag != tag_cons)
    {
        error("in car type error: expected a cons, got a %s", label_type_of(k));
    }
    return k->value.k.car;
}

object cdr(object k)
{
    if (!k) return k;
    if (k->tag != tag_cons)
    {
        error("in cdr type error: expected a cons, got a %s", label_type_of(k));
    }
    return k->value.k.cdr;
}

bool null(object r)
{
    return r?false:true;
}

/* ==================== */

typedef const char * (*label_pr)(CK_ULONG);

const char * certificate_type_label(CK_ULONG value)
{
    switch (value)
    {

      case CKC_X_509:            return "X_509";
      case CKC_X_509_ATTR_CERT:  return "X_509_ATTR_CERT";
      case CKC_WTLS:             return "WTLS";
      case CKC_VENDOR_DEFINED:   return "VENDOR_DEFINED";

      default:
          {
              static char buffer[80];
              snprintf(buffer, sizeof (buffer), "Unknown_Certificate_Type_%lu", value);
              return buffer;
          }
    }
}

const char * hardware_feature_label(CK_ULONG value)
{
    switch (value)
    {

      case CKH_MONOTONIC_COUNTER: return "MONOTONIC_COUNTER";
      case CKH_CLOCK:             return "CLOCK";
      case CKH_USER_INTERFACE:    return "USER_INTERFACE";
      case CKH_VENDOR_DEFINED:    return "VENDOR_DEFINED";

      case CKA_PIXEL_X:          return "PIXEL_X";
      case CKA_PIXEL_Y:          return "PIXEL_Y";
      case CKA_RESOLUTION:       return "RESOLUTION";
      case CKA_CHAR_ROWS:        return "CHAR_ROWS";
      case CKA_CHAR_COLUMNS:     return "CHAR_COLUMNS";
      case CKA_COLOR:            return "COLOR";
      case CKA_BITS_PER_PIXEL:   return "BITS_PER_PIXEL";
      case CKA_CHAR_SETS:        return "CHAR_SETS";
      case CKA_ENCODING_METHODS: return "ENCODING_METHODS";
      case CKA_MIME_TYPES:       return "MIME_TYPES";

      default:
          {
              static char buffer[80];
              snprintf(buffer, sizeof (buffer), "Unknown_Hardware_Feature_%lu", value);
              return buffer;
          }
    }
}

const char * key_type_label(CK_ULONG value)
{
    switch (value)
    {

      case CKK_RSA:            return "RSA";
      case CKK_DSA:            return "DSA";
      case CKK_DH:             return "DH";
      case CKK_ECDSA:          return "ECDSA";
      case CKK_X9_42_DH:       return "X9_42_DH";
      case CKK_KEA:            return "KEA";
      case CKK_GENERIC_SECRET: return "GENERIC_SECRET";
      case CKK_RC2:            return "RC2";
      case CKK_RC4:            return "RC4";
      case CKK_DES:            return "DES";
      case CKK_DES2:           return "DES2";
      case CKK_DES3:           return "DES3";
      case CKK_CAST:           return "CAST";
      case CKK_CAST3:          return "CAST3";
      case CKK_CAST128:        return "CAST128";
      case CKK_RC5:            return "RC5";
      case CKK_IDEA:           return "IDEA";
      case CKK_SKIPJACK:       return "SKIPJACK";
      case CKK_BATON:          return "BATON";
      case CKK_JUNIPER:        return "JUNIPER";
      case CKK_CDMF:           return "CDMF";
      case CKK_AES:            return "AES";
      case CKK_BLOWFISH:       return "BLOWFISH";
      case CKK_TWOFISH:        return "TWOFISH";
      case CKK_VENDOR_DEFINED: return "VENDOR_DEFINED";

      default:
          {
              static char buffer[80];
              snprintf(buffer, sizeof (buffer), "Unknown_Key_Type_%lu", value);
              return buffer;
          }
    }
}

const char * mechanism_type_label(CK_ULONG value)
{
    switch (value)
    {

      case CKM_RSA_PKCS_KEY_PAIR_GEN:     return "RSA_PKCS_KEY_PAIR_GEN";
      case CKM_RSA_PKCS:                  return "RSA_PKCS";
      case CKM_RSA_9796:                  return "RSA_9796";
      case CKM_RSA_X_509:                 return "RSA_X_509";
      case CKM_MD2_RSA_PKCS:              return "MD2_RSA_PKCS";
      case CKM_MD5_RSA_PKCS:              return "MD5_RSA_PKCS";
      case CKM_SHA1_RSA_PKCS:             return "SHA1_RSA_PKCS";
      case CKM_RIPEMD128_RSA_PKCS:        return "RIPEMD128_RSA_PKCS";
      case CKM_RIPEMD160_RSA_PKCS:        return "RIPEMD160_RSA_PKCS";
      case CKM_RSA_PKCS_OAEP:             return "RSA_PKCS_OAEP";
      case CKM_RSA_X9_31_KEY_PAIR_GEN:    return "RSA_X9_31_KEY_PAIR_GEN";
      case CKM_RSA_X9_31:                 return "RSA_X9_31";
      case CKM_SHA1_RSA_X9_31:            return "SHA1_RSA_X9_31";
      case CKM_RSA_PKCS_PSS:              return "RSA_PKCS_PSS";
      case CKM_SHA1_RSA_PKCS_PSS:         return "SHA1_RSA_PKCS_PSS";
      case CKM_DSA_KEY_PAIR_GEN:          return "DSA_KEY_PAIR_GEN";
      case CKM_DSA:                       return "DSA";
      case CKM_DSA_SHA1:                  return "DSA_SHA1";
      case CKM_DH_PKCS_KEY_PAIR_GEN:      return "DH_PKCS_KEY_PAIR_GEN";
      case CKM_DH_PKCS_DERIVE:            return "DH_PKCS_DERIVE";
      case CKM_X9_42_DH_KEY_PAIR_GEN:     return "X9_42_DH_KEY_PAIR_GEN";
      case CKM_X9_42_DH_DERIVE:           return "X9_42_DH_DERIVE";
      case CKM_X9_42_DH_HYBRID_DERIVE:    return "X9_42_DH_HYBRID_DERIVE";
      case CKM_X9_42_MQV_DERIVE:          return "X9_42_MQV_DERIVE";
      case CKM_SHA256_RSA_PKCS:           return "SHA256_RSA_PKCS";
      case CKM_SHA384_RSA_PKCS:           return "SHA384_RSA_PKCS";
      case CKM_SHA512_RSA_PKCS:           return "SHA512_RSA_PKCS";
      case CKM_SHA256_RSA_PKCS_PSS:       return "SHA256_RSA_PKCS_PSS";
      case CKM_SHA384_RSA_PKCS_PSS:       return "SHA384_RSA_PKCS_PSS";
      case CKM_SHA512_RSA_PKCS_PSS:       return "SHA512_RSA_PKCS_PSS";
      case CKM_RC2_KEY_GEN:               return "RC2_KEY_GEN";
      case CKM_RC2_ECB:                   return "RC2_ECB";
      case CKM_RC2_CBC:                   return "RC2_CBC";
      case CKM_RC2_MAC:                   return "RC2_MAC";
      case CKM_RC2_MAC_GENERAL:           return "RC2_MAC_GENERAL";
      case CKM_RC2_CBC_PAD:               return "RC2_CBC_PAD";
      case CKM_RC4_KEY_GEN:               return "RC4_KEY_GEN";
      case CKM_RC4:                       return "RC4";
      case CKM_DES_KEY_GEN:               return "DES_KEY_GEN";
      case CKM_DES_ECB:                   return "DES_ECB";
      case CKM_DES_CBC:                   return "DES_CBC";
      case CKM_DES_MAC:                   return "DES_MAC";
      case CKM_DES_MAC_GENERAL:           return "DES_MAC_GENERAL";
      case CKM_DES_CBC_PAD:               return "DES_CBC_PAD";
      case CKM_DES2_KEY_GEN:              return "DES2_KEY_GEN";
      case CKM_DES3_KEY_GEN:              return "DES3_KEY_GEN";
      case CKM_DES3_ECB:                  return "DES3_ECB";
      case CKM_DES3_CBC:                  return "DES3_CBC";
      case CKM_DES3_MAC:                  return "DES3_MAC";
      case CKM_DES3_MAC_GENERAL:          return "DES3_MAC_GENERAL";
      case CKM_DES3_CBC_PAD:              return "DES3_CBC_PAD";
      case CKM_CDMF_KEY_GEN:              return "CDMF_KEY_GEN";
      case CKM_CDMF_ECB:                  return "CDMF_ECB";
      case CKM_CDMF_CBC:                  return "CDMF_CBC";
      case CKM_CDMF_MAC:                  return "CDMF_MAC";
      case CKM_CDMF_MAC_GENERAL:          return "CDMF_MAC_GENERAL";
      case CKM_CDMF_CBC_PAD:              return "CDMF_CBC_PAD";
      case CKM_MD2:                       return "MD2";
      case CKM_MD2_HMAC:                  return "MD2_HMAC";
      case CKM_MD2_HMAC_GENERAL:          return "MD2_HMAC_GENERAL";
      case CKM_MD5:                       return "MD5";
      case CKM_MD5_HMAC:                  return "MD5_HMAC";
      case CKM_MD5_HMAC_GENERAL:          return "MD5_HMAC_GENERAL";
      case CKM_SHA_1:                     return "SHA_1";
      case CKM_SHA_1_HMAC:                return "SHA_1_HMAC";
      case CKM_SHA_1_HMAC_GENERAL:        return "SHA_1_HMAC_GENERAL";
      case CKM_RIPEMD128:                 return "RIPEMD128";
      case CKM_RIPEMD128_HMAC:            return "RIPEMD128_HMAC";
      case CKM_RIPEMD128_HMAC_GENERAL:    return "RIPEMD128_HMAC_GENERAL";
      case CKM_RIPEMD160:                 return "RIPEMD160";
      case CKM_RIPEMD160_HMAC:            return "RIPEMD160_HMAC";
      case CKM_RIPEMD160_HMAC_GENERAL:    return "RIPEMD160_HMAC_GENERAL";
      case CKM_SHA256:                    return "SHA256";
      case CKM_SHA256_HMAC:               return "SHA256_HMAC";
      case CKM_SHA256_HMAC_GENERAL:       return "SHA256_HMAC_GENERAL";
      case CKM_SHA384:                    return "SHA384";
      case CKM_SHA384_HMAC:               return "SHA384_HMAC";
      case CKM_SHA384_HMAC_GENERAL:       return "SHA384_HMAC_GENERAL";
      case CKM_SHA512:                    return "SHA512";
      case CKM_SHA512_HMAC:               return "SHA512_HMAC";
      case CKM_SHA512_HMAC_GENERAL:       return "SHA512_HMAC_GENERAL";
      case CKM_CAST_KEY_GEN:              return "CAST_KEY_GEN";
      case CKM_CAST_ECB:                  return "CAST_ECB";
      case CKM_CAST_CBC:                  return "CAST_CBC";
      case CKM_CAST_MAC:                  return "CAST_MAC";
      case CKM_CAST_MAC_GENERAL:          return "CAST_MAC_GENERAL";
      case CKM_CAST_CBC_PAD:              return "CAST_CBC_PAD";
      case CKM_CAST3_KEY_GEN:             return "CAST3_KEY_GEN";
      case CKM_CAST3_ECB:                 return "CAST3_ECB";
      case CKM_CAST3_CBC:                 return "CAST3_CBC";
      case CKM_CAST3_MAC:                 return "CAST3_MAC";
      case CKM_CAST3_MAC_GENERAL:         return "CAST3_MAC_GENERAL";
      case CKM_CAST3_CBC_PAD:             return "CAST3_CBC_PAD";
      case CKM_CAST5_KEY_GEN:             return "CAST5_KEY_GEN";
      case CKM_CAST5_ECB:                 return "CAST5_ECB";
      case CKM_CAST5_CBC:                 return "CAST5_CBC";
      case CKM_CAST5_MAC:                 return "CAST5_MAC";
      case CKM_CAST5_MAC_GENERAL:         return "CAST5_MAC_GENERAL";
      case CKM_CAST5_CBC_PAD:             return "CAST5_CBC_PAD";
      case CKM_RC5_KEY_GEN:               return "RC5_KEY_GEN";
      case CKM_RC5_ECB:                   return "RC5_ECB";
      case CKM_RC5_CBC:                   return "RC5_CBC";
      case CKM_RC5_MAC:                   return "RC5_MAC";
      case CKM_RC5_MAC_GENERAL:           return "RC5_MAC_GENERAL";
      case CKM_RC5_CBC_PAD:               return "RC5_CBC_PAD";
      case CKM_IDEA_KEY_GEN:              return "IDEA_KEY_GEN";
      case CKM_IDEA_ECB:                  return "IDEA_ECB";
      case CKM_IDEA_CBC:                  return "IDEA_CBC";
      case CKM_IDEA_MAC:                  return "IDEA_MAC";
      case CKM_IDEA_MAC_GENERAL:          return "IDEA_MAC_GENERAL";
      case CKM_IDEA_CBC_PAD:              return "IDEA_CBC_PAD";
      case CKM_GENERIC_SECRET_KEY_GEN:    return "GENERIC_SECRET_KEY_GEN";
      case CKM_CONCATENATE_BASE_AND_KEY:  return "CONCATENATE_BASE_AND_KEY";
      case CKM_CONCATENATE_BASE_AND_DATA: return "CONCATENATE_BASE_AND_DATA";
      case CKM_CONCATENATE_DATA_AND_BASE: return "CONCATENATE_DATA_AND_BASE";
      case CKM_XOR_BASE_AND_DATA:         return "XOR_BASE_AND_DATA";
      case CKM_EXTRACT_KEY_FROM_KEY:      return "EXTRACT_KEY_FROM_KEY";
      case CKM_SSL3_PRE_MASTER_KEY_GEN:   return "SSL3_PRE_MASTER_KEY_GEN";
      case CKM_SSL3_MASTER_KEY_DERIVE:    return "SSL3_MASTER_KEY_DERIVE";
      case CKM_SSL3_KEY_AND_MAC_DERIVE:   return "SSL3_KEY_AND_MAC_DERIVE";
      case CKM_SSL3_MASTER_KEY_DERIVE_DH: return "SSL3_MASTER_KEY_DERIVE_DH";
      case CKM_TLS_PRE_MASTER_KEY_GEN:    return "TLS_PRE_MASTER_KEY_GEN";
      case CKM_TLS_MASTER_KEY_DERIVE:     return "TLS_MASTER_KEY_DERIVE";
      case CKM_TLS_KEY_AND_MAC_DERIVE:    return "TLS_KEY_AND_MAC_DERIVE";
      case CKM_TLS_MASTER_KEY_DERIVE_DH:  return "TLS_MASTER_KEY_DERIVE_DH";
      case CKM_SSL3_MD5_MAC:              return "SSL3_MD5_MAC";
      case CKM_SSL3_SHA1_MAC:             return "SSL3_SHA1_MAC";
      case CKM_MD5_KEY_DERIVATION:        return "MD5_KEY_DERIVATION";
      case CKM_MD2_KEY_DERIVATION:        return "MD2_KEY_DERIVATION";
      case CKM_SHA1_KEY_DERIVATION:       return "SHA1_KEY_DERIVATION";
      case CKM_PBE_MD2_DES_CBC:           return "PBE_MD2_DES_CBC";
      case CKM_PBE_MD5_DES_CBC:           return "PBE_MD5_DES_CBC";
      case CKM_PBE_MD5_CAST_CBC:          return "PBE_MD5_CAST_CBC";
      case CKM_PBE_MD5_CAST3_CBC:         return "PBE_MD5_CAST3_CBC";
      case CKM_PBE_MD5_CAST5_CBC:         return "PBE_MD5_CAST5_CBC";
      case CKM_PBE_SHA1_CAST5_CBC:        return "PBE_SHA1_CAST5_CBC";
      case CKM_PBE_SHA1_RC4_128:          return "PBE_SHA1_RC4_128";
      case CKM_PBE_SHA1_RC4_40:           return "PBE_SHA1_RC4_40";
      case CKM_PBE_SHA1_DES3_EDE_CBC:     return "PBE_SHA1_DES3_EDE_CBC";
      case CKM_PBE_SHA1_DES2_EDE_CBC:     return "PBE_SHA1_DES2_EDE_CBC";
      case CKM_PBE_SHA1_RC2_128_CBC:      return "PBE_SHA1_RC2_128_CBC";
      case CKM_PBE_SHA1_RC2_40_CBC:       return "PBE_SHA1_RC2_40_CBC";
      case CKM_PKCS5_PBKD2:               return "PKCS5_PBKD2";
      case CKM_PBA_SHA1_WITH_SHA1_HMAC:   return "PBA_SHA1_WITH_SHA1_HMAC";
      case CKM_KEY_WRAP_LYNKS:            return "KEY_WRAP_LYNKS";
      case CKM_KEY_WRAP_SET_OAEP:         return "KEY_WRAP_SET_OAEP";
      case CKM_SKIPJACK_KEY_GEN:          return "SKIPJACK_KEY_GEN";
      case CKM_SKIPJACK_ECB64:            return "SKIPJACK_ECB64";
      case CKM_SKIPJACK_CBC64:            return "SKIPJACK_CBC64";
      case CKM_SKIPJACK_OFB64:            return "SKIPJACK_OFB64";
      case CKM_SKIPJACK_CFB64:            return "SKIPJACK_CFB64";
      case CKM_SKIPJACK_CFB32:            return "SKIPJACK_CFB32";
      case CKM_SKIPJACK_CFB16:            return "SKIPJACK_CFB16";
      case CKM_SKIPJACK_CFB8:             return "SKIPJACK_CFB8";
      case CKM_SKIPJACK_WRAP:             return "SKIPJACK_WRAP";
      case CKM_SKIPJACK_PRIVATE_WRAP:     return "SKIPJACK_PRIVATE_WRAP";
      case CKM_SKIPJACK_RELAYX:           return "SKIPJACK_RELAYX";
      case CKM_KEA_KEY_PAIR_GEN:          return "KEA_KEY_PAIR_GEN";
      case CKM_KEA_KEY_DERIVE:            return "KEA_KEY_DERIVE";
      case CKM_FORTEZZA_TIMESTAMP:        return "FORTEZZA_TIMESTAMP";
      case CKM_BATON_KEY_GEN:             return "BATON_KEY_GEN";
      case CKM_BATON_ECB128:              return "BATON_ECB128";
      case CKM_BATON_ECB96:               return "BATON_ECB96";
      case CKM_BATON_CBC128:              return "BATON_CBC128";
      case CKM_BATON_COUNTER:             return "BATON_COUNTER";
      case CKM_BATON_SHUFFLE:             return "BATON_SHUFFLE";
      case CKM_BATON_WRAP:                return "BATON_WRAP";
      case CKM_ECDSA_KEY_PAIR_GEN:        return "ECDSA_KEY_PAIR_GEN";
      case CKM_ECDSA:                     return "ECDSA";
      case CKM_ECDSA_SHA1:                return "ECDSA_SHA1";
      case CKM_ECDH1_DERIVE:              return "ECDH1_DERIVE";
      case CKM_ECDH1_COFACTOR_DERIVE:     return "ECDH1_COFACTOR_DERIVE";
      case CKM_ECMQV_DERIVE:              return "ECMQV_DERIVE";
      case CKM_JUNIPER_KEY_GEN:           return "JUNIPER_KEY_GEN";
      case CKM_JUNIPER_ECB128:            return "JUNIPER_ECB128";
      case CKM_JUNIPER_CBC128:            return "JUNIPER_CBC128";
      case CKM_JUNIPER_COUNTER:           return "JUNIPER_COUNTER";
      case CKM_JUNIPER_SHUFFLE:           return "JUNIPER_SHUFFLE";
      case CKM_JUNIPER_WRAP:              return "JUNIPER_WRAP";
      case CKM_FASTHASH:                  return "FASTHASH";
      case CKM_AES_KEY_GEN:               return "AES_KEY_GEN";
      case CKM_AES_ECB:                   return "AES_ECB";
      case CKM_AES_CBC:                   return "AES_CBC";
      case CKM_AES_MAC:                   return "AES_MAC";
      case CKM_AES_MAC_GENERAL:           return "AES_MAC_GENERAL";
      case CKM_AES_CBC_PAD:               return "AES_CBC_PAD";
      case CKM_DSA_PARAMETER_GEN:         return "DSA_PARAMETER_GEN";
      case CKM_DH_PKCS_PARAMETER_GEN:     return "DH_PKCS_PARAMETER_GEN";
      case CKM_X9_42_DH_PARAMETER_GEN:    return "X9_42_DH_PARAMETER_GEN";
      case CKM_VENDOR_DEFINED:            return "VENDOR_DEFINED";

      default:
          {
              static char buffer[80];
              snprintf(buffer, sizeof (buffer), "Unknown_Mechanism_Type_%lu", value);
              return buffer;
          }
    }
}

const char * object_class_label(CK_ULONG value)
{
    switch (value)
    {

      case CKO_DATA:              return "DATA";
      case CKO_CERTIFICATE:       return "CERTIFICATE";
      case CKO_PUBLIC_KEY:        return "PUBLIC_KEY";
      case CKO_PRIVATE_KEY:       return "PRIVATE_KEY";
      case CKO_SECRET_KEY:        return "SECRET_KEY";
      case CKO_HW_FEATURE:        return "HW_FEATURE";
      case CKO_DOMAIN_PARAMETERS: return "DOMAIN_PARAMETERS";
      case CKO_MECHANISM:         return "MECHANISM";
      case CKO_VENDOR_DEFINED:    return "VENDOR_DEFINED";

      default:
          {
              static char buffer[80];
              snprintf(buffer, sizeof (buffer), "Unknown_Object_Class_%lu", value);
              return buffer;
          }
    }
}


const char * rv_label(CK_RV rv)
{
    switch (rv)
    {
      case CKR_OK                               : return "OK";
      case CKR_CANCEL                           : return "CANCEL";
      case CKR_HOST_MEMORY                      : return "HOST_MEMORY";
      case CKR_SLOT_ID_INVALID                  : return "SLOT_ID_INVALID";
      case CKR_GENERAL_ERROR                    : return "GENERAL_ERROR";
      case CKR_FUNCTION_FAILED                  : return "FUNCTION_FAILED";
      case CKR_ARGUMENTS_BAD                    : return "ARGUMENTS_BAD";
      case CKR_NO_EVENT                         : return "NO_EVENT";
      case CKR_NEED_TO_CREATE_THREADS           : return "NEED_TO_CREATE_THREADS";
      case CKR_CANT_LOCK                        : return "CANT_LOCK";
      case CKR_ATTRIBUTE_READ_ONLY              : return "ATTRIBUTE_READ_ONLY";
      case CKR_ATTRIBUTE_SENSITIVE              : return "ATTRIBUTE_SENSITIVE";
      case CKR_ATTRIBUTE_TYPE_INVALID           : return "ATTRIBUTE_TYPE_INVALID";
      case CKR_ATTRIBUTE_VALUE_INVALID          : return "ATTRIBUTE_VALUE_INVALID";
      case CKR_DATA_INVALID                     : return "DATA_INVALID";
      case CKR_DATA_LEN_RANGE                   : return "DATA_LEN_RANGE";
      case CKR_DEVICE_ERROR                     : return "DEVICE_ERROR";
      case CKR_DEVICE_MEMORY                    : return "DEVICE_MEMORY";
      case CKR_DEVICE_REMOVED                   : return "DEVICE_REMOVED";
      case CKR_ENCRYPTED_DATA_INVALID           : return "ENCRYPTED_DATA_INVALID";
      case CKR_ENCRYPTED_DATA_LEN_RANGE         : return "ENCRYPTED_DATA_LEN_RANGE";
      case CKR_FUNCTION_CANCELED                : return "FUNCTION_CANCELED";
      case CKR_FUNCTION_NOT_PARALLEL            : return "FUNCTION_NOT_PARALLEL";
      case CKR_FUNCTION_NOT_SUPPORTED           : return "FUNCTION_NOT_SUPPORTED";
      case CKR_KEY_HANDLE_INVALID               : return "KEY_HANDLE_INVALID";
      case CKR_KEY_SIZE_RANGE                   : return "KEY_SIZE_RANGE";
      case CKR_KEY_TYPE_INCONSISTENT            : return "KEY_TYPE_INCONSISTENT";
      case CKR_KEY_NOT_NEEDED                   : return "KEY_NOT_NEEDED";
      case CKR_KEY_CHANGED                      : return "KEY_CHANGED";
      case CKR_KEY_NEEDED                       : return "KEY_NEEDED";
      case CKR_KEY_INDIGESTIBLE                 : return "KEY_INDIGESTIBLE";
      case CKR_KEY_FUNCTION_NOT_PERMITTED       : return "KEY_FUNCTION_NOT_PERMITTED";
      case CKR_KEY_NOT_WRAPPABLE                : return "KEY_NOT_WRAPPABLE";
      case CKR_KEY_UNEXTRACTABLE                : return "KEY_UNEXTRACTABLE";
      case CKR_MECHANISM_INVALID                : return "MECHANISM_INVALID";
      case CKR_MECHANISM_PARAM_INVALID          : return "MECHANISM_PARAM_INVALID";
      case CKR_OBJECT_HANDLE_INVALID            : return "OBJECT_HANDLE_INVALID";
      case CKR_OPERATION_ACTIVE                 : return "OPERATION_ACTIVE";
      case CKR_OPERATION_NOT_INITIALIZED        : return "OPERATION_NOT_INITIALIZED";
      case CKR_PIN_INCORRECT                    : return "PIN_INCORRECT";
      case CKR_PIN_INVALID                      : return "PIN_INVALID";
      case CKR_PIN_LEN_RANGE                    : return "PIN_LEN_RANGE";
      case CKR_PIN_EXPIRED                      : return "PIN_EXPIRED";
      case CKR_PIN_LOCKED                       : return "PIN_LOCKED";
      case CKR_SESSION_CLOSED                   : return "SESSION_CLOSED";
      case CKR_SESSION_COUNT                    : return "SESSION_COUNT";
      case CKR_SESSION_HANDLE_INVALID           : return "SESSION_HANDLE_INVALID";
      case CKR_SESSION_PARALLEL_NOT_SUPPORTED   : return "SESSION_PARALLEL_NOT_SUPPORTED";
      case CKR_SESSION_READ_ONLY                : return "SESSION_READ_ONLY";
      case CKR_SESSION_EXISTS                   : return "SESSION_EXISTS";
      case CKR_SESSION_READ_ONLY_EXISTS         : return "SESSION_READ_ONLY_EXISTS";
      case CKR_SESSION_READ_WRITE_SO_EXISTS     : return "SESSION_READ_WRITE_SO_EXISTS";
      case CKR_SIGNATURE_INVALID                : return "SIGNATURE_INVALID";
      case CKR_SIGNATURE_LEN_RANGE              : return "SIGNATURE_LEN_RANGE";
      case CKR_TEMPLATE_INCOMPLETE              : return "TEMPLATE_INCOMPLETE";
      case CKR_TEMPLATE_INCONSISTENT            : return "TEMPLATE_INCONSISTENT";
      case CKR_TOKEN_NOT_PRESENT                : return "TOKEN_NOT_PRESENT";
      case CKR_TOKEN_NOT_RECOGNIZED             : return "TOKEN_NOT_RECOGNIZED";
      case CKR_TOKEN_WRITE_PROTECTED            : return "TOKEN_WRITE_PROTECTED";
      case CKR_UNWRAPPING_KEY_HANDLE_INVALID    : return "UNWRAPPING_KEY_HANDLE_INVALID";
      case CKR_UNWRAPPING_KEY_SIZE_RANGE        : return "UNWRAPPING_KEY_SIZE_RANGE";
      case CKR_UNWRAPPING_KEY_TYPE_INCONSISTENT : return "UNWRAPPING_KEY_TYPE_INCONSISTENT";
      case CKR_USER_ALREADY_LOGGED_IN           : return "USER_ALREADY_LOGGED_IN";
      case CKR_USER_NOT_LOGGED_IN               : return "USER_NOT_LOGGED_IN";
      case CKR_USER_PIN_NOT_INITIALIZED         : return "USER_PIN_NOT_INITIALIZED";
      case CKR_USER_TYPE_INVALID                : return "USER_TYPE_INVALID";
      case CKR_USER_ANOTHER_ALREADY_LOGGED_IN   : return "USER_ANOTHER_ALREADY_LOGGED_IN";
      case CKR_USER_TOO_MANY_TYPES              : return "USER_TOO_MANY_TYPES";
      case CKR_WRAPPED_KEY_INVALID              : return "WRAPPED_KEY_INVALID";
      case CKR_WRAPPED_KEY_LEN_RANGE            : return "WRAPPED_KEY_LEN_RANGE";
      case CKR_WRAPPING_KEY_HANDLE_INVALID      : return "WRAPPING_KEY_HANDLE_INVALID";
      case CKR_WRAPPING_KEY_SIZE_RANGE          : return "WRAPPING_KEY_SIZE_RANGE";
      case CKR_WRAPPING_KEY_TYPE_INCONSISTENT   : return "WRAPPING_KEY_TYPE_INCONSISTENT";
      case CKR_RANDOM_SEED_NOT_SUPPORTED        : return "RANDOM_SEED_NOT_SUPPORTED";
      case CKR_RANDOM_NO_RNG                    : return "RANDOM_NO_RNG";
      case CKR_DOMAIN_PARAMS_INVALID            : return "DOMAIN_PARAMS_INVALID";
      case CKR_BUFFER_TOO_SMALL                 : return "BUFFER_TOO_SMALL";
      case CKR_SAVED_STATE_INVALID              : return "SAVED_STATE_INVALID";
      case CKR_INFORMATION_SENSITIVE            : return "INFORMATION_SENSITIVE";
      case CKR_STATE_UNSAVEABLE                 : return "STATE_UNSAVEABLE";
      case CKR_CRYPTOKI_NOT_INITIALIZED         : return "CRYPTOKI_NOT_INITIALIZED";
      case CKR_CRYPTOKI_ALREADY_INITIALIZED     : return "CRYPTOKI_ALREADY_INITIALIZED";
      case CKR_MUTEX_BAD                        : return "MUTEX_BAD";
      case CKR_MUTEX_NOT_LOCKED                 : return "MUTEX_NOT_LOCKED";
      case CKR_FUNCTION_REJECTED                : return "FUNCTION_REJECTED";
      case CKR_VENDOR_DEFINED                   : return "VENDOR_DEFINED";
      default:

          {
              static char buffer[80];
              snprintf(buffer, sizeof(buffer) - 1, "Unknown return value code: %d (0x%x)", (int)rv, (int)rv);
              return buffer;
          }
    }
}

void check_rv(CK_RV rv, const char *function)
{
    if (rv != CKR_OK)
    {
        fprintf(stderr, "%s returned %s\n", function, rv_label(rv));
        exit(EX_OSERR);
    }
}

void WLog_ERR(const char * tag, const char * format,  ...)
{
    va_list args;
    va_start(args, format);
    fprintf(stderr, "[%s] ", tag);
    vfprintf(stderr,format, args);
    va_end(args);
}


const unsigned int PKCS11_MAGIC = 0xd00bed00;

typedef struct pkcs11_module
{
	unsigned int magic;
	void* handle;
}  pkcs11_module_t;



CK_RV C_UnloadModule(void* module)
{
	pkcs11_module_t* mod = (pkcs11_module_t*) module;
	if (!mod || mod->magic != PKCS11_MAGIC)
    {
        return CKR_ARGUMENTS_BAD;
    }
	if (mod->handle != NULL && dlclose(mod->handle) < 0)
    {
        return CKR_FUNCTION_FAILED;
    }
	memset(mod, 0, sizeof(*mod));
	free(mod);
	return CKR_OK;
}

void* C_LoadModule(const char* mspec, CK_FUNCTION_LIST_PTR_PTR funcs)
{
	CK_RV rv;
    CK_RV (*c_get_function_list)(CK_FUNCTION_LIST_PTR_PTR);
	pkcs11_module_t* mod = calloc(1, sizeof(*mod));
	if (mspec == NULL)
	{
		free(mod);
		return NULL;
	}
	mod->magic = PKCS11_MAGIC;
	mod->handle = dlopen(mspec, RTLD_LAZY);
	if (mod->handle == NULL)
	{
		WLog_ERR(TAG, "dlopen failed: %s\n", dlerror());
		goto failed;
	}

	/* Get the list of function pointers */
	c_get_function_list = (CK_RV(*)(CK_FUNCTION_LIST_PTR_PTR))
            dlsym(mod->handle, "C_GetFunctionList");

	if (!c_get_function_list)
		goto failed;

	rv = c_get_function_list(funcs);

	if (rv == CKR_OK)
		return (void*) mod;
	else
		WLog_ERR(TAG, "C_GetFunctionList failed %lx", rv);

failed:
	C_UnloadModule((void*) mod);
	return NULL;
}

typedef struct ulvector
{
    CK_ULONG count;
    CK_ULONG_PTR elements;
} *  ulvectorp;

ulvectorp make_ulvector(CK_ULONG count)
{
    ulvectorp result = check_memory(malloc(sizeof(*result)));
    result->count = count;
    result->elements = check_memory(calloc(result->count, sizeof(result->elements[0])));
    memset(result->elements, 0, result->count * sizeof(result->elements[0]));
    return result;
}

ulvectorp ulvector_concatenate(ulvectorp a, ulvectorp b)
{
    ulvectorp result = make_ulvector(a->count + b->count);
    memcpy(result->elements,            a->elements, sizeof(a->elements[0])*a->count);
    memcpy(result->elements + a->count, b->elements, sizeof(b->elements[0])*b->count);
    return result;
}

void ulvector_free(ulvectorp * v)
{
    free((*v)->elements);
    free(*v);
    v = NULL;
}

void ulvector_print(FILE* out,ulvectorp v)
{
    unsigned long i;
    for (i = 0; i < v->count; i++ )
    {
        fprintf(out," %lu",v->elements[i]);
    }
    fprintf(out,"\n");
}

ulvectorp get_slot_list(bool token_present)
{
    CK_ULONG count = 0;
    check_rv(p11->C_GetSlotList(token_present, NULL, &count), "C_GetSlotList");
    ulvectorp result = make_ulvector(count);
    check_rv(p11->C_GetSlotList(token_present, result->elements, &(result->count)), "C_GetSlotList");
    return result;
}

/* ==================== attributes ==================== */

void attribute_initialize(CK_ATTRIBUTE_PTR attribute, CK_ULONG length, unsigned char filler)
{
    attribute->ulValueLen = length;
    attribute->pValue = check_memory(malloc(attribute->ulValueLen));
    if (length > 0)
    {
        memset(attribute->pValue, filler, length);
    }
}

void attribute_free(CK_ATTRIBUTE_PTR attribute)
{
    attribute->ulValueLen = CK_UNAVAILABLE_INFORMATION;
    free(attribute->pValue);
    attribute->pValue = 0;
}

typedef enum
{
    t_bool             =   1,
    t_string           =   2,
    t_date             =   3,
    t_bytes            =   4,
    t_ulong            =   (1 << 10),
    t_array            =   (1 << 11),
    t_pkcs_types_mask  = ~((1 << 12) - 1),
    t_key_type         =   (1 << 12),
    t_attribute        =   (2 << 12),
    t_attribute_type   =   (3 << 12),
    t_certificate_type =   (4 << 12),
    t_hardware_feature =   (5 << 12),
    t_mechanism_type   =   (6 << 12),
    t_object_class     =   (7 << 12),
} type_t;

const char * descriptor_type_label(type_t type)
{
    switch (type)
    {
      case t_bool:             return "bool";
      case t_string:           return "string";
      case t_date:             return "date";
      case t_bytes:            return "bytes";
      case t_ulong:            return "ulong";
      case t_array:            return "array";
      default:
          if (type & t_ulong)
          {
              switch (type & t_pkcs_types_mask)
              {
                case t_key_type:         return "key_type";
                case t_attribute:        return "attribute";
                case t_attribute_type:   return "attribute_type";
                case t_certificate_type: return "certificate_type";
                case t_hardware_feature: return "hardware_feature";
                case t_mechanism_type:   return "mechanism_type";
                case t_object_class:     return "object_class";
                default:                 return "UNKNOWN_TYPE";
              }
          }
          else if (type & t_array)
          {
              switch (type & t_pkcs_types_mask)
              {
                case t_attribute:           return "(array attribute)";
                case t_mechanism_type:      return "(array mechanism_type)";
                default:                    return "(array UNKNOWN_TYPE)";
              }
          }
          else
          {
              return "UNKNOWN_TYPE";
          }
    }
}

typedef struct {
    CK_ATTRIBUTE_TYPE attribute;
    type_t            type;
    const char*       name;
} attribute_type_map_t;

// OBJECT_ID = DER encoding of the OID indicating the data object type {empty by default},

attribute_type_map_t attribute_type_map[] = {
    {CKA_CLASS,                       t_ulong|t_object_class,        "CLASS"},
    {CKA_TOKEN,                       t_bool,                        "TOKEN"},
    {CKA_PRIVATE,                     t_bool,                        "PRIVATE"},
    {CKA_LABEL,                       t_string,                      "LABEL"},
    {CKA_APPLICATION,                 t_string,                      "APPLICATION"},
    {CKA_VALUE,                       t_bytes,                       "VALUE"},
    {CKA_OBJECT_ID,                   t_bytes,                       "OBJECT_ID"},  // DER
    {CKA_CERTIFICATE_TYPE,            t_ulong|t_certificate_type,    "CERTIFICATE_TYPE"},
    {CKA_ISSUER,                      t_bytes,                      "ISSUER"}, // DER
    {CKA_SERIAL_NUMBER,               t_bytes,                      "SERIAL_NUMBER"},
    {CKA_AC_ISSUER,                   t_string,                      "AC_ISSUER"},
    {CKA_OWNER,                       t_string,                      "OWNER"},
    {CKA_ATTR_TYPES,                  t_ulong|t_attribute_type,      "ATTR_TYPES"},
    {CKA_TRUSTED,                     t_bool,                        "TRUSTED"},
    {CKA_CERTIFICATE_CATEGORY,        t_ulong,                       "CERTIFICATE_CATEGORY"},
    {CKA_JAVA_MIDP_SECURITY_DOMAIN,   t_bytes,                       "JAVA_MIDP_SECURITY_DOMAIN"},
    {CKA_URL,                         t_string,                      "URL"},
    {CKA_HASH_OF_SUBJECT_PUBLIC_KEY,  t_bytes,                       "HASH_OF_SUBJECT_PUBLIC_KEY"},
    {CKA_HASH_OF_ISSUER_PUBLIC_KEY,   t_bytes,                       "HASH_OF_ISSUER_PUBLIC_KEY"},
    {CKA_CHECK_VALUE,                 t_bytes,                       "CHECK_VALUE"},
    {CKA_KEY_TYPE,                    t_ulong|t_key_type,            "KEY_TYPE"},
    {CKA_SUBJECT,                     t_bytes,                       "SUBJECT"},
    {CKA_ID,                          t_bytes,                       "ID"},
    {CKA_SENSITIVE,                   t_bool,                        "SENSITIVE"},
    {CKA_ENCRYPT,                     t_bool,                        "ENCRYPT"},
    {CKA_DECRYPT,                     t_bool,                        "DECRYPT"},
    {CKA_WRAP,                        t_bool,                        "WRAP"},
    {CKA_UNWRAP,                      t_bool,                        "UNWRAP"},
    {CKA_SIGN,                        t_bool,                        "SIGN"},
    {CKA_SIGN_RECOVER,                t_bool,                        "SIGN_RECOVER"},
    {CKA_VERIFY,                      t_bool,                        "VERIFY"},
    {CKA_VERIFY_RECOVER,              t_bool,                        "VERIFY_RECOVER"},
    {CKA_DERIVE,                      t_bool,                        "DERIVE"},
    {CKA_START_DATE,                  t_date,                        "START_DATE"},
    {CKA_END_DATE,                    t_date,                        "END_DATE"},
    {CKA_MODULUS,                     t_bytes,                       "MODULUS"},
    {CKA_MODULUS_BITS,                t_ulong,                       "MODULUS_BITS"}, // CKA_MODULUS},
    {CKA_PUBLIC_EXPONENT,             t_bytes,                       "PUBLIC_EXPONENT"},
    {CKA_PRIVATE_EXPONENT,            t_bytes,                       "PRIVATE_EXPONENT"},
    {CKA_PRIME_1,                     t_bytes,                       "PRIME_1"},
    {CKA_PRIME_2,                     t_bytes,                       "PRIME_2"},
    {CKA_EXPONENT_1,                  t_bytes,                       "EXPONENT_1"},
    {CKA_EXPONENT_2,                  t_bytes,                       "EXPONENT_2"},
    {CKA_COEFFICIENT,                 t_bytes,                       "COEFFICIENT"},
    {CKA_PRIME,                       t_bytes,                       "PRIME"},
    {CKA_SUBPRIME,                    t_bytes,                       "SUBPRIME"}, // CKA_SUB_PRIME_BITS},
    {CKA_BASE,                        t_ulong,                       "BASE"},
    {CKA_PRIME_BITS,                  t_ulong,                       "PRIME_BITS"}, // CKA_PRIME},
    {CKA_SUB_PRIME_BITS,              t_ulong,                       "SUB_PRIME_BITS"},
    {CKA_VALUE_BITS,                  t_ulong,                       "VALUE_BITS"},
    {CKA_VALUE_LEN,                   t_ulong,                       "VALUE_LEN"},
    {CKA_EXTRACTABLE,                 t_bool,                        "EXTRACTABLE"},
    {CKA_LOCAL,                       t_bool,                        "LOCAL"},
    {CKA_NEVER_EXTRACTABLE,           t_bool,                        "NEVER_EXTRACTABLE"},
    {CKA_ALWAYS_SENSITIVE,            t_bool,                        "ALWAYS_SENSITIVE"},
    {CKA_KEY_GEN_MECHANISM,           t_ulong|t_mechanism_type,      "KEY_GEN_MECHANISM"},
    {CKA_MODIFIABLE,                  t_bool,                        "MODIFIABLE"},
    {CKA_ECDSA_PARAMS,                t_bytes,                       "ECDSA_PARAMS"},
    {CKA_EC_PARAMS,                   t_bytes,                       "EC_PARAMS"},
    {CKA_EC_POINT,                    t_bytes,                       "EC_POINT"},
    {CKA_SECONDARY_AUTH,              t_bytes,                       "SECONDARY_AUTH"},
    {CKA_AUTH_PIN_FLAGS,              t_bytes,                       "AUTH_PIN_FLAGS"},
    {CKA_ALWAYS_AUTHENTICATE,         t_bool,                        "ALWAYS_AUTHENTICATE"},
    {CKA_WRAP_WITH_TRUSTED,           t_bool,                        "WRAP_WITH_TRUSTED"},
    {CKA_HW_FEATURE_TYPE,             t_ulong|t_hardware_feature,    "HW_FEATURE_TYPE"},
    {CKA_RESET_ON_INIT,               t_bool,                        "RESET_ON_INIT"},
    {CKA_HAS_RESET,                   t_bool,                        "HAS_RESET"},
    {CKA_PIXEL_X,                     t_ulong,                       "PIXEL_X"},
    {CKA_PIXEL_Y,                     t_ulong,                       "PIXEL_Y"},
    {CKA_RESOLUTION,                  t_ulong,                       "RESOLUTION"},
    {CKA_CHAR_ROWS,                   t_ulong,                       "CHAR_ROWS"},
    {CKA_CHAR_COLUMNS,                t_ulong,                       "CHAR_COLUMNS"},
    {CKA_COLOR,                       t_bool,                        "COLOR"},
    {CKA_BITS_PER_PIXEL,              t_ulong,                       "BITS_PER_PIXEL"},
    {CKA_CHAR_SETS,                   t_string,                      "CHAR_SETS"},
    {CKA_ENCODING_METHODS,            t_string,                      "ENCODING_METHODS"},
    {CKA_MIME_TYPES,                  t_string,                      "MIME_TYPES"},
    {CKA_MECHANISM_TYPE,              t_ulong|t_mechanism_type,      "MECHANISM_TYPE"},
    {CKA_REQUIRED_CMS_ATTRIBUTES,     t_bytes,                       "REQUIRED_CMS_ATTRIBUTES"},
    {CKA_DEFAULT_CMS_ATTRIBUTES,      t_bytes,                       "DEFAULT_CMS_ATTRIBUTES"},
    {CKA_SUPPORTED_CMS_ATTRIBUTES,    t_bytes,                       "SUPPORTED_CMS_ATTRIBUTES"},
    {CKA_WRAP_TEMPLATE,               t_array|t_attribute,           "WRAP_TEMPLATE"},
    {CKA_UNWRAP_TEMPLATE,             t_array|t_attribute,           "UNWRAP_TEMPLATE"},
    {CKA_ALLOWED_MECHANISMS,          t_array|t_mechanism_type,      "ALLOWED_MECHANISMS"},
    {CKA_VENDOR_DEFINED,              t_bytes,                       "VENDOR_DEFINED"},
};


#define countof(a) (sizeof(a) / sizeof(a[0]))

attribute_type_map_t * find_attribute_descriptor(CK_ATTRIBUTE_TYPE atype)
{
    int i = 0;
    while ((i < countof(attribute_type_map))
           && (attribute_type_map[i].attribute != atype))
    {
        i++;
    }
    return (i < countof(attribute_type_map))
            ?  &(attribute_type_map[i])
            : NULL;
}


const char * attribute_type_label(CK_ULONG value)
{
    attribute_type_map_t * descriptor = find_attribute_descriptor(value);
    if (descriptor)
    {
        return descriptor->name;
    }
    else
    {
        static char buffer[80];
        snprintf(buffer, sizeof (buffer), "Unknown_Attribute_Type_%lu", value);
        return buffer;
    }
}


/* struct ck_attribute */
/* { */
/*   ck_attribute_type_t type; */
/*   void *value; */
/*   unsigned long value_len; */
/* }; */
/*  */
/*  */
/* struct ck_date */
/* { */
/*   unsigned char year[4]; */
/*   unsigned char month[2]; */
/*   unsigned char day[2]; */
/* }; */

void warnIfTooLong(FILE * out, CK_ULONG length, CK_ULONG maximum)
{
    if (length > maximum)
    {
        fprintf(out, " ; warning: length = %lu greater than expected %lu",
                length, maximum);
    }
}

void raw_string_print(FILE * out, const char * string, unsigned long length)
{
    while (0 < length)
    {
        if ((*string == '"') || (*string ==  '\\'))
        {
            fprintf(out, "\\%c", *string);
        }else
        {
            fprintf(out, "%c", *string);
        }
        string++;
        length--;
    }
}

void raw_ulong_print(FILE * out, CK_ULONG value, label_pr label_of, const char * title)
{
    if (title)
    {
        fprintf(out, "%s", title);
    }
    if (CK_UNAVAILABLE_INFORMATION ==  value)
    {
        fprintf(out, ":UNAVAILABLE_INFORMATION");
    }
    else if (label_of)
    {
        fprintf(out, "%s", label_of(value));
    }
    else
    {
        fprintf(out, "%lu", value);
    }
}


bool attribute_present_p(CK_ATTRIBUTE_PTR attribute)
{
    return (attribute->ulValueLen != CK_UNAVAILABLE_INFORMATION);
}

void attribute_print(FILE* out, CK_ATTRIBUTE_PTR attribute)
{
    attribute_type_map_t * descriptor = find_attribute_descriptor(attribute->type);
    CK_ULONG maximum = 0;
    fprintf(out,"(:%-30s ", attribute_type_label(attribute->type));
    if (attribute->ulValueLen == CK_UNAVAILABLE_INFORMATION)
    {
        fprintf(out,":UNAVAILABLE_INFORMATION");
    }
    else if (descriptor)
    {
        fprintf(out, "(:length %4lu) ",  attribute->ulValueLen);
        switch (descriptor->type)
        {
          case t_bool:
              maximum = sizeof(CK_BBOOL);
              if (attribute->ulValueLen >= sizeof(CK_BBOOL))
              {
                  fprintf(out, "%s",(*(CK_BBOOL*)(attribute->pValue)
                                     ? "true"
                                     :"false"));
              }
              else
              {
                  fprintf(out, "unknown");
              }
              break;

          case t_ulong:
              if (attribute->ulValueLen >= sizeof(CK_ULONG))
              {
                  raw_ulong_print(out, *(CK_ULONG*)(attribute->pValue), NULL, NULL);
              }
              else
              {
                  fprintf(out, "unknown");
              }
              break;

          case t_string:

              fprintf(out, "\"");
              raw_string_print(out, (const char *)(attribute->pValue), attribute->ulValueLen);
              fprintf(out, "\"");
              break;

          case t_bytes:
              {
                  CK_ULONG length = attribute->ulValueLen;
                  const CK_CHAR * bytes = (const CK_CHAR *)(attribute->pValue);
                  const char *  sep = "";
                  fprintf(out, "#(");
                  while (0 < length)
                  {
                      fprintf(out, "%s#x%02x", sep, *bytes);
                      bytes++;
                      length--;
                      sep = " ";
                  }
                  fprintf(out, ")");
              }
              break;

          case t_date:
              maximum = 8 + 8;
              if (attribute->ulValueLen == 8)
              {
                  raw_string_print(out, (const char *)(attribute->pValue), attribute->ulValueLen);
              }
              else if (attribute->ulValueLen ==  8 + 8)
              {
                  raw_string_print(out, (const char *)(attribute->pValue), attribute->ulValueLen);
              }
              else
              {
                  fprintf(out, "unknown");
              }
              break;

          default:
              if (descriptor->type & t_ulong)
              {
                  label_pr label_of = 0;
                  const char * title = 0;
                  maximum = sizeof(CK_ULONG);
                  switch (descriptor->type & t_pkcs_types_mask)
                  {
                    case t_attribute_type:       label_of = attribute_type_label;     break;
                    case t_certificate_type:     label_of = certificate_type_label;   break;
                    case t_hardware_feature:     label_of = hardware_feature_label;   break;
                    case t_key_type:             label_of = key_type_label;           break;
                    case t_mechanism_type:       label_of = mechanism_type_label;     break;
                    case t_object_class:         label_of = object_class_label;       break;
                    default:                     title = "# | unknown type |# ";      break;
                  }
                  raw_ulong_print(out, *(CK_ULONG*)(attribute->pValue), label_of, title);
              }
              else if (descriptor->type & t_array)
              {
                  switch (descriptor->type & t_pkcs_types_mask)
                  {
                    case t_attribute: /* fall thru */
                    case t_mechanism_type:
                        /* attribute_initialize( &(template->attributes[i]), 0,  0); */
                        break;
                    default:
                        /* error("Invalid type in attribute_type_map[%lu]: 0x%08x", */
                        /*       i, attribute_type_map[i].type); */
                        break;
                  }
              }
              else
              {
                  /* error("Invalid type in attribute_type_map[%lu]: 0x%08x", */
                  /*       i, attribute_type_map[i].type); */
              }
        }
    }

    fprintf(out, ")");
    if (maximum > 0)
    {
        warnIfTooLong(out, attribute->ulValueLen, maximum);
    }
}

/* ==================== templates ==================== */

typedef struct template
{
    CK_ULONG count;
    CK_ATTRIBUTE_PTR  attributes;
} * templatep;

templatep make_template(CK_ULONG count)
{
    templatep result = check_memory(malloc(sizeof(*result)));
    result->count = count;
    result->attributes = check_memory(calloc(result->count, sizeof(result->attributes[0])));
    memset(result->attributes, 0, result->count * sizeof(result->attributes[0]));
    return result;
}

void template_free(templatep * template)
{
    CK_ULONG i;
    for(i = 0; i < (*template)->count; i++)
    {
        CK_ATTRIBUTE_PTR attribute = & ((*template)->attributes[i]);
        attribute_free(attribute);
    }
    free((*template)->attributes);
    free(*template);
    (*template) = NULL;
}

CK_ATTRIBUTE_PTR template_find_attribute(templatep template, CK_ATTRIBUTE_TYPE atype)
{
    CK_ULONG i = 0;
    while ((i < template->count) && (template->attributes[i].type != atype))
    {
        i++;
    }
    return (i < template->count)
            ? &(template->attributes[i])
            : NULL;
}

void check_descriptor_type(CK_ATTRIBUTE_TYPE atype, type_t type)
{
    attribute_type_map_t * descriptor = find_attribute_descriptor(atype);
    if (descriptor->type != type)
    {
        error("Attribute %s: type error, trying to get %s, is %s",
              attribute_type_label(atype),
              descriptor_type_label(type),
              descriptor_type_label(descriptor->type));
    }
}

CK_ULONG template_get_ulong_attribute(templatep template, CK_ATTRIBUTE_TYPE atype)
{
    check_descriptor_type(atype, t_ulong);
    CK_ATTRIBUTE_PTR attribute = template_find_attribute(template,atype);
    if (attribute && (attribute->ulValueLen == sizeof (CK_ULONG)))
    {
        return *(CK_ULONG *)attribute->pValue;
    }
    else
    {
        return CK_UNAVAILABLE_INFORMATION;
    }
}

char * template_get_string_attribute(templatep template, CK_ATTRIBUTE_TYPE atype)
{
    check_descriptor_type(atype, t_string);
    CK_ATTRIBUTE_PTR attribute = template_find_attribute(template,atype);
    if (attribute)
    {
        char * string = check_memory(malloc(1 + attribute->ulValueLen));
        strncpy(string, attribute->pValue, attribute->ulValueLen);
        string[attribute->ulValueLen] = '\0';
        return string;
    }
    else
    {
        return NULL;
    }
}

const unsigned char * template_get_buffer_attribute(templatep template, CK_ATTRIBUTE_TYPE atype, CK_ULONG * length)
{
    check_descriptor_type(atype, t_bytes);
    CK_ATTRIBUTE_PTR attribute = template_find_attribute(template,atype);
    if (attribute)
    {
        (*length) = attribute->ulValueLen;
        return attribute->pValue;
    }
    else
    {
        (*length) = CK_UNAVAILABLE_INFORMATION;
        return NULL;
    }
}

void template_print(FILE * out, templatep template)
{
    const char * sep = "";
    CK_ULONG i;
    fprintf(out, "(");
    for (i = 0; i < template->count; i++)
    {
        if (attribute_present_p(&(template->attributes[i])))
        {
            fprintf(out, "%s", sep);
            attribute_print(out, &(template->attributes[i]));
            sep = "\n ";
        }
    }
    fprintf(out, ")");
}

templatep make_full_template(bool allocate_buffers)
{
    templatep template = make_template(countof(attribute_type_map));
    CK_ULONG i;
    for (i = 0; i < template->count; i++)
    {
        template->attributes[i].type = attribute_type_map[i].attribute;
        switch (attribute_type_map[i].type)
        {
          case t_bool:
              attribute_initialize( &(template->attributes[i]), sizeof(CK_BBOOL),  0);
              break;

          case t_ulong:
              attribute_initialize( &(template->attributes[i]), sizeof(CK_ULONG),  0);
              break;

          case t_string: /* fall thru */
          case t_bytes:
              attribute_initialize( &(template->attributes[i]), allocate_buffers?128:0,  0);
              break;

          case t_date:
              attribute_initialize( &(template->attributes[i]), 16,  '0');
              break;

          default:
              if (attribute_type_map[i].type & t_ulong)
              {
                  attribute_initialize( &(template->attributes[i]), sizeof(CK_ULONG),  0);
                  /*
                  switch (attribute_type_map[i].type & t_pkcs_types_mask)
                  {
                  case t_attribute_type:
                  case t_certificate_type:
                  case t_hardware_feature:
                  case t_key_type:
                  case t_mechanism_type:
                  case t_object_class:
                  default:
                  }
                  */
              }
              else if (attribute_type_map[i].type & t_array)
              {
                  switch (attribute_type_map[i].type & t_pkcs_types_mask)
                  {
                    case t_attribute: /* fall thru */
                    case t_mechanism_type:
                        attribute_initialize( &(template->attributes[i]), 0,  0);
                        break;
                    default:
                        error("Invalid type in attribute_type_map[%lu]: 0x%08x",
                              i, attribute_type_map[i].type);
                  }
              }
              else
              {
                  error("Invalid type in attribute_type_map[%lu]: 0x%08x",
                        i, attribute_type_map[i].type);
              }
        }
    }
    return template;
}

/* ==================== objects ==================== */


templatep object_get_all_attributes(CK_SESSION_HANDLE session, CK_OBJECT_HANDLE object)
{
    templatep template = make_full_template(true);
    CK_RV rv = p11->C_GetAttributeValue(session, object, template->attributes, template->count);
    switch (rv)
    {
      case CKR_ATTRIBUTE_SENSITIVE: /* fall thru */
      case CKR_ATTRIBUTE_TYPE_INVALID: /* fall thru */
          /* that's ok */
          break;
      case CKR_BUFFER_TOO_SMALL:
          {
              CK_ULONG valuelen = template_get_ulong_attribute(template, CKA_VALUE_LEN);
              CK_ATTRIBUTE_PTR value_attribute = template_find_attribute(template, CKA_VALUE);
              if (value_attribute && valuelen)
              {
                  /*  Let's try again with a bigger buffer */
                  value_attribute->pValue = realloc(value_attribute->pValue, valuelen);
                  value_attribute->ulValueLen = valuelen;
                  rv = p11->C_GetAttributeValue(session, object, template->attributes, template->count);
                  /* TODO: check the other buffers! */
                  switch (rv)
                  {
                    case CKR_ATTRIBUTE_SENSITIVE: /* fall thru */
                    case CKR_ATTRIBUTE_TYPE_INVALID: /* fall thru */
                        /* that's ok */
                        break;
                    case CKR_BUFFER_TOO_SMALL:
                        break;
                    default:
                        check_rv(rv,"C_GetAttributeValue");
                        break;
                  }
              }
          }
          break;

      default:
          check_rv(rv,"C_GetAttributeValue");
          break;
    }
    return template;
}



ulvectorp find_all_objects(CK_SESSION_HANDLE session, templatep template)
{
    CK_ULONG total = 0;
    CK_ULONG count = 0;
    ulvectorp objects = make_ulvector(0);
    ulvectorp new_objects = make_ulvector(128);
    fprintf(stderr,"p11->C_FindObjectsInit = %p\n",(void*)(p11->C_FindObjectsInit));
    check_rv(p11->C_FindObjectsInit(session,
                                    template?template->attributes:0,
                                    template?template->count:0), "C_FindObjectsInit");
    check_rv(p11->C_FindObjects(session, new_objects->elements, new_objects->count, &count), "C_FindObjects");
    while (count > 0)
    {
        ulvectorp old_objects = objects;
        new_objects->count = count;
        objects = ulvector_concatenate(old_objects, new_objects);
        ulvector_free( & old_objects);
        ulvector_free( & new_objects);
        new_objects = make_ulvector(128);
        check_rv(p11->C_FindObjects(session, new_objects->elements, new_objects->count, &count), "C_FindObjects");
    }
    check_rv(p11->C_FindObjectsFinal(session), "C_FindObjectsFinal");
    ulvector_free( & new_objects);
    return objects;
}




//  #define PKCS11_LIBRARY_PATH "/usr/local/lib/libiaspkcs11.so"
#define PKCS11_LIBRARY_PATH "/usr/local/lib/opensc-pkcs11.so"

int main()
{
    pkcs11_module_t* pkcs11_module = C_LoadModule(PKCS11_LIBRARY_PATH, &p11);
    check_rv(p11->C_Initialize(NULL_PTR), "C_Initialize");
    ulvectorp slots = get_slot_list(true);
    if (slots->count >= 1)
    {
        CK_ULONG slot = slots->elements[0];
        CK_SESSION_HANDLE session;
        printf("(:slots "); ulvector_print(stdout,slots); printf(")\n");
        ulvector_free(&slots);
        check_rv(p11->C_OpenSession(slot, CKF_SERIAL_SESSION,  NULL, NULL, & session), "C_OpenSession");

        templatep template = make_full_template(false);
        ulvectorp objects = find_all_objects(session, NULL);
        CK_ULONG i;
        printf("(:objects "); ulvector_print(stdout,objects); printf(")\n");
        printf("(");
        for (i = 0;i < objects->count; i++ )
        {
            CK_ULONG oid = objects->elements[i];
            printf("(:object %lu\n", oid);
            templatep attributes = object_get_all_attributes(session, oid);
            template_print(stdout,attributes);
            printf(")\n");
        }
        printf(")\n");
        check_rv(p11->C_CloseSession(session), "C_CloseSession");
    }else
    {
        printf("No smartcard!\n");
    }
    fflush(stdout);
    check_rv(p11->C_Finalize(NULL_PTR), "C_Finalize");
    C_UnloadModule(pkcs11_module);
    return 0;
}
