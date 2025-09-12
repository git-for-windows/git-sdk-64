/*
 * Copyright (c) 2012 Red Hat Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *     * Redistributions of source code must retain the above
 *       copyright notice, this list of conditions and the
 *       following disclaimer.
 *     * Redistributions in binary form must reproduce the
 *       above copyright notice, this list of conditions and
 *       the following disclaimer in the documentation and/or
 *       other materials provided with the distribution.
 *     * The names of contributors to this software may not be
 *       used to endorse or promote products derived from this
 *       software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 * Author: Stef Walter <stefw@redhat.com>
 */

#ifndef PKCS11_X_H_
#define PKCS11_X_H_ 1

#if defined(__cplusplus)
extern "C" {
#endif

/* -------------------------------------------------------------------
 * NSS TRUST OBJECTS
 *
 * And related, non-standard
 */

/* Define this if you want the NSS specific symbols */
#define CRYPTOKI_NSS_VENDOR_DEFINED 1
#ifdef CRYPTOKI_NSS_VENDOR_DEFINED

/* Various NSS objects */
#define CKO_NSS_CRL                     0xce534351UL
#define CKO_NSS_SMIME                   0xce534352UL
#define CKO_NSS_TRUST                   0xce534353UL
#define CKO_NSS_BUILTIN_ROOT_LIST       0xce534354UL
#define CKO_NSS_NEWSLOT                 0xce534355UL
#define CKO_NSS_DELSLOT                 0xce534356UL

/* Various NSS key types */
#define CKK_NSS_PKCS8                   0xce534351UL

/* Various NSS attributes */
#define CKA_NSS_URL                     0xce534351UL
#define CKA_NSS_EMAIL                   0xce534352UL
#define CKA_NSS_SMIME_INFO              0xce534353UL
#define CKA_NSS_SMIME_TIMESTAMP         0xce534354UL
#define CKA_NSS_PKCS8_SALT              0xce534355UL
#define CKA_NSS_PASSWORD_CHECK          0xce534356UL
#define CKA_NSS_EXPIRES                 0xce534357UL
#define CKA_NSS_KRL                     0xce534358UL
#define CKA_NSS_PQG_COUNTER             0xce534364UL
#define CKA_NSS_PQG_SEED                0xce534365UL
#define CKA_NSS_PQG_H                   0xce534366UL
#define CKA_NSS_PQG_SEED_BITS           0xce534367UL
#define CKA_NSS_MODULE_SPEC             0xce534368UL
#define CKA_NSS_MOZILLA_CA_POLICY       0xce534372UL
#define CKA_NSS_SERVER_DISTRUST_AFTER   0xce534373UL
#define CKA_NSS_EMAIL_DISTRUST_AFTER    0xce534374UL

/* NSS trust attributes */
#define CKA_TRUST_DIGITAL_SIGNATURE     0xce536351UL
#define CKA_TRUST_NON_REPUDIATION       0xce536352UL
#define CKA_TRUST_KEY_ENCIPHERMENT      0xce536353UL
#define CKA_TRUST_DATA_ENCIPHERMENT     0xce536354UL
#define CKA_TRUST_KEY_AGREEMENT         0xce536355UL
#define CKA_TRUST_KEY_CERT_SIGN         0xce536356UL
#define CKA_TRUST_CRL_SIGN              0xce536357UL
#define CKA_TRUST_SERVER_AUTH           0xce536358UL
#define CKA_TRUST_CLIENT_AUTH           0xce536359UL
#define CKA_TRUST_CODE_SIGNING          0xce53635aUL
#define CKA_TRUST_EMAIL_PROTECTION      0xce53635bUL
#define CKA_TRUST_IPSEC_END_SYSTEM      0xce53635cUL
#define CKA_TRUST_IPSEC_TUNNEL          0xce53635dUL
#define CKA_TRUST_IPSEC_USER            0xce53635eUL
#define CKA_TRUST_TIME_STAMPING         0xce53635fUL
#define CKA_TRUST_STEP_UP_APPROVED      0xce536360UL
#define CKA_CERT_SHA1_HASH              0xce5363b4UL
#define CKA_CERT_MD5_HASH               0xce5363b5UL

/* NSS trust values */
typedef CK_ULONG                        CK_TRUST;
#define CKT_NSS_TRUSTED                 0xce534351UL
#define CKT_NSS_TRUSTED_DELEGATOR       0xce534352UL
#define CKT_NSS_MUST_VERIFY_TRUST       0xce534353UL
#define CKT_NSS_NOT_TRUSTED             0xce53435AUL
#define CKT_NSS_TRUST_UNKNOWN           0xce534355UL
#define CKT_NSS_VALID_DELEGATOR         0xce53435BUL

/* NSS specific mechanisms */
#define CKM_NSS_AES_KEY_WRAP            0xce534351UL
#define CKM_NSS_AES_KEY_WRAP_PAD        0xce534352UL

/* NSS specific return values */
#define CKR_NSS_CERTDB_FAILED           0xce534351UL
#define CKR_NSS_KEYDB_FAILED            0xce534352UL

#endif /* CRYPTOKI_NSS_VENDOR_DEFINED */

/* Define this if you want the vendor specific symbols */
#define CRYPTOKI_X_VENDOR_DEFINED 1
#ifdef CRYPTOKI_X_VENDOR_DEFINED

#define CKA_X_VENDOR   (CKA_VENDOR_DEFINED | 0x58444700UL)
#define CKO_X_VENDOR   (CKA_VENDOR_DEFINED | 0x58444700UL)

/* -------------------------------------------------------------------
 * BLOCKLISTS
 */

#define CKA_X_DISTRUSTED                             (CKA_X_VENDOR + 100)

/* -------------------------------------------------------------------
 * CERTIFICATE EXTENSIONS
 *
 * For attaching certificate extensions to certificates
 */

#define CKO_X_CERTIFICATE_EXTENSION                  (CKO_X_VENDOR + 200)

/* From the 2.40 draft */
#ifndef CKA_PUBLIC_KEY_INFO
#define CKA_PUBLIC_KEY_INFO                          0x00000129UL
#endif

#endif /* CRYPTOKI_X_VENDOR_DEFINED */

/* Define this if you want the vendor specific symbols */
#define CRYPTOKI_RU_TEAM_TC26_VENDOR_DEFINED 1
#ifdef CRYPTOKI_RU_TEAM_TC26_VENDOR_DEFINED

/* See https://tc26.ru/standarts/perevody/guidelines-the-pkcs-11-extensions-for-implementing-the-gost-r-34-10-2012-and-gost-r-34-11-2012-russian-standards-.html */

#define NSSCK_VENDOR_PKCS11_RU_TEAM 0xD4321000 /* 0x80000000 | 0x54321000 */
#define CK_VENDOR_PKCS11_RU_TEAM_TC26 NSSCK_VENDOR_PKCS11_RU_TEAM

/* GOST KEY TYPES */
#define CKK_GOSTR3410_512 (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x003)
#define CKK_KUZNECHIK (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x004)
#define CKK_MAGMA (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x005)

/* PKCS #5 PRF Functions */
#define CKP_PKCS5_PBKD2_HMAC_GOSTR3411_2012_512 (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x003)

/* GOST MECHANISMS */
#define CKM_GOSTR3410_512_KEY_PAIR_GEN (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x005)
#define CKM_GOSTR3410_512 (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x006)
#define CKM_GOSTR3410_2012_DERIVE (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x007)
#define CKM_GOSTR3410_WITH_GOSTR3411_2012_256 (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x008)
#define CKM_GOSTR3410_WITH_GOSTR3411_2012_512 (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x009)
#define CKM_GOSTR3410_PUBLIC_KEY_DERIVE (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x00A)
#define CKM_GOSTR3411_2012_256 (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x012)
#define CKM_GOSTR3411_2012_512 (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x013)
#define CKM_GOSTR3411_2012_256_HMAC (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x014)
#define CKM_GOSTR3411_2012_512_HMAC (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x015)
#define CKM_TLS_GOST_PRF_2012_256 (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x016)
#define CKM_TLS_GOST_PRF_2012_512 (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x017)
#define CKM_TLS_GOST_MASTER_KEY_DERIVE_2012_256 (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x018)
#define CKM_KDF_4357 (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x025)
#define CKM_KDF_GOSTR3411_2012_256 (CK_VENDOR_PKCS11_RU_TEAM_TC26 |0x026)

#endif /* CRYPTOKI_RU_TEAM_TC26_VENDOR_DEFINED */

/* Define this if you want the IBM specific symbols */
#define CRYPTOKI_IBM_VENDOR_DEFINED 1
#ifdef CRYPTOKI_IBM_VENDOR_DEFINED

#define CKK_IBM_PQC_DILITHIUM    CKK_VENDOR_DEFINED + 0x10023

/* Secure key tokens store the secure key blob in attribute CKA_IBM_OPAQUE
in their key object. During an HSM master key change, the secure key blob
is being re-enciphered with the new master key. This re-enciphered secure
key blob is stored in attribute CKA_IBM_OPAQUE_REENC while the HSM master
key change operation is active.

When a HSM master key change operation is finalized, the secure key blob
enciphered with the old master key is backed up into attribute CKA_IBM_OPAQUE_OLD,
and the re-enciphered secure key blob from CKA_IBM_OPAQUE_REENC becomes
the current one in CKA_IBM_OPAQUE. */
#define CKA_IBM_OPAQUE                         (CKA_VENDOR_DEFINED + 1)
#define CKA_IBM_OPAQUE_REENC                   (CKA_VENDOR_DEFINED + 3)
#define CKA_IBM_OPAQUE_OLD                     (CKA_VENDOR_DEFINED + 4)
#define CKA_IBM_RESTRICTABLE                   (CKA_VENDOR_DEFINED + 0x10001)
#define CKA_IBM_NEVER_MODIFIABLE               (CKA_VENDOR_DEFINED + 0x10002)
#define CKA_IBM_RETAINKEY                      (CKA_VENDOR_DEFINED + 0x10003)
#define CKA_IBM_ATTRBOUND                      (CKA_VENDOR_DEFINED + 0x10004)
#define CKA_IBM_KEYTYPE                        (CKA_VENDOR_DEFINED + 0x10005)
#define CKA_IBM_CV                             (CKA_VENDOR_DEFINED + 0x10006)
#define CKA_IBM_MACKEY                         (CKA_VENDOR_DEFINED + 0x10007)
#define CKA_IBM_USE_AS_DATA                    (CKA_VENDOR_DEFINED + 0x10008)
#define CKA_IBM_STRUCT_PARAMS                  (CKA_VENDOR_DEFINED + 0x10009)
#define CKA_IBM_STD_COMPLIANCE1                (CKA_VENDOR_DEFINED + 0x1000a)
#define CKA_IBM_PROTKEY_EXTRACTABLE            (CKA_VENDOR_DEFINED + 0x1000c)
#define CKA_IBM_PROTKEY_NEVER_EXTRACTABLE      (CKA_VENDOR_DEFINED + 0x1000d)
#define CKA_IBM_DILITHIUM_KEYFORM              (CKA_VENDOR_DEFINED + 0xd0001)
#define CKA_IBM_DILITHIUM_RHO                  (CKA_VENDOR_DEFINED + 0xd0002)
#define CKA_IBM_DILITHIUM_SEED                 (CKA_VENDOR_DEFINED + 0xd0003)
#define CKA_IBM_DILITHIUM_TR                   (CKA_VENDOR_DEFINED + 0xd0004)
#define CKA_IBM_DILITHIUM_S1                   (CKA_VENDOR_DEFINED + 0xd0005)
#define CKA_IBM_DILITHIUM_S2                   (CKA_VENDOR_DEFINED + 0xd0006)
#define CKA_IBM_DILITHIUM_T0                   (CKA_VENDOR_DEFINED + 0xd0007)
#define CKA_IBM_DILITHIUM_T1                   (CKA_VENDOR_DEFINED + 0xd0008)
#define CKA_IBM_DILITHIUM_MODE                 (CKA_VENDOR_DEFINED + 0x00010)
#define CKA_IBM_CCA_AES_KEY_MODE               (CKA_VENDOR_DEFINED + 0xd0101)
#define CKA_IBM_OPAQUE_PKEY                    (CKA_VENDOR_DEFINED + 0xd0100)
#define CKA_IBM_KYBER_MODE                     (CKA_VENDOR_DEFINED + 0x0000E)
#define CKA_IBM_KYBER_KEYFORM                  (CKA_VENDOR_DEFINED + 0xd0009)
#define CKA_IBM_KYBER_PK                       (CKA_VENDOR_DEFINED + 0xd000A)
#define CKA_IBM_KYBER_SK                       (CKA_VENDOR_DEFINED + 0xd000B)

#define CKM_IBM_SHA3_224                       (CKM_VENDOR_DEFINED + 0x10001)
#define CKM_IBM_SHA3_256                       (CKM_VENDOR_DEFINED + 0x10002)
#define CKM_IBM_SHA3_384                       (CKM_VENDOR_DEFINED + 0x10003)
#define CKM_IBM_SHA3_512                       (CKM_VENDOR_DEFINED + 0x10004)
#define CKM_IBM_CMAC                           (CKM_VENDOR_DEFINED + 0x10007)
#define CKM_IBM_EC_X25519                      (CKM_VENDOR_DEFINED + 0x1001b)
#define CKM_IBM_ED25519_SHA512                 (CKM_VENDOR_DEFINED + 0x1001c)
#define CKM_IBM_EC_X448                        (CKM_VENDOR_DEFINED + 0x1001e)
#define CKM_IBM_ED448_SHA3                     (CKM_VENDOR_DEFINED + 0x1001f)
#define CKM_IBM_DILITHIUM                      (CKM_VENDOR_DEFINED + 0x10023)
#define CKM_IBM_KYBER                          (CKM_VENDOR_DEFINED + 0x10024)
#define CKM_IBM_SHA3_224_HMAC                  (CKM_VENDOR_DEFINED + 0x10025)
#define CKM_IBM_SHA3_256_HMAC                  (CKM_VENDOR_DEFINED + 0x10026)
#define CKM_IBM_SHA3_384_HMAC                  (CKM_VENDOR_DEFINED + 0x10027)
#define CKM_IBM_SHA3_512_HMAC                  (CKM_VENDOR_DEFINED + 0x10028)
#define CKM_IBM_ECDSA_OTHER                    (CKM_VENDOR_DEFINED + 0x10031)
#define CKM_IBM_ATTRIBUTEBOUND_WRAP            (CKM_VENDOR_DEFINED + 0x20004)
#define CKM_IBM_BTC_DERIVE                     (CKM_VENDOR_DEFINED + 0x70001)

/*
 * If the caller is using the PKCS#11 GNU calling convention, then we cater
 * to that here.
 */
#ifdef CRYPTOKI_GNU
#define CK_BYTE_PTR unsigned char *
#define CK_BYTE unsigned char

#define childKeyIndex child_key_index
#define pChainCode p_chain_code
#define ulChainCodeLen ul_cahin_code_len

#define ulVersion ul_version
#define bPrepend b_prepend
#define pCipher p_cipher
#define ulCipherLen ul_cipher_len
#define pSharedData p_shared_data
#define hSecret h_secret

#define hSignVerifyKey h_sign_verify_key
#endif

#define CKM_IBM_ECSDSA_RAND                3
#define CKM_IBM_ECSDSA_COMPR_MULTI         5

struct ck_ibm_ecdsa_other {
	CK_ULONG submechanism;
};

typedef struct ck_ibm_ecdsa_other CK_IBM_ECDSA_OTHER_PARAMS;

struct ck_ibm_btc_derive_params {
    CK_ULONG type;
    CK_ULONG childKeyIndex;
    CK_BYTE_PTR pChainCode;
    CK_ULONG ulChainCodeLen;
    CK_ULONG version;
};

typedef struct ck_ibm_btc_derive_params CK_IBM_BTC_DERIVE_PARAMS;

#define CK_IBM_BIP0032_HARDENED 0x80000000 // key index flag

#define CK_IBM_BIP0032_PRV2PRV 1
#define CK_IBM_BIP0032_PRV2PUB 2
#define CK_IBM_BIP0032_PUB2PUB 3
#define CK_IBM_BIP0032_MASTERK 4
#define CK_IBM_SLIP0010_PRV2PRV 5
#define CK_IBM_SLIP0010_PRV2PUB 6
#define CK_IBM_SLIP0010_PUB2PUB 7
#define CK_IBM_SLIP0010_MASTERK 8

#define CK_IBM_BTC_CHAINCODE_LENGTH 32

#define CK_IBM_BTC_DERIVE_PARAMS_VERSION_1 1

#define CK_IBM_KYBER_KEYFORM_ROUND2_768    1
#define CK_IBM_KYBER_KEYFORM_ROUND2_1024   2

#define CK_IBM_KYBER_KEM_VERSION           0

typedef CK_ULONG CK_IBM_KYBER_KDF_TYPE;
typedef CK_ULONG CK_IBM_KYBER_KEM_MODE;

#define CK_IBM_KYBER_KEM_ENCAPSULATE       1
#define CK_IBM_KYBER_KEM_DECAPSULATE       2

struct ck_ibm_kyber_params {
    CK_ULONG                ulVersion;
    CK_IBM_KYBER_KEM_MODE   mode;
    CK_IBM_KYBER_KDF_TYPE   kdf;
    CK_BBOOL                bPrepend;
    CK_BYTE                 *pCipher;
    CK_ULONG                ulCipherLen;
    CK_BYTE                 *pSharedData;
    CK_ULONG                ulSharedDataLen;
    CK_OBJECT_HANDLE        hSecret;
};

typedef struct ck_ibm_kyber_params CK_IBM_KYBER_PARAMS;

struct ck_ibm_attributebound_wrap {
	CK_OBJECT_HANDLE hSignVerifyKey;
};

typedef struct ck_ibm_attributebound_wrap CK_IBM_ATTRIBUTEBOUND_WRAP_PARAMS;

#ifdef CRYPTOKI_GNU
#undef CK_BYTE_PTR
#undef CK_BYTE

#undef childKeyIndex
#undef pChainCode
#undef ulChainCodeLen

#undef ulVersion
#undef bPrepend
#undef pCipher
#undef ulCipherLen
#undef pSharedData
#undef hSecret

#undef hSignVerifyKey
#endif

#endif /* CRYPTOKI_IBM_VENDOR_DEFINED */

#if defined(__cplusplus)
}
#endif

#endif	/* PKCS11_X_H_ */
