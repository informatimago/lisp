# -*- mode:shell-script -*-

pkcs11_module=/usr/local/lib/libiaspkcs11.so
pkcs11_module=/usr/local/lib/opensc-pkcs11.so


pkcs11-tool --module "${pkcs11_module}"  --list-objects    > objects.txt
pkcs11-tool --module "${pkcs11_module}"  --list-slots      > slots.txt
pkcs11-tool --module "${pkcs11_module}"  --list-mechanisms > mechanisms.txt


pkcs11-tool --module "${pkcs11_module}" --read-object --type pubkey --id e828bd080fd2500000104d494f4300010101 > 'Clé d'\''authentification1.public.der'
pkcs11-tool --module "${pkcs11_module}" --read-object --type pubkey --id e828bd080fd2500000104d494f4300010103 > 'Clé de signature1.public.der'
pkcs11-tool --module "${pkcs11_module}" --read-object --type cert   --id e828bd080fd2500000104d494f4300010101 > 'Clé d'\''authentification1.cert.der'
pkcs11-tool --module "${pkcs11_module}" --read-object --type cert   --id e828bd080fd2500000104d494f4300010103 > 'Clé de signature1.cert.der'

for f in *.cert.der ; do openssl x509 -in "${f}" -inform DER -out "${f/.der/.pem}" ; done



pkcs11_module="/usr/local/lib/opensc-pkcs11.so"
pkcs11_module="/usr/local/lib/libiaspkcs11.so"
encr_key="e828bd080fd2500000104d494f4300010101"
sign_key="e828bd080fd2500000104d494f4300010103"
sign_mec="SHA1-RSA-PKCS"
sign_mec="RSA-PKCS"

echo "data to sign (max 100 bytes)" > "data"

# prepare data with padding
( echo -ne "\x00\x01" && for i in `seq 224`; do echo -ne "\xff"; done && echo -ne "\00" && cat "data") > "data_pad"

# sign data
pkcs11-tool --id "${sign_key}" --sign --pin "${pin}" --mechanism RSA-X-509 --module "${pkcs11_module}" < "data_pad" > "data_pad.sig"
pkcs11-tool --id "${sign_key}" --sign                --mechanism RSA-X-509 --module "${pkcs11_module}" < "data_pad" > "data_pad.sig"

# verify signature
openssl rsautl -verify -inkey "${sign_key}.pub" -in "data_pad.sig" -pubin -raw





echo "data to encrpyt should be longer, better, faster and whatever we need to hide in front of nasty eyes of the ones that should not see them. " > "data"


# Get certificate
pkcs11-tool -r -p "${pin}" --id "${encr_key}" --type cert --module  "${pkcs11_module}" > "${encr_key}.cert"
pkcs11-tool -r             --id "${encr_key}" --type cert --module  "${pkcs11_module}" > "${encr_key}.cert"

# convert certificate to pem:
openssl x509 -inform DER -in "${encr_key}.cert" -pubkey > "${encr_key}.pub"












# Sign/Verify using private key/certificate

# Create a "data" to sign

echo "data to sign (max 100 bytes)" > "data"

# Get the certificate from the card:

pkcs11-tool -r -p "${pin}" --id "${sign_key}" --type cert --module "${pkcs11_module}" > "${sign_key}.cert"

# Convert it to the public key (PEM format)

openssl x509 -inform DER -in "${sign_key}.cert" -pubkey > "${sign_key}.pub"

# or

# Get the public key from the card:

pkcs11-tool -r -p "${pin}" --id "${sign_key}" --type pubkey --module "${pkcs11_module}" > "${sign_key}.der"

# Convert it to PEM format:

openssl rsa -inform DER -outform PEM -in "${sign_key}.der" -pubin > "${sign_key}.pub"

## RSA-PKCS

# Sign the "data" on the smartcard using private key:

cat "data" | pkcs11-tool --id "${sign_key}" -s -p "${pin}" -m RSA-PKCS --module "${pkcs11_module}" > "data.sig"

#Verify

openssl rsautl -verify -inkey "${sign_key}.pub" -in "data.sig" -pubin

## SHA1-RSA-PKCS

# Sign the "data" on the smartcard using private key:

cat "data" | pkcs11-tool --id "${sign_key}" -s -p "${pin}" -m SHA1-RSA-PKCS --module "${pkcs11_module}" > "data.sig"

# Verify and parse the returned ASN1 structure:

openssl rsautl -verify -inkey "${sign_key}.pub" -in "data.sig" -pubin | openssl asn1parse -inform DER

# Compare the result with the sha1 sum of the input file:

sha1sum "data"

# Similarily can be tested the SHA256, SHA384 and SHA512, just by replacing SHA1 with these hashes in above commands.

## SHA1-RSA-PKCS-PSS

# Sign the "data" on the smartcard using private key:

cat "data" | pkcs11-tool --id "${sign_key}" -s -p "${pin}" -m SHA1-RSA-PKCS-PSS --module "${pkcs11_module}" > "data.sig"

# Verify

openssl dgst -keyform DER -verify "${sign_key}.pub" -sha1 -sigopt rsa_padding_mode:pss -sigopt rsa_pss_saltlen:-1 -signature "data.sig" "data"

# For other parameters, replace the hash algorithsm, add a `--salt-len` parameter for the `pkcs11-tool` and adjust `rsa_pss_saltlen` argument of `openssl`.

## RSA-X-509

# Prepare "data" with padding:

(echo -ne "\x00\x01" && for i in `seq 224`; do echo -ne "\xff"; done && echo -ne "\00" && cat "data") > "data_pad"

# Sign the "data" on the smartcard using private key:

pkcs11-tool --id "${sign_key}" -s -p "${pin}" -m RSA-X-509 --module "${pkcs11_module}" <  "data_pad" > "data_pad.sig"

# Verify

openssl rsautl -verify -inkey "${sign_key}.pub" -in "data_pad.sig" -pubin -raw


# Encrypt/Decrypt using private key/certificate

# Create a "data" to encrypt

echo "data to encrpyt should be longer, better, faster and whatever we need to hide in front of nasty eyes of the ones that should not see them. " > "data"

# Get the certificate from the card:

pkcs11-tool -r -p "${pin}" --id "${encr_key} "--type cert --module "${pkcs11_module}" > "${encr_key}.cert"

# Convert it to the public key (PEM format)

openssl x509 -inform DER -in "${encr_key}.cert" -pubkey > "${encr_key}.pub"

## RSA-PKCS

# Encrypt the "data" locally

openssl rsautl -encrypt -inkey "${encr_key}.pub" -in "data" -pubin -out "data.crypt"

# Decrypt the "data" on the card

pkcs11-tool --id "${encr_key} "--decrypt -p "${pin}" -m RSA-PKCS --module "${pkcs11_module}" <  "data.crypt"

## RSA-X-509

# Prepare "data" with padding:

(echo -ne "\x00\x02" && for i in `seq 113`; do echo -ne "\xff"; done && echo -ne "\00" && cat "data") > "data_pad"

# Encrypt the "data" locally

openssl rsautl -encrypt -inkey "${encr_key}.pub" -in "data_pad" -pubin -out "data_pad.crypt" -raw

# Decrypt the "data" on the card

pkcs11-tool --id "${encr_key} "--decrypt -p "${pin}" -m RSA-X-509 --module "${pkcs11_module}" < "data_pad.crypt"

## RSA-PKCS-OAEP

# Encrypt the "data" locally

openssl rsautl -encrypt -inkey "${encr_key}.pub" -in "data" -pubin -out "data.crypt" -oaep

#   or

openssl pkeyutl -encrypt -inkey "${encr_key}.pub" -pubin -pkeyopt rsa_padding_mode:oaep -pkeyopt rsa_oaep_md:sha256 -pkeyopt rsa_mgf1_md:sha256 -in "data" -out "data.sha256.crypt"

# Decrypt the "data" on the card

pkcs11-tool --id "${encr_key} "--decrypt -p "${pin}" -m RSA-PKCS-OAEP --module "${pkcs11_module}" < "data.crypt"

#    or

pkcs11-tool --id "${encr_key} "--decrypt -p "${pin}" -m RSA-PKCS-OAEP --hash-algorithm=sha256  --module "${pkcs11_module}" < "data.sha256.crypt"


