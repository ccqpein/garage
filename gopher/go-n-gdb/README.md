# README #

## Sign gdb ##

| Step                                                                                                                                         | Check                                  |
|:---------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------|
| [Create a certificate in the System Keychain](https://sourceware.org/gdb/wiki/PermissionsDarwin#Create_a_certificate_in_the_System_Keychain) | `security find-certificate -c gdb-cert | grep System.keychain` && `security find-certificate -p -c gdb-cert | openssl x509 -checkend 0` |
| [Trust the certificate for code signing](https://sourceware.org/gdb/wiki/PermissionsDarwin#Trust_the_certificate_for_code_signing)           | `security dump-trust-settings -d`      |
| [Sign and entitle gdb using the certificate](https://sourceware.org/gdb/wiki/PermissionsDarwin#Sign_and_entitle_the_gdb_binary)              | `codesign -vv $(which gdb)` && `codesign -d --entitlements :- $(which gdb) | grep -a com.apple.security.cs.debugger`|
|[Refresh the system's certificates and code-signing data](https://sourceware.org/gdb/wiki/PermissionsDarwin#Refresh_the_system.27s_certificates_and_code-signing_data)| None known (besides checking that your gdb now works)|

## Commands ##

> go build -gcflags "-N -l" .

> go build -ldflags "-s -w"
