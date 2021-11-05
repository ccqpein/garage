# README #

## Sign gdb ##

| Step                                                                                                                                         | Check                                  |
|:---------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------|
| [Create a certificate in the System Keychain](https://sourceware.org/gdb/wiki/PermissionsDarwin#Create_a_certificate_in_the_System_Keychain) | `security find-certificate -c gdb-cert | grep System.keychain` && `security find-certificate -p -c gdb-cert | openssl x509 -checkend 0` |
| [Trust the certificate for code signing](https://sourceware.org/gdb/wiki/PermissionsDarwin#Trust_the_certificate_for_code_signing)           | `security dump-trust-settings -d`      |
| [Sign and entitle gdb using the certificate](https://sourceware.org/gdb/wiki/PermissionsDarwin#Sign_and_entitle_the_gdb_binary)              | `codesign -vv $(which gdb)` && `codesign -d --entitlements :- $(which gdb) | grep -a com.apple.security.cs.debugger`|
|[Refresh the system's certificates and code-signing data](https://sourceware.org/gdb/wiki/PermissionsDarwin#Refresh_the_system.27s_certificates_and_code-signing_data)| None known (besides checking that your gdb now works)|

use this script `https://github.com/conda-forge/gdb-feedstock/blob/master/recipe/macos-codesign/macos-setup-codesign.sh`

the script upper isn't all. 

need generate `gdb-entitlement.xml` file: 

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
"http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>com.apple.security.cs.allow-jit</key>
    <true/>
    <key>com.apple.security.cs.allow-unsigned-executable-memory</key>
    <true/>
    <key>com.apple.security.cs.allow-dyld-environment-variables</key>
    <true/>
    <key>com.apple.security.cs.disable-library-validation</key>
    <true/>
    <key>com.apple.security.cs.disable-executable-page-protection</key>
    <true/>
    <key>com.apple.security.cs.debugger</key>
    <true/>
    <key>com.apple.security.get-task-allow</key>
    <true/>
</dict>
</plist>
```

then `codesign --entitlements gdb-entitlement.xml -fs gdb_codesign $(which gdb)`

## Commands ##

> go build -gcflags "-N -l" .

> go build -ldflags "-s -w"
