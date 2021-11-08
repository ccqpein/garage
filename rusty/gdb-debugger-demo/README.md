# README #

```elisp
(dap-register-debug-template
  "GDB::Run"
  (list :type "gdb"
        :request "launch"
        :name "GDB::Run"
        :miDebuggerPath "rust-gdb"
        :program "${workspaceFolder}/target/debug/gdb-debugger-demo"
        :target "${workspaceFolder}/target/debug/gdb-debugger-demo"
        :cwd "${workspaceFolder}"
        :dap-compilation "cargo build"
        :dap-compilation-dir "${workspaceFolder}"))


(dap-register-debug-template
  "LLDB::Run"
  (list :type "lldb"
        :request "launch"
        :name "LLDB::Run"
        :miDebuggerPath "rust-lldb"
        :program "${workspaceFolder}/target/debug/gdb-debugger-demo"
        :target "${workspaceFolder}/target/debug/gdb-debugger-demo"
        :cwd "${workspaceFolder}"
        :dap-compilation "cargo build"
        :dap-compilation-dir "${workspaceFolder}"))

```

## Useful links ##

https://code.visualstudio.com/docs/cpp/lldb-mi
https://rustc-dev-guide.rust-lang.org/debugging-support-in-rustc.html#lldb

https://gagbo.net/post/dap-mode-rust/
