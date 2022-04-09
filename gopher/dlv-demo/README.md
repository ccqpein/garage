# README #

```
dlv debug .
(dlv) break main.main
(dlv) continue
```

**installation**
`go install github.com/go-delve/delve/cmd/dlv@latest`

**Open the server**
`dlv debug --headless --listen=:2345 --api-version=2 --accept-multiclient`

**run dap-mode**

select `Go Launch File Configuration`

**run test**
test files use "Go Launch File Configuration"


**run in subpkg**

