# README #

Can I update fasl file in runtime?

**Yes**

1. `(load "./main.lisp")` gonna load lib and run a swank server.
2. `(slime-connect)` connect to the swank server created upper
3. `(load "./lib-update.fasl")` update the runtime with `lib-update`
4. test changes

