# lambda
This is a lambda calculus parser and evaluator written in F#.

It is actually pretty much the same parser and evaluator twice: once as a set of modules with Lambda.fs as the entry point, and once as a script file (Lambda.fsx)

You can build the project using the build script. 

On Windows, do roughly this:

```
.paket/paket.exe install
build.cmd
build/Lambda.exe
```

On Mac OS X and maybe Linux, do roughly this:
```
mono .paket/paket.exe install
./build.sh
mono build/Lambda.exe
```

Now you can type lambda expressions into the REPL, say:

```
(λn.λf.λx.f (n f x)) (λf.λx.x)
```

Which should give you this back:

```
λf.λx.f x
```
