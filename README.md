# lambda

Lambda is an untyped lambda calculus interpreter written in OCaml. This interpreter supports 2 evaluation strategies, Full beta-reduction and Call by Value.

## Usage

```sh
$ dune exec lambda # default strategy is Full beta-reduction
$ dune exec lambda -- -v # output all reduction steps
$ dune exec lambda -- -s value # use Call by Value strategy
$ dune exec lambda -- --help # help
```

## Example

You can use [some macros](./src/cui.ml#L11-L31).

```
$ dune exec lambda
# (\x.x)x
→ x
# test tru v w
→ v
# scc c1
→ (λs. (λz. (s (s z))))
# factorial (times (scc (scc c0)) (scc c1))
→ (λs. (λz. (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))
```
