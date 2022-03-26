# Tiger in Haskell

## Interactive test
```
$ stack ghci
ghci> Lexer.REPL.main
tiger-lexer>
ghci> Parser.REPL.main
tiger-parser>
```

## File test
```
$ stack ghci
ghci> Lexer.Test.main
[tiger-lexer]file name here: [file_name]
ghci> Parser.Test.main
[tiger-parser]file name here: [file_name]
ghci> Semant.Test.main
[tiger-typecheck]file name here: [file_name]
```
where *[file_name]* is a name of a file in *./testcases*