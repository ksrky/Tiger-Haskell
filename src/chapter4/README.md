# Tiger in Haskell - Chapter 4

## REPL

```command
$ stack exec chapter4
```

## File test

```command
$ stack exec chapter4 <args>
```

where _args_ := all | [*file_number*]\*

## Implematation notes

- Using Alex ([user guide](https://haskell-alex.readthedocs.io/en/latest/)) as a lexer analyzer generator
  > Alex is a tool for generating lexical analysers in Haskell, given a description of the tokens to be recognised in the form of regular expressions. It is similar to the tools lex and flex for C/C++.
- Using Happy ([Happy User Guide](https://www.haskell.org/happy/doc/html/)) as a parser generator
  > Happy is a parser generator system for Haskell, similar to the tool yacc for C. Like yacc, it takes a file containing an annotated BNF specification of a grammar and produces a Haskell module containing a parser for the grammar.
- Function and type definitions 　\
  In the original specification, only a sequence of function definitions can be recursive functions, but not a sequence of type definitions. Since I cannot come up with a rational reason for such a specification, I have made it so that any function or type defined in the same level can be recursive. By doing so, the parser become more simple. Therefore, `Syntac.Absyn.Dec` consists of a variable definition, a function definition, or a type definition, and any functions definition and type defenitions in the list `[Syntax.Absyn.Dec]` can be recursive. As a side effect of this, functions and types with the same name defined in the same level throw errors during the process of semantic analysis (see test47.tig and test48.tig)
- `Syntax.Absyn.SeqExp` \
  Since all `Exp` have `Pos`, the `Pos` assigned to each `Exp` won't be used. So I changed `SeqExp [(Exp, Pos)]` into `SeqExp [Exp] Pos`.

Translated with www.DeepL.com/Translator (free version)
