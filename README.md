[![Build Status](https://travis-ci.com/vijayphoenix/Compiler-written-in-Haskell.svg?token=67qmZmyfex1ST6G5tpZK&branch=master)](https://travis-ci.com/vijayphoenix/Compiler-written-in-Haskell)

# Implementing a JIT Compiled Language with Haskell   

## About  

The project aimed to implement a simple procedural language.     
We named it **HASKULL** :-)  
The frontend is written in Haskell and the backend it managed LLVM-hs-pure package.   
The project extensively uses Monads, State Monads, Applicative functors and Transformers.  

### Built With  

You will need GHC 7.8 or newer as well as LLVM 4.0.  

### Installation  

Clone the repository:    

```
git clone https://github.com/vijayphoenix/Compiler-written-in-Haskell.git
```

Browse to the directory where all the files of this repository are located.  

Run the following command to build the project.  

```
stack build 
```
# Usage  

Run the following command to get an interactive console.  

```
stack repl
```
Type "main" in the interactive console.  

Write any code using the following Syntax rules.   

```
Syntax rules: 
[A] means optional arg. of type A .

All the symbol starting with lower-case letter are terminal(lexer units).
All the operators are left associative

Command = Expr ;
Expr : DeclarationStmt| FuncCallStmt | LiteralStmt | ifthenStmt | (Expr)

DeclarationStmt : ExternDecl

ExternDecl : extern Name([ArgList]) : Type 

Type : int 
VList: Name[, VList]

FuncCallStmt : Call 
Call : Name ( [Args] ) 

BinOpCallStmt : BinOpCall
BinOpCall : Expr Op Expr 

Op : + | - | * | / | ; | <
reserved keywords: int char def extern string if then else
Args : Expr[, Args]

LiteralStmt : IntLiteral 
IntLiteral  : integer

Name : ident
ArgList : Type Name[, ArgList]

Func : def Name([ArgList]) : Type { Command-list }
Command-list = Command [Command-list]

Command = Expr ;
```
Some sample example functions are provided in examples.txt file.
For more insight on the language grammar, refer to Language.hs, AST.hs files.    


### Documentation  

This project is documented under [Haddock](https://www.haskell.org/haddock/#Documentation) Documentation  

To generate documentation run:  

```
stack haddock
```

### Authors  

* [**Vijay Tadikamalla**](https://github.com/vijayphoenix)  
* [**Anjani Kumar**](https://github.com/anjani-1)  
* [**Anupam Saini**](https://github.com/anupamsaini98)  
* [**Yogesh Singh**](https://github.com/yo5sh)  

#### License  
* [LICENSE](LICENSE)  

#### Acknowledgments  
* [Haskell LLVM JIT Compiler Tutorial](http://www.stephendiehl.com/llvm)  
