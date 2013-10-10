{

module Main where

-- vim: filetype=haskell

import Control.Monad.State

import Lexer
import ParserState
import Template

}

%name cheapParse
%tokentype { Token }
%monad { Parser }
%error { parseError }

%token
  'function' { TokenFunction }
  '(' { TokenParL }
  ')' { TokenParR }
  '{' { TokenCurL }
  '}' { TokenCurR }
  I_IDENTITY { TokenIdentity $$ }
  I_NUMBER { TokenNum $$ }
  '+' { TokenPlus }
  '-' { TokenMinus }
  '<=' { TokenLessEqual }
  'if' { TokenIf }
  'else' { TokenElse }
  'arg' { TokenArg }

%left '<='
%left '+' '-'

%%

program :: { [Char] }
  : code
    { programTmpl $1
    }

code :: { [Char] }
  : expr
    {% resetLocal >> (return $ mainTmpl $1)
    }
  | func code
    { $1 ++
      $2
    }

func :: { [Char] }
  : 'function' I_IDENTITY '{' expr '}'
    {% do
      resetLocal
      funcLabel <- newFunc $2
      return $
        "define i32 " ++ funcLabel ++ "(i32 %arg) {\n" ++
        "entry:\n" ++
        "  %val.addr = alloca i32\n" ++
        $4 ++
        "  %val = load i32* %val.addr\n" ++
        "  ret i32 %val\n" ++
        "}\n\n"
    }

expr :: { [Char] }
  : 'arg'
    { "  store i32 %arg, i32* %val.addr\n"
    }
  | I_NUMBER
    { "  store i32 " ++ show $1 ++ ", i32* %val.addr\n"
    }
  | I_IDENTITY '(' expr ')'
    {% do
      funcLabel <- lookupFunc $1
      arg <- nextLocal
      val <- nextLocal
      return $
        $3 ++
        "  " ++ arg ++ " = load i32* %val.addr\n" ++
        "  " ++ val ++ " = call " ++ "i32 " ++ funcLabel ++ "(i32 " ++ arg ++ ")\n" ++
        "  store i32 " ++ val ++ ", i32* %val.addr\n"
    }
  | expr '+' expr
    {% do
      lhs <- nextLocal
      rhs <- nextLocal
      val <- nextLocal
      return $
        $1 ++
        "  " ++ lhs ++ " = load i32* %val.addr\n" ++
        $3 ++
        "  " ++ rhs ++ " = load i32* %val.addr\n" ++
        "  " ++ val ++ " = add nsw i32 " ++ lhs ++ ", " ++ rhs ++ "\n" ++
        "  store i32 " ++ val ++ ", i32* %val.addr\n"
    }
  | expr '-' expr
    { $1 ++
      $3 ++
      "popq %rbx\n" ++
      "popq %rax\n" ++
      "subq %rbx, %rax\n" ++
      "pushq %rax\n"
    }
  | expr '<=' expr
    {% do
      trueLabel <- nextLabel
      endLabel <- nextLabel
      return $
        $1 ++
        $3 ++
        "popq %rbx\n" ++
        "popq %rax\n" ++
        "cmpq %rbx, %rax\n" ++
        "jbe " ++ trueLabel ++ "\n" ++
        "pushq $0\n" ++
        "jmp " ++ endLabel ++ "\n" ++
        trueLabel ++ ":\n" ++
        "pushq $1\n" ++
        endLabel ++ ":\n"
    }
  | 'if' '(' expr ')' '{' expr '}' 'else' '{' expr '}'
    {% do
      elseLabel <- nextLabel
      endLabel <- nextLabel
      return $
        $3 ++
        "popq %rax\n" ++
        "cmpq $0, %rax\n" ++
        "jz " ++ elseLabel ++ "\n" ++
        $6 ++
        "jmp " ++ endLabel ++ "\n" ++
        elseLabel ++ ":\n" ++
        $10 ++
        endLabel ++ ":\n"
    }

{

parseError :: Monad m => [Token] -> ParserT m a
parseError tks = fail $ "parseError: " ++ show tks

main = do
  source <- getContents
  result <- evalStateT (cheapParse (lexer source)) initParserState
  putStr result

}
