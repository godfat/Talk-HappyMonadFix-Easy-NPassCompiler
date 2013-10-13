{

{-# LANGUAGE DoRec #-}
-- vim: filetype=haskell

module Main where

import Control.Monad.State
import Data.List

import Lexer
import ParserState3
import Template

}

%name cheapParse
%tokentype { Token }
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

program :: { Int -> Parser ([Char], Int) }
  : code
    { \offset -> do
      let
        [prefixSize, postfixSize] = map (length . filter (== '\n')) [programTmplPrefix, programTmplPostfix]
      (body, bodySize) <- $1 (offset + prefixSize)
      return (programTmplPrefix ++ body ++ programTmplPostfix, prefixSize + bodySize + postfixSize)
    }

code :: { Int -> Parser ([Char], Int) }
  : funcs expr
    { \offset -> do
      let
        [prefixSize, postfixSize] = map (length . filter (== '\n')) [mainTmplPrefix, mainTmplPostfix]
      (funcs, funcsSize) <- $1 offset
      (body, bodySize) <- $2 (offset + funcsSize + prefixSize)
      resetLocal
      return (funcs ++ mainTmplPrefix ++ body ++ mainTmplPostfix, funcsSize + prefixSize + bodySize + postfixSize)
    }

funcs :: { Int -> Parser ([Char], Int) }
  : funcs func
    { \offset -> do
      (funcs, funcsSize) <- $1 offset
      (func, funcSize) <- $2 (offset + funcsSize)
      return (funcs ++ func, funcsSize + funcSize)
    }
  |
    { \_ -> return ("", 0) }

func :: { Int -> Parser ([Char], Int) }
  : 'function' I_IDENTITY '{' funcs expr '}'
    { \offset -> do
      (funcs, funcsSize) <- $4 offset
      funcLabel <- newFunc $2 (offset + funcsSize + 1)
      (body, bodySize) <- $5 (offset + funcsSize + 3)
      resetLocal
      return
        ( funcs ++
          "define i32 " ++ funcLabel ++ "(i32 %arg) {\n" ++
          "entry:\n" ++
          "  %val.addr = alloca i32\n" ++
          "  %con.addr = alloca i1\n" ++
          body ++
          "  %val = load i32* %val.addr\n" ++
          "  ret i32 %val\n" ++
          "}\n" ++
          "\n"
        , bodySize + funcsSize + 8
        )
    }

expr :: { Int -> Parser ([Char], Int) }
  : 'arg'
    { \_ -> return
      ( "  store i32 %arg, i32* %val.addr\n"
      , 2)
    }
  | I_NUMBER
    { \_ -> return
      ( "  store i32 " ++ show $1 ++ ", i32* %val.addr\n"
      , 1)
    }
  | I_IDENTITY '(' expr ')'
    { \offset -> do
      funcLabel <- lookupFunc $1
      (arg, argSize) <- $3 offset
      argR <- nextLocal
      valR <- nextLocal
      return
        (
          arg ++
          "  " ++ argR ++ " = load i32* %val.addr\n" ++
          "  " ++ valR ++ " = call " ++ "i32 " ++ funcLabel ++ "(i32 " ++ argR ++ ")\n" ++
          "  store i32 " ++ valR ++ ", i32* %val.addr\n"
        , argSize + 3
        )
    }
  | expr '+' expr
    { binaryOp $1 $3 $ \valR lhsR rhsR ->
        return
          ( "  " ++ valR ++ " = add nsw i32 " ++ lhsR ++ ", " ++ rhsR ++ "\n" ++
            "  store i32 " ++ valR ++ ", i32* %val.addr\n"
          , 2
          )
    }
  | expr '-' expr
    { binaryOp $1 $3 $ \valR lhsR rhsR ->
        return
          ( "  " ++ valR ++ " = sub nsw i32 " ++ lhsR ++ ", " ++ rhsR ++ "\n" ++
            "  store i32 " ++ valR ++ ", i32* %val.addr\n"
          , 2
          )
    }
  | expr '<=' expr
    { binaryOp $1 $3 $ \valR lhsR rhsR ->
        return
          ( "  " ++ valR ++ " = icmp sle i32 " ++ lhsR ++ ", " ++ rhsR ++ "\n" ++
            "  store i1 " ++ valR ++ ", i1* %con.addr\n"
          , 2
          )
    }
  | 'if' '(' expr ')' '{' expr '}' 'else' '{' expr '}'
    { \offset -> do
      (cond, condSize) <- $3 offset
      cmpR <- nextLocal
      (positive, positiveSize) <- $6 (offset + condSize + 3)
      (negative, negativeSize) <- $10 (offset + condSize + 3 + positiveSize + 2)
      let ifOffset = offset + condSize + 3 + positiveSize + 2
          elseOffset = ifOffset + negativeSize
      ifLabel <- liftM ("if." ++) $ nextLabel ifOffset
      elseLabel <- liftM ("else." ++) $ nextLabel elseOffset
      endLabel <- liftM ("end." ++) $ nextLabel elseOffset
      return
        ( cond ++
          "  " ++ cmpR ++ " = load i1* %con.addr\n" ++
          "  br i1 " ++ cmpR ++ ", label %" ++ ifLabel ++ ", label %" ++ elseLabel ++ "\n" ++
          "\n" ++
          ifLabel ++ ":\n" ++
          positive ++
          "  br label %" ++ endLabel ++ "\n" ++
          "\n" ++
          elseLabel ++ ":\n" ++
          negative ++
          "  br label %" ++ endLabel ++ "\n" ++
          "\n" ++
          endLabel ++ ":\n"
        , elseOffset + 10
        )
    }

{

type Expr = Int -> Parser ([Char], Int)

binaryOp :: Expr -> Expr
         -> ([Char] -> [Char] -> [Char] -> Parser ([Char], Int))
         -> Expr
binaryOp lhs rhs op offset = do
  (arg1, arg1Size) <- lhs offset
  lhsR <- nextLocal
  (arg2, arg2Size) <- rhs (offset + arg1Size)
  rhsR <- nextLocal
  valR <- nextLocal
  (body, bodySize) <- op valR lhsR rhsR
  return
    ( arg1 ++
      "  " ++ lhsR ++ " = load i32* %val.addr\n" ++
      arg2 ++
      "  " ++ rhsR ++ " = load i32* %val.addr\n" ++
      body
    , arg1Size + arg2Size + bodySize + 2
    )


parseError :: [Token] -> a
parseError tks = error $ "parseError: " ++ show tks

main = do
  source <- getContents
  rec ((result, _), parserState) <- runStateT (cheapParse (lexer source) 0) (initParserState (parserFuncTable parserState))
  putStr result

}
