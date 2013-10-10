module Template
  ( programTmpl
  , mainTmpl
  , programTmplPrefix
  , programTmplPostfix
  , mainTmplPrefix
  , mainTmplPostfix
  ) where

programTmplPrefix =
  "@.r = private unnamed_addr constant [3 x i8] c\"%d\\00\", align 1\n" ++
  "@.w = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n" ++
  "\n" ++
  "declare i32 @scanf(i8*, ...)\n" ++
  "declare i32 @printf(i8*, ...)\n" ++
  "\n" ++
  "define i32 @read(i32 %arg) {\n" ++
  "entry:\n" ++
  "  %i = alloca i32\n" ++
  "  call i32 (i8*, ...)* @scanf(i8* getelementptr inbounds ([3 x i8]* @.r, i32 0, i32 0), i32* %i)\n" ++
  "  %val = load i32* %i\n" ++
  "  ret i32 %val\n" ++
  "}\n" ++
  "\n" ++
  "define i32 @write(i32 %arg) {\n" ++
  "entry:\n" ++
  "  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.w, i32 0, i32 0), i32 %arg)\n" ++
  "  ret i32 %call\n" ++
  "}\n" ++
  "\n"

programTmplPostfix = ""

programTmpl body =
  programTmplPrefix ++ body ++ programTmplPostfix

mainTmplPrefix =
  "define i32 @main() {\n" ++
  "entry:\n" ++
  "  %val.addr = alloca i32\n"

mainTmplPostfix =
  "  ret i32 0\n" ++
  "}\n"

mainTmpl body =
  mainTmplPrefix ++ body ++ mainTmplPostfix
