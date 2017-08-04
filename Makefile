RESULT =reversi
SOURCES=board.ml color.ml command.ml ai.ml commandParser.mly commandLexer.mll play.ml main.ml 
LIBS=unix 
all: byte-code 

-include OCamlMakefile 
