%{
/* lexer for RNPL */
/* $Header: /home/cvs/rnpl/src/rnpl.l,v 1.1.1.1 2013/07/09 00:38:27 cvs Exp $ */
/* Copyright (c) 1994-1998 by Robert Marsa */

#include <stdlib.h>
#include "rnpl.h"
#include "rnpl.tab.h"

int lineno=1;

%}

%p 3500

word ([a-zA-Z_][a-zA-Z_0-9]*)

%%

[ \t] ; /* ignore whitespace */

^#.* ; /* ignore comments (must start line) */

\/\/.* ; /* ignore comments (c++ like) */

(int|INT) {
  rnpllval.inum=INT;
  return INT;
}

(float|FLOAT) {
  rnpllval.inum=FLOAT;
  return FLOAT;
}

(complex|COMPLEX) {
  rnpllval.inum=COMPLEX;
  return COMPLEX;
}

(string|STRING) {
  rnpllval.inum=STRING;
  return STRING;
}

(ivec|IVEC) {
  rnpllval.inum=IVEC;
  return IVEC;
}

(constant|CONSTANT) {
  rnpllval.inum=CONST;
  return CONST;
}

(system|SYSTEM) {
  rnpllval.inum=SYS;
  return SYS;
}

(parameter|PARAMETER) {
  rnpllval.inum=PARAM;
  return PARAM;
}

(attribute|ATTRIBUTE) {
  rnpllval.inum=ATTRIB;
  return ATTRIB;
}

(encodeone|ENCODEONE) {
  rnpllval.inum=ENCODEONE;
  return ENCODEONE;
}

(encodeall|ENCODEALL) {
  rnpllval.inum=ENCODEALL;
  return ENCODEALL;
}

(looper|LOOPER) {
  rnpllval.inum=LOOPER;
  return LOOPER;
}

(coordinates|COORDINATES) {
  rnpllval.inum=COORDINATES;
  return COORDINATES;
}

(grid|GRID) {
  rnpllval.inum=GRID;
  return GRID;
}

(on|ON) {
  rnpllval.inum=ON;
  return ON;
}

(at|AT) {
  rnpllval.inum=AT;
  return AT;
}

(alias|ALIAS) {
  rnpllval.inum=ALIAS;
  return ALIAS;
}

(expand|EXPAND) {
  rnpllval.inum=EXPAND;
  return EXPAND;
}

(operator|OPERATOR) {
  rnpllval.inum=OPERATOR;
  return OPERATOR;
}

(residual|RESIDUAL) {
  rnpllval.inum=RESID;
  return RESID;
}

(evaluate|EVALUATE) {
  rnpllval.inum=EVALUATE;
  return EVALUATE;
}

(auto|AUTO) {
  rnpllval.inum=AUTO;
  return AUTO;
}

(stub|STUB) {
  rnpllval.inum=STUB;
  return STUB;
}

(static|STATIC) {
  rnpllval.inum=STATIC;
  return STATIC;
}

(header|HEADER) {
  rnpllval.inum=HEADER;
  return HEADER;
}

(work|WORK) {
  rnpllval.inum=WORK;
  return WORK;
}

(if|IF) {
  rnpllval.inum=IF;
  return IF;
}

(then|THEN) {
  rnpllval.inum=THEN;
  return THEN;
}

(else|ELSE) {
  rnpllval.inum=ELSE;
  return ELSE;
}

(:=) {
  rnpllval.inum=ASSIGNOP;
  return ASSIGNOP;
}

(\") {
  rnpllval.inum=QUOTE;
  return QUOTE;
}

"+" {
  rnpllval.op='+';
  return PLUS;
}

"=" {
  rnpllval.op='=';
  return EQUALS;
}

"-" {
  rnpllval.op='-';
  return MINUS;
}

"*" {
  rnpllval.op='*';
  return TIMES;
}

\/ {
  rnpllval.op='/';
  return DIVIDE;
}

\x25 {
  rnpllval.op='%';
  return MODULUS;
}

("^")|("**") {
  rnpllval.op='^';
  return CARET;
}

"==" {
  rnpllval.op='e';
  return EQUIV;
}

(\<) {
  rnpllval.op='<';
  return LESS;
}

(\>) {
  rnpllval.op='>';
  return GREAT;
}

"<=" {
  rnpllval.op='l';
  return LESSEQ;
}

">=" {
  rnpllval.op='g';
  return GREATEQ;
}

"!=" {
  rnpllval.op='n';
  return NOTEQ;
}

"||" {
  rnpllval.op='o';
  return OR;
}

"&&" {
  rnpllval.op='a';
  return AND;
}

"#" {
  rnpllval.inum=POUND;
  return POUND;
}

(\,) {
  rnpllval.inum=COMMA;
  return COMMA;
}

(\[) {
  rnpllval.inum=OBRACK;
  return OBRACK;
}

(\]) {
  rnpllval.inum=CBRACK;
  return CBRACK;
}

(\{) {
  rnpllval.inum=OBRACE;
  return OBRACE;
}

(\}) {
  rnpllval.inum=CBRACE;
  return CBRACE;
}

(\() {
  rnpllval.inum=OPAREN;
  return OPAREN;
}

(\)) {
  rnpllval.inum=CPAREN;
  return CPAREN;
}

(\:) {
  rnpllval.inum=COLON;
  return COLON;
}

(\;) {
  rnpllval.inum=SCOLON;
  return SCOLON;
}

(update|UPDATE|updates|UPDATES) {
  rnpllval.inum=UPDATE;
  return UPDATE;
}

(initialize|INITIALIZE|initializes|INITIALIZES) {
  rnpllval.inum=INITIALIZE;
  return INITIALIZE;
}

(uniform|UNIFORM) {
  rnpllval.inum=UNIFORM;
  return UNIFORM;
}

(nonuniform|NONUNIFORM) {
  rnpllval.inum=NONUNIFORM;
  return NONUNIFORM;
}

(\<[ ]*-?[0-9]+[ ]*\>) {
  rnpllval.inum = atoi((const char *)(rnpltext+1));
  return TIME;
}

([0-9]+) {
  rnpllval.inum = atoi((const char *)rnpltext);
  return INUM;
}

(([0-9]*\.[0-9]+)([eE][+-]?[0-9]+)?) {
  sscanf((const char *)rnpltext,"%lf",&rnpllval.num);
  return NUM;
}

{word}((\.{word})|("->"{word}))* {
  rnpllval.str=(char *)malloc(strlen((const char *)rnpltext)+1);
  strcpy(rnpllval.str,(const char *)rnpltext);
  return IDEN;
}

(\"[^\"\n]*\") {
  rnpllval.str = (char *)malloc(strlen((const char *)rnpltext)-1);
  strncpy(rnpllval.str,(const char *)rnpltext+1,strlen((const char *)rnpltext)-2);
  rnpllval.str[strlen((const char *)rnpltext)-2]=0;
  return STR;
}

\n { lineno++;}

%%

void rnplerror(const char *msg)
{
  fprintf(stderr,"RNPLERROR:%d: %s at '%s'\n",lineno,msg,rnpltext);
  exit(1);
}

int rnplwrap()
{
	return 1;
}
