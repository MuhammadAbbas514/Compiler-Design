%{
#include <string>
#include <iostream>
#include "parser.tab.h"
#include "ast.h"

extern int yyerror(const char* msg);
extern int lineNum;
int lineNum = 1;

// For string literal handling
std::string string_buffer;
%}

%option noyywrap

/* States for handling string literals and comments */
%x STRING_STATE
%x CHAR_STATE
%x SINGLE_COMMENT
%x MULTI_COMMENT

%%

    /* Keywords */
"int"       { return INT; }
"bool"      { return BOOL; }
"string"    { return STRING; }
"char"      { return CHAR; }
"if"        { return IF; }
"else"      { return ELSE; }
"while"     { return WHILE; }
"return"    { return RETURN; }
"true"      { yylval.bool_val = true; return BOOL_LITERAL; }
"false"     { yylval.bool_val = false; return BOOL_LITERAL; }
"and"       { return AND; }
"or"        { return OR; }
"not"       { return NOT; }
"void"      { return VOID; }
"print"     { return PRINT; }
"scan"      { return SCAN; }
"main"      { return MAIN; }

    /* Identifiers */
[A-Za-z][A-Za-z0-9_]*  { 
    yylval.id = strdup(yytext); 
    return ID; 
}

    /* Integer literals */
[0-9]+  { 
    yylval.int_val = atoi(yytext); 
    return INT_LITERAL; 
}

    /* String literals */
\"      { 
    string_buffer.clear(); 
    BEGIN(STRING_STATE); 
}

<STRING_STATE>\"    { 
    BEGIN(INITIAL); 
    yylval.str_val = strdup(string_buffer.c_str()); 
    return STRING_LITERAL; 
}

<STRING_STATE>\n    { 
    lineNum++; 
    string_buffer += '\n'; 
}

<STRING_STATE>\\\"  { string_buffer += '\"'; }
<STRING_STATE>\\n   { string_buffer += '\n'; }
<STRING_STATE>\\t   { string_buffer += '\t'; }
<STRING_STATE>\\\\  { string_buffer += '\\'; }
<STRING_STATE>.     { string_buffer += yytext[0]; }

    /* Character literals */
\'      { 
    BEGIN(CHAR_STATE); 
}

<CHAR_STATE>\'    { 
    BEGIN(INITIAL); 
    return CHAR_LITERAL; 
}

<CHAR_STATE>\\\'  { yylval.char_val = '\''; }
<CHAR_STATE>\\n   { yylval.char_val = '\n'; }
<CHAR_STATE>\\t   { yylval.char_val = '\t'; }
<CHAR_STATE>\\\\  { yylval.char_val = '\\'; }
<CHAR_STATE>.     { yylval.char_val = yytext[0]; }

    /* Operators */
"+"     { return '+'; }
"-"     { return '-'; }
"*"     { return '*'; }
"/"     { return '/'; }
"%"     { return '%'; }
":="    { return ASSIGN; }
"<"     { return '<'; }
">"     { return '>'; }
"<="    { return LE; }
">="    { return GE; }
"=="    { return EQ; }
"!="    { return NE; }

    /* Delimiters */
";"     { return ';'; }
","     { return ','; }
"{"     { return '{'; }
"}"     { return '}'; }
"("     { return '('; }
")"     { return ')'; }

    /* Comments */
"//"    { BEGIN(SINGLE_COMMENT); }
<SINGLE_COMMENT>\n    { 
    lineNum++; 
    BEGIN(INITIAL); 
}
<SINGLE_COMMENT>.     { /* Ignore comment content */ }

"/*"    { BEGIN(MULTI_COMMENT); }
<MULTI_COMMENT>"*/"   { BEGIN(INITIAL); }
<MULTI_COMMENT>\n     { lineNum++; }
<MULTI_COMMENT>.      { /* Ignore comment content */ }

    /* Whitespace */
[ \t]+  { /* Ignore whitespace */ }
\n      { lineNum++; }

    /* Error handling */
.       { 
    std::cerr << "Error: Unexpected character '" << yytext << "' at line " << lineNum << std::endl; 
    return ERROR; 
}

%%
