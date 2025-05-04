%{
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <cstring> 
#include <cstdlib> 
#include "ast.h"

extern int yylex();
extern FILE* yyin;
extern int lineNum;

void yyerror(const char* s);

std::vector<std::string> threeAddressCode;
std::string currentBaseType;
int tempVarCounter = 0;
int labelCounter = 0;
int parameterCount = 0;

std::string newTemp() {
    return "t" + std::to_string(tempVarCounter++);
}

std::string newLabel() {
    return "L" + std::to_string(labelCounter++);
}

struct SymbolInfo {
    std::string name;
    std::string type;
    bool isFunction;
    std::vector<std::string> paramTypes;
};

std::vector<SymbolInfo> symbolTable;
bool hasMainFunction = false;
std::string currentFunctionType;

bool addSymbol(const std::string& name, const std::string& type, bool isFunction = false) {
    for (const auto& symbol : symbolTable) {
        if (symbol.name == name) {
            return false; 
        }
    }
    
    symbolTable.push_back({name, type, isFunction});
    return true;
}

std::string inferType(const std::string &expr) {
	std::cout<<expr;
    
    if (expr.empty()) return "unknown"; 
    if (expr[0] == '"') return "string"; 
    try {
        std::stoi(expr); 
        return "int";
    } catch (std::invalid_argument&) {
        try {
            std::stof(expr); 
            return "float";
        } catch (std::invalid_argument&) {
            return "unknown"; 
        }
    }
}

SymbolInfo* findSymbol(const std::string& name) {
    for (auto& symbol : symbolTable) {
        if (symbol.name == name) {
            return &symbol;
        }
    }
    return nullptr;
}

std::string exprType;

struct LoopLabels {
    std::string startLabel;
    std::string endLabel;
};

std::vector<LoopLabels> loopLabelsStack;
%}

%union {
    int int_val;
    bool bool_val;
    char char_val;
    char* str_val;
    char* id;
    char* expr_val;
    char* type;
    int op_val;
}

%token INT BOOL STRING CHAR VOID
%token IF ELSE WHILE RETURN
%token PRINT SCAN MAIN
%token ASSIGN LE GE EQ NE
%token AND OR NOT
%token ERROR

%token <int_val> INT_LITERAL
%token <bool_val> BOOL_LITERAL
%token <str_val> STRING_LITERAL
%token <char_val> CHAR_LITERAL
%token <id> ID

%type <expr_val> Expression AndExpr NotExpr RelationalExpr AdditiveExpr MultExpr UnaryExpr Factor FunctionCall
%type <expr_val> ArgumentList Arguments
%type <type> ReturnType BaseType
%type <op_val> RelOp
%type <op_val> AddOp
%type <op_val> MultOp

%right ASSIGN
%left OR
%left AND
%right NOT
%left EQ NE
%left '<' '>' LE GE
%left '+' '-'
%left '*' '/' '%'
%right UNARY_MINUS

%%

Program : FunctionDeclarations MainFunction {
    if (!hasMainFunction) {
        yyerror("Error: No main function defined");
    }
}
;

MainFunction : INT MAIN '(' ')' {
    hasMainFunction = true;
    currentFunctionType = "int";
    if (!addSymbol("main", "int", true)) {
        yyerror("Error: Function main already defined");
    }
    threeAddressCode.push_back("func_begin main");
} CompoundStmt {
    threeAddressCode.push_back("func_end main");
}
;

FunctionDeclarations : FunctionDeclarations FunctionDecl
                      | /* empty */
;

FunctionDecl : ReturnType ID {
    currentFunctionType = $1;
    if (!addSymbol($2, $1, true)) {
        std::string error = "Error: Function " + std::string($2) + " already defined";
        yyerror(error.c_str());
    }
    std::string funcName = $2;
    threeAddressCode.push_back("func_begin " + funcName);
} '(' Parameters ')' CompoundStmt {
    std::string funcName = $2;
    threeAddressCode.push_back("func_end " + funcName);
    free($2);
}
;

ReturnType : VOID { $$ = strdup("void"); }
           | BaseType { $$ = $1; }
;

BaseType : INT { $$ = strdup("int"); currentBaseType = "int"; }
         | BOOL { $$ = strdup("bool"); currentBaseType = "bool"; }
         | STRING { $$ = strdup("string"); currentBaseType = "string"; }
         | CHAR { $$ = strdup("char"); currentBaseType = "char"; }
;

Parameters : ParameterList
           | /* empty */
;

ParameterList : Parameter
              | ParameterList ',' Parameter
;

Parameter : BaseType ID {
    if (!addSymbol($2, $1)) {
        std::string error = "Error: Parameter " + std::string($2) + " already defined";
        yyerror(error.c_str());
    }
    free($2);
}
;

CompoundStmt : '{' {
    
} LocalDeclarations StmtList '}' {
    
}
;

LocalDeclarations : VarDeclaration LocalDeclarations
                  | /* empty */
;

VarDeclaration : BaseType IdList ';' {
    free($1);
}
;

IdList : IdInit
       | IdInit ',' IdList
;

IdInit : ID {
    
    if (!addSymbol($1, currentBaseType)) {
        std::string error = "Error: Variable " + std::string($1) + " already defined";
        yyerror(error.c_str());
    }
    free($1);
} 
| ID ASSIGN Expression {
    SymbolInfo* symbol = findSymbol($1);
    

    if (!symbol) {
        if (!addSymbol($1, exprType)) {
            std::string error = "Error: Variable " + std::string($1) + " already defined";
            yyerror(error.c_str());
        }
    } else {
        if (symbol->type != exprType) {
            std::string error = "Error: Type mismatch in assignment to '" + std::string($1) +
                                "'. Expected type '" + symbol->type + "', but got type '" + exprType + "'";
            yyerror(error.c_str());
        }
    }

    threeAddressCode.push_back(std::string($1) + " := " + std::string($3));
    free($1);
}
;


StmtList : Statement StmtList
         | /* empty */
;

Statement : ExprStmt
          | CompoundStmt
          | SelectionStmt
          | IterationStmt
          | ReturnStmt
          | IOStmt
;

ExprStmt : ID ASSIGN Expression ';' {
    SymbolInfo* symbol = findSymbol($1);
    if (!symbol) {
        std::string error = "Error: Variable " + std::string($1) + " used before declaration";
        yyerror(error.c_str());
    } else {
        
        if (symbol->type != exprType) {
            std::string error = "Error: Type mismatch in assignment to '" + std::string($1) +
                                "'. Expected '" + symbol->type + "', but got '" + exprType + "'";
            yyerror(error.c_str());
        }
    }

    threeAddressCode.push_back(std::string($1) + " := " + std::string($3));
    free($1);
}
;


SelectionStmt : IF '(' Expression ')' {
    std::string elseLabel = newLabel();
    std::string endLabel = newLabel();
    
    threeAddressCode.push_back("if " + std::string($3) + " == 0 goto " + elseLabel);
    
    loopLabelsStack.push_back({elseLabel, endLabel});
} MatchedStmt ELSE {
    
    threeAddressCode.push_back("goto " + loopLabelsStack.back().endLabel);
    
    threeAddressCode.push_back(loopLabelsStack.back().startLabel + ":");
} MatchedStmt {
    
    threeAddressCode.push_back(loopLabelsStack.back().endLabel + ":");
    loopLabelsStack.pop_back();
}
| IF '(' Expression ')' {
    std::string endLabel = newLabel();
    threeAddressCode.push_back("if " + std::string($3) + " == 0 goto " + endLabel);
   
    loopLabelsStack.push_back({"", endLabel});
} Statement {
   
    threeAddressCode.push_back(loopLabelsStack.back().endLabel + ":");
    loopLabelsStack.pop_back();
}
;

MatchedStmt : ExprStmt
            | CompoundStmt
            | IF '(' Expression ')' {
                std::string elseLabel = newLabel();
                std::string endLabel = newLabel();
                
                threeAddressCode.push_back("if " + std::string($3) + " == 0 goto " + elseLabel);
                loopLabelsStack.push_back({elseLabel, endLabel});
            } MatchedStmt ELSE {
                
                threeAddressCode.push_back("goto " + loopLabelsStack.back().endLabel);
                
                threeAddressCode.push_back(loopLabelsStack.back().startLabel + ":");
            } MatchedStmt {
                
                threeAddressCode.push_back(loopLabelsStack.back().endLabel + ":");
                loopLabelsStack.pop_back();
            }
            | IterationStmt
            | ReturnStmt
            | IOStmt
;

IterationStmt : WHILE {
    std::string startLabel = newLabel();
    threeAddressCode.push_back(startLabel + ":");
    
    loopLabelsStack.push_back({startLabel, ""});
} '(' Expression ')' {
    std::string endLabel = newLabel();
    threeAddressCode.push_back("if " + std::string($4) + " == 0 goto " + endLabel);
    
    loopLabelsStack.back().endLabel = endLabel;
} Statement {
    
    threeAddressCode.push_back("goto " + loopLabelsStack.back().startLabel);
    
    threeAddressCode.push_back(loopLabelsStack.back().endLabel + ":");
    loopLabelsStack.pop_back();
}
;

ReturnStmt : RETURN Expression ';' {
    
    threeAddressCode.push_back("return " + std::string($2));
}
| RETURN ';' {
    
    if (currentFunctionType != "void") {
        yyerror("Error: Non-void function must return a value");
    }
    threeAddressCode.push_back("return");
}
;

IOStmt : PrintStmt
       | ScanStmt
;

PrintStmt : PRINT '(' Expression ')' ';' {
    threeAddressCode.push_back("param " + std::string($3));
    threeAddressCode.push_back("call print, 1");
}
;

ScanStmt : SCAN '(' ID ')' ';' {
    auto symbol = findSymbol($3);
    if (!symbol) {
        std::string error = "Error: Variable " + std::string($3) + " used before declaration";
        yyerror(error.c_str());
    }
    threeAddressCode.push_back("param " + std::string($3));
    threeAddressCode.push_back("call scan, 1");
    free($3);
}
;

Expression : Expression OR AndExpr {
    std::string temp = newTemp();
    threeAddressCode.push_back(temp + " := " + std::string($1) + " or " + std::string($3));
    $$ = strdup(temp.c_str());
    exprType = "bool";
}
| AndExpr {
    $$ = $1;
}
;

AndExpr : AndExpr AND NotExpr {
    std::string temp = newTemp();
    threeAddressCode.push_back(temp + " := " + std::string($1) + " and " + std::string($3));
    $$ = strdup(temp.c_str());
    exprType = "bool";
}
| NotExpr {
    $$ = $1;
}
;

NotExpr : NOT NotExpr {
    std::string temp = newTemp();
    threeAddressCode.push_back(temp + " := not " + std::string($2));
    $$ = strdup(temp.c_str());
    exprType = "bool";
}
| RelationalExpr {
    $$ = $1;
}
;

RelationalExpr : AdditiveExpr RelOp AdditiveExpr {
    std::string temp = newTemp();
    std::string op;
    
    switch($2) {
        case '<': op = "<"; break;
        case '>': op = ">"; break;
        case LE: op = "<="; break;
        case GE: op = ">="; break;
        case EQ: op = "=="; break;
        case NE: op = "!="; break;
        default: op = "=="; 
    }
    
    threeAddressCode.push_back(temp + " := " + std::string($1) + " " + op + " " + std::string($3));
    $$ = strdup(temp.c_str());
    exprType = "bool";
}
| AdditiveExpr {
    $$ = $1;
}
;

RelOp : '<' { $$ = '<'; }
       | '>' { $$ = '>'; }
       | LE { $$ = LE; }
       | GE { $$ = GE; }
       | EQ { $$ = EQ; }
       | NE { $$ = NE; }
;

AdditiveExpr : AdditiveExpr AddOp MultExpr {
    std::string temp = newTemp();
    char op = (char)$2;
    threeAddressCode.push_back(temp + " := " + std::string($1) + " " + op + " " + std::string($3));
    $$ = strdup(temp.c_str());
    exprType = "int";
}
| MultExpr {
    $$ = $1;
}
;

AddOp : '+' { $$ = '+'; }
      | '-' { $$ = '-'; }
;

MultExpr : MultExpr MultOp UnaryExpr {
    std::string temp = newTemp();
    char op = (char)$2;
    threeAddressCode.push_back(temp + " := " + std::string($1) + " " + op + " " + std::string($3));
    $$ = strdup(temp.c_str());
    exprType = "int";
}
| UnaryExpr {
    $$ = $1;
}
;

MultOp : '*' { $$ = '*'; }
       | '/' { $$ = '/'; }
       | '%' { $$ = '%'; }
;

UnaryExpr : '-' Factor %prec UNARY_MINUS {
    std::string temp = newTemp();
    threeAddressCode.push_back(temp + " := -" + std::string($2));
    $$ = strdup(temp.c_str());
    exprType = "int";
}
| Factor {
    $$ = $1;
}
;

Factor : INT_LITERAL {
    std::string temp = newTemp();
    threeAddressCode.push_back(temp + " := " + std::to_string($1));
    $$ = strdup(temp.c_str());
    exprType = "int";
}
| BOOL_LITERAL {
    std::string temp = newTemp();
    threeAddressCode.push_back(temp + " := " + ($1 ? "true" : "false"));
    $$ = strdup(temp.c_str());
    exprType = "bool";
}
| STRING_LITERAL {
    std::string temp = newTemp();
    threeAddressCode.push_back(temp + " := \"" + std::string($1) + "\"");
    $$ = strdup(temp.c_str());
    exprType = "string";
    free($1);
}
| CHAR_LITERAL {
    std::string temp = newTemp();
    threeAddressCode.push_back(temp + " := '" + std::string(1, $1) + "'");
    $$ = strdup(temp.c_str());
    exprType = "char";
}
| FunctionCall {
    $$ = $1;
}
| ID {
    auto symbol = findSymbol($1);
    if (!symbol) {
        std::string error = "Error: Variable " + std::string($1) + " used before declaration";
        yyerror(error.c_str());
    }
    exprType = symbol ? symbol->type : "unknown";
    $$ = strdup($1);
    free($1);
}
| '(' Expression ')' {
    $$ = $2;
}
;

FunctionCall : ID '(' { 
    parameterCount = 0;
    }
    Arguments ')' {
    auto symbol = findSymbol($1);
    if (!symbol) {
        std::string error = "Error: Function " + std::string($1) + " used before declaration";
        yyerror(error.c_str());
    }
    if (symbol && !symbol->isFunction) {
        std::string error = "Error: " + std::string($1) + " is not a function";
        yyerror(error.c_str());
    }
    
   
    std::string temp = newTemp();
    threeAddressCode.push_back("call " + std::string($1) + ", " + std::to_string(parameterCount)); 
    threeAddressCode.push_back(temp + " := return_value");
    $$ = strdup(temp.c_str());
    
    exprType = symbol ? symbol->type : "unknown";
    free($1);
}
;

Arguments : ArgumentList {
    $$ = $1;
}
| /* empty */ {
    $$ = strdup("");
}
;

ArgumentList : ArgumentList ',' Expression {
    
    threeAddressCode.push_back("param " + std::string($3));
    parameterCount++;
    $$ = strdup(""); 
}
| Expression {
    threeAddressCode.push_back("param " + std::string($1));
    parameterCount = 1;
    $$ = strdup(""); 
}
;

%%

void yyerror(const char* s) {
    std::cerr << "Error at line " << lineNum << ": " << s << std::endl;
}
