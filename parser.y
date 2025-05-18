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
extern bool hasError;
extern char* yytext;

void yyerror(const char* s);

std::vector<std::string> threeAddressCode;
std::string currentBaseType;
std::vector<std::string> currentArgumentTypes;
int tempVarCounter = 0;
int labelCounter = 0;
int parameterCount = 0;
bool inFunctionBody = false;

std::string newTemp() {
    return "t" + std::to_string(tempVarCounter++);
}

std::string newLabel() {
    return "L" + std::to_string(labelCounter++);
}

struct ExprType {
    std::string type;
    std::string value;
};

struct SymbolInfo {
    std::string name;
    std::string type;
    bool isFunction;
    std::vector<std::string> paramTypes;
    int scope;
};

std::vector<SymbolInfo> symbolTable;
int currentScope = 0;
bool hasMainFunction = false;
std::string currentFunctionType;
std::vector<std::string> currentParameterTypes;

void enterScope() {
    currentScope++;
}

void exitScope() {
    for (auto it = symbolTable.begin(); it != symbolTable.end();) {
        if (it->scope == currentScope) {
            it = symbolTable.erase(it);
        } else {
            ++it;
        }
    }
    currentScope--;
}

bool addSymbol(const std::string& name, const std::string& type, bool isFunction = false) {
    for (const auto& symbol : symbolTable) {
        if (symbol.name == name && symbol.scope == currentScope) {
            return false;
        }
    }
    
    SymbolInfo newSymbol = {name, type, isFunction, {}, currentScope};
    if (isFunction) {
        newSymbol.paramTypes = currentParameterTypes;
    }
    symbolTable.push_back(newSymbol);
    return true;
}

std::string checkType(const std::string& expr) {
    if (expr.empty()) return "unknown";
    
    if (expr[0] == '"') return "string";
    if (expr[0] == '\'') return "char";
    if (expr == "true" || expr == "false") return "bool";
    
    try {
        std::stoi(expr);
        return "int";
    } catch (std::invalid_argument&) {
        try {
            std::stof(expr);
            return "float";
        } catch (std::invalid_argument&) {
            for (const auto& symbol : symbolTable) {
                if (symbol.name == expr) {
                    return symbol.type;
                }
            }
            return "unknown";
        }
    }
}

bool areTypesCompatible(const std::string& type1, const std::string& type2, const std::string& operation) {
    if (operation == "assign") {
        return type1 == type2;
    }
    
    if (operation == "+" || operation == "-" || operation == "*" || operation == "/" || operation == "%") {
        return (type1 == "int" && type2 == "int");
    }
    
    if (operation == "<" || operation == ">" || operation == "<=" || operation == ">=" || 
        operation == "==" || operation == "!=") {
        return ((type1 == "int" && type2 == "int") || 
                (type1 == "bool" && type2 == "bool") ||
                (type1 == "char" && type2 == "char") ||
                (type1 == "string" && type2 == "string"));
    }
    
    if (operation == "and" || operation == "or") {
        return (type1 == "bool" && type2 == "bool");
    }
    
    return false;
}

SymbolInfo* findSymbol(const std::string& name) {
    for (int scope = currentScope; scope >= 0; scope--) {
        for (auto& symbol : symbolTable) {
            if (symbol.name == name && symbol.scope <= scope) {
                return &symbol;
            }
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

%define parse.error verbose

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

%type <expr_val> Expr AndExpr NotExpr RelationalExpr AdditiveExpr MultExpr UnaryExpr Factor FunctionCall
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
                      | 
;

FunctionDecl : ReturnType ID {
    enterScope();
    inFunctionBody = true;
    currentFunctionType = $1;
} '(' Parameters ')' {
    int previousScope = currentScope;
    currentScope = 0;
    if (!addSymbol($2, $1, true)) {
        std::string error = "Error: Function " + std::string($2) + " already defined";
        yyerror(error.c_str());
    } else {
        SymbolInfo* symbol = findSymbol($2);
        if (symbol) {
            symbol->paramTypes = currentParameterTypes;
            currentParameterTypes.clear();
        }
    }
    currentScope = previousScope;
    threeAddressCode.push_back("func_begin " + std::string($2));
} CompoundStmt {
    threeAddressCode.push_back("func_end " + std::string($2));
    exitScope();
    inFunctionBody = false;
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
           | 
;

ParameterList : Parameter
              | ParameterList ',' Parameter
;

Parameter : BaseType ID {
    if (!addSymbol($2, $1)) {
        std::string error = "Error: Parameter " + std::string($2) + " already defined";
        yyerror(error.c_str());
    }
    currentParameterTypes.push_back($1);
    free($2);
}
;

CompoundStmt : '{' {
    if (!inFunctionBody) enterScope();
} LocalDeclarations StmtList '}' {
    if (!inFunctionBody) exitScope();
};

LocalDeclarations : VarDeclaration LocalDeclarations
                  | 
;

VarDeclaration : BaseType IdList ';' {
    free($1);
}
;

IdList : IdInit
       | IdList ',' IdInit
;

IdInit : ID {
    if (!addSymbol($1, currentBaseType)) {
        std::string error = "Error: Variable " + std::string($1) + " already defined";
        yyerror(error.c_str());
    }
    free($1);
} 
| ID ASSIGN Expr {
    if (!addSymbol($1, currentBaseType)) {
        std::string error = "Error: Variable " + std::string($1) + " already defined";
        yyerror(error.c_str());
    }
    
    if (currentBaseType != exprType && exprType != "unknown") {
        std::string error = "Error: Type mismatch in assignment to '" + std::string($1) + 
                            "'. Expected '" + currentBaseType + "', but got '" + exprType + "'";
        yyerror(error.c_str());
    }

    threeAddressCode.push_back(std::string($1) + " := " + std::string($3));
    free($1);
}
;

StmtList : Stmt StmtList
         | 
;

Stmt : ExprStmt
          | CompoundStmt
          | SelectionStmt
          | IterationStmt
          | ReturnStmt
          | IOStmt
          | ';'
;

ExprStmt : ID ASSIGN Expr ';' {
    SymbolInfo* symbol = findSymbol($1);
    if (!symbol) {
        std::string error = "Error: Variable " + std::string($1) + " used before declaration";
        yyerror(error.c_str());
    } else {
        if (symbol->type != exprType && exprType != "unknown") {
            std::string error = "Error: Type mismatch in assignment to '" + std::string($1) +
                                "'. Expected '" + symbol->type + "', but got '" + exprType + "'";
            yyerror(error.c_str());
        }
    }

    threeAddressCode.push_back(std::string($1) + " := " + std::string($3));
    free($1);
}
| Expr ';'
;

SelectionStmt : IF '(' Expr ')' {
    if (exprType != "bool") {
        yyerror("Error: Condition in if Stmt must be a boolean Expr");
    }
    
    std::string falseLabel = newLabel();
    std::string endLabel = newLabel();
    
    threeAddressCode.push_back("if " + std::string($3) + " == 0 goto " + falseLabel);
    
    loopLabelsStack.push_back({falseLabel, endLabel});
} Stmt ElsePart
;

ElsePart : ELSE {
    threeAddressCode.push_back("goto " + loopLabelsStack.back().endLabel);
    
    threeAddressCode.push_back(loopLabelsStack.back().startLabel + ":");
} Stmt {
    threeAddressCode.push_back(loopLabelsStack.back().endLabel + ":");
    loopLabelsStack.pop_back();
}
| {
    threeAddressCode.push_back(loopLabelsStack.back().startLabel + ":");
    threeAddressCode.push_back(loopLabelsStack.back().endLabel + ":");
    loopLabelsStack.pop_back();
}
;

IterationStmt : WHILE {
    std::string startLabel = newLabel();
    threeAddressCode.push_back(startLabel + ":");
    
    loopLabelsStack.push_back({startLabel, ""});
} '(' Expr ')' {
    if (exprType != "bool") {
        yyerror("Error: Condition in while Stmt must be a boolean Expr");
    }
    
    std::string endLabel = newLabel();
    threeAddressCode.push_back("if " + std::string($4) + " == 0 goto " + endLabel);
    
    loopLabelsStack.back().endLabel = endLabel;
} Stmt {
    threeAddressCode.push_back("goto " + loopLabelsStack.back().startLabel);
    
    threeAddressCode.push_back(loopLabelsStack.back().endLabel + ":");
    loopLabelsStack.pop_back();
}
;

ReturnStmt : RETURN Expr ';' {
    if (currentFunctionType != exprType && exprType != "unknown" && currentFunctionType != "int") {
        std::string error = "Error: Return type mismatch. Expected '" + currentFunctionType + 
                           "', but got '" + exprType + "'";
        yyerror(error.c_str());
    }
    
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

PrintStmt : PRINT '(' Expr ')' ';' {
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

Expr : Expr OR AndExpr {
    if (exprType != "bool") {
        yyerror("Error: Operands of OR operator must be boolean");
    }
    
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
    if (exprType != "bool") {
        yyerror("Error: Operands of AND operator must be boolean");
    }
    
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
    if (exprType != "bool") {
        yyerror("Error: Operand of NOT operator must be boolean");
    }
    
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
    std::string leftType = exprType;
    std::string rightType = exprType;
    
    if (leftType != rightType) {
        yyerror("Error: Type mismatch in relational Expr");
    }
    
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
    if (exprType != "int") {
        yyerror("Error: Type mismatch in additive Expr, expected int");
    }
    
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
    if (exprType != "int") {
        yyerror("Error: Type mismatch in multiplicative Expr, expected int");
    }
    
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
    if (exprType != "int") {
        yyerror("Error: Unary minus can only be applied to integers");
    }
    
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
        exprType = "unknown";
    } else {
        exprType = symbol->type;
    }
    $$ = strdup($1);
    free($1);
}
| '(' Expr ')' {
    $$ = $2;
}
;

FunctionCall : ID '(' { 
    parameterCount = 0;
    currentArgumentTypes.clear();
} Arguments ')' {
    auto symbol = findSymbol($1);
    if (!symbol) {
        yyerror(("Error: Function " + std::string($1) + " used before declaration").c_str());
        exprType = "unknown";
    } else if (!symbol->isFunction) {
        yyerror(("Error: " + std::string($1) + " is not a function").c_str());
        exprType = "unknown";
    } else {
        if (symbol->paramTypes.size() != parameterCount) {
            yyerror(("Parameter count mismatch for " + std::string($1)).c_str());
        } else {
            for (int i = 0; i < parameterCount; ++i) {
                if (currentArgumentTypes[i] != symbol->paramTypes[i]) {
                    std::string msg = "Type mismatch for argument " + std::to_string(i+1) + 
                                    " in " + std::string($1) + 
                                    ". Expected '" + symbol->paramTypes[i] + 
                                    "', got '" + currentArgumentTypes[i] + "'";
                    yyerror(msg.c_str());
                }
            }
        }
        exprType = symbol->type;
    }
    std::string temp = newTemp();
    threeAddressCode.push_back("call " + std::string($1) + ", " + std::to_string(parameterCount)); 
    threeAddressCode.push_back(temp + " := return_value");
    $$ = strdup(temp.c_str());
    free($1);
}
;

Arguments : ArgumentList {
    $$ = $1;
}
| {
    $$ = strdup("");
}
;

ArgumentList : ArgumentList ',' Expr {
    currentArgumentTypes.push_back(exprType);
    threeAddressCode.push_back("param " + std::string($3));
    parameterCount++;
    $$ = strdup(""); 
}
| Expr {
    currentArgumentTypes.push_back(exprType);
    threeAddressCode.push_back("param " + std::string($1));
    parameterCount = 1;
    $$ = strdup(""); 
}
;

%%

void yyerror(const char* s) {
    extern char* yytext;
    std::cerr << "Error at line " << lineNum << ": " << s << " (near token: '" << yytext << "')" << std::endl;
    hasError = true;
}