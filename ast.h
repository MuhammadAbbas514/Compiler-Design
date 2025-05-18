#ifndef AST_H
#define AST_H

#include <string>
#include <vector>

class node;
class Expr;
class Stmt;
class Decl;

class node {
public:
    virtual ~node() {}
    virtual void generateCode(std::vector<std::string>& code) = 0;
};

class Stmt : public node {
public:
    virtual ~Stmt() {}
};

class Expr : public node {
public:
    std::string type;
    virtual std::string getExprType() { return type; }
};

class BinaryExpr : public Expr {
public:
    Expr* left;
    Expr* right;
    std::string op;

    BinaryExpr(Expr* l, std::string o, Expr* r)
        : left(l), right(r), op(o) {
    }

    ~BinaryExpr() {
        delete left;
        delete right;
    }

    void generateCode(std::vector<std::string>& code) override;
};

class Variable : public Expr {
public:
    std::string name;

    Variable(const std::string& n) : name(n) {}

    void generateCode(std::vector<std::string>& code) override;
};

class AssignmentStmt : public Stmt {
public:
    std::string variable;
    Expr* value;

    AssignmentStmt(const std::string& var, Expr* val)
        : variable(var), value(val) {
    }

    ~AssignmentStmt() {
        delete value;
    }

    void generateCode(std::vector<std::string>& code) override;
};

class FunctionDecl : public node {
public:
    std::string name;
    std::string returnType;
    std::vector<std::pair<std::string, std::string>> parameters;  // (type, name)
    std::vector<Stmt*> body;

    FunctionDecl(const std::string& n, const std::string& rt)
        : name(n), returnType(rt) {
    }

    ~FunctionDecl() {
        for (auto stmt : body) {
            delete stmt;
        }
    }

    void generateCode(std::vector<std::string>& code) override;
};

class Decl : public node {
public:
    virtual ~Decl() {}
};

class VariableDecl : public Decl {
public:
    std::string type;
    std::string name;
    Expr* initialValue;  // Can be nullptr

    VariableDecl(const std::string& t, const std::string& n, Expr* init = nullptr)
        : type(t), name(n), initialValue(init) {
    }

    ~VariableDecl() {
        delete initialValue;
    }

    void generateCode(std::vector<std::string>& code) override;
};

inline void BinaryExpr::generateCode(std::vector<std::string>& code) {
    
}

inline void Variable::generateCode(std::vector<std::string>& code) {
    
}

inline void AssignmentStmt::generateCode(std::vector<std::string>& code) {
    
}

inline void FunctionDecl::generateCode(std::vector<std::string>& code) {
    
}

inline void VariableDecl::generateCode(std::vector<std::string>& code) {
    
}

#endif // AST_H
