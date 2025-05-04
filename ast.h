#ifndef AST_H
#define AST_H

#include <string>
#include <vector>

class ASTNode;
class Expression;
class Statement;
class Declaration;

class ASTNode {
public:
    virtual ~ASTNode() {}
    virtual void generateCode(std::vector<std::string>& code) = 0;
};

class Statement : public ASTNode {
public:
    virtual ~Statement() {}
};

class Expression : public ASTNode {
public:
    std::string type;
    virtual std::string getExpressionType() { return type; }
};

class BinaryExpression : public Expression {
public:
    Expression* left;
    Expression* right;
    std::string op;

    BinaryExpression(Expression* l, std::string o, Expression* r)
        : left(l), right(r), op(o) {
    }

    ~BinaryExpression() {
        delete left;
        delete right;
    }

    void generateCode(std::vector<std::string>& code) override;
};

class Variable : public Expression {
public:
    std::string name;

    Variable(const std::string& n) : name(n) {}

    void generateCode(std::vector<std::string>& code) override;
};

class AssignmentStatement : public Statement {
public:
    std::string variable;
    Expression* value;

    AssignmentStatement(const std::string& var, Expression* val)
        : variable(var), value(val) {
    }

    ~AssignmentStatement() {
        delete value;
    }

    void generateCode(std::vector<std::string>& code) override;
};

class FunctionDeclaration : public ASTNode {
public:
    std::string name;
    std::string returnType;
    std::vector<std::pair<std::string, std::string>> parameters;  // (type, name)
    std::vector<Statement*> body;

    FunctionDeclaration(const std::string& n, const std::string& rt)
        : name(n), returnType(rt) {
    }

    ~FunctionDeclaration() {
        for (auto stmt : body) {
            delete stmt;
        }
    }

    void generateCode(std::vector<std::string>& code) override;
};

class Declaration : public ASTNode {
public:
    virtual ~Declaration() {}
};

class VariableDeclaration : public Declaration {
public:
    std::string type;
    std::string name;
    Expression* initialValue;  // Can be nullptr

    VariableDeclaration(const std::string& t, const std::string& n, Expression* init = nullptr)
        : type(t), name(n), initialValue(init) {
    }

    ~VariableDeclaration() {
        delete initialValue;
    }

    void generateCode(std::vector<std::string>& code) override;
};

inline void BinaryExpression::generateCode(std::vector<std::string>& code) {
    
}

inline void Variable::generateCode(std::vector<std::string>& code) {
    
}

inline void AssignmentStatement::generateCode(std::vector<std::string>& code) {
    
}

inline void FunctionDeclaration::generateCode(std::vector<std::string>& code) {
    
}

inline void VariableDeclaration::generateCode(std::vector<std::string>& code) {
    
}

#endif // AST_H
