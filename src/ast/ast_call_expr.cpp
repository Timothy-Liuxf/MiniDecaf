
#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

/* Creates a new CallExpr node.
 *
 * PARAMETERS:
 *   func_name   - the name of the function
 *   args        - the arguments of the function
 *   l           - position in the source text
 */
CallExpr::CallExpr(std::string func_name, ExprList *args, Location *l) {
    setBasicInfo(CALL_EXPR, l);
    this->func_name = func_name;
    this->args = args;
}

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void CallExpr::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void CallExpr::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    os << "call " << this->func_name;
    newLine(os);
    os << this->args << ")";
    decIndent(os);
}
