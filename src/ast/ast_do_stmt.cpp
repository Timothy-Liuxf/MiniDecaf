/*****************************************************
 *  Implementation of "DoStmt".
 *
 *  Please refer to ast/ast.hpp for the definition.
 *
 *  Keltin Leung
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

/* Creates a new DoStmt node.
 *
 * PARAMETERS:
 *   cond    - the test expression
 *   body    - the loop body
 *   l       - position in the source text
 */
DoStmt::DoStmt(Expr *cond, Statement *body, Location *l) {

    setBasicInfo(WHILE_STMT, l);

    condition = cond;
    loop_body = body;
}

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void DoStmt::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void DoStmt::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    os << condition;

    newLine(os);
    os << loop_body << ")";
    decIndent(os);
}
