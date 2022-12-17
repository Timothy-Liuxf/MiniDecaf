/*****************************************************
 *  Implementation of "ForStmt".
 *
 *  Please refer to ast/ast.hpp for the definition.
 *
 *  Timothy Liu
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

/* Creates a new ForStmt node.
 *
 * PARAMETERS:
 *   init    - the initialization statement
 *   cond    - the test expression
 *   update  - the update expression
 *   loop_body    - the loop body
 *   l       - position in the source text
 */
ForStmt::ForStmt(Statement *init, Expr *cond, Expr *update, Statement *body,
                 Location *l) {

    setBasicInfo(FOR_STMT, l);

    this->init = init;
    this->condition = cond;
    this->update = update;
    this->loop_body = body;
}

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void ForStmt::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void ForStmt::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    os << init;
    newLine(os);
    os << condition;
    newLine(os);
    os << update;

    newLine(os);
    os << loop_body << ")";
    decIndent(os);
}
