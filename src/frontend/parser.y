/*****************************************************
 *  The GNU Bison Specification for Mind Language.
 *
 *  We have provided complete SECTION I & IV for you.
 *  Please complete SECTION II & III.
 *
 *  In case you want some debug support, we provide a
 *  "diagnose()" function for you. All you need is to
 *  call this function in main.cpp.
 *
 *  Please refer to the ``GNU Flex Manual'' if you have
 *  problems about how to write the lexical rules.
 *
 *  Keltin Leung 
 */
%output "parser.cpp"
%skeleton "lalr1.cc"
%defines
%define api.value.type variant
%define api.token.constructor
%define parse.assert
%locations
/* SECTION I: preamble inclusion */
%code requires{
#include "config.hpp"
#include "ast/ast.hpp"
#include "location.hpp"
#include "parser.hpp"

using namespace mind;

void yyerror (char const *);
void setParseTree(ast::Program* tree);

  /* This macro is provided for your convenience. */
#define POS(pos)    (new Location(pos.begin.line, pos.begin.column))


void scan_begin(const char* filename);
void scan_end();
}
%code{
  #include "compiler.hpp"
}
/* SECTION II: definition & declaration */

/*   SUBSECTION 2.1: token(terminal) declaration */


%define api.token.prefix {TOK_}
%token
   END  0  "end of file"
   BOOL "bool"
   INT  "int"
   RETURN "return"
   IF "if"
   ELSE  "else"
   DO "do"
   WHILE "while"
   FOR "for"
   BREAK "break"
   EQU "=="
   NEQ "!="
   AND "&&" 
   OR "||"
   LEQ "<="
   GEQ ">="
   PLUS "+"
   MINUS "-"
   TIMES "*"
   SLASH "/"
   MOD "%"
   LT "<"
   GT ">"
   COLON ":"
   SEMICOLON ";"
   LNOT "!"
   BNOT "~"
   COMMA ","
   DOT "."
   ASSIGN "="
   QUESTION "?"
   LPAREN "("
   RPAREN ")"
   LBRACK "["
   RBRACK "]"
   LBRACE "{"
   RBRACE "}"
;
%token <std::string> IDENTIFIER "identifier"
%token<int> ICONST "iconst"
%nterm<mind::ast::StmtList*> StmtList
%nterm<mind::ast::VarList* > FormalList 
%nterm<mind::ast::Program* > Program FoDList
%nterm<mind::ast::FuncDefn* > FuncDefn
%nterm<mind::ast::Type*> Type
%nterm<mind::ast::VarDecl*> VarDecl
%nterm<mind::ast::Statement*> Stmt ReturnStmt ExprStmt IfStmt  CompStmt WhileStmt 
%nterm<mind::ast::Statement*> BlockItem
%nterm<mind::ast::Expr*> Expr AssignExpr ConditionalExpr LogicalOrExpr LogicalAndExpr EqualityExpr RationalExpr AdditiveExpr MultiplicativeExpr UnaryExpr PrimaryExpr

/*   SUBSECTION 2.2: associativeness & precedences */
%nonassoc QUESTION
%left     OR
%left     AND
%left EQU NEQ
%left LEQ GEQ LT GT
%left     PLUS MINUS
%left     TIMES SLASH MOD
%nonassoc LNOT NEG BNOT
%nonassoc LBRACK DOT

%{
  /* we have to include scanner.hpp here... */
#define YY_NO_UNISTD_H 1
%}

/*   SUBSECTION 2.5: start symbol of the grammar */
%start Program

/* SECTION III: grammar rules (and actions) */
%%
Program     : FoDList
                { /* we don't write $$ = XXX here. */
				  setParseTree($1); }
            ;
FoDList :   
            FuncDefn 
                {$$ = new ast::Program($1,POS(@1)); } |
            FoDList FuncDefn{
                 {$1->func_and_globals->append($2);
                  $$ = $1; }
                }

FuncDefn : Type IDENTIFIER LPAREN FormalList RPAREN LBRACE StmtList RBRACE {
              $$ = new ast::FuncDefn($2,$1,$4,$7,POS(@1));
          } |
          Type IDENTIFIER LPAREN FormalList RPAREN SEMICOLON{
              $$ = new ast::FuncDefn($2,$1,$4,new ast::EmptyStmt(POS(@6)),POS(@1));
          }
FormalList :  /* EMPTY */
            {$$ = new ast::VarList();} 

Type        : INT
                { $$ = new ast::IntType(POS(@1)); }
StmtList    : /* empty */
                { $$ = new ast::StmtList(); }
            | StmtList BlockItem
                { $1->append($2);
                  $$ = $1; }
            ;
BlockItem   : Stmt  {$$ = $1;}      |
              VarDecl {$$ = $1;}
            ;
Stmt        : ReturnStmt {$$ = $1;} |
              ExprStmt   {$$ = $1;} |
              IfStmt     {$$ = $1;} |
              WhileStmt  {$$ = $1;} |
              CompStmt   {$$ = $1;} |
              BREAK SEMICOLON
                {$$ = new ast::BreakStmt(POS(@1));} |
              SEMICOLON
                {$$ = new ast::EmptyStmt(POS(@1));}
            ;
CompStmt    : LBRACE StmtList RBRACE
                {$$ = new ast::CompStmt($2,POS(@1));}
            ;
WhileStmt   : WHILE LPAREN Expr RPAREN Stmt
                { $$ = new ast::WhileStmt($3, $5, POS(@1)); }
            ;
IfStmt      : IF LPAREN Expr RPAREN Stmt
                { $$ = new ast::IfStmt($3, $5, new ast::EmptyStmt(POS(@5)), POS(@1)); }
            | IF LPAREN Expr RPAREN Stmt ELSE Stmt
                { $$ = new ast::IfStmt($3, $5, $7, POS(@1)); }
            ;

ReturnStmt  : RETURN Expr SEMICOLON
                { $$ = new ast::ReturnStmt($2, POS(@1)); }
            ;
VarDecl     : Type IDENTIFIER SEMICOLON
                { $$ = new ast::VarDecl($2, $1, POS(@1)); }
            | Type IDENTIFIER ASSIGN Expr SEMICOLON
                { $$ = new ast::VarDecl($2, $1, $4, POS(@1)); }
            ;
ExprStmt    : Expr SEMICOLON
                { $$ = new ast::ExprStmt($1, POS(@1)); } 
            ;         
Expr        : AssignExpr
            ;
AssignExpr  : IDENTIFIER ASSIGN AssignExpr
                { $$ = new ast::AssignExpr(new ast::VarRef($1, POS(@1)), $3, POS(@2)); }
            | ConditionalExpr
            ;
ConditionalExpr     : LogicalOrExpr
                    | Expr QUESTION Expr COLON Expr
                        { $$ = new ast::IfExpr($1, $3, $5, POS(@2)); }
                    ;
LogicalOrExpr       : LogicalAndExpr
                    | LogicalOrExpr OR LogicalAndExpr
                        { $$ = new ast::OrExpr($1, $3, POS(@2)); }
                    ;
LogicalAndExpr      : EqualityExpr
                    | LogicalAndExpr AND EqualityExpr
                        { $$ = new ast::AndExpr($1, $3, POS(@2)); }
                    ;
EqualityExpr        : RationalExpr
                    | EqualityExpr EQU RationalExpr
                        { $$ = new ast::EquExpr($1, $3, POS(@2)); }
                    | EqualityExpr NEQ RationalExpr
                        { $$ = new ast::NeqExpr($1, $3, POS(@2)); }
                    ;
RationalExpr        : AdditiveExpr
                    | RationalExpr LT AdditiveExpr
                        { $$ = new ast::LesExpr($1, $3, POS(@2)); }
                    | RationalExpr GT AdditiveExpr
                        { $$ = new ast::GrtExpr($1, $3, POS(@2)); }
                    | RationalExpr LEQ AdditiveExpr
                        { $$ = new ast::LeqExpr($1, $3, POS(@2)); }
                    | RationalExpr GEQ AdditiveExpr
                        { $$ = new ast::GeqExpr($1, $3, POS(@2)); }
                    ;
AdditiveExpr        : MultiplicativeExpr
                    | AdditiveExpr PLUS MultiplicativeExpr
                        { $$ = new ast::AddExpr($1, $3, POS(@2)); }
                    | AdditiveExpr MINUS MultiplicativeExpr
                        { $$ = new ast::SubExpr($1, $3, POS(@2)); }
                    ;
MultiplicativeExpr  : UnaryExpr
                    | MultiplicativeExpr TIMES UnaryExpr
                        { $$ = new ast::MulExpr($1, $3, POS(@2)); }
                    | MultiplicativeExpr SLASH UnaryExpr
                        { $$ = new ast::DivExpr($1, $3, POS(@2)); }
                    | MultiplicativeExpr MOD UnaryExpr
                        { $$ = new ast::ModExpr($1, $3, POS(@2)); }
                    ;
UnaryExpr   : PrimaryExpr
            | MINUS UnaryExpr  %prec NEG
                { $$ = new ast::NegExpr($2, POS(@1)); }
            | LNOT UnaryExpr
                { $$ = new ast::NotExpr($2, POS(@1)); }
            | BNOT UnaryExpr
                { $$ = new ast::BitNotExpr($2, POS(@1)); }
            ;
PrimaryExpr : ICONST
                { $$ = new ast::IntConst($1, POS(@1)); }            
            | LPAREN Expr RPAREN
                { $$ = $2; }
            | IDENTIFIER
                { $$ = new ast::LvalueExpr(new ast::VarRef($1, POS(@1)), POS(@1)); }
%%

/* SECTION IV: customized section */
#include "compiler.hpp"
#include <cstdio>

static ast::Program* ptree = NULL;
extern int myline, mycol;   // defined in scanner.l

// bison will generate code to invoke me
void
yyerror (char const *msg) {
  err::issue(new Location(myline, mycol), new err::SyntaxError(msg));
  scan_end();
  std::exit(1);
}

// call me when the Program symbol is reduced
void
setParseTree(ast::Program* tree) {
  ptree = tree;
}

/* Parses a given mind source file.
 *
 * PARAMETERS:
 *   filename - name of the source file
 * RETURNS:
 *   the parse tree (in the form of abstract syntax tree)
 * NOTE:
 *   should any syntax error occur, this function would not return.
 */
ast::Program*
mind::MindCompiler::parseFile(const char* filename) {  
  scan_begin(filename);
  /* if (NULL == filename)
	yyin = stdin;
  else
	yyin = std::fopen(filename, "r"); */
  yy::parser parse;
  parse();
  scan_end();
  /* if (yyin != stdin)
	std::fclose(yyin); */
  
  return ptree;
}

void
yy::parser::error (const location_type& l, const std::string& m)
{
  //std::cerr << l << ": " << m << '\n';
  err::issue(new Location(l.begin.line, l.begin.column), new err::SyntaxError(m));
  
  scan_end();
  std::exit(1);
}
