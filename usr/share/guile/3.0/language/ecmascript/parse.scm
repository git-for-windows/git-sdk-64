;;; ECMAScript for Guile

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (language ecmascript parse)
  #:use-module (system base lalr)
  #:use-module (language ecmascript tokenize)
  #:export (read-ecmascript read-ecmascript/1 make-parser))

(define* (syntax-error message #:optional token)
  (if (lexical-token? token)
      (throw 'syntax-error #f message
             (and=> (lexical-token-source token)
                    source-location->source-properties)
             (or (lexical-token-value token)
                 (lexical-token-category token))
             #f)
      (throw 'syntax-error #f message #f token #f)))

(define (read-ecmascript port)
  (let ((parse (make-parser)))
    (parse (make-tokenizer port) syntax-error)))

(define (read-ecmascript/1 port)
  (let ((parse (make-parser)))
    (parse (make-tokenizer/1 port) syntax-error)))

(define *eof-object*
  (call-with-input-string "" read-char))

(define (make-parser)
  ;; Return a fresh ECMAScript parser.  Parsers produced by `lalr-scm' are now
  ;; stateful (e.g., they won't invoke the tokenizer any more once it has
  ;; returned `*eoi*'), hence the need to instantiate new parsers.

  (lalr-parser
   ;; terminal (i.e. input) token types
   (lbrace rbrace lparen rparen lbracket rbracket dot semicolon comma <
    > <= >= == != === !== + - * % ++ -- << >> >>> & bor ^ ! ~ && or ? 
    colon = += -= *= %= <<= >>= >>>= &= bor= ^= / /=

    break else new var case finally return void catch for switch while
    continue function this with default if throw delete in try do
    instanceof typeof null true false

    Identifier StringLiteral NumericLiteral RegexpLiteral)


   (Program (SourceElements) : $1
            (*eoi*) : *eof-object*)

   ;;
   ;; Verily, here we define statements. Expressions are defined
   ;; afterwards.
   ;;

   (SourceElement (Statement) : $1
                  (FunctionDeclaration) : $1)

   (FunctionDeclaration (function Identifier lparen rparen lbrace FunctionBody rbrace) : `(var (,$2 (lambda () ,$6)))
                        (function Identifier lparen FormalParameterList rparen lbrace FunctionBody rbrace) : `(var (,$2 (lambda ,$4 ,$7))))
   (FunctionExpression (function lparen rparen lbrace FunctionBody rbrace) : `(lambda () ,$5)
                       (function Identifier lparen rparen lbrace FunctionBody rbrace) : `(lambda () ,$6)
                       (function lparen FormalParameterList rparen lbrace FunctionBody rbrace) : `(lambda ,$3 ,$6)
                       (function Identifier lparen FormalParameterList rparen lbrace FunctionBody rbrace) : `(lambda ,$4 ,$7))
   (FormalParameterList (Identifier) : `(,$1)
                        (FormalParameterList comma Identifier) : `(,@$1 ,$3))
   (SourceElements (SourceElement) : $1
                   (SourceElements SourceElement) : (if (and (pair? $1) (eq? (car $1) 'begin))
                                                         `(begin ,@(cdr $1) ,$2)
                                                         `(begin ,$1 ,$2)))
   (FunctionBody (SourceElements) : $1
                 () : '(begin))

   (Statement (Block) : $1
              (VariableStatement) : $1
              (EmptyStatement) : $1
              (ExpressionStatement) : $1
              (IfStatement) : $1
              (IterationStatement) : $1
              (ContinueStatement) : $1
              (BreakStatement) : $1
              (ReturnStatement) : $1
              (WithStatement) : $1
              (LabelledStatement) : $1
              (SwitchStatement) : $1
              (ThrowStatement) : $1
              (TryStatement) : $1)

   (Block (lbrace StatementList rbrace) : `(block ,$2))
   (StatementList (Statement) : $1
                  (StatementList Statement) : (if (and (pair? $1) (eq? (car $1) 'begin))
                                                   `(begin ,@(cdr $1) ,$2)
                                                   `(begin ,$1 ,$2)))

   (VariableStatement (var VariableDeclarationList) : `(var ,@$2))
   (VariableDeclarationList (VariableDeclaration) : `(,$1)
                            (VariableDeclarationList comma VariableDeclaration) : `(,@$1 ,$2))
   (VariableDeclarationListNoIn (VariableDeclarationNoIn) : `(,$1)
                                (VariableDeclarationListNoIn comma VariableDeclarationNoIn) : `(,@$1 ,$2))
   (VariableDeclaration (Identifier) : `(,$1)
                        (Identifier Initialiser) : `(,$1 ,$2))
   (VariableDeclarationNoIn (Identifier) : `(,$1)
                            (Identifier Initialiser) : `(,$1 ,$2))
   (Initialiser (= AssignmentExpression) : $2)
   (InitialiserNoIn (= AssignmentExpressionNoIn) : $2)

   (EmptyStatement (semicolon) : '(begin))

   (ExpressionStatement (Expression semicolon) : $1)

   (IfStatement (if lparen Expression rparen Statement else Statement) : `(if ,$3 ,$5 ,$7)
                (if lparen Expression rparen Statement) : `(if ,$3 ,$5))
   
   (IterationStatement (do Statement while lparen Expression rparen semicolon) : `(do ,$2 ,$5)

                       (while lparen Expression rparen Statement) : `(while ,$3 ,$5)

                       (for lparen semicolon semicolon rparen Statement) : `(for #f #f #f ,$6)
                       (for lparen semicolon semicolon Expression rparen Statement) : `(for #f #f ,$5 ,$7)
                       (for lparen semicolon Expression semicolon rparen Statement) : `(for #f ,$4 #f ,$7)
                       (for lparen semicolon Expression semicolon Expression rparen Statement) : `(for #f ,$4 ,$6 ,$8)

                       (for lparen ExpressionNoIn semicolon semicolon rparen Statement) : `(for ,$3 #f #f ,$7)
                       (for lparen ExpressionNoIn semicolon semicolon Expression rparen Statement) : `(for ,$3 #f ,$6 ,$8)
                       (for lparen ExpressionNoIn semicolon Expression semicolon rparen Statement) : `(for ,$3 ,$5 #f ,$8)
                       (for lparen ExpressionNoIn semicolon Expression semicolon Expression rparen Statement) : `(for ,$3 ,$5 ,$7 ,$9)

                       (for lparen var VariableDeclarationListNoIn semicolon semicolon rparen Statement) : `(for (var ,@$4) #f #f ,$8)
                       (for lparen var VariableDeclarationListNoIn semicolon semicolon Expression rparen Statement) : `(for (var ,@$4) #f ,$7 ,$9)
                       (for lparen var VariableDeclarationListNoIn semicolon Expression semicolon rparen Statement) : `(for (var ,@$4) ,$6 #f ,$9)
                       (for lparen var VariableDeclarationListNoIn semicolon Expression semicolon Expression rparen Statement) : `(for (var ,@$4) ,$6 ,$8 ,$10)

                       (for lparen LeftHandSideExpression in Expression rparen Statement) : `(for-in ,$3 ,$5 ,$7)
                       (for lparen var VariableDeclarationNoIn in Expression rparen Statement) : `(begin (var ,$4) (for-in (ref ,@$4) ,$6 ,$8)))

   (ContinueStatement (continue Identifier semicolon) : `(continue ,$2)
                      (continue semicolon) : `(continue))

   (BreakStatement (break Identifier semicolon) : `(break ,$2)
                   (break semicolon) : `(break))

   (ReturnStatement (return Expression semicolon) : `(return ,$2)
                    (return semicolon) : `(return))

   (WithStatement (with lparen Expression rparen Statement) : `(with ,$3 ,$5))

   (SwitchStatement (switch lparen Expression rparen CaseBlock) : `(switch ,$3 ,@$5))
   (CaseBlock (lbrace rbrace) : '()
              (lbrace CaseClauses rbrace) : $2
              (lbrace CaseClauses DefaultClause rbrace) : `(,@$2 ,@$3)
              (lbrace DefaultClause rbrace) : `(,$2)
              (lbrace DefaultClause CaseClauses rbrace) : `(,@$2 ,@$3))
   (CaseClauses (CaseClause) : `(,$1)
                (CaseClauses CaseClause) : `(,@$1 ,$2))
   (CaseClause (case Expression colon) : `(case ,$2)
               (case Expression colon StatementList) : `(case ,$2 ,$4))
   (DefaultClause (default colon) : `(default)
                  (default colon StatementList) : `(default ,$3))

   (LabelledStatement (Identifier colon Statement) : `(label ,$1 ,$3))

   (ThrowStatement (throw Expression semicolon) : `(throw ,$2))

   (TryStatement (try Block Catch) : `(try ,$2 ,$3 #f)
                 (try Block Finally) : `(try ,$2 #f ,$3)
                 (try Block Catch Finally) : `(try ,$2 ,$3 ,$4))
   (Catch (catch lparen Identifier rparen Block) : `(catch ,$3 ,$5))
   (Finally (finally Block) : `(finally ,$2))

   ;;
   ;; As promised, expressions. We build up to Expression bottom-up, so
   ;; as to get operator precedence right.
   ;;

   (PrimaryExpression (this) : 'this
                      (null) : 'null
                      (true) : 'true
                      (false) : 'false
                      (Identifier) : `(ref ,$1)
                      (StringLiteral) : `(string ,$1)
                      (RegexpLiteral) : `(regexp ,$1)
                      (NumericLiteral) : `(number ,$1)
                      (dot NumericLiteral) : `(number ,(string->number (string-append "." (number->string $2))))
                      (ArrayLiteral) : $1
                      (ObjectLiteral) : $1
                      (lparen Expression rparen) : $2)

   (ArrayLiteral (lbracket rbracket) : '(array)
                 (lbracket Elision rbracket) : '(array ,@$2)
                 (lbracket ElementList rbracket) : `(array ,@$2)
                 (lbracket ElementList comma rbracket) : `(array ,@$2)
                 (lbracket ElementList comma Elision rbracket) : `(array ,@$2))
   (ElementList (AssignmentExpression) : `(,$1)
                (Elision AssignmentExpression) : `(,@$1 ,$2)
                (ElementList comma AssignmentExpression) : `(,@$1 ,$3)
                (ElementList comma Elision AssignmentExpression) : `(,@$1 ,@$3 ,$4))
   (Elision (comma) : '((number 0))
            (Elision comma) : `(,@$1 (number 0)))

   (ObjectLiteral (lbrace rbrace) : `(object)
                  (lbrace PropertyNameAndValueList rbrace) : `(object ,@$2))
   (PropertyNameAndValueList (PropertyName colon AssignmentExpression) : `((,$1 ,$3))
                             (PropertyNameAndValueList comma PropertyName colon AssignmentExpression) : `(,@$1 (,$3 ,$5)))
   (PropertyName (Identifier) : $1
                 (StringLiteral) : (string->symbol $1)
                 (NumericLiteral) : $1)

   (MemberExpression (PrimaryExpression) : $1
                     (FunctionExpression) : $1
                     (MemberExpression lbracket Expression rbracket) : `(aref ,$1 ,$3)
                     (MemberExpression dot Identifier) : `(pref ,$1 ,$3)
                     (new MemberExpression Arguments) : `(new ,$2 ,$3))

   (NewExpression (MemberExpression) : $1
                  (new NewExpression) : `(new ,$2 ()))

   (CallExpression (MemberExpression Arguments) : `(call ,$1 ,$2)
                   (CallExpression Arguments) : `(call ,$1 ,$2)
                   (CallExpression lbracket Expression rbracket) : `(aref ,$1 ,$3)
                   (CallExpression dot Identifier) : `(pref ,$1 ,$3))
   (Arguments (lparen rparen) : '()
              (lparen ArgumentList rparen) : $2)
   (ArgumentList (AssignmentExpression) : `(,$1)
                 (ArgumentList comma AssignmentExpression) : `(,@$1 ,$3))

   (LeftHandSideExpression (NewExpression) : $1
                           (CallExpression) : $1)

   (PostfixExpression (LeftHandSideExpression) : $1
                      (LeftHandSideExpression ++) : `(postinc ,$1)
                      (LeftHandSideExpression --) : `(postdec ,$1))

   (UnaryExpression (PostfixExpression) : $1
                    (delete UnaryExpression) : `(delete ,$2)
                    (void UnaryExpression) : `(void ,$2)
                    (typeof UnaryExpression) : `(typeof ,$2)
                    (++ UnaryExpression) : `(preinc ,$2)
                    (-- UnaryExpression) : `(predec ,$2)
                    (+ UnaryExpression) : `(+ ,$2)
                    (- UnaryExpression) : `(- ,$2)
                    (~ UnaryExpression) : `(~ ,$2)
                    (! UnaryExpression) : `(! ,$2))

   (MultiplicativeExpression (UnaryExpression) : $1
                             (MultiplicativeExpression * UnaryExpression) : `(* ,$1 ,$3)
                             (MultiplicativeExpression / UnaryExpression) : `(/ ,$1 ,$3)
                             (MultiplicativeExpression % UnaryExpression) : `(% ,$1 ,$3))

   (AdditiveExpression (MultiplicativeExpression) : $1
                       (AdditiveExpression + MultiplicativeExpression) : `(+ ,$1 ,$3)
                       (AdditiveExpression - MultiplicativeExpression) : `(- ,$1 ,$3))

   (ShiftExpression (AdditiveExpression) : $1
                    (ShiftExpression << MultiplicativeExpression) : `(<< ,$1 ,$3)
                    (ShiftExpression >> MultiplicativeExpression) : `(>> ,$1 ,$3)
                    (ShiftExpression >>> MultiplicativeExpression) : `(>>> ,$1 ,$3))

   (RelationalExpression (ShiftExpression) : $1
                         (RelationalExpression < ShiftExpression) : `(< ,$1 ,$3)
                         (RelationalExpression > ShiftExpression) : `(> ,$1 ,$3)
                         (RelationalExpression <= ShiftExpression) : `(<= ,$1 ,$3)
                         (RelationalExpression >= ShiftExpression) : `(>= ,$1 ,$3)
                         (RelationalExpression instanceof ShiftExpression) : `(instanceof ,$1 ,$3)
                         (RelationalExpression in ShiftExpression) : `(in ,$1 ,$3))

   (RelationalExpressionNoIn (ShiftExpression) : $1
                             (RelationalExpressionNoIn < ShiftExpression) : `(< ,$1 ,$3)
                             (RelationalExpressionNoIn > ShiftExpression) : `(> ,$1 ,$3)
                             (RelationalExpressionNoIn <= ShiftExpression) : `(<= ,$1 ,$3)
                             (RelationalExpressionNoIn >= ShiftExpression) : `(>= ,$1 ,$3)
                             (RelationalExpressionNoIn instanceof ShiftExpression) : `(instanceof ,$1 ,$3))

   (EqualityExpression (RelationalExpression) : $1
                       (EqualityExpression == RelationalExpression) : `(== ,$1 ,$3)
                       (EqualityExpression != RelationalExpression) : `(!= ,$1 ,$3)
                       (EqualityExpression === RelationalExpression) : `(=== ,$1 ,$3)
                       (EqualityExpression !== RelationalExpression) : `(!== ,$1 ,$3))

   (EqualityExpressionNoIn (RelationalExpressionNoIn) : $1
                           (EqualityExpressionNoIn == RelationalExpressionNoIn) : `(== ,$1 ,$3)
                           (EqualityExpressionNoIn != RelationalExpressionNoIn) : `(!= ,$1 ,$3)
                           (EqualityExpressionNoIn === RelationalExpressionNoIn) : `(=== ,$1 ,$3)
                           (EqualityExpressionNoIn !== RelationalExpressionNoIn) : `(!== ,$1 ,$3))

   (BitwiseANDExpression (EqualityExpression) : $1
                         (BitwiseANDExpression & EqualityExpression) : `(& ,$1 ,$3))
   (BitwiseANDExpressionNoIn (EqualityExpressionNoIn) : $1
                             (BitwiseANDExpressionNoIn & EqualityExpressionNoIn) : `(& ,$1 ,$3))

   (BitwiseXORExpression (BitwiseANDExpression) : $1
                         (BitwiseXORExpression ^ BitwiseANDExpression) : `(^ ,$1 ,$3))
   (BitwiseXORExpressionNoIn (BitwiseANDExpressionNoIn) : $1
                             (BitwiseXORExpressionNoIn ^ BitwiseANDExpressionNoIn) : `(^ ,$1 ,$3))

   (BitwiseORExpression (BitwiseXORExpression) : $1
                        (BitwiseORExpression bor BitwiseXORExpression) : `(bor ,$1 ,$3))
   (BitwiseORExpressionNoIn (BitwiseXORExpressionNoIn) : $1
                            (BitwiseORExpressionNoIn bor BitwiseXORExpressionNoIn) : `(bor ,$1 ,$3))

   (LogicalANDExpression (BitwiseORExpression) : $1
                         (LogicalANDExpression && BitwiseORExpression) : `(and ,$1 ,$3))
   (LogicalANDExpressionNoIn (BitwiseORExpressionNoIn) : $1
                             (LogicalANDExpressionNoIn && BitwiseORExpressionNoIn) : `(and ,$1 ,$3))

   (LogicalORExpression (LogicalANDExpression) : $1
                        (LogicalORExpression or LogicalANDExpression) : `(or ,$1 ,$3))
   (LogicalORExpressionNoIn (LogicalANDExpressionNoIn) : $1
                            (LogicalORExpressionNoIn or LogicalANDExpressionNoIn) : `(or ,$1 ,$3))

   (ConditionalExpression (LogicalORExpression) : $1
                          (LogicalORExpression ? AssignmentExpression colon AssignmentExpression) : `(if ,$1 ,$3 ,$5))
   (ConditionalExpressionNoIn (LogicalORExpressionNoIn) : $1
                              (LogicalORExpressionNoIn ? AssignmentExpressionNoIn colon AssignmentExpressionNoIn) : `(if ,$1 ,$3 ,$5))

   (AssignmentExpression (ConditionalExpression) : $1
                         (LeftHandSideExpression AssignmentOperator AssignmentExpression) : `(,$2 ,$1 ,$3))
   (AssignmentExpressionNoIn (ConditionalExpressionNoIn) : $1
                             (LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn) : `(,$2 ,$1 ,$3))
   (AssignmentOperator (=) : '=
                       (*=) : '*=
                       (/=) : '/=
                       (%=) : '%=
                       (+=) : '+=
                       (-=) : '-=
                       (<<=) : '<<=
                       (>>=) : '>>=
                       (>>>=) : '>>>=
                       (&=) : '&=
                       (^=) : '^=
                       (bor=) : 'bor=)

   (Expression (AssignmentExpression) : $1
               (Expression comma AssignmentExpression) : `(begin ,$1 ,$3))
   (ExpressionNoIn (AssignmentExpressionNoIn) : $1
                   (ExpressionNoIn comma AssignmentExpressionNoIn) : `(begin ,$1 ,$3))))
