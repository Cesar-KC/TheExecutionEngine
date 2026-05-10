#lang racket
;RDP from Previous Prog 2------------------------
(struct token (type value) #:transparent)   

(define token-types
  
  '(IF-TOK THEN-TOK ELSE-TOK WHILE-TOK DO-TOK END-TOK PRINT-TOK ID-TOK INT-TOK FP-TOK PLUS-TOK MINUS-TOK MULT-TOK DIV-TOK
           ASSIGN-TOK EQ-TOK NEQ-TOK GT-TOK GTE-TOK LT-TOK LTE-TOK SEMI-TOK LPAREN-TOK RPAREN-TOK  EOF-TOK))                                             

(define (strip-comments source)
  (when (string=? source "")
    (error "Lexical Error: source input is empty"))
  
  (define (helper chars in-comment?)
    (cond
      [(and (null? chars) in-comment?)
       (error "Lexical Error: unclosed comment, missing */")]
      [(null? chars) '()]
      [(and in-comment?
            (char=? (car chars) #\*)
            (not (null? (cdr chars)))
            (char=? (cadr chars) #\/))
       (cons #\space (helper (cddr chars) #f))]
      [in-comment?
       (helper (cdr chars) #t)]
      [(and (char=? (car chars) #\/)
            (not (null? (cdr chars)))
            (char=? (cadr chars) #\*))
       (helper (cddr chars) #t)]
      [else
       (cons (car chars) (helper (cdr chars) #f))]))
  
  (list->string (helper (string->list source) #f)))

(define (make-number-token num-str)  

  (cond
    [(regexp-match? #px"^[+-]?[0-9]+\\.[0-9]+$" num-str)     ; FP: optional sign, digits, decimal, digits
     (token 'FP-TOK (string->number num-str))]
    [(regexp-match? #px"^[+-]?[0-9]+\\.$" num-str)     ; FP: trailing decimal allowed
     (token 'FP-TOK (string->number (string-append num-str "0")))]
    [(regexp-match? #px"^[+-]?0$" num-str)    ; INT: bare 0 exception
     (token 'INT-TOK 0)]
    [(regexp-match? #px"^[+-]?[1-9][0-9]*$" num-str)    ; INT: optional sign, non-zero leading digit
     (token 'INT-TOK (string->number num-str))]
    
    [else
     (error "Lexical Error: invalid number literal")])) ;aka '05' or .5' eetc --Invalid

(define (tokenize source) ;Tokenize
  (letrec
     ([collect-word
      (lambda (chars)
        (cond
          [(null? chars) '()]
          [(or (char-alphabetic? (car chars))
               (char-numeric? (car chars))
               (char=? (car chars) #\_)
               (char=? (car chars) #\-))
           (cons (car chars) (collect-word (cdr chars)))]
          [else '()]))]
    
     [collect-number ; function to collect valid number characters, digits and decimals
      (lambda (chars)
        (cond
          [(null? chars) '()]
          [(or (char-numeric? (car chars))
               (char=? (car chars) #\.))
           (cons (car chars) (collect-number (cdr chars)))]
          [else '()]))]
    
     [helper ; main function, recursive, walks character list and produces token list, has 'prev-token' to clear amiguity of unary vs binary
      (lambda (chars prev-token)
        (cond
          [(null? chars) '()]
          [(char-whitespace? (car chars))
           (helper (cdr chars) prev-token)]
          [(char=? (car chars) #\:)
           (if (and (not (null? (cdr chars)))
                    (char=? (cadr chars) #\=))
               (cons (token 'ASSIGN-TOK #f)
                     (helper (cddr chars) (token 'ASSIGN-TOK #f)))
               (error "Lexical Error: expected ':='"))]
          [(char=? (car chars) #\!)
           (if (and (not (null? (cdr chars)))
                    (char=? (cadr chars) #\=))
               (cons (token 'NEQ-TOK #f)
                     (helper (cddr chars) (token 'NEQ-TOK #f)))
               (error "Lexical Error: expected '!='"))]
          [(char=? (car chars) #\>)
           (if (and (not (null? (cdr chars)))
                    (char=? (cadr chars) #\=))
               (cons (token 'GTE-TOK #f)
                     (helper (cddr chars) (token 'GTE-TOK #f)))
               (cons (token 'GT-TOK #f)
                     (helper (cdr chars) (token 'GT-TOK #f))))]
          [(char=? (car chars) #\<)
           (if (and (not (null? (cdr chars)))
                    (char=? (cadr chars) #\=))
               (cons (token 'LTE-TOK #f)
                     (helper (cddr chars) (token 'LTE-TOK #f)))
               (cons (token 'LT-TOK #f)
                     (helper (cdr chars) (token 'LT-TOK #f))))]
          [(char=? (car chars) #\=)
           (cons (token 'EQ-TOK #f)
                 (helper (cdr chars) (token 'EQ-TOK #f)))]
          [(char=? (car chars) #\;)
           (cons (token 'SEMI-TOK #f)
                 (helper (cdr chars) (token 'SEMI-TOK #f)))]
          [(char=? (car chars) #\()
           (cons (token 'LPAREN-TOK #f)
                 (helper (cdr chars) (token 'LPAREN-TOK #f)))]
          [(char=? (car chars) #\))
           (cons (token 'RPAREN-TOK #f)
                 (helper (cdr chars) (token 'RPAREN-TOK #f)))]
          [(char=? (car chars) #\*)
           (cons (token 'MULT-TOK #f)
                 (helper (cdr chars) (token 'MULT-TOK #f)))]
          [(char=? (car chars) #\/)
           (cons (token 'DIV-TOK #f)
                 (helper (cdr chars) (token 'DIV-TOK #f)))]

          [(char=? (car chars) #\+)  ;----here we check if prev-token is valid, then operation, else assignment(unary)
           (if (and prev-token  ; + unary or binary        
                    (member (token-type prev-token)
                            '(ID-TOK INT-TOK FP-TOK RPAREN-TOK)))
               (cons (token 'PLUS-TOK #f)
                     (helper (cdr chars) (token 'PLUS-TOK #f)))
               (let* ([num-chars (cons (car chars) (collect-number (cdr chars)))]
                      [num-str   (list->string num-chars)]
                      [remaining (list-tail chars (length num-chars))]
                      [num-tok   (make-number-token num-str)])
                 (cons num-tok (helper remaining num-tok))))]
          [(char=? (car chars) #\-)
           (if (and prev-token       ; - unary or binary
                    (member (token-type prev-token)
                            '(ID-TOK INT-TOK FP-TOK RPAREN-TOK)))
               (cons (token 'MINUS-TOK #f)
                     (helper (cdr chars) (token 'MINUS-TOK #f)))
               (let* ([num-chars (cons (car chars) (collect-number (cdr chars)))]
                      [num-str   (list->string num-chars)]
                      [remaining (list-tail chars (length num-chars))]
                      [num-tok   (make-number-token num-str)])
                 (cons num-tok (helper remaining num-tok))))]
          
          [(char-numeric? (car chars))  ; numbers, we used defined 'make-number-token' to classify float or int
           (let* ([num-chars (collect-number chars)]
                  [num-str   (list->string num-chars)]
                  [remaining (list-tail chars (length num-chars))]
                  [num-tok   (make-number-token num-str)])
             (if (and (not (null? remaining))
                      (char-alphabetic? (car remaining)))
                 (error "Lexical Error: invalid token, number followed by letter")
                 (cons num-tok (helper remaining num-tok))))]
          [(or (char-alphabetic? (car chars)) ; Keywords, else its an identifier
               (char=? (car chars) #\_))
           (let* ([word-chars (collect-word chars)]
                  [word       (list->string word-chars)]
                  [remaining  (list-tail chars (length word-chars))]
                  [word-tok   (cond
                                [(string=? word "IF")    (token 'IF-TOK #f)]
                                [(string=? word "THEN")  (token 'THEN-TOK #f)]
                                [(string=? word "ELSE")  (token 'ELSE-TOK #f)]
                                [(string=? word "WHILE") (token 'WHILE-TOK #f)]
                                [(string=? word "DO")    (token 'DO-TOK #f)]
                                [(string=? word "END")   (token 'END-TOK #f)]
                                [(string=? word "PRINT") (token 'PRINT-TOK #f)]
                                [else                    (token 'ID-TOK word)])])
             (cons word-tok (helper remaining word-tok)))]
          
          [else ; invalid character, throw error
           (error "Lexical Error: invalid character")]))])

    (helper (string->list source) #f)))

(define (scan source) ;;---------SCANNER---------
 (tokenize (strip-comments source)))

(define (peek tokens) ;;--check tokens           
  (if (null? tokens)
      (token 'EOF-TOK #f)        
      (car tokens)))

(define (consume tokens expected-type)  ;---valid tokens only
  (if (null? tokens)
      (error "Syntax Error: unexpected end of input")
      (if (equal? (token-type (car tokens)) expected-type)
          (cdr tokens)
          (error "Syntax Error: unexpected token"))))

(define (parse-factor tokens) ;---------PARSER, from bottom up, parse factor
  (let ([tok (peek tokens)])
    (cond
      [(or (equal? (token-type tok) 'INT-TOK)
           (equal? (token-type tok) 'FP-TOK))
       (cons (token-value tok) (cdr tokens))]
      
      [(equal? (token-type tok) 'ID-TOK)
       (cons (string->symbol (token-value tok)) (cdr tokens))]
      
      [(equal? (token-type tok) 'LPAREN-TOK)
       (let* ([tokens1 (consume tokens 'LPAREN-TOK)]
              [result  (parse-expression tokens1)]     ; define parse expression
              [tokens2 (consume (cdr result) 'RPAREN-TOK)])
         (cons (car result) tokens2))]
      
      [else
       (error "Syntax Error: expected number, identifier, or '('")])))

(define (parse-term tokens) ;------Parse_Term---------
  (let* ([r (parse-factor tokens)]) ; r stores parse_facotr
    (let loop ([left-ast (car r)]   ; left-AST is first of r, rest is rest
               [rest     (cdr r)])
      (let ([tok (peek rest)])
        (cond
          [(equal? (token-type tok) 'MULT-TOK)   ; multiply term
           (let* ([t  (consume rest 'MULT-TOK)]
                  [r2 (parse-factor t)])
             (loop (list '* left-ast (car r2))
                   (cdr r2)))]
          [(equal? (token-type tok) 'DIV-TOK)   ; Divide term
           (let* ([t  (consume rest 'DIV-TOK)]
                  [r2 (parse-factor t)])
             (loop (list '/ left-ast (car r2))
                   (cdr r2)))]
          
          [else ; anythign else, done, return
           (cons left-ast rest)])))))

(define (parse-expression tokens) ;-----Parse_expression------
  (let* ([r (parse-term tokens)])   ;r stores parse-expressions return
    (let loop ([left-ast (car r)]   ; left-ast first result in r
               [rest     (cdr r)])  ;rest is rest of pair(list)
      (let ([tok (peek rest)])
        (cond
          
          [(equal? (token-type tok) 'PLUS-TOK)
           (let* ([t  (consume rest 'PLUS-TOK)]
                  [r2 (parse-term t)])
             (loop (list '+ left-ast (car r2))
                   (cdr r2)))]
          
          [(equal? (token-type tok) 'MINUS-TOK)
           (let* ([t  (consume rest 'MINUS-TOK)]
                  [r2 (parse-term t)])
             (loop (list '- left-ast (car r2))
                   (cdr r2)))]
          
          [else
           (cons left-ast rest)])))))

(define (parse-comparison tokens) ;------Parse_comparison, needs to parse left , compare, parse right, return node
  (let* ([left  (parse-expression tokens)]
         [tok   (peek (cdr left))]  
         [op    (cond   ;conditioanls for each token, convert to symbol
                  [(equal? (token-type tok) 'EQ-TOK)  'eq]
                  [(equal? (token-type tok) 'NEQ-TOK) 'neq]
                  [(equal? (token-type tok) 'GT-TOK)  'gt]
                  [(equal? (token-type tok) 'GTE-TOK) 'gte]
                  [(equal? (token-type tok) 'LT-TOK)  'lt]
                  [(equal? (token-type tok) 'LTE-TOK) 'lte]
                  [else (error "Syntax Error: expected relational operator")])]
         [t     (cdr (cdr left))] ;remaining tokesn
         [right (parse-expression t)])  ;parse right
    (cons (list op (car left) (car right))
          (cdr right))))

(define (parse-statement tokens) ;------Parse_statemtnt, takes a 'peek' , then chooses correct parse statement
  (let ([tok (peek tokens)])
    (cond
      [(equal? (token-type tok) 'IF-TOK) ;  call if stamement
       (parse-if-stmt tokens)]
      [(equal? (token-type tok) 'WHILE-TOK)  ; call while statemtn
       (parse-while-stmt tokens)]
      [(equal? (token-type tok) 'ID-TOK) ; id, assignemnt statemtn
       (parse-assign-stmt tokens)]
      [(equal? (token-type tok) 'PRINT-TOK) ; print statemtn
       (parse-print-stmt tokens)]

      [else
       (error "Syntax Error: unexpected token in statement")])))

(define (parse-if-stmt tokens) ;-------Parse-If--------
  (let* ([t1       (consume tokens 'IF-TOK)]  ;several  checks for each token in an 'if-else'
         [cmp      (parse-comparison t1)]
         [t2       (consume (cdr cmp) 'THEN-TOK)]
         [then-body (parse-stmt-list t2)]
         [t3       (consume (cdr then-body) 'ELSE-TOK)]
         [else-body (parse-stmt-list t3)]
         [t4       (consume (cdr else-body) 'END-TOK)])
    
    (cons (list 'if (car cmp)
                (list 'then (car then-body))
                (list 'else (car else-body)))
          t4)))
(define (parse-while-stmt tokens)   ;-------Parse_while-----
  (let* ([t1   (consume tokens 'WHILE-TOK)]
         [cmp  (parse-comparison t1)]
         [t2   (consume (cdr cmp) 'DO-TOK)]
         [body (parse-stmt-list t2)]
         [t3   (consume (cdr body) 'END-TOK)])

    (cons (list 'while (car cmp) (car body))
          t3)))

(define (parse-assign-stmt tokens) ;--------Parse_assign, ;assign value to ID after assign token until semi-token
  (let* ([id-tok (peek tokens)]
         [t1     (consume tokens 'ID-TOK)]
         [t2     (consume t1 'ASSIGN-TOK)]
         [expr   (parse-expression t2)]
         [t3     (consume (cdr expr) 'SEMI-TOK)])
    (cons (list 'assign (string->symbol (token-value id-tok)) (car expr))
          t3)))

(define (parse-print-stmt tokens) ;-------Parse_print, print tokens after print-token and until sem-token
  (let* ([t1   (consume tokens 'PRINT-TOK)]
         [expr (parse-expression t1)]
         [t2   (consume (cdr expr) 'SEMI-TOK)])
    (cons (list 'print (car expr))
          t2)))

(define (parse-stmt-list tokens) ;----Parse-statement-List, statemtns empy & tokens is not --> check next token. If statetment -->add to statements, else done.
  (let loop ([stmts '()]
             [rest  tokens])
    (let ([tok (peek rest)])
      (cond
        [(equal? (token-type tok) 'SEMI-TOK) ; skip stray semicolons between statements
         (loop stmts (cdr rest))]
        [(member (token-type tok) ; valid statement starter, parse it
                 '(IF-TOK WHILE-TOK ID-TOK PRINT-TOK))
         (let ([result (parse-statement rest)])
           (loop (append stmts (list (car result)))
                 (cdr result)))]
        
        [else  ; not a statement starter, done — always return list for consistency
           (cons stmts rest)]))))

(define (parse-program tokens) ;--------PARSE-PROGRAM, this is the entry point
  (let* ([result (parse-stmt-list tokens)]) ;call parse-stmtn-list on tokens
    (if (equal? (token-type (peek (cdr result))) 'EOF-TOK) ; if token is EOf-tok, then all parsed, eturn
        (cons 'program (car result))  ; creates AST(pair of program(root) and others nodes)
        
        (error "Syntax Error: unexpected token at end"))))

;--------------------------------------------------------------------------------------------------------------------------------

;PROGRAM3 - Evaluator Implementation HERE

;ENV LOOKUP--lookup if variable in envireomtn--
(define (env-lookup var env)                                                                                       
    (cond                                                                                                          
      ; enviroment empty, variable never found, error                                                                 
      [(null? env)                                                                                                   
       (error (string-append "Unbound Variable: " (symbol->string var)))]                                            
                                                                                                                     
      ; first pair's name matches var we're looking up, return its value (second name of pair)                                                     
      [(equal? (caar env) var)                                                                                       
       (cdar env)]                                                                                                   
                                                                                                                   
      ; not this pair, recurse on rest of environment until found or null(return error
      [else
       (env-lookup var (cdr env))]))

;ENV ADD--adds variable to enviroment, if already present, variable is 'updated' by shadowing old value by being to front of list, ie the old variable-value pair never accessed/called--
(define (env-update var val env)
    (cons (cons var val) env))

;EVAL RULES(Three Eval Functions):

;;Evaluate Expression:
(define (eval-expr node env)                                                                                       
    (cond                                                                                                                                                                                         
      [(number? node) node]     ; AST node is a number literal, return as-is                                                                                                                                                                                                                                                                                     
      [(symbol? node) (env-lookup node env)]  ; AST node is a variable, look up in environment , error if not in env
      
      [(list? node)              ; AST node is a list, check operator, then recursion handles the operands.  Handles nested expressions!
       (let ([op    (car node)]                                                                                      
             [left  (eval-expr (cadr node) env)]                                                                   
             [right (eval-expr (caddr node) env)])
         (cond                     ; check operator                                                                
           [(equal? op '+) (+ left right)]
           [(equal? op '-) (- left right)]                                                                           
           [(equal? op '*) (* left right)]                                                                         
           [(equal? op '/) (if (= right 0)
                              (error "Runtime Error: division by zero")
                              (/ left right))]
           [else (error "Runtime Error: unknown operator")]))]                                                       
   
      [else (error "Runtime Error: invalid expression")]))

;;Evaluate Comparison:                                                                                             
  (define (eval-comp node env)                                                                                     
    (let ([op    (car node)]          ; 'let' binds comparison operator (eq, gt, lte, etc) to 'op' to first value in AST node                        
          [left  (eval-expr (cadr node) env)]    ; evaluate left operand — could be number, symbol, or nested expression
          [right (eval-expr (caddr node) env)])  ; evaluate right operand — same, both resolved to numbers before conditon runs and we make comparison                                                                                                             

      (cond                                                                                                          
        ; by this point left and right are numbers, op is comparison operator                                        
        ; eval-expr handled any arithmetic recursion inside operands                                                 
        ; we just apply the comparison and return True or false(#t or #f)                                                          
        [(equal? op 'eq)  (= left right)]                                                                            
        [(equal? op 'neq) (not (= left right))]                                                                      
        [(equal? op 'gt)  (> left right)]                                                                            
        [(equal? op 'gte) (>= left right)]                                                                           
        [(equal? op 'lt)  (< left right)]
        [(equal? op 'lte) (<= left right)]                                                                           
        [else (error "Runtime Error: unknown comparison operator")])))

;;Execute Statement (or multiple statements):
(define (exec-stmt stmt env)
    (cond                                                                                                            
      ; AST node is 'assign', we should eval expression, add to env, return new env
      [(equal? (car stmt) 'assign)                                                                                   
       (env-update (cadr stmt) (eval-expr (caddr stmt) env) env)]                                                    
                                                                 
      ; AST node is 'print', we need to eval expression, display, return env unchanged                                                       
      [(equal? (car stmt) 'print)                                                                                  
       (displayln (eval-expr (cadr stmt) env))                                                                       
       env]                                                                                                        
           
      ; AST node is 'if-then-else', we eval condition, execute correct branch, return resulting env
      [(equal? (car stmt) 'if)                                                                                       
       (if (eval-comp (cadr stmt) env)
           (exec-stmts (cadr (caddr stmt)) env)                                                                      
           (exec-stmts (cadr (cadddr stmt)) env))]                                                                   
                                                  
      ;AST node is 'while loop', we eval condition, if #t exec body with new env and recurse                                             
      [(equal? (car stmt) 'while)                                                                                  
       (if (eval-comp (cadr stmt) env)                                                                               
           (exec-stmt stmt (exec-stmts (caddr stmt) env))                                                            
           env)]                                         
                                                                                                                     
      [else (error "Runtime Error: unknown statement type")]))     ;unknown AST node, output an error                                                
                                                                                                                     

;;;Mutliple statements handling, 
(define (exec-stmts stmts env)                                                                                     
  (if (null? stmts)  ; stmts null, return enviroment 
      env                                                                                                          
      (exec-stmts (cdr stmts)  ; else, not emmpty, so recurse on rests of multiple statments,                                                                                   
                  (exec-stmt (car stmts) env)))) ; executing first, and passing enviroment forward(Enviroment Threading!)





;THE INTERPRETER------------------------------
;THE ENTIRE WORKFLOW PIPELINE WIRED TOGETHER!
;;source code -> scan into valid token list -> parse into a valid AST tree -> Evaluate AST(recursively walking it to completion, updating the enviroment) -> Desired Output!

(define (run source)                                                                                               
  (exec-stmts (cdr (parse-program (scan source))) '()))







;--TESTS--------------------------------------------------------------------

; basic assignment and print — expect: 5
(run "x := 5; PRINT x;")

; arithmetic precedence — expect: 7  (1 + 2*3 = 7)
(run "x := 1 + 2 * 3; PRINT x;")

; full precedence — expect: 5  ((1 + 2*3) - 4/2 = 5)
(run "x := 1 + 2 * 3 - 4 / 2; PRINT x;")

; multiple variables — expect: 10
(run "x := 5; y := x * 2; PRINT y;")

; if true branch — expect: 20
(run "x := 10; IF x > 5 THEN y := 20; ELSE y := 30; END PRINT y;")

; if false branch — expect: 30
(run "x := 3; IF x > 5 THEN y := 20; ELSE y := 30; END PRINT y;")

; while countdown — expect: 2 1 0
(run "x := 3; WHILE x > 0 DO x := x - 1; PRINT x; END")

; scope test from spec — expect: 20
(run "x := 10; IF x > 5 THEN y := 20; ELSE y := 30; END PRINT y;")

; nested arithmetic in while — expect: 0 2 6
(run "x := 0; y := 1; WHILE y < 4 DO x := x + y * 2; PRINT x; y := y + 1; END")

; unbound variable error — uncomment to test
;(run "PRINT x;")

; division by zero error — uncomment to test
;(run "x := 5 / 0; PRINT x;")




;NOTES:----------
;; eval-expr: takes AST node and environment, always returns a number                                               
  ; three cases:                                                                                                   
  ;   1. number  → return directly (base case)                                                                       
  ;   2. symbol  → lookup in env, return value (base case)                                                           
  ;   3. list    → recurse on left and right operands first (downward),                                              
  ;                apply operator once both resolve to numbers (upward)                                              
  ;                                                                                                                  
  ; recursion uses local let bindings — each call has its own left/right                                             
  ; nesting handled automatically — no matter how deep, always bottoms out at a number 


;; eval-comp: takes a comparison AST node and environment, always returns #t or #f
  ; one case:
  ;   comparison node (eq, neq, gt, gte, lt, lte)
  ;     → call eval-expr on left operand  → number
  ;     → call eval-expr on right operand → number
  ;     → apply comparison operator       → boolean
  ;
  ; depends on eval-expr to resolve both sides to numbers first
  ; maps directly to Racket built-ins: =, not(=), >, >=, <, <=
  ;
  ; eval-comp just answers the question — exec-stmt decides what to do with the answer
  ; eval-comp never needs to know about then/else branches, it just returns a boolean


;; exec-stmt: takes a single statement AST node and environment, always returns updated environment
  ; four cases:
  ;   assign → eval the expression, prepend new binding to env, return new env
  ;   print  → eval the expression, display result, return env unchanged (side effect only)
  ;   if     → eval condition via eval-comp (#t or #f)
  ;              #t → execute then-body, return resulting env
  ;              #f → execute else-body, return resulting env
  ;            env from chosen branch is returned to main sequence — variables assigned inside if persist
  ;   while  → eval condition via eval-comp
  ;              #t → execute body via exec-stmts, get new env, recurse on same while node with new env
  ;              #f → return current env, loop done
  ;            condition is re-evaluated every iteration against the latest env — no stale data


;; exec-stmts: takes a list of statement nodes and environment, threads env through each statement
  ; base case  → list is empty, return env as-is
  ; recursive  → execute first statement, get new env, pass new env to rest of list
  ; this is environment threading — each statement receives the env left by the previous one
  ; the chain: stmt1 → env1 → stmt2 → env2 → stmt3 → final env


;; env-lookup: takes a variable name and environment, returns its value
  ; walks association list recursively, checking first pair each time
  ; found     → return value (cdar of current pair)
  ; not found → error "Unbound Variable: x" — forces initialization before use


;; env-update: takes a variable name, value, and environment, returns new environment
  ; prepends new (var . val) pair to front of list
  ; old binding for same var is shadowed — lookup finds newest first
  ; environment grows as history — all past bindings preserved, only newest accessed


;; -------------------------------------------------------------------------
;; FULL INTERPRETER SUMMARY
;; -------------------------------------------------------------------------
;; This interpreter implements a complete pipeline from source string to output.
;; Built in three phases, each feeding the next:
;;
;; PHASE 1 — SCANNER (scan)
;;   Input:  source string
;;   Output: flat list of tokens in source order
;;   Work:   strips comments, classifies every character into a token type
;;           whitespace ignored, keywords vs identifiers distinguished
;;           unary vs binary +/- resolved using previous token context
;;
;; PHASE 2 — PARSER (parse-program)
;;   Input:  token list
;;   Output: Abstract Syntax Tree (AST)
;;   Work:   recursive descent — each grammar rule is one function
;;           operator precedence encoded in call hierarchy (factor → term → expression)
;;           produces nested list structure mirroring the program's logic
;;           (program (assign x (+ 1 (* 2 3))) (print x))
;;
;; PHASE 3 — INTERPRETER (exec-stmts)
;;   Input:  AST + empty environment '()
;;   Output: console output (PRINT statements) + final environment
;;   Work:   walks the AST recursively, three eval functions:
;;             eval-expr  — computes arithmetic, returns number
;;             eval-comp  — evaluates comparison, returns #t or #f
;;             exec-stmt  — executes statement, returns updated environment
;;           environment is an immutable association list — never mutated,
;;           always threaded forward as a new value through every function call
;;           while loop re-evaluates condition each iteration with latest env
;;           variables assigned inside if/while blocks persist after the block ends
;;
;; PIPELINE:
;;   source string
;;     → scan        → token list
;;     → parse       → AST
;;     → exec-stmts  → final env  (+ printed output as side effects)
;;
;; ENTRY POINT: (run "source code here")
;; -------------------------------------------------------------------------
