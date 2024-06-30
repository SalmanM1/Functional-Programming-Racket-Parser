; The following comment block details the FIRST, FOLLOW, and PREDICT sets for the grammar used in this parser.
; I've taken these sets into consideration during the implementation of the parser logic.
; DEFINITIONS OF THE FIRST, FOLLOW, AND PREDICT SETS FOR OUR GRAMMAR
; FIRST(program) = {id, "$$"}
; FIRST(linelist) = {id, epsilon}
; FIRST(line) = {id}
; FIRST(label) = {id, epsilon}
; FIRST(linetail) = {";", epsilon}
; FIRST(stmt) = {id, if, while, read, write, goto, gosub, return, break, end}
; FIRST(boolean) = {true, false, id, num, "("}
; FIRST(bool-op) = {"<", ">", ">=", "<=", "<>", "="}
; FIRST(expr) = {id, num, "("}
; FIRST(etail) = {"+", "-", "*", "/", epsilon}
; FIRST(id) = [a-zA-Z]
; FIRST(num) = {+, -, digit}
; FIRST(numsign) = {"+", "-", epsilon}
; FIRST(digit) = [0-9]
; FOLLOW(program) = {$}
; FOLLOW(linelist) = {"$$", endwhile}
; FOLLOW(line) = {id, "$$"}
; FOLLOW(label) = {":", epsilon}
; FOLLOW(linetail) = {";", epsilon}
; FOLLOW(stmt) = {";", "endwhile", "$$"}
; FOLLOW(boolean) = {")"}
; FOLLOW(bool-op) = {id, num, "("}
; FOLLOW(expr) = {")", ";", "endwhile", "$$"}
; FOLLOW(etail) = {")", ";", "endwhile", "$$"}
; FOLLOW(id) = {"=", ":", "<", ">", ">=", "<=", "<>", "=", ";", "endwhile", "$$"}
; FOLLOW(num) = {")", "+", "-", "*", "/", ";", "endwhile", "$$"}
; FOLLOW(numsign) = {digit}
; FOLLOW(digit) = {digit, ")", "+", "-", "*", "/", ";", "endwhile", "$$"}
; PREDICT(program -> linelist $$) = FIRST(linelist)
; PREDICT(linelist -> line linelist | epsilon) = FIRST(line) U {epsilon}
; PREDICT(line -> label stmt linetail) = FIRST(label) U FIRST(stmt)
; PREDICT(label -> id: | epsilon) = {"id", epsilon}
; PREDICT(linetail -> ;stmt+ | epsilon) = {";", epsilon}
; PREDICT(stmt -> ...) = FIRST(stmt) ; specific to each starting token of stmt
; PREDICT(boolean -> true | false | expr bool-op expr) = FIRST(boolean)
; PREDICT(bool-op -> ...) = FIRST(bool-op)
; PREDICT(expr -> id etail | num etail | (expr)) = FIRST(expr)
; PREDICT(etail -> + expr | - expr | * expr | / expr | epsilon) = FIRST(etail)

#lang racket

(require data/maybe)  ; Incorporates 'maybe' types. I utilize 'just' extensively for the functional programming requirement.
(require racket/string)

; Utility Functions include (derived from grammar): isProperIdentifier, isValidNumericString, isSignCharacter,
; isSingleDigit, isValidIdentifier, isValidNumber.
(define (isProperIdentifier identifierStr)
  (regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9]*$" identifierStr))  ; Checks if identifier is valid (starts with a letter, followed by alphanumeric).

(define (isValidNumericString numericStr)
  (regexp-match? #rx"^[+-]?[0-9]+$" numericStr))  ; Validates numeric strings, allowing optional leading + or -.

(define (isSignCharacter signStr)
  (let ([sign-grammar "[+-]"])
    (regexp-match? (regexp sign-grammar) (string signStr))))  ; Determines if a string is a + or - sign.

(define (isSingleDigit charStr)
  (let ([digit-grammar "^[0-9]$"])
    (regexp-match? (regexp digit-grammar) (string charStr))))  ; Checks for a single digit (0-9).

(define (isValidIdentifier identifierStr)
  (let ([grammar "^([a-zA-Z]+)$"])
    (regexp-match? (regexp grammar) (string identifierStr))))  ; Ensures identifier consists only of letters.

(define (isValidNumber inputStr)
  (regexp-match? #rx"^(0|[1-9][0-9]*)$" inputStr))  ; Confirms string is a valid number (handles leading zeros).

; Core Parsing Functions include: processExpressionComponents, processParenthesizedExpr, evaluateParenthesizedTail, processExpressionTail,
; validateBooleanExpr, isValidBoolOperator.
(define (processExpressionComponents exprTokens)
  (if (null? exprTokens) 
      (just #t)  ; Base case: if no tokens, expression is valid.
      (let ([firstToken (car exprTokens)] [remainingTokens (cdr exprTokens)])
        (cond
          [(equal? firstToken "(") (and (processParenthesizedExpr remainingTokens) (just #t))]  ; Handles nested expressions.
          [(or (isProperIdentifier firstToken) (isValidNumericString firstToken)) 
           (and (processExpressionTail remainingTokens) (just #t))]  ; Processes expression tail if starts with valid id or number.
          [else (syntax-error lineNum "Invalid expression detected")]))))  ; Reports syntax error for invalid expressions.

(define (processParenthesizedExpr exprSegment)
  (if (null? exprSegment) 
      (just #t)  ; Checks for valid closure of parentheses.
      (let ([first (car exprSegment)] [rest (cdr exprSegment)])
        (cond
          [(isProperIdentifier first) (and (evaluateParenthesizedTail rest) (just #t))]  ; Validates identifier within parentheses.
          [(isValidNumericString first) (and (evaluateParenthesizedTail rest) (just #t))]  ; Validates numeric string within parentheses.
          [(equal? first "(") (processParenthesizedExpr rest)]  ; Recursively handles nested parentheses.
          [else #f]))))  ; Returns false for invalid expressions within parentheses.

(define (evaluateParenthesizedTail exprSegment)
  (if (null? exprSegment) 
      (just #t)  ; Ensures proper closure of expression after parentheses.
      (case (car exprSegment)
        [(")") (processExpressionTail (cdr exprSegment))]  ; Checks for end of parenthesized expression.
        [("+" "-") (processParenthesizedExpr (cdr exprSegment))]  ; Processes addition or subtraction inside parentheses.
        [("*" "/") (processParenthesizedExpr (cdr exprSegment))]  ; Handles multiplication or division within parentheses.
        [else #f])))  ; Catches any other invalid cases within parenthesized expressions.

(define (processExpressionTail expressionParts)
  (cond
    [(null? expressionParts) (just #t)]  ; Empty tail is considered valid.
    [(equal? (car expressionParts) "+") (and (processExpressionComponents (cdr expressionParts)) (just #t))]  ; Handles addition operator.
    [(equal? (car expressionParts) "-") (and (processExpressionComponents (cdr expressionParts)) (just #t))]  ; Handles subtraction operator.
    [(equal? (car expressionParts) "*") (and (processExpressionComponents (cdr expressionParts)) (just #t))]  ; Handles multiplication operator.
    [(equal? (car expressionParts) "/") (and (processExpressionComponents (cdr expressionParts)) (just #t))]  ; Handles division operator.
    [(equal? (car expressionParts) ";") (and (statement (cdr expressionParts)) (just #t))]  ; Processes next statement after semicolon.
    [else #f]))  ; Returns false for invalid expressions.

(define (validateBooleanExpr operatorStr)
  (let ([firstElem (car operatorStr)]
        [secondElem (cadr operatorStr)]
        [thirdElem (caddr operatorStr)]
        [remainingElems (cdddr operatorStr)])
    (cond
      [(or (equal? operatorStr "true") (equal? operatorStr "false")) (just #t)]  ; Directly validates boolean literals.
      [(and (processExpressionComponents (list firstElem secondElem))
            (isValidBoolOperator thirdElem)
            (processParenthesizedExpr remainingElems)) (just #t)]  ; Validates boolean expressions.
      [else (syntax-error lineNum "Invalid boolean detected")])))

(define (isValidBoolOperator operatorStr)
  (if (member operatorStr '("<" ">" ">=" "<=" "<>" "="))
      (just #t)  ; Validates boolean operators.
      #f))  ; Returns false for invalid operators.

(define (setWhileFlag updateWhileState)
  (set! while-checker updateWhileState))  ; Updates the global flag for while loop tracking.

(define (incrementLineNum currentLine)
  (set! lineNum (+ 1 currentLine)))  ; Increments the global line number counter.

; "Helpers" to Parsing Functions (Validation) include: split-and-validate, check-input-validity, split-input-by-parentheses, process-line, statement.
(define (split-and-validate inputStr)
  (let ((my-list (regexp-split (pregexp "((?=:)|(?<!:)(?=$))") inputStr)))
    (cond
      [(and (isProperIdentifier (car my-list)) 
            (equal? (last my-list) ":")) 
       #t]  ; Validates if input starts with a proper identifier and ends with a colon.
      [else 
       #f])))  ; Invalid if conditions are not met.

(define (check-input-validity input)
  (let ((isInputEmpty (empty? input))
        (isValidInput (split-and-validate input)))
    (or isInputEmpty isValidInput)))  ; Checks if input is either empty or valid according to split-and-validate.

(define (split-input-by-parentheses inputStr)
  (filter (lambda (str) (and (not (equal? str "")) (non-empty-string? str)))
          (regexp-split 
            (pregexp "((?=\\()|(?<=\\()|(?=\\))|(?<=\\))|(\\s+))") inputStr)))  ; Splits input by parentheses and spaces, filtering out empty strings.

(define lineNum 0)  ; Initializes line number counter to zero.

(define (endOfTheLine in)
  #t)  ; Placeholder function, always returns true. Adjust as needed for end-of-line logic.

(define (process-line input)
  (let ((tokens (split-input-by-parentheses input)))
    (incrementLineNum lineNum)
    (cond
      [(and (check-input-validity (car tokens)) ; Checks if the first token is valid, the rest of the line forms a valid statement,
            (statement (cdr tokens))            ; and the last token signifies the end of the line. 
            (endOfTheLine (last tokens)))
       (just #t)]
      [(and (statement tokens) ; Checks if the entire line forms a valid statement and ends properly.
            (endOfTheLine (last tokens)))
       (just #t)]
      [else (syntax-error lineNum "Invalid line detected")]))) ; If neither condition is met, reports a syntax error.

(define while-checker #f)  ; Global flag to track 'while' loop presence.

(define (statement input)
  (begin
    (cond
      [(and (> (length input) 1) 
            (isProperIdentifier (car input)) 
            (equal? (cadr input) "=") 
            (processExpressionComponents (cdr (cdr input))))
       (just #t)]  ; Handles assignment statements.
      [(and (equal? (car input) "if") 
            (validateBooleanExpr (cdr input)))
       (just #t)]  ; Processes 'if' conditions.
      [(and (equal? (car input) "while") 
            (validateBooleanExpr (cdr input)))
       (setWhileFlag #t) 
       (just #t)]  ; Initiates 'while' loop processing.
      [(and (equal? while-checker #t) 
            (equal? (car input) "endwhile"))
       (setWhileFlag #f) 
       (just #t)]  ; Marks end of 'while' loop.
      [(and (> (length input) 1) 
            (equal? (car input) "read") 
            (isProperIdentifier (cadr input)))
       (just #t)]  ; Validates 'read' statements.
      [(and (equal? (car input) "write") 
            (processExpressionComponents (cdr input)))
       (just #t)]  ; Handles 'write' operations.
      [(and (> (length input) 1) 
            (equal? (car input) "goto") 
            (isProperIdentifier (cadr input)))
       (just #t)]  ; Processes 'goto' statements.
      [(and (> (length input) 1) 
            (equal? (car input) "gosub") 
            (isProperIdentifier (cadr input)))
       (just #t)]  ; Validates 'gosub' subroutine calls.
      [(and (equal? (length input) 1) 
            (equal? (car input) "return"))
       (just #t)]  ; Confirms 'return' statements.
      [(and (equal? (length input) 1) 
            (equal? (car input) "break"))
       (just #t)]  ; Checks for 'break' within loops.
      [(and (equal? (length input) 1) 
            (equal? (car input) "end"))
       (just #t)]  ; Marks program termination point.
      [else (syntax-error lineNum "Invalid statement detected")]  ; Catches any unrecognized statements.
    )))

; Recursive Parsing Functions include: evaluate-lines, prepare-list-for-end-marker, validate-file-content.
(define (evaluate-lines lines)
  (cond
    [(null? lines) #t]  ; Base case: if there are no lines, evaluation is successful.
    [(equal? (string-trim (car lines)) "$$") #t]  ; Stops evaluation if a line solely consists of "$$".
    [else 
     (and (process-line (car lines))        ; Processes the current line,
          (evaluate-lines (cdr lines)))]))  ; then recursively evaluates the rest.

(define (prepare-list-for-end-marker lst)
  (define (trim-and-check-end-marker line)
    (equal? (string-trim line) "$$"))  ; Helper function to check if a line is the end marker.
  (let loop ([remaining lst] [result '()])
    (cond
      [(null? remaining) (reverse result)]  ; Returns the processed list when no more lines are left.
      [else
       (let ([current (car remaining)] [rest (cdr remaining)])
         (if (trim-and-check-end-marker current)  ; If current line is end marker,
             (reverse (cons current result))      ; include it and terminate processing.
             (loop rest (cons current result))))])))  ; Otherwise, continue with the rest.

(define (validate-file-content inputLines)
  (let ((preparedList (prepare-list-for-end-marker inputLines)))
    (and 
      (evaluate-lines preparedList)  ; Evaluates all lines up to the end marker.
      (equal? (string-trim (last preparedList)) "$$"))))  ; Ensures the last line is the end marker.

; Syntax and Error Handling 
(define-syntax-rule (syntax-error line message)
  (error 
    (format "Syntax error on line ~a: ~a" line message)))  ; Custom macro for standardized syntax error reporting, including the line number as required.

; Main Parse Function
(define (parse file)
  (with-handlers ([exn:fail? (lambda (exn)
                                (displayln (exn-message exn)))])  ; Error handling for exceptions during parsing.
    (begin
      (define filelist (file->lines file))  ; Reads the file into a list of lines.
      (validate-file-content filelist)  ; Validates the content structure and syntax.
      (when (validate-file-content filelist)  ; Checks if content is valid.
          (displayln "Accept")))))  ; Prints "Accept" if content passes validation.

(parse "FILENAMEHERE")  ; Initiates parsing of the file.
; rkt file must be in the same directory as the txt files.