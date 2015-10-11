#| Assignment 1 - Functional Shakespeare Interpreter

Read through the starter code carefully. In particular, look for:

- interpret: the main function used to drive the program.
  This is provided for you, and should not be changed.
- evaluate: this is the main function you'll need to change.
  Please put all helper functions you write below this one.
  Doing so will greatly help TAs when they are marking. :)
|#
#lang racket

; You are allowed to use all the string functions in this module.
; You may *not* import any other modules for this assignment.
(require racket/string)

; This exports the main driver function. Used for testing purposes.
; This is the only function you should export. Don't change this line!
(provide interpret)

;------------------------------------------------------------------------------
; Parsing constants
;------------------------------------------------------------------------------

; Sections dividers
(define personae "Dramatis personae")
(define settings "Settings")
(define finis "Finis")

; Comment lines
(define comments '("Act" "Scene"))

; List of all "bad words" in a definition
(define bad-words
  '("vile"
    "villainous"
    "wicked"
    "naughty"
    "blackhearted"
    "shameless"
    "scoundrelous"))

; Arithmetic
(define add "join'd with")
(define mult "entranc'd by")

; Self-reference keywords
(define self-refs
  '("I"
    "me"
    "Me"
    "myself"
    "Myself"))

; Function call
(define call "The song of")

; Function parameter name
(define param "Hamlet")

(define description-type "description")
;------------------------------------------------------------------------------
; Interpreter driver
;------------------------------------------------------------------------------

#|
(interpret filename)
  filename: a string representing the path to a FunShake file

  Returns a list of numbers produced when evaluating the FunShake file.
  You can complete this assignment without modifying this function at all,
  but you may change the implementation if you like. Please note that you may
  not change the interface, as this is the function that will be autotested.
|#
(define (interpret filename)
  (let* ([contents (port->string (open-input-file filename))]
         [lines (map normalize-line (string-split contents "\n"))]
         ; Ignore title, empty, and comment lines
         [body (remove-empty-and-comments (rest lines))])
    (evaluate body)))

#|
(normalize-line str)
  str: the line string to normalize

  Remove trailing period and whitespace.
|#
(define (normalize-line str)
  (string-trim (string-normalize-spaces (string-trim str)) "."))

#|
(remove-empty-and-comments strings)
  strings: a list of strings

  Removes all empty strings and FunShake comment strings from 'strings'.
|#
(define (remove-empty-and-comments strings)
  (filter (lambda (s)
            (and
             (< 0 (string-length s))
             (not (ormap (lambda (comment) (prefix? comment s))
                         comments))))
          strings))

#|
(prefix? s1 s2)
  s1, s2: strings

  Returns whether 's1' is a prefix of 's2'.
|#
(define (prefix? s1 s2)
  (and (<= (string-length s1) (string-length s2))
       (equal? s1 (substring s2 0 (string-length s1)))))

;------------------------------------------------------------------------------
; Main evaluation (YOUR WORK GOES HERE)
;------------------------------------------------------------------------------

#|
(evaluate body)
  body: a list of lines corresponding to the semantically meaningful text
  of a FunShake file.

  Returns a list of numbers produced when evaluating the FunShake file.
  This should be the main starting point of your work! Currently,
  it just outputs the semantically meaningful lines in the file.
|#
(define (evaluate body)
  (let* ([dramatis-personae (get-dramatis-personae body)]
         [settings (get-settings body)]
         [dialogues (get-dialogues body dramatis-personae settings)])
    body))

#|
  This section of the code is to get the dramatis personae
  list and evaluate it. Dramatis personae do not contain any
  self referencing or function calls, and are best thought of
  as program constants.
|#

; helper function that takes the body list given
; to evaluate and returns a list of name value pairs
; which are supposed to stand for the name of the person
; and the value of their description
(define (get-dramatis-personae body)
  (map evaluate-dramatis (get-elements-between body personae finis)))

; helper function to evaluate the dramatis personae
; to be used as as a functor input to map in get-dramatis-personae
(define (evaluate-dramatis dramatis)
  (let* ([name-description-pair (string-split dramatis ",")]
         [name (first name-description-pair)]
         [description (second name-description-pair)])
    (list name (evaluate-dramatis-description description))))

; helper that takes a description from a name-description pair
; and returns the number value of it. 
(define (evaluate-dramatis-description description)
  (let* ([description-list (string-split description)]
         [description-length (length description-list)]
         [bad-adjective-count (count-bad-adjectives description-list)]
         )
    (calculate-description description-length bad-adjective-count)))

; helper function that calculates the description value based
; on the given length and bad adjective count
(define (calculate-description description-length bad-adjective-count)
  (if (= bad-adjective-count 0)
      description-length
      (* -1 (expt 2 bad-adjective-count) description-length)))

; helper function that counts how many bad adjectives
; are in the given description-list. 
(define (count-bad-adjectives description-list)
  (length (filter (lambda (str) (not (equal? (member str bad-words) #f))) description-list)))

#|

This section is for the settings section in a funshake program.
Settings define functions that can be called in a funshake program.
Each function can accept an argument, and it is possible that the function
does not use this argument, called a Hamlet. Functions can call other
functions, which can call other functions, and so on.

Notice that functions in funshake have the following property: in order
for them to be evaluated, we need the following syntax:

       "The song of <func-name> and <func-arg>"

The reason we don't have to directly translate funshake functions into
scheme functions is that we can defer all translation until the body
of the function virtually becomes a description. Since descriptions are
much easier to evaluate, we evaluate the description, and we're done.

So in order to evaluate <func-name> as function, we first need to look at
<func-arg>, and evaluate that. One thing to take advantage of is that
<func-arg> *cannot* be a function call: it must be a description of some sort.
Since this is the case, the only other possiblities are a multiplication operation,
an addition operation, or some other constant (description).

In other words, we do eager evaluation but late interpretation.

|#

; this function takes the settings map created in
; get-settings and resolves all function calls made
; inside other functions. Since the functions can be declared
; and called in any order, we cannot rely on order in funshake
; to help us in resolving function definitions
; since functions can call other functions there may be a call stack
; an arbitrary number of levels deep; thus, we will need to go to the top
; of this stack and resolve the calls from the top to bottom.
(define (resolve-functors settings-map)
  (void))

; helper function that takes the body list given
; to evaluate and returns a list of function evaluation
; rule pairs which are supposed to be equivalent to the function
; definition provided in the funshake file
(define (get-settings body)
  (map evaluate-setting (get-elements-between body settings finis)))

; helper function to evaluate the settings
; to be used as a functor input to map in get-settings
(define (evaluate-setting setting)
  (let* ([name-description-pair (string-split setting ",")]
         [name (first name-description-pair)]
         [description (normalize-line (second name-description-pair))]) ; after normalizing the description we can safely check for whether it is a functor call or not
    (list name description))) ; defer functor evaluation until we get to dialogue

; helper function that takes in a function description in funshake
; and returns a functor that is the representation of this description
; in racket/scheme
(define (get-functor description)
  (let* ([is-function-call (is-functor-call description)])
    (get-functor-helper is-function-call description)))

(define (get-functor-helper is-function-call description)
  (if is-function-call ; if its a function call we can't evaluate it yet
      description      ; so just return the description
      (evaluate-functor description))) ; otherwise evaluate the description

(define (evaluate-functor description)
  (let* ([is-description (is-description description)]
         [is-arithmetic (is-arithmetic description)])
    (if is-description
        (create-description-functor description)
        (if (equal? (typeof-arithmetic description) add)
            (create-op-functor description add +)
            (create-op-functor description mult *)))))

; helper function that returns a lambda that is supposed
; to return the value of the given description. 
(define (create-description-functor description)
  (lambda (hamlet) (evaluate-dramatis-description description))) ; hamlet is unused here

; generic functor generator for arithmetic operations. Given an
; arithmetic operation and a functor that performs the same operation
; in racket, return a functor that is the equivalent of the funshake
; function specification in racket. 
(define (create-op-functor description operation functor)
  (let* ([expr (string-split description operation)]
         [arg-1 (normalize-line (first expr))]
         [arg-2 (normalize-line (second expr))]
         [arg-types (get-arg-types arg-1 arg-2)])
    (create-functor-helper arg-1 arg-2 arg-types functor)))

(define (get-arg-types arg-1 arg-2)
  (cond
    [(and (equal? arg-1 param) (equal? arg-2 param)) (list param param)]
    [(and (not (equal? arg-1 param)) (equal? arg-2 param)) (list description-type param)]
    [(and (not (equal? arg-2 param)) (not (equal? arg-2 param))) (list description-type description-type)]
    [(and (equal? arg-1 param) (not (equal? arg-2 param))) (list param description-type)]))

(define (create-functor-helper arg-1 arg-2 arg-types functor)
  (cond
    [(and (equal? (first arg-types) param) (equal? (second arg-types) param))
     (lambda (x) (functor x x))]
    [(and (equal? (first arg-types) param) (equal? (second arg-types) description-type))
     (lambda (x) (functor x (evaluate-dramatis-description arg-2)))]
    [(and (equal? (first arg-types) description-type) (equal? (second arg-types) param))
     (lambda (x) (functor x (evaluate-dramatis-description arg-1)))]
    [(and (equal? (first arg-types) description-type) (equal? (second arg-types) description-type))
     (lambda (x) (functor (evaluate-dramatis-description arg-1) (evaluate-dramatis-description arg-2)))]))

; returns true if and only if the description of the function
; is itself a function call. Since funshake requires functions
; to be top level expressions we don't need to do any wizardry by
; checking for nested function calls. (in this case we'd need an actual lexer and parser)
(define (is-functor-call text)
  (prefix? call text))

; helper function that takes the body list given
; to evaluate and returns a list of (speaker, evaluation of description)
; pairs. The function is also given the dramatis personae map created
; in get-dramatis-personae and the settings map created in get-settings
; in order to evaluate dialogue descriptions (which may be function calls,
; name lookups, etc.)
(define (get-dialogues body dramatis-personae-map settings-map)
  (let* ([name-dialogue-pairs (get-name-dialogue-pairs body (list))])
    (map (evaluate-dialogue dramatis-personae-map settings-map) name-dialogue-pairs)))

; helper function that takes the body of the program
; and returns a list of name dialogue pairs that are supposed
; to represent all the dialogues in the program.
(define (get-name-dialogue-pairs body acc)
  (if (null? body)
      acc
      (if (list? (member #\: (string->list (car body)))) ; if we find the colon indicating a dialogue
          (get-name-dialogue-pairs (cdr (cdr body)) (append acc (list (list (remove-last-char (car body)) (car (cdr body))))))
          (get-name-dialogue-pairs (cdr body) acc))))

; helper function that takes in the dramatis personae and the settings
; and returns a lambda that takes in a name-dialogue pair
; that is able to evaluate the dialogue. evaluating the dialogue
; is the heart of a funshake program
(define (evaluate-dialogue dramatis-personae-map settings-map)
  (lambda (name-dialogue-pair)
    (let* (
           ; general dialogue info
           [name (first name-dialogue-pair)] ; the name of the person talking
           [dialogue (second name-dialogue-pair)] ; the dialogue: what the person is saying
           
           ; function call info
           [is-function-call (is-functor-call dialogue)] ; #t if the dialogue is a function call
           [function-name-and-arg (get-function-name-and-arg dialogue)]
           [function-name (if is-function-call (first function-name-and-arg) #f)] ; name of the function if there is a function call
           [function-argument (if is-function-call (second function-name-and-arg) #f)] ; the argument to the function if it exists
           
           ; arithmetic info
           [is-top-level-arithmetic (if (and (not is-function-call) (is-arithmetic dialogue)) #t #f)] ; #t if the dialogue has top level arithmetic, #f otherwise
           [arithmetic-type (if is-top-level-arithmetic (typeof-arithmetic dialogue) #f)] ; type of arithmetic: + or *
           
           ; other info
           [is-description (if (and (not is-function-call) (not is-top-level-arithmetic)) #t #f)] ; easy case: dialogue is simply a description to be evaluated
           )
      (cond [is-description (evaluate-dramatis-description dialogue)]
            [is-function-call (evaluate-function-call name dialogue function-name function-argument settings-map dramatis-personae-map)]
            [is-top-level-arithmetic (evaluate-top-level-arithmetic name dialogue arithmetic-type dramatis-personae-map)]))))

; helper that returns true if and only if the dialogue contains
; a self reference
(define (is-self-referencing? name dialogue)
  (let* ([dialogue-as-list (string->list dialogue)]
         [contains-name (sublist (string->list name) dialogue-as-list)])
    (if contains-name
        #t
        (is-self-referencing-helper dialogue-as-list self-refs))))

; helper for is-self-referencing: check all the self-ref forms
(define (is-self-referencing-helper dialogue-as-list self-refs-list)
  (if (null? self-refs-list)
      #f ; not self referencing
      (if (number? (sublist (car self-refs-list) (dialogue-as-list)))
          #t
          (is-self-referencing-helper dialogue-as-list (cdr self-refs-list)))))

(define (get-function-name-and-arg dialogue)
  (let* ([the-song (normalize-line (car (string-split dialogue call)))]
         [the-lyrics (string-split the-song)]
         [the-name (first the-lyrics)]
         [the-argument (cdr the-lyrics)])
    (list the-name the-argument)))

(define (evaluate-description dialogue dramatis-personae-map)
  (void))

(define (evaluate-name speaker dialogue dramatis-personae-map)
  (if (is-self-ref? dialogue)
      (evaluate-name speaker speaker dramatis-personae-map)
      (second (first (filter (lambda (pair) (equal? (first pair) dialogue)) dramatis-personae-map)))))

(define (evaluate-function-call name dialogue function-name function-argument settings-map dramatis-personae-map)
  (void))

; returns the value of the pair in the settings map with function name as the name
; precondition: function-name is a valid function name
(define (get-func-body function-name settings-map)
  (second (first (filter (lambda (pair) (equal? (first pair) function-name)) settings-map))))
  

(define (evaluate-top-level-arithmetic name dialogue arithmetic-type dramatis-personae-map)
  (let* ([op-functor (if (equal? arithmetic-type add) + *)]
         [exprs (string-split dialogue arithmetic-type)]
         [expr1 (normalize-line (first exprs))]
         [expr2 (normalize-line (second exprs))]
         [expr1-evaluated (evaluate-expr expr1 name dramatis-personae-map)]
         [expr2-evaluated (evaluate-expr expr2 name dramatis-personae-map)]
         [result (op-functor expr1-evaluated expr2-evaluated)])
    result))

(define (evaluate-expr expr name dramatis-personae-map)
  (let* (
         [word-count (length (string-split expr))] ; how many words are in the expression?
         [is-name-lookup (if (= word-count 1) (is-name-lookup? expr dramatis-personae-map) #f)] ; is it a name lookup?
         [name-lookup-value (if is-name-lookup (evaluate-name name expr dramatis-personae-map) #f)] ; if it is a lookup, what is the value?
         [is-description (or (not is-name-lookup) (> word-count 1))] ; is it a description?
         )
    (if is-name-lookup
        name-lookup-value
        (evaluate-dramatis-description expr))))

(define (is-self-ref? dialogue)
  (if (member dialogue self-refs)
      #t
      #f))

(define (is-name-lookup? dialogue dramatis-personae-map)
  (or (is-self-ref? dialogue)
      (not (equal?
            0
            (length (filter (lambda (pair) (equal? (first pair) dialogue)) dramatis-personae-map))))))

; returns true if and only if the text is a description
; in particular, it cannot be a functor call or an arithmetic expression
(define (is-description text)
  (and (not (is-arithmetic text)) (not (is-functor-call text))))

; returns true if and only if text is of arithmetic type
(define (is-arithmetic text)
  (let* ([contains-joind (not (equal? (first (string-split text add)) text))]
         [contains-entrancd (not (equal? (first (string-split text mult)) text))])
    (if (or contains-joind contains-entrancd)
        #t
        #f)))

; precondition: text must be of arithmetic type
(define (typeof-arithmetic text)
  (let* ([contains-joind (not (equal? (first (string-split text add)) text))]
         [contains-entrancd (not (equal? (first (string-split text mult)) text))])
    (if contains-joind
        add
        mult)))

; helper function: given a list and two elements belonging 
; to it, get the sublist [i1,...,i2]. This will aid in
; getting dramatis personae and settings. 
(define (get-elements-between lst i1 i2)
  (if (null? lst)
      null
      (if (not (equal? (car lst) i1))
          (get-elements-between (cdr lst) i1 i2)
          (append null (get-elements-until (cdr lst) i2)))))

; helper function for get-elements-between
(define (get-elements-until lst i2)
  (if (null? lst)
      null
      (if (not (equal? (car lst) i2))
          (cons (car lst) (get-elements-until (cdr lst) i2))
          (list))))

; helper function for starts-with: checks if str-list starts
; with start-list
(define (list-starts-with start-list str-list)
  (if (null? str-list)
      #f
      (if (null? start-list)
          #t
          (if (equal? (car start-list) (car str-list))
              (and #t (list-starts-with (cdr start-list) (cdr str-list)))
              #f))))

(define (add-functor num bool)
  (if (and (boolean? bool) (equal? bool #f))
      #f
      (+ num bool)))

; ex1 import
(define (sublist sub lst)
  (cond ((null? sub) 0)
        ((null? lst) #f)
        ((list-starts-with sub lst) 0)
        ((not (list-starts-with sub lst)) (add-functor 1 (sublist sub (cdr lst))))
        ))

#|
(remove-last-char string)
  string: a string

  Returns string with its last letter removed.
|#
(define (remove-last-char string)
  (if (equal? (string-length string) 0)
      string
      (substring string 0 (- (string-length string) 1))))

(define bdy (interpret "functions.txt"))
(define functors (get-elements-between bdy settings finis))
(define dramatis (get-dramatis-personae bdy))
(define gsettings (get-settings bdy))
(define name-dialogue-pairs (get-name-dialogue-pairs bdy (list)))