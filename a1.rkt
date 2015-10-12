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
(define lookup-type "lookup")

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
    dialogues))

#|
  This section of the code is to get the dramatis personae
  list and evaluate it. Dramatis personae do not contain any
  self referencing or function calls, and are best thought of
  as program constants.
|#

#|
  (get-dramatis-personae body)
  body: the list of semantically meaningful lines in a funshake file.

  Returns a map (i.e a list of (name, value) pairs) where the name/key is
  the name of the person in the play, and their value is the value obtained
  when calculating their description via evaluate-dramatis-description (see ahead).

|#
(define (get-dramatis-personae body)
  (map evaluate-dramatis (get-elements-between body personae finis)))

#|
  (evaluate-dramatis dramatis)
  dramatis: a dramatis personae line extracted from the Dramatis Personae section
  in a funshake file.

  Returns a (name, value) pair where the name is the name of the person in the play
  and value is the value of their description, as calculated by evaluate-dramatis-description.
|#
(define (evaluate-dramatis dramatis)
  (let* ([name-description-pair (string-split dramatis ",")]
         [name (first name-description-pair)]
         [description (second name-description-pair)])
    (list name (evaluate-dramatis-description description))))

#|
  (evaluate-dramatis-description description)
  description: the description of a person declared in the dramatis personae
  section of a funshake file.

  Returns the numerical value of the description of the given description, as 
  entailed in the functional shakespeare specification.
|#
(define (evaluate-dramatis-description description)
  (let* ([description-list (string-split description)]
         [description-length (length description-list)]
         [bad-adjective-count (count-bad-adjectives description-list)]
         )
    (calculate-description description-length bad-adjective-count)))

#|
  (calculate-description description-length bad-adjective-count)
  description: the description of a person declared in the dramatis personae
  section of a funshake file.

  Returns the numerical value of the description of the given description, as 
  entailed in the functional shakespeare specification.
|#
(define (calculate-description description-length bad-adjective-count)
  (if (= bad-adjective-count 0)
      description-length
      (* -1 (expt 2 bad-adjective-count) description-length)))

; helper function that counts how many bad adjectives
; are in the given description-list. 
(define (count-bad-adjectives description-list)
  (length (filter (lambda (str) (not (equal? (member str bad-words) #f))) description-list)))

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
    (map (lambda (pair) (eval-pair (first pair) (second pair) dramatis-personae-map settings-map)) name-dialogue-pairs)))

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
(define (eval-pair speaker dialogue dramatis-personae-map settings-map [hamlet (void)])
  (cond [(is-functor-call dialogue)
         (let* ([function-name-and-arg (get-function-name-and-arg dialogue)]
           [function-name (if (is-functor-call dialogue) (first function-name-and-arg) #f)] ; name of the function if there is a function call
           [function-argument (if (is-functor-call dialogue) (second function-name-and-arg) #f)] ; the argument to the function if it exists
           ) (eval-function speaker function-name function-argument dramatis-personae-map settings-map hamlet))] ; todo : change arguments to eval-func-call
        [(is-arithmetic dialogue) (evaluate-top-level-arithmetic speaker dialogue (typeof-arithmetic dialogue) dramatis-personae-map settings-map hamlet)]
        [(and (= 1 (length (string-split dialogue))) (is-name-lookup? dialogue dramatis-personae-map)) (evaluate-name speaker dialogue dramatis-personae-map hamlet)]
        [else (evaluate-dramatis-description dialogue)]))

(define (eval-function speaker function-name function-argument dramatis-personae-map settings-map [hamlet (void)])
  (eval-pair speaker (get-func-body function-name settings-map) dramatis-personae-map
             settings-map (eval-pair speaker function-argument dramatis-personae-map settings-map hamlet)))

(define (evaluate-top-level-arithmetic name dialogue arithmetic-type dramatis-personae-map settings-map [hamlet (void)])
  (let* ([op-functor (if (equal? arithmetic-type add) + *)]
         [exprs (string-split dialogue arithmetic-type)]
         [expr1 (normalize-line (first exprs))]
         [expr2 (normalize-line (second exprs))]
         [expr1-evaluated (eval-pair name expr1 dramatis-personae-map settings-map hamlet)]
         [expr2-evaluated (eval-pair name expr2 dramatis-personae-map settings-map hamlet)]
         [result (op-functor expr1-evaluated expr2-evaluated)])
    result))

(define (get-function-name-and-arg dialogue)
  (let* ([the-song (normalize-line (car (string-split dialogue call)))]
         [the-lyrics (string-split the-song)]
         [the-name (first the-lyrics)]
         [the-argument (foldr (lambda (x y) (string-append " " (string-append x y))) "" (cddr the-lyrics))])
    (list the-name the-argument)))

(define (evaluate-name speaker dialogue dramatis-personae-map [hamlet (void)])
  (cond [(is-self-ref? dialogue) (evaluate-name speaker speaker dramatis-personae-map hamlet)]
        [(and (not (void? hamlet)) (equal? dialogue param)) hamlet]
        [else (second (first (filter (lambda (pair) (equal? (first pair) dialogue)) dramatis-personae-map)))]))

; returns the value of the pair in the settings map with function name as the name
; precondition: function-name is a valid function name
(define (get-func-body function-name settings-map)
  (second (first (filter (lambda (pair) (equal? (first pair) function-name)) settings-map))))

(define (is-self-ref? dialogue)
  (if (member dialogue self-refs)
      #t
      #f))

#|
(is-name-lookup? dialogue dramatis-personae-map)
  dialogue: a string
  dramatis-personae-map: the name-value pairs of the dramatis personae of the play being
  interpreted

  Returns true if and only if dialogue is either a self reference, a name in dramatis personae, or Hamlet.
|#
(define (is-name-lookup? dialogue dramatis-personae-map)
  (or (is-self-ref? dialogue)
      (not (equal?
            0
            (length (filter (lambda (pair) (equal? (first pair) dialogue)) dramatis-personae-map))))
      (equal? dialogue param)))

#|
(is-arithmetic text)
  text: a string

  Returns true if and only if text is an arithmetic expression
|#
(define (is-arithmetic text)
  (let* ([contains-joind (not (equal? (first (string-split text add)) text))]
         [contains-entrancd (not (equal? (first (string-split text mult)) text))])
    (if (or contains-joind contains-entrancd)
        #t
        #f)))

#|
(typeof-arithmetic text)
  text: a string

  Returns the `add` constant if the text contains 'join'd with' and mult if the
  text contains 'entranc'd by'.
|#
(define (typeof-arithmetic text)
  (let* ([contains-joind (not (equal? (first (string-split text add)) text))]
         [contains-entrancd (not (equal? (first (string-split text mult)) text))])
    (if contains-joind
        add
        mult)))

#|
(get-elements-between lst i1 i2)
  lst: a list you want to scan, that contains i1 and i2
  i1: an element of list
  i2: an element of lst

  Returns the list of elements in lst between element i1 and i2
|#
(define (get-elements-between lst i1 i2)
  (if (null? lst)
      null
      (if (not (equal? (car lst) i1))
          (get-elements-between (cdr lst) i1 i2)
          (append null (get-elements-until (cdr lst) i2)))))

#|
(get-elements-until lst i2)
  lst: a list you want to scan, that contains i2
  i2: an element of lst

  Returns the list of elements in lst up until we reach element i2
|#
(define (get-elements-until lst i2)
  (if (null? lst)
      null
      (if (not (equal? (car lst) i2))
          (cons (car lst) (get-elements-until (cdr lst) i2))
          (list))))

#|
(remove-last-char string)
  string: a string

  Returns string with its last character removed.
|#
(define (remove-last-char string)
  (if (equal? (string-length string) 0)
      string
      (substring string 0 (- (string-length string) 1))))

;(define functors (get-elements-between body settings finis))
;(define dramatis (get-dramatis-personae body))
;(define gsettings (get-settings bdy))
;(define name-dialogue-pairs (get-name-dialogue-pairs bdy (list)))
(define body2 (interpret "functions.txt"))
body2