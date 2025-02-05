;;;; "format.scm" Common LISP text output formatter for SLIB
;;; 	Copyright (C) 2010-2013,2019 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;

;;; This code was orignally in the public domain.
;;;
;;; Written 1992-1994 by Dirk Lutzebaeck (lutzeb@cs.tu-berlin.de).
;;;
;;; Authors of the version from SLIB (< 1.4) were Ken Dickey and Aubrey
;;; Jaffer.
;;;
;;; Assimilated into Guile May 1999.
;;;
;;; Please don't bother the original authors with bug reports, though;
;;; send them to bug-guile@gnu.org.
;;;

(define-module (ice-9 format)
  #:autoload (ice-9 pretty-print) (pretty-print truncated-print)
  #:autoload (ice-9 i18n)         (%global-locale number->locale-string)
  ;; Actually replaces the global format as soon as loaded; see the end
  ;; of this file.
  #:replace (format))

(define format:version "3.0")

(define (format destination format-string . format-args)
  (define port
    (begin
      (unless (string? format-string)
        (error "format: expected a string for format string" format-string))

      (cond
       ((not destination) (open-output-string))
       ((boolean? destination) (current-output-port)) ; boolean but not false
       ((output-port? destination) destination)
       (else
        (error
          (simple-format #f "format: bad destination `~a'" destination))))))

  (define %output-col (or (port-column port) 0))
  (define %flush-output? #f)
  (define %case-conversion #f)
  (define %pos 0)        ; curr. format string parsing position
  (define %arg-pos 0)    ; curr. format argument position

  ;; format string and char output routines on port

  (define (put-string str)
    (if %case-conversion
        (display (%case-conversion str) port)
        (display str port))
    (set! %output-col
          (+ %output-col (string-length str))))

  (define (put-char ch)
    (if %case-conversion
        (display (%case-conversion (string ch))
                 port)
        (write-char ch port))
    (set! %output-col
          (if (char=? ch #\newline)
              0
              (+ %output-col 1))))

  (define (put-substring str i n)
    (put-string (substring str i n)))

  (define (put-fill-chars n ch)
    (put-string (make-string n ch)))

  ;; format's user error handler

  (define (format-error . args)       ; never returns!
    (with-throw-handler #t 
      (lambda ()
        (let ((port (current-error-port)))
          (unless (zero? %arg-pos)
            (set! %arg-pos (- %arg-pos 1)))
          (format port
                  "~%FORMAT: error with call: (format ~a \"~a<===~a\" ~
                                  ~{~a ~}===>~{~a ~})~%        "
                  destination
                  (substring format-string 0 %pos)
                  (substring format-string %pos
                             (string-length format-string))
                  (list-head format-args %arg-pos)
                  (list-tail format-args %arg-pos))
          (apply format port args)
          (newline port)
          (error "error in format")))
      (lambda (key . args)
        (display "FORMAT: INTERNAL ERROR IN FORMAT-ERROR!") (newline)
        (display "        destination: ") (write destination) (newline)
        (display "        format string: ") (write format-string) (newline)
        (display "        format args: ") (write format-args) (newline)
        (display "        error args:  ") (write args) (newline))))

  (define format:parameter-characters
    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+ #\v #\# #\'))

  (define (format:format-work format-string arglist) ; does the formatting work
    (define format-string-len (string-length format-string))
    (define arg-pos 0)                ; argument position in arglist
    (define arg-len (length arglist)) ; number of arguments
    (define modifier #f)              ; 'colon | 'at | 'colon-at | #f
    (define params '())               ; directive parameter list
    (define param-value-found #f)     ; a directive parameter value found
    (define conditional-nest 0)       ; conditional nesting level
    (define clause-pos 0)             ; last cond. clause beginning char pos
    (define clause-default #f)        ; conditional default clause string
    (define clauses '())              ; conditional clause string list
    (define conditional-type #f)      ; reflects the conditional modifiers
    (define conditional-arg #f)       ; argument to apply the conditional
    (define iteration-nest 0)         ; iteration nesting level
    (define iteration-pos 0)          ; iteration string beginning char pos
    (define iteration-type #f)        ; reflects the iteration modifiers
    (define max-iterations #f)        ; maximum number of iterations
    (define recursive-pos-save %pos)

    (define (next-char)               ; gets the next char from format-string
      (let ((ch (peek-next-char)))
        (set! %pos (+ 1 %pos))
        ch))

    (define (peek-next-char)
      (when (>= %pos format-string-len)
        (format-error "illegal format string"))
      (string-ref format-string %pos))

    (define (one-positive-integer? params )
      (cond
       ((null? params) #f)
       ((and (integer? (car params))
             (>= (car params) 0)
             (= (length params) 1)) #t)
       (else
        (format-error
         "one positive integer parameter expected"))))

    (define (next-arg)
      (when (>= arg-pos arg-len)
        (set! %arg-pos (+ arg-len 1))
        (format-error "missing argument(s)"))
      (add-arg-pos 1)
      (list-ref arglist (- arg-pos 1)))

    (define (prev-arg)
      (add-arg-pos -1)
      (when (negative? arg-pos)
        (format-error "missing backward argument(s)"))
      (list-ref arglist arg-pos))

    (define (rest-args)
      (let loop ((l arglist) (k arg-pos)) ; list-tail definition
        (if (= k 0) l (loop (cdr l) (- k 1)))))

    (define (add-arg-pos n)
      (set! arg-pos (+ n arg-pos))
      (set! %arg-pos arg-pos))

    (define (anychar-dispatch)        ; dispatches the format-string
      (if (>= %pos format-string-len)
          arg-pos                     ; used for ~? continuance
          (let ((char (next-char)))
            (cond
             ((char=? char #\~)
              (set! modifier #f)
              (set! params '())
              (set! param-value-found #f)
              (tilde-dispatch))
             (else
              (when (and (zero? conditional-nest)
                         (zero? iteration-nest))
                (put-char char))
              (anychar-dispatch))))))

    (define (tilde-dispatch)
      (cond
       ((>= %pos format-string-len)
        (put-string "~")          ; tilde at end of
					; string is just
					; output
        arg-pos)                      ; used for ~?
					; continuance
       ((and (or (zero? conditional-nest)
                 (memv (peek-next-char) ; find conditional
					; directives
                       (append '(#\[ #\] #\; #\: #\@ #\^)
                               format:parameter-characters)))
             (or (zero? iteration-nest)
                 (memv (peek-next-char) ; find iteration
					; directives
                       (append '(#\{ #\} #\: #\@ #\^)
                               format:parameter-characters))))
        (case (char-upcase (next-char))

          ;; format directives

          ((#\A)                      ; Any -- for humans
           (set! format:read-proof
                 (memq modifier '(colon colon-at)))
           (format:out-obj-padded (memq modifier '(at colon-at))
                                  (next-arg) #f params)
           (anychar-dispatch))
          ((#\S)                      ; Slashified -- for parsers
           (set! format:read-proof
                 (memq modifier '(colon colon-at)))
           (format:out-obj-padded (memq modifier '(at colon-at))
                                  (next-arg) #t params)
           (anychar-dispatch))
          ((#\D)                      ; Decimal
           (format:out-num-padded modifier (next-arg) params 10)
           (anychar-dispatch))
          ((#\H)                      ; Localized number
           (let* ((num      (next-arg))
                  (locale   (case modifier
                              ((colon) (next-arg))
                              (else    %global-locale)))
                  (argc     (length params))
                  (width    (format:par params argc 0 #f "width"))
                  (decimals (format:par params argc 1 #t "decimals"))
                  (padchar  (integer->char
                             (format:par params argc 2 format:space-ch
                                         "padchar")))
                  (str      (number->locale-string num decimals
                                                   locale)))
             (put-string (if (and width
                                      (< (string-length str) width))
                                 (string-pad str width padchar)
                                 str)))
           (anychar-dispatch))
          ((#\X)                      ; Hexadecimal
           (format:out-num-padded modifier (next-arg) params 16)
           (anychar-dispatch))
          ((#\O)                      ; Octal
           (format:out-num-padded modifier (next-arg) params 8)
           (anychar-dispatch))
          ((#\B)                      ; Binary
           (format:out-num-padded modifier (next-arg) params 2)
           (anychar-dispatch))
          ((#\R)
           (if (null? params)
               (format:out-obj-padded ; Roman, cardinal,
					; ordinal numerals
                #f
                ((case modifier
                   ((at) format:num->roman)
                   ((colon-at) format:num->old-roman)
                   ((colon) format:num->ordinal)
                   (else format:num->cardinal))
                 (next-arg))
                #f params)
               (format:out-num-padded ; any Radix
                modifier (next-arg) (cdr params) (car params)))
           (anychar-dispatch))
          ((#\F)                      ; Fixed-format floating-point
           (format:out-fixed modifier (next-arg) params)
           (anychar-dispatch))
          ((#\E)                      ; Exponential floating-point
           (format:out-expon modifier (next-arg) params)
           (anychar-dispatch))
          ((#\G)                      ; General floating-point
           (format:out-general modifier (next-arg) params)
           (anychar-dispatch))
          ((#\$)                      ; Dollars floating-point
           (format:out-dollar modifier (next-arg) params)
           (anychar-dispatch))
          ((#\I)                      ; Complex numbers
           (let ((z (next-arg)))
             (unless (complex? z)
               (format-error "argument not a complex number"))
             (format:out-fixed modifier (real-part z) params)
             (format:out-fixed 'at (imag-part z) params)
             (put-char #\i))
           (anychar-dispatch))
          ((#\C)                      ; Character
           (let ((ch (if (one-positive-integer? params)
                         (integer->char (car params))
                         (next-arg))))
             (unless (char? ch)
               (format-error "~~c expects a character"))
             (case modifier
               ((at)
                (put-string (object->string ch)))
               ((colon)
                (let ((c (char->integer ch)))
                  (when (< c 0)
                    (set! c (+ c 256))) ; compensate
					; complement
					; impl.
                  (cond
                   ((< c #x20)	; assumes that control
					; chars are < #x20
                    (put-char #\^)
                    (put-char
                     (integer->char (+ c #x40))))
                   ((>= c #x7f)
                    (put-string "#\\")
                    (put-string
                     (number->string c 8)))
                   (else
                    (put-char ch)))))
               (else (put-char ch))))
           (anychar-dispatch))
          ((#\P)                      ; Plural
           (when (memq modifier '(colon colon-at))
             (prev-arg))
           (let ((arg (next-arg)))
             (unless (number? arg)
               (format-error "~~p expects a number argument"))
             (if (= arg 1)
                 (when (memq modifier '(at colon-at))
                   (put-char #\y))
                 (if (memq modifier '(at colon-at))
                     (put-string "ies")
                     (put-char #\s))))
           (anychar-dispatch))
          ((#\~)                      ; Tilde
           (if (one-positive-integer? params)
               (put-fill-chars (car params) #\~)
               (put-char #\~))
           (anychar-dispatch))
          ((#\%)                      ; Newline
           (if (one-positive-integer? params)
               (put-fill-chars (car params) #\newline)
               (put-char #\newline))
           (set! %output-col 0)
           (anychar-dispatch))
          ((#\&)                      ; Fresh line
           (if (one-positive-integer? params)
               (begin
                 (when (> (car params) 0)
                   (put-fill-chars (- (car params)
                                       (if (> %output-col 0) 0 1))
                                    #\newline))
                 (set! %output-col 0))
               (when (> %output-col 0)
                 (put-char #\newline)))
           (anychar-dispatch))
          ((#\_)                      ; Space character
           (if (one-positive-integer? params)
               (put-fill-chars (car params) #\space)
               (put-char #\space))
           (anychar-dispatch))
          ((#\/)                      ; Tabulator character
           (if (one-positive-integer? params)
               (put-fill-chars (car params) #\tab)
               (put-char #\tab))
           (anychar-dispatch))
          ((#\|)                      ; Page seperator
           (if (one-positive-integer? params)
               (put-fill-chars (car params) #\page)
               (put-char #\page))
           (set! %output-col 0)
           (anychar-dispatch))
          ((#\T)                      ; Tabulate
           (format:tabulate modifier params)
           (anychar-dispatch))
          ((#\Y)                      ; Structured print
           (let ((width (if (one-positive-integer? params)
                            (car params)
                            79)))
             (case modifier
               ((at)
                (put-string
                 (call-with-output-string
                  (lambda (p)
                    (truncated-print (next-arg) p
                                     #:width width)))))
               ((colon-at)
                (put-string
                 (call-with-output-string
                  (lambda (p)
                    (truncated-print (next-arg) p
                                     #:width
                                     (max (- width
                                             %output-col)
                                          1))))))
               ((colon)
                (format-error "illegal modifier in ~~?"))
               (else
                (pretty-print (next-arg) port
                              #:width width)
                (set! %output-col 0))))
           (anychar-dispatch))
          ((#\? #\K)               ; Indirection (is "~K" in T-Scheme)
           (cond
            ((memq modifier '(colon colon-at))
             (format-error "illegal modifier in ~~?"))
            ((eq? modifier 'at)
             (let* ((frmt (next-arg))
                    (args (rest-args)))
               (add-arg-pos (format:format-work frmt args))))
            (else
             (let* ((frmt (next-arg))
                    (args (next-arg)))
               (format:format-work frmt args))))
           (anychar-dispatch))
          ((#\!)                      ; Flush output
           (set! %flush-output? #t)
           (anychar-dispatch))
          ((#\newline)		; Continuation lines
           (when (eq? modifier 'at)
             (put-char #\newline))
           (if (< %pos format-string-len)
               (do ((ch (peek-next-char) (peek-next-char)))
                   ((or (not (char-whitespace? ch))
                        (= %pos (- format-string-len 1))))
                 (if (eq? modifier 'colon)
                     (put-char (next-char))
                     (next-char))))
           (anychar-dispatch))
          ((#\*)                      ; Argument jumping
           (case modifier
             ((colon)                 ; jump backwards
              (if (one-positive-integer? params)
                  (do ((i 0 (+ i 1)))
                      ((= i (car params)))
                    (prev-arg))
                  (prev-arg)))
             ((at)                    ; jump absolute
              (set! arg-pos
                    (if (one-positive-integer? params) (car params) 0)))
             ((colon-at)
              (format-error "illegal modifier `:@' in ~~* directive"))
             (else                    ; jump forward
              (if (one-positive-integer? params)
                  (do ((i 0 (+ i 1)))
                      ((= i (car params)))
                    (next-arg))
                  (next-arg))))
           (anychar-dispatch))
          ((#\()                      ; Case conversion begin
           (set! %case-conversion
                 (case modifier
                   ((at) string-capitalize-first)
                   ((colon) string-capitalize)
                   ((colon-at) string-upcase)
                   (else string-downcase)))
           (anychar-dispatch))
          ((#\))                      ; Case conversion end
           (unless %case-conversion
             (format-error "missing ~~("))
           (set! %case-conversion #f)
           (anychar-dispatch))
          ((#\[)                      ; Conditional begin
           (set! conditional-nest (+ conditional-nest 1))
           (cond
            ((= conditional-nest 1)
             (set! clause-pos %pos)
             (set! clause-default #f)
             (set! clauses '())
             (set! conditional-type
                   (case modifier
                     ((at) 'if-then)
                     ((colon) 'if-else-then)
                     ((colon-at) (format-error "illegal modifier in ~~["))
                     (else 'num-case)))
             (set! conditional-arg
                   (if (one-positive-integer? params)
                       (car params)
                       (next-arg)))))
           (anychar-dispatch))
          ((#\;)                      ; Conditional separator
           (when (zero? conditional-nest)
             (format-error "~~; not in ~~[~~] conditional"))
           (unless (null? params)
             (format-error "no parameter allowed in ~~;"))
           (when (= conditional-nest 1)
             (let ((clause-str
                    (cond
                     ((eq? modifier 'colon)
                      (set! clause-default #t)
                      (substring format-string clause-pos
                                 (- %pos 3)))
                     ((memq modifier '(at colon-at))
                      (format-error "illegal modifier in ~~;"))
                     (else
                      (substring format-string clause-pos
                                 (- %pos 2))))))
               (set! clauses (append clauses (list clause-str)))
               (set! clause-pos %pos)))
           (anychar-dispatch))
          ((#\])                      ; Conditional end
           (when (zero? conditional-nest)
             (format-error "missing ~~["))
           (set! conditional-nest (- conditional-nest 1))
           (when modifier
             (format-error "no modifier allowed in ~~]"))
           (unless (null? params)
             (format-error "no parameter allowed in ~~]"))
           (cond
            ((zero? conditional-nest)
             (let ((clause-str (substring format-string clause-pos
                                          (- %pos 2))))
               (if clause-default
                   (set! clause-default clause-str)
                   (set! clauses (append clauses (list clause-str)))))
             (case conditional-type
               ((if-then)
                (when conditional-arg
                  (format:format-work (car clauses)
                                      (list conditional-arg))))
               ((if-else-then)
                (add-arg-pos
                 (format:format-work (if conditional-arg
                                         (cadr clauses)
                                         (car clauses))
                                     (rest-args))))
               ((num-case)
                (when (or (not (integer? conditional-arg))
                          (< conditional-arg 0))
                  (format-error "argument not a positive integer"))
                (unless (and (>= conditional-arg (length clauses))
                             (not clause-default))
                  (add-arg-pos
                   (format:format-work
                    (if (>= conditional-arg (length clauses))
                        clause-default
                        (list-ref clauses conditional-arg))
                    (rest-args))))))))
           (anychar-dispatch))
          ((#\{)                      ; Iteration begin
           (set! iteration-nest (+ iteration-nest 1))
           (cond
            ((= iteration-nest 1)
             (set! iteration-pos %pos)
             (set! iteration-type
                   (case modifier
                     ((at) 'rest-args)
                     ((colon) 'sublists)
                     ((colon-at) 'rest-sublists)
                     (else 'list)))
             (set! max-iterations
                   (if (one-positive-integer? params)
                       (car params)
                       #f))))
           (anychar-dispatch))
          ((#\})                      ; Iteration end
           (when (zero? iteration-nest) (format-error "missing ~~{"))
           (set! iteration-nest (- iteration-nest 1))
           (case modifier
             ((colon)
              (unless max-iterations (set! max-iterations 1)))
             ((colon-at at) (format-error "illegal modifier")))
           (unless (null? params)
             (format-error "no parameters allowed in ~~}"))
           (if (zero? iteration-nest)
               (let ((iteration-str
                      (substring format-string iteration-pos
                                 (- %pos (if modifier 3 2)))))
                 (when (string=? iteration-str "")
                   (set! iteration-str (next-arg)))
                 (case iteration-type
                   ((list)
                    (let ((args (next-arg))
                          (args-len 0))
                      (unless (list? args)
                        (format-error "expected a list argument"))
                      (set! args-len (length args))
                      (do ((arg-pos 0 (+ arg-pos
                                         (format:format-work
                                          iteration-str
                                          (list-tail args arg-pos))))
                           (i 0 (+ i 1)))
                          ((or (>= arg-pos args-len)
                               (and max-iterations
                                    (>= i max-iterations)))))))
                   ((sublists)
                    (let ((args (next-arg))
                          (args-len 0))
                      (unless (list? args)
                        (format-error "expected a list argument"))
                      (set! args-len (length args))
                      (do ((arg-pos 0 (+ arg-pos 1)))
                          ((or (>= arg-pos args-len)
                               (and max-iterations
                                    (>= arg-pos max-iterations))))
                        (let ((sublist (list-ref args arg-pos)))
                          (unless (list? sublist)
                            (format-error "expected a list of lists argument"))
                          (format:format-work iteration-str sublist)))))
                   ((rest-args)
                    (let* ((args (rest-args))
                           (args-len (length args))
                           (usedup-args
                            (do ((arg-pos 0 (+ arg-pos
                                               (format:format-work
                                                iteration-str
                                                (list-tail
                                                 args arg-pos))))
                                 (i 0 (+ i 1)))
                                ((or (>= arg-pos args-len)
                                     (and max-iterations
                                          (>= i max-iterations)))
                                 arg-pos))))
                      (add-arg-pos usedup-args)))
                   ((rest-sublists)
                    (let* ((args (rest-args))
                           (args-len (length args))
                           (usedup-args
                            (do ((arg-pos 0 (+ arg-pos 1)))
                                ((or (>= arg-pos args-len)
                                     (and max-iterations
                                          (>= arg-pos max-iterations)))
                                 arg-pos)
                              (let ((sublist (list-ref args arg-pos)))
                                (unless (list? sublist)
                                  (format-error "expected list arguments"))
                                (format:format-work iteration-str sublist)))))
                      (add-arg-pos usedup-args)))
                   (else (format-error "internal error in ~~}")))))
           (anychar-dispatch))
          ((#\^)                      ; Up and out
           (let* ((continue
                   (cond
                    ((not (null? params))
                     (not
                      (case (length params)
                        ((1) (zero? (car params)))
                        ((2) (= (list-ref params 0) (list-ref params 1)))
                        ((3) (<= (list-ref params 0)
                                 (list-ref params 1)
                                 (list-ref params 2)))
                        (else (format-error "too much parameters")))))
                    (%case-conversion ; if conversion stop conversion
                     (set! %case-conversion string-copy) #t)
                    ((= iteration-nest 1) #t)
                    ((= conditional-nest 1) #t)
                    ((>= arg-pos arg-len)
                     (set! %pos format-string-len) #f)
                    (else #t))))
             (when continue
               (anychar-dispatch))))

          ;; format directive modifiers and parameters

          ((#\@)                      ; `@' modifier
           (when (memq modifier '(at colon-at))
             (format-error "double `@' modifier"))
           (set! modifier (if (eq? modifier 'colon) 'colon-at 'at))
           (tilde-dispatch))
          ((#\:)                      ; `:' modifier
           (when (memq modifier '(colon colon-at))
             (format-error "double `:' modifier"))
           (set! modifier (if (eq? modifier 'at) 'colon-at 'colon))
           (tilde-dispatch))
          ((#\')                      ; Character parameter
           (when modifier
             (format-error "misplaced modifier"))
           (set! params (append params (list (char->integer (next-char)))))
           (set! param-value-found #t)
           (tilde-dispatch))
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+) ; num. paramtr
           (when modifier
             (format-error "misplaced modifier"))
           (let ((num-str-beg (- %pos 1))
                 (num-str-end %pos))
             (do ((ch (peek-next-char) (peek-next-char)))
                 ((not (char-numeric? ch)))
               (next-char)
               (set! num-str-end (+ 1 num-str-end)))
             (set! params
                   (append params
                           (list (string->number
                                  (substring format-string
                                             num-str-beg
                                             num-str-end))))))
           (set! param-value-found #t)
           (tilde-dispatch))
          ((#\V)                 ; Variable parameter from next argum.
           (when modifier
             (format-error "misplaced modifier"))
           (set! params (append params (list (next-arg))))
           (set! param-value-found #t)
           (tilde-dispatch))
          ((#\#)               ; Parameter is number of remaining args
           (when param-value-found
             (format-error "misplaced '#'"))
           (when modifier
             (format-error "misplaced modifier"))
           (set! params (append params (list (length (rest-args)))))
           (set! param-value-found #t)
           (tilde-dispatch))
          ((#\,)                      ; Parameter separators
           (when modifier
             (format-error "misplaced modifier"))
           (unless param-value-found
             (set! params (append params '(#f)))) ; append empty paramtr
           (set! param-value-found #f)
           (tilde-dispatch))
          ((#\Q)                      ; Inquiry messages
           (if (eq? modifier 'colon)
               (put-string format:version)
               (let ((nl (string #\newline)))
                 (put-string
                  (string-append
                   "SLIB Common LISP format version " format:version nl
                   "  (C) copyright 1992-1994 by Dirk Lutzebaeck" nl
                   "  please send bug reports to `lutzeb@cs.tu-berlin.de'"
                   nl))))
           (anychar-dispatch))
          (else                       ; Unknown tilde directive
           (format-error "unknown control character `~c'"
                         (string-ref format-string (- %pos 1))))))
       (else (anychar-dispatch))))    ; in case of conditional

    (set! %pos 0)
    (set! %arg-pos 0)
    (anychar-dispatch)                ; start the formatting
    (set! %pos recursive-pos-save)
    arg-pos)                 ; return the position in the arg. list

  ;; when format:read-proof is true, format:obj->str will wrap
  ;; result strings starting with "#<" in an extra pair of double
  ;; quotes.

  (define format:read-proof #f)

  ;; format:obj->str returns a R4RS representation as a string of
  ;; an arbitrary scheme object.

  (define (format:obj->str obj slashify)
    (let ((res (if slashify
                   (object->string obj)
                   (call-with-output-string (lambda (p) (display obj p))))))
      (if (and format:read-proof (string-prefix? "#<" res))
          (object->string res)
          res)))

  (define format:space-ch (char->integer #\space))
  (define format:zero-ch (char->integer #\0))

  (define (format:par pars length index default name)
    (if (> length index)
        (let ((par (list-ref pars index)))
          (if par
              (if name
                  (if (< par 0)
                      (format-error
                       "~s parameter must be a positive integer" name)
                      par)
                  par)
              default))
        default))

  (define (format:out-obj-padded pad-left obj slashify pars)
    (if (null? pars)
        (put-string (format:obj->str obj slashify))
        (let ((l (length pars)))
          (let ((mincol (format:par pars l 0 0 "mincol"))
                (colinc (format:par pars l 1 1 "colinc"))
                (minpad (format:par pars l 2 0 "minpad"))
                (padchar (integer->char
                          (format:par pars l 3 format:space-ch #f)))
                (objstr (format:obj->str obj slashify)))
            (unless pad-left
              (put-string objstr))
            (do ((objstr-len (string-length objstr))
                 (i minpad (+ i colinc)))
                ((>= (+ objstr-len i) mincol)
                 (put-fill-chars i padchar)))
            (when pad-left
              (put-string objstr))))))

  (define (format:out-num-padded modifier number pars radix)
    (unless (integer? number)
      (format-error "argument not an integer"))
    (let ((numstr (number->string number radix)))
      (if (and (null? pars) (not modifier))
          (put-string numstr)
          (let ((l (length pars))
                (numstr-len (string-length numstr)))
            (let ((mincol (format:par pars l 0 #f "mincol"))
                  (padchar (integer->char
                            (format:par pars l 1 format:space-ch #f)))
                  (commachar (integer->char
                              (format:par pars l 2 (char->integer #\,) #f)))
                  (commawidth (format:par pars l 3 3 "commawidth")))
              (when mincol
                (let ((numlen numstr-len)) ; calc. the output len of number
                  (when (and (memq modifier '(at colon-at)) (>= number 0))
                    (set! numlen (+ numlen 1)))
                  (when (memq modifier '(colon colon-at))
                    (set! numlen (+ (quotient (- numstr-len
                                                 (if (< number 0) 2 1))
                                              commawidth)
                                    numlen)))
                  (when (> mincol numlen)
                    (put-fill-chars (- mincol numlen) padchar))))
              (when (and (memq modifier '(at colon-at))
                         (>= number 0))
                (put-char #\+))
              (if (memq modifier '(colon colon-at)) ; insert comma character
                  (let ((start (remainder numstr-len commawidth))
                        (ns (if (< number 0) 1 0)))
                    (put-substring numstr 0 start)
                    (do ((i start (+ i commawidth)))
                        ((>= i numstr-len))
                      (when (> i ns)
                        (put-char commachar))
                      (put-substring numstr i (+ i commawidth))))
                  (put-string numstr)))))))

  (define (format:tabulate modifier pars)
    (let ((l (length pars)))
      (let ((colnum (format:par pars l 0 1 "colnum"))
            (colinc (format:par pars l 1 1 "colinc"))
            (padch (integer->char (format:par pars l 2 format:space-ch #f))))
        (case modifier
          ((colon colon-at)
           (format-error "unsupported modifier for ~~t"))
          ((at)                       ; relative tabulation
           (put-fill-chars
            (if (= colinc 0)
                colnum                ; colnum = colrel
                (do ((c 0 (+ c colinc))
                     (col (+ %output-col colnum)))
                    ((>= c col)
                     (- c %output-col))))
            padch))
          (else                       ; absolute tabulation
           (put-fill-chars
            (cond
             ((< %output-col colnum)
              (- colnum %output-col))
             ((= colinc 0)
              0)
             (else
              (do ((c colnum (+ c colinc)))
                  ((>= c %output-col)
                   (- c %output-col)))))
            padch))))))


  ;; roman numerals (from dorai@cs.rice.edu).

  (define format:roman-alist
    '((1000 #\M) (500 #\D) (100 #\C) (50 #\L)
      (10 #\X) (5 #\V) (1 #\I)))

  (define format:roman-boundary-values
    '(100 100 10 10 1 1 #f))

  (define (format:num->old-roman n)
    (if (and (integer? n) (>= n 1))
        (let loop ((n n)
                   (romans format:roman-alist)
                   (s '()))
          (if (null? romans)
              (list->string (reverse s))
              (let ((roman-val (caar romans))
                    (roman-dgt (cadar romans)))
                (do ((q (quotient n roman-val) (- q 1))
                     (s s (cons roman-dgt s)))
                    ((= q 0)
                     (loop (remainder n roman-val)
                       (cdr romans) s))))))
        (format-error "only positive integers can be romanized")))

  (define (format:num->roman n)
    (unless (and (integer? n) (> n 0))
      (format-error "only positive integers can be romanized"))
    (let loop ((n n)
               (romans format:roman-alist)
               (boundaries format:roman-boundary-values)
               (s '()))
      (if (null? romans)
          (list->string (reverse s))
          (let ((roman-val (caar romans))
                (roman-dgt (cadar romans))
                (bdry (car boundaries)))
            (let loop2 ((q (quotient n roman-val))
                        (r (remainder n roman-val))
                        (s s))
              (if (= q 0)
                  (if (and bdry (>= r (- roman-val bdry)))
                      (loop (remainder r bdry) (cdr romans)
                            (cdr boundaries)
                            (cons roman-dgt
                                  (append
                                   (cdr (assv bdry romans))
                                   s)))
                      (loop r (cdr romans) (cdr boundaries) s))
                  (loop2 (- q 1) r (cons roman-dgt s))))))))

  ;; cardinals & ordinals (from dorai@cs.rice.edu)

  (define format:cardinal-ones-list
    '(#f "one" "two" "three" "four" "five"
         "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen"
         "fourteen" "fifteen" "sixteen" "seventeen" "eighteen"
         "nineteen"))

  (define format:cardinal-tens-list
    '(#f #f "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty"
         "ninety"))

  (define (format:num->cardinal999 n)
    ;; this procedure is inspired by the Bruno Haible's CLisp
    ;; function format-small-cardinal, which converts numbers
    ;; in the range 1 to 999, and is used for converting each
    ;; thousand-block in a larger number
    (let* ((hundreds (quotient n 100))
           (tens+ones (remainder n 100))
           (tens (quotient tens+ones 10))
           (ones (remainder tens+ones 10)))
      (append
       (if (> hundreds 0)
           (append
            (string->list
             (list-ref format:cardinal-ones-list hundreds))
            (string->list" hundred")
            (if (> tens+ones 0) '(#\space) '()))
           '())
       (if (< tens+ones 20)
           (if (> tens+ones 0)
               (string->list
                (list-ref format:cardinal-ones-list tens+ones))
               '())
           (append
            (string->list
             (list-ref format:cardinal-tens-list tens))
            (if (> ones 0)
                (cons #\-
                      (string->list
                       (list-ref format:cardinal-ones-list ones)))
                '()))))))

  (define format:cardinal-thousand-block-list
    '("" " thousand" " million" " billion" " trillion" " quadrillion"
      " quintillion" " sextillion" " septillion" " octillion" " nonillion"
      " decillion" " undecillion" " duodecillion" " tredecillion"
      " quattuordecillion" " quindecillion" " sexdecillion" " septendecillion"
      " octodecillion" " novemdecillion" " vigintillion"))

  (define (format:num->cardinal n)
    (cond ((not (integer? n))
           (format-error
            "only integers can be converted to English cardinals"))
          ((= n 0) "zero")
          ((< n 0) (string-append "minus " (format:num->cardinal (- n))))
          (else
           (let ((power3-word-limit
                  (length format:cardinal-thousand-block-list)))
             (let loop ((n n)
                        (power3 0)
                        (s '()))
               (if (= n 0)
                   (list->string s)
                   (let ((n-before-block (quotient n 1000))
                         (n-after-block (remainder n 1000)))
                     (loop n-before-block
                       (+ power3 1)
                       (if (> n-after-block 0)
                           (append
                            (if (> n-before-block 0)
                                (string->list ", ")
                                '())
                            (format:num->cardinal999 n-after-block)
                            (if (< power3 power3-word-limit)
                                (string->list
                                 (list-ref
                                  format:cardinal-thousand-block-list
                                  power3))
                                (append
                                 (string->list " times ten to the ")
                                 (string->list
                                  (format:num->ordinal
                                   (* power3 3)))
                                 (string->list " power")))
                            s)
                           s)))))))))

  (define format:ordinal-ones-list
    '(#f "first" "second" "third" "fourth" "fifth"
         "sixth" "seventh" "eighth" "ninth" "tenth" "eleventh" "twelfth"
         "thirteenth" "fourteenth" "fifteenth" "sixteenth" "seventeenth"
         "eighteenth" "nineteenth"))

  (define format:ordinal-tens-list
    '(#f #f "twentieth" "thirtieth" "fortieth" "fiftieth" "sixtieth"
         "seventieth" "eightieth" "ninetieth"))

  (define (format:num->ordinal n)
    (cond ((not (integer? n))
           (format-error
            "only integers can be converted to English ordinals"))
          ((= n 0) "zeroth")
          ((< n 0) (string-append "minus " (format:num->ordinal (- n))))
          (else
           (let ((hundreds (quotient n 100))
                 (tens+ones (remainder n 100)))
             (string-append
              (if (> hundreds 0)
                  (string-append
                   (format:num->cardinal (* hundreds 100))
                   (if (= tens+ones 0) "th" " "))
                  "")
              (if (= tens+ones 0)
                  ""
                  (if (< tens+ones 20)
                      (list-ref format:ordinal-ones-list tens+ones)
                      (let ((tens (quotient tens+ones 10))
                            (ones (remainder tens+ones 10)))
                        (if (= ones 0)
                            (list-ref format:ordinal-tens-list tens)
                            (string-append
                             (list-ref format:cardinal-tens-list tens)
                             "-"
                             (list-ref format:ordinal-ones-list ones))))
                      )))))))

  ;; format inf and nan.

  (define (format:out-inf-nan number width digits edigits overch padch)
    ;; inf and nan are always printed exactly as "+inf.0", "-inf.0" or
    ;; "+nan.0", suitably justified in their field.  We insist on
    ;; printing this exact form so that the numbers can be read back in.
    (let* ((str (number->string number))
           (len (string-length str))
           (dot (string-index str #\.))
           (digits (+ (or digits 0)
                      (if edigits (+ edigits 2) 0))))
      (if (and width overch (< width len))
          (put-fill-chars width (integer->char overch))
          (let* ((leftpad (if width
                              (max (- width (max len (+ dot 1 digits))) 0)
                              0))
                 (rightpad (if width
                               (max (- width leftpad len) 0)
                               0))
                 (padch (integer->char (or padch format:space-ch))))
            (put-fill-chars leftpad padch)
            (put-string str)
            (put-fill-chars rightpad padch)))))

  ;; format fixed flonums (~F)

  (define (format:out-fixed modifier number pars)
    (unless (or (number? number) (string? number))
      (format-error "argument is not a number or a number string"))

    (let ((l (length pars)))
      (let ((width (format:par pars l 0 #f "width"))
            (digits (format:par pars l 1 #f "digits"))
            (scale (format:par pars l 2 0 #f))
            (overch (format:par pars l 3 #f #f))
            (padch (format:par pars l 4 format:space-ch #f)))

        (cond
         ((and (number? number)
               (or (inf? number) (nan? number)))
          (format:out-inf-nan number width digits #f overch padch))

         (digits
          (format:parse-float number #t scale)
          (if (<= (- format:fn-len format:fn-dot) digits)
              (format:fn-zfill #f (- digits (- format:fn-len format:fn-dot)))
              (format:fn-round digits))
          (if width
              (let ((numlen (+ format:fn-len 1)))
                (when (or (not format:fn-pos?) (eq? modifier 'at))
                  (set! numlen (+ numlen 1)))
                (when (and (= format:fn-dot 0) (> width (+ digits 1)))
                  (set! numlen (+ numlen 1)))
                (when (< numlen width)
                  (put-fill-chars (- width numlen) (integer->char padch)))
                (if (and overch (> numlen width))
                    (put-fill-chars width (integer->char overch))
                    (format:fn-out modifier (> width (+ digits 1)))))
              (format:fn-out modifier #t)))

         (else
          (format:parse-float number #t scale)
          (format:fn-strip)
          (if width
              (let ((numlen (+ format:fn-len 1)))
                (when (or (not format:fn-pos?) (eq? modifier 'at))
                  (set! numlen (+ numlen 1)))
                (when (= format:fn-dot 0)
                  (set! numlen (+ numlen 1)))
                (when (< numlen width)
                  (put-fill-chars (- width numlen) (integer->char padch)))
                (if (> numlen width)	; adjust precision if possible
                    (let ((dot-index (- numlen
                                        (- format:fn-len format:fn-dot))))
                      (if (> dot-index width)
                          (if overch ; numstr too big for required width
                              (put-fill-chars width (integer->char overch))
                              (format:fn-out modifier #t))
                          (begin
                            (format:fn-round (- width dot-index))
                            (format:fn-out modifier #t))))
                    (format:fn-out modifier #t)))
              (format:fn-out modifier #t)))))))

  ;; format exponential flonums (~E)

  (define (format:out-expon modifier number pars)
    (unless (or (number? number) (string? number))
      (format-error "argument is not a number"))

    (let ((l (length pars)))
      (let ((width (format:par pars l 0 #f "width"))
            (digits (format:par pars l 1 #f "digits"))
            (edigits (format:par pars l 2 #f "exponent digits"))
            (scale (format:par pars l 3 1 #f))
            (overch (format:par pars l 4 #f #f))
            (padch (format:par pars l 5 format:space-ch #f))
            (expch (format:par pars l 6 #f #f)))

        (cond
         ((and (number? number)
               (or (inf? number) (nan? number)))
          (format:out-inf-nan number width digits edigits overch padch))

         (digits                      ; fixed precision

          (let ((digits (if (> scale 0)
                            (if (< scale (+ digits 2))
                                (+ (- digits scale) 1)
                                0)
                            digits)))
            (format:parse-float number #f scale)
            (if (<= (- format:fn-len format:fn-dot) digits)
                (format:fn-zfill #f (- digits (- format:fn-len format:fn-dot)))
                (format:fn-round digits))
            (if width
                (if (and edigits overch (> format:en-len edigits))
                    (put-fill-chars width (integer->char overch))
                    (let ((numlen (+ format:fn-len 3))) ; .E+
                      (when (or (not format:fn-pos?) (eq? modifier 'at))
                        (set! numlen (+ numlen 1)))
                      (when (and (= format:fn-dot 0) (> width (+ digits 1)))
                        (set! numlen (+ numlen 1)))
                      (set! numlen
                            (+ numlen
                               (if (and edigits (>= edigits format:en-len))
                                   edigits
                                   format:en-len)))
                      (when (< numlen width)
                        (put-fill-chars (- width numlen)
                                         (integer->char padch)))
                      (if (and overch (> numlen width))
                          (put-fill-chars width (integer->char overch))
                          (begin
                            (format:fn-out modifier (> width (- numlen 1)))
                            (format:en-out edigits expch)))))
                (begin
                  (format:fn-out modifier #t)
                  (format:en-out edigits expch)))))

         (else
          (format:parse-float number #f scale)
          (format:fn-strip)
          (if width
              (if (and edigits overch (> format:en-len edigits))
                  (put-fill-chars width (integer->char overch))
                  (let ((numlen (+ format:fn-len 3))) ; .E+
                    (when (or (not format:fn-pos?) (eq? modifier 'at))
                      (set! numlen (+ numlen 1)))
                    (when (= format:fn-dot 0)
                      (set! numlen (+ numlen 1)))
                    (set! numlen
                          (+ numlen
                             (if (and edigits (>= edigits format:en-len))
                                 edigits
                                 format:en-len)))
                    (when (< numlen width)
                      (put-fill-chars (- width numlen)
                                       (integer->char padch)))
                    (if (> numlen width) ; adjust precision if possible
                        (let ((f (- format:fn-len format:fn-dot))) ; fract len
                          (if (> (- numlen f) width)
                              (if overch ; numstr too big for required width
                                  (put-fill-chars width
                                                   (integer->char overch))
                                  (begin
                                    (format:fn-out modifier #t)
                                    (format:en-out edigits expch)))
                              (begin
                                (format:fn-round (+ (- f numlen) width))
                                (format:fn-out modifier #t)
                                (format:en-out edigits expch))))
                        (begin
                          (format:fn-out modifier #t)
                          (format:en-out edigits expch)))))
              (begin
                (format:fn-out modifier #t)
                (format:en-out edigits expch))))))))

  ;; format general flonums (~G)

  (define (format:out-general modifier number pars)
    (unless (or (number? number) (string? number))
      (format-error "argument is not a number or a number string"))

    (let ((l (length pars)))
      (let ((width (if (> l 0) (list-ref pars 0) #f))
            (digits (if (> l 1) (list-ref pars 1) #f))
            (edigits (if (> l 2) (list-ref pars 2) #f))
            (overch (if (> l 4) (list-ref pars 4) #f))
            (padch (if (> l 5) (list-ref pars 5) #f)))
        (cond
         ((and (number? number)
               (or (inf? number) (nan? number)))
          ;; FIXME: this isn't right.
          (format:out-inf-nan number width digits edigits overch padch))
         (else
          (format:parse-float number #t 0)
          (format:fn-strip)
          (let* ((ee (if edigits (+ edigits 2) 4)) ; for the following algorithm
                 (ww (if width (- width ee) #f)) ; see Steele's CL book p.395
                 (n (if (= format:fn-dot 0) ; number less than (abs 1.0) ?
                        (- (format:fn-zlead))
                        format:fn-dot))
                 (d (if digits
                        digits
                        (max format:fn-len (min n 7)))) ; q = format:fn-len
                 (dd (- d n)))
            (if (<= 0 dd d)
                (begin
                  (format:out-fixed modifier number (list ww dd #f overch padch))
                  (put-fill-chars ee #\space)) ;~@T not implemented yet
                (format:out-expon modifier number pars))))))))

  ;; format dollar flonums (~$)

  (define (format:out-dollar modifier number pars)
    (unless (or (number? number) (string? number))
      (format-error "argument is not a number or a number string"))

    (let ((l (length pars)))
      (let ((digits (format:par pars l 0 2 "digits"))
            (mindig (format:par pars l 1 1 "mindig"))
            (width (format:par pars l 2 0 "width"))
            (padch (format:par pars l 3 format:space-ch #f)))

        (cond
         ((and (number? number)
               (or (inf? number) (nan? number)))
          (format:out-inf-nan number width digits #f #f padch))

         (else
          (format:parse-float number #t 0)
          (if (<= (- format:fn-len format:fn-dot) digits)
              (format:fn-zfill #f (- digits (- format:fn-len format:fn-dot)))
              (format:fn-round digits))
          (let ((numlen (+ format:fn-len 1)))
            (when (or (not format:fn-pos?) (memq modifier '(at colon-at)))
              (set! numlen (+ numlen 1)))
            (when (and mindig (> mindig format:fn-dot))
              (set! numlen (+ numlen (- mindig format:fn-dot))))
            (when (and (= format:fn-dot 0) (not mindig))
              (set! numlen (+ numlen 1)))
            (if (< numlen width)
                (case modifier
                  ((colon)
                   (unless format:fn-pos?
                     (put-char #\-))
                   (put-fill-chars (- width numlen) (integer->char padch)))
                  ((at)
                   (put-fill-chars (- width numlen) (integer->char padch))
                   (put-char (if format:fn-pos? #\+ #\-)))
                  ((colon-at)
                   (put-char (if format:fn-pos? #\+ #\-))
                   (put-fill-chars (- width numlen) (integer->char padch)))
                  (else
                   (put-fill-chars (- width numlen) (integer->char padch))
                   (unless format:fn-pos?
                     (put-char #\-))))
                (if format:fn-pos?
                    (when (memq modifier '(at colon-at))
                      (put-char #\+))
                    (put-char #\-))))
          (when (and mindig (> mindig format:fn-dot))
            (put-fill-chars (- mindig format:fn-dot) #\0))
          (when (and (= format:fn-dot 0) (not mindig))
            (put-char #\0))
          (put-substring format:fn-str 0 format:fn-dot)
          (put-char #\.)
          (put-substring format:fn-str format:fn-dot format:fn-len))))))

					; the flonum buffers

  (define format:fn-max 400)          ; max. number of number digits
  (define format:fn-str (make-string format:fn-max)) ; number buffer
  (define format:fn-len 0)            ; digit length of number
  (define format:fn-dot #f)           ; dot position of number
  (define format:fn-pos? #t)          ; number positive?
  (define format:en-max 10)           ; max. number of exponent digits
  (define format:en-str (make-string format:en-max)) ; exponent buffer
  (define format:en-len 0)            ; digit length of exponent
  (define format:en-pos? #t)          ; exponent positive?

  (define (format:parse-float num fixed? scale)
    (let ((num-str (if (string? num)
                       num
                       (number->string (exact->inexact num)))))
      (set! format:fn-pos? #t)
      (set! format:fn-len 0)
      (set! format:fn-dot #f)
      (set! format:en-pos? #t)
      (set! format:en-len 0)
      (do ((i 0 (+ i 1))
           (left-zeros 0)
           (mantissa? #t)
           (all-zeros? #t)
           (num-len (string-length num-str))
           (c #f))                ; current exam. character in num-str
          ((= i num-len)
           (unless format:fn-dot
             (set! format:fn-dot format:fn-len))

           (when all-zeros?
             (set! left-zeros 0)
             (set! format:fn-dot 0)
             (set! format:fn-len 1))

           ;; now format the parsed values according to format's need

           (if fixed?

               (begin                 ; fixed format m.nnn or .nnn
                 (when (and (> left-zeros 0) (> format:fn-dot 0))
                   (if (> format:fn-dot left-zeros)
                       (begin         ; norm 0{0}nn.mm to nn.mm
                         (format:fn-shiftleft left-zeros)
                         (set! format:fn-dot (- format:fn-dot left-zeros))
                         (set! left-zeros 0))
                       (begin         ; normalize 0{0}.nnn to .nnn
                         (format:fn-shiftleft format:fn-dot)
                         (set! left-zeros (- left-zeros format:fn-dot))
                         (set! format:fn-dot 0))))
                 (when (or (not (= scale 0)) (> format:en-len 0))
                   (let ((shift (+ scale (format:en-int))))
                     (cond
                      (all-zeros? #t)
                      ((> (+ format:fn-dot shift) format:fn-len)
                       (format:fn-zfill
                        #f (- shift (- format:fn-len format:fn-dot)))
                       (set! format:fn-dot format:fn-len))
                      ((< (+ format:fn-dot shift) 0)
                       (format:fn-zfill #t (- (- shift) format:fn-dot))
                       (set! format:fn-dot 0))
                      (else
                       (if (> left-zeros 0)
                           (if (<= left-zeros shift) ; shift always > 0 here
                               (begin
                                 (format:fn-shiftleft left-zeros)
                                 (set! format:fn-dot (- shift left-zeros)))
                               (format:fn-shiftleft shift)) ; shift out 0s
                           (set! format:fn-dot (+ format:fn-dot shift))))))))

               (let ((negexp          ; expon format m.nnnEee
                      (if (> left-zeros 0)
                          (- left-zeros format:fn-dot -1)
                          (if (= format:fn-dot 0) 1 0))))
                 (if (> left-zeros 0)
                     (begin           ; normalize 0{0}.nnn to n.nn
                       (format:fn-shiftleft left-zeros)
                       (set! format:fn-dot 1))
                     (when (= format:fn-dot 0)
                       (set! format:fn-dot 1)))
                 (format:en-set (- (+ (- format:fn-dot scale) (format:en-int))
                                   negexp))
                 (cond
                  (all-zeros?
                   (format:en-set 0)
                   (set! format:fn-dot 1))
                  ((< scale 0)        ; leading zero
                   (format:fn-zfill #t (- scale))
                   (set! format:fn-dot 0))
                  ((> scale format:fn-dot)
                   (format:fn-zfill #f (- scale format:fn-dot))
                   (set! format:fn-dot scale))
                  (else
                   (set! format:fn-dot scale)))))
           #t)

        ;; do body
        (set! c (string-ref num-str i)) ; parse the output of number->string
        (cond                         ; which can be any valid number
         ((char-numeric? c)           ; representation of R4RS except
          (if mantissa?               ; complex numbers
              (begin
                (if (char=? c #\0)
                    (when all-zeros?
                      (set! left-zeros (+ left-zeros 1)))
                    (begin
                      (set! all-zeros? #f)))
                (string-set! format:fn-str format:fn-len c)
                (set! format:fn-len (+ format:fn-len 1)))
              (begin
                (string-set! format:en-str format:en-len c)
                (set! format:en-len (+ format:en-len 1)))))
         ((or (char=? c #\-) (char=? c #\+))
          (if mantissa?
              (set! format:fn-pos? (char=? c #\+))
              (set! format:en-pos? (char=? c #\+))))
         ((char=? c #\.)
          (set! format:fn-dot format:fn-len))
         ((char=? c #\e)
          (set! mantissa? #f))
         ((char=? c #\E)
          (set! mantissa? #f))
         ((char-whitespace? c) #t)
         ((char=? c #\d) #t)          ; decimal radix prefix
         ((char=? c #\#) #t)
         (else
          (format-error "illegal character `~c' in number->string" c))))))

  (define (format:en-int)         ; convert exponent string to integer
    (if (= format:en-len 0)
        0
        (do ((i 0 (+ i 1))
             (n 0))
            ((= i format:en-len)
             (if format:en-pos?
                 n
                 (- n)))
          (set! n (+ (* n 10) (- (char->integer (string-ref format:en-str i))
                                 format:zero-ch))))))

  (define (format:en-set en)          ; set exponent string number
    (set! format:en-len 0)
    (set! format:en-pos? (>= en 0))
    (let ((en-str (number->string en)))
      (do ((i 0 (+ i 1))
           (en-len (string-length en-str))
           (c #f))
          ((= i en-len))
        (set! c (string-ref en-str i))
        (when (char-numeric? c)
          (string-set! format:en-str format:en-len c)
          (set! format:en-len (+ format:en-len 1))))))

  (define (format:fn-zfill left? n) ; fill current number string with 0s
    (when (> (+ n format:fn-len) format:fn-max) ; from the left or right
      (format-error "number is too long to format (enlarge format:fn-max)"))
    (set! format:fn-len (+ format:fn-len n))
    (if left?
        (do ((i format:fn-len (- i 1))) ; fill n 0s to left
            ((< i 0))
          (string-set! format:fn-str i
                       (if (< i n)
                           #\0
                           (string-ref format:fn-str (- i n)))))
        (do ((i (- format:fn-len n) (+ i 1))) ; fill n 0s to the right
            ((= i format:fn-len))
          (string-set! format:fn-str i #\0))))

  (define (format:fn-shiftleft n) ; shift left current number n positions
    (when (> n format:fn-len)
      (format-error "internal error in format:fn-shiftleft (~d,~d)"
                    n format:fn-len))
    (do ((i n (+ i 1)))
        ((= i format:fn-len)
         (set! format:fn-len (- format:fn-len n)))
      (string-set! format:fn-str (- i n) (string-ref format:fn-str i))))

  (define (format:fn-round digits)    ; round format:fn-str
    (set! digits (+ digits format:fn-dot))
    (do ((i digits (- i 1))		; "099",2 -> "10"
         (c 5))                       ; "023",2 -> "02"
        ((or (= c 0) (< i 0))         ; "999",2 -> "100"
         (if (= c 1)			; "005",2 -> "01"
             (begin			; carry overflow
               (set! format:fn-len digits)
               (format:fn-zfill #t 1) ; add a 1 before fn-str
               (string-set! format:fn-str 0 #\1)
               (set! format:fn-dot (+ format:fn-dot 1)))
             (set! format:fn-len digits)))
      (set! c (+ (- (char->integer (string-ref format:fn-str i))
                    format:zero-ch) c))
      (string-set! format:fn-str i (integer->char
                                    (if (< c 10)
                                        (+ c format:zero-ch)
                                        (+ (- c 10) format:zero-ch))))
      (set! c (if (< c 10) 0 1))))

  (define (format:fn-out modifier add-leading-zero?)
    (if format:fn-pos?
        (when (eq? modifier 'at)
          (put-char #\+))
        (put-char #\-))
    (if (= format:fn-dot 0)
        (when add-leading-zero?
          (put-char #\0))
        (put-substring format:fn-str 0 format:fn-dot))
    (put-char #\.)
    (put-substring format:fn-str format:fn-dot format:fn-len))

  (define (format:en-out edigits expch)
    (put-char (if expch (integer->char expch) #\E))
    (put-char (if format:en-pos? #\+ #\-))
    (when (and edigits (< format:en-len edigits))
      (put-fill-chars (- edigits format:en-len) #\0))
    (put-substring format:en-str 0 format:en-len))

  (define (format:fn-strip)           ; strip trailing zeros but one
    (string-set! format:fn-str format:fn-len #\0)
    (do ((i format:fn-len (- i 1)))
        ((or (not (char=? (string-ref format:fn-str i) #\0))
             (<= i format:fn-dot))
         (set! format:fn-len (+ i 1)))))

  (define (format:fn-zlead)           ; count leading zeros
    (do ((i 0 (+ i 1)))
        ((or (= i format:fn-len)
             (not (char=? (string-ref format:fn-str i) #\0)))
         (if (= i format:fn-len)      ; found a real zero
             0
             i))))


;;; some global functions not found in SLIB

  (define (string-capitalize-first str) ; "hello" -> "Hello"
    (let ((cap-str (string-copy str))   ; "hELLO" -> "Hello"
          (non-first-alpha #f)          ; "*hello" -> "*Hello"
          (str-len (string-length str))) ; "hello you" -> "Hello you"
      (do ((i 0 (+ i 1)))
          ((= i str-len) cap-str)
        (let ((c (string-ref str i)))
          (when (char-alphabetic? c)
            (if non-first-alpha
                (string-set! cap-str i (char-downcase c))
                (begin
                  (set! non-first-alpha #t)
                  (string-set! cap-str i (char-upcase c)))))))))

  (define arg-pos (format:format-work format-string format-args))
  (define arg-len (length format-args))

  (cond
   ((> arg-pos arg-len)
    (set! %arg-pos (+ arg-len 1))
    (display %arg-pos)
    (format-error "~a missing argument~:p" (- arg-pos arg-len)))
   (else
    (when %flush-output?
      (force-output port))
    (if destination
        #t
        (let ((str (get-output-string port)))
          (close-port port)
          str)))))

;; Set the format variable in the root module.  This is legacy and
;; no longer necessary.  It means that as soon as (ice-9 format) is
;; loaded somewhere by some module, the predefined binding for format
;; becomes the extended format function, even in modules where (ice-9 format)
;; isn't imported.  Because of this, removing this line should be done
;; when a backwards compatibility break is allowed.
(module-set! the-root-module 'format format)
