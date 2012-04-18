(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect . a)
    "Incorrect password")
  (define (dispatch pass m)
    (if (eq? password pass)
      (cond ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        ((eq? m 'pass) 'correct)
        (else (error "Unknown request"
                     m)))
      incorrect))
  dispatch)

(define (make-joint account password new-password)
  (define (incorrect . a)
    "Incorrect password")
  (define (dispatch pass m)
    (if (eq? new-password pass)
      (account password m)
      incorrect))

  (if (eq? (account password 'pass) 'correct)
    dispatch
    (incorrect)))

; Usage:
; (define piyo-acc (make-account 100 'hoge))
; (define poyo-acc (make-joint piyo-acc 'hoge 'fuga))
; (define ugyo-acc (make-joint piyo-acc 'agya 'hunyo)) ; "Incorrect password"
; ((piyo-acc 'hoge 'withdraw) 40)
; ((poyo-acc 'fuga 'deposit) 30)
; ((poyo-acc 'hoge 'withdraw) 20) ; "Incorrect password"
