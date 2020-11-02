#!/usr/bin/env python3

# constants
NAcceptors = 4
NBallots = 4
NValues = 2

# the set of identifiers that hold the potential messages, like in an arena
msgs = []

def next_defs(no):
    text = \
    f"""
;; Step {no}: definitions

(declare-const maxBal_{no} Fun)
(declare-const maxVBal_{no} Fun)
(declare-const maxVal_{no} Fun)
(declare-const msgs_{no} SetMsg)

;; existentials
(declare-const b_{no} Int)
(declare-const a_{no} Int)
(declare-const v_{no} Int)
(declare-const m_{no} Msg)

;;(assert (and (<= 0 b_{no}) (<= b_{no} {NBallots})))
;;(assert (and (<= 0 a_{no}) (<= a_{no} {NAcceptors})))
;;(assert (and (<= 0 v_{no}) (<= v_{no} {NValues})))

;; Step {no}: oracle
(declare-const oracle_{no} Int)
(assert (or (= oracle_{no} 0) (= oracle_{no} 1) (= oracle_{no} 2) (= oracle_{no} 3)))
    """

    return text


def phase_1a(no):
    text = \
    f"""
;; Step {no}: Phase1a
(declare-const msg1a_{no} Msg)
;; a singleton set in Send
(declare-const singleton1a_{no} SetMsg)

;; [type |> "1a", bal |> b]
(assert (= msg1a_{no}
           (mk-msg type1a 0 b_1 0 0 0)))
;; {{[type |-> "1a", bal |-> b]}}
(assert (= singleton1a_{no} (store emptyMsgs msg1a_{no} true)))

;; the assertions imposed by the action
(assert
  (or
    (not (= oracle_{no} 0))
    (and 
        (= msgs_{no} ((_ map or) msgs_0 singleton1a_{no}))
        (= maxBal_{no} maxBal_{no-1})
        (= maxVBal_{no} maxVBal_{no-1})
        (= maxVal_{no} maxVal_{no-1})
        (= m_{no} msg1a_{no})
    )    
  )
)
    """

    return text


def phase_1b(no):
    text = \
    f"""
;; Step 1: Phase1b
;; \E m \in msgs
(declare-const m1b_{no} Msg)
;; a singleton set in Send
(declare-const singleton1b_{no} SetMsg)
(declare-const msg1b_{no} Msg)
;; [type |-> "1b", acc |-> a, bal |-> m.bal, mbal |-> maxVBal[a], mval |-> maxVal[a]]
(assert (= msg1b_{no}
           (mk-msg type1b a_{no} (bal m1b_{no})
                   (select maxVBal_{no-1} a_{no}) (select maxVal_{no-1} a_{no}) 0)))
;; ((Msg (mk-msg (type MsgType) (acc Int) (bal Int) (mbal Int) (mval Int) (val Int)))))

;; {{ msg1b_{no} }}
(assert (= singleton1b_{no} (store emptyMsgs msg1b_{no} true)))

;; the assertions imposed by the action
(assert
  (or
    (not (= oracle_{no} 1))
    (and 
        ;; m.type = "1a"
        (= (type m1b_{no}) type1a)
        ;; m.bal > maxBal[a]
        (> (select bal m1b_{no}) (select maxBal_{no-1} a_{no}))
        ;; maxBal' = [maxBal EXCEPT ![a] = m.bal]
        (= maxBal_{no} (store maxBal_{no-1} a_{no} (bal m1b_{no})))
        ;; Send([type |-> "1b", acc |-> a, bal |-> m.bal, 
        ;;       mbal |-> maxVBal[a], mval |-> maxVal[a]])
        (= msgs_{no} ((_ map or) msgs_{no-1} singleton1b_{no}))
        ;; UNCHANGED <<maxVBal, maxVal>>
        (= maxVBal_{no} maxVBal_{no-1})
        (= maxVal_{no} maxVal_{no-1})
        (= m_{no} msg1b_{no})
    )    
  )
)    
    """

    return text


def phase_2a(no):
    text = f"""
;; Step 1: Phase2a
;; a singleton set in Send
(declare-const singleton2a_{no} SetMsg)
(declare-const msg2a_{no} Msg)
(declare-const m2a_{no} Msg)
;; [type |-> "2a", bal |-> b, val |-> v]
(assert (= msg2a_{no}
           (mk-msg type2a -1 b_{no}
                   -1 -1 v_{no})))
;; ((Msg (mk-msg (type MsgType) (acc Int) (bal Int) (mbal Int) (mval Int) (val Int)))))

;; {{ msg1b_{no} }}
(assert (= singleton2a_{no} (store emptyMsgs msg2a_{no} true)))

;; \E Q \in Quorum :
(declare-const Q_{no} Set1)
(assert (= true (select Quorum Q_{no})))
(declare-const Q1b_{no} SetMsg)
(declare-const Q1bv_{no} SetMsg)

(declare-const pred1b_{no} Bool)

;; the following assertion blows up the solver
;;(assert
;;    (forall ((m Msg))
;;        (iff (= Q1b_{no} (store Q1b_{no} m true))
;;             (and
;;                (= (type m) type1b)
;;                (= (select Q_{no} (acc m)) true)
;;                (= b_{no} (bal m))
;;             )
;;        )
;;    )
;;)

;; instead, we use arenas
(declare-const Q1b_tmp_{no} SetMsg)
(declare-const Q1bv_tmp_{no} SetMsg)
    """

    text2 = ""
    for i in range(1, no):
        text2 += f"""
        ;;  Q1b == {{ m in msgs : m.type = "1b" and m.acc in Q and m.bal = b }}
        (assert
            (=>
                (= Q1b_tmp_{no} (store Q1b_tmp_{no} m_{i} true))
                (and
                    ;; m in msgs
                    (select msgs_{no-1} m_{i})
                    ;; m.type = "1b"
                    (= (type m_{i}) type1b)
                    ;; m.acc in Q
                    (= true (select Q_{no} (acc m_{i})))
                    ;; m.bal = b
                    (= (bal m_{i}) b_{no})
                )
            )
        )
        ;; Q1bv == {{ m in Q1b : m.mbal geq 0 }}
        (assert
            (=>
                (= Q1bv_tmp_{no} (store Q1bv_tmp_{no} m_{i} true))
                (and
                    ;; m in Q1b
                    (select Q1b_{no} m_{i})
                    ;; m.mbal >= 0
                    (>= (mbal m_{i}) 0)
                )
            )
        )
        """

    # translate \A a \in Q : \E m \in Q1b : m.acc = a 
    text3 = f"""
        (assert
          (=> pred1b_{no}
             (and       
        """

    if no <= 1:
        text3 += "true"
    else:
        for a in range(1, NAcceptors + 1):
            text3 += f"""
                   (=> (select Q_{no} {a})
                     (or
            """
            for i in range(1, no):
                text3 += f"""
                        (and (select Q1b_{no} m_{i}) (= {a} (acc m_{i})))
                """

            text3 += "))"

    text3 += ")))"

    last_cond = f"""
    (and 
        (or
    """

    if no <= 1:
        last_cond += "true"
    else:
        for i in range(1, no):
            last_cond += f"""
              (select Q1b_{no} m_{i})"""
            
    last_cond += f"""
        )
        (and
            ;; exists m in Q1bv
          (select Q1bv_{no} m2a_{no})
          (= (mval m2a_{no}) v_{no})
          """

    for i in range(1, no):
        last_cond += f"""
          (=>
            (select Q1bv_{no} m_{i})
            (>= (mbal m2a_{no}) (mbal m_{no}))
          )"""

    last_cond += f"""
        )
    )
    """

    text4 = f"""
(assert (= Q1b_{no} ((_ map and) Q1b_tmp_{no} msgs_{no-1})))
;; Q1bv == {{ m \in Q1b: ... }}
(assert (= Q1bv_{no} ((_ map and) Q1bv_tmp_{no} Q1b_{no})))
;; the assertions imposed by the action
(assert
  (or
    (not (= oracle_{no} 2))
    (and
        (forall ((m Msg))
            (or
                (not (= (type m) type2a))
                (not (= (bal m) b_{no}))
            )
        )
        ;; forall a in Q: exists m in Q1b: m.acc = a
        pred1b_{no}
        ;; and or Q1bv = {{ }}
        {last_cond}
        ;; Send([type |-> "2a", bal |-> b, val |-> v])
        (= msgs_{no} ((_ map or) msgs_{no-1} singleton2a_{no}))
        ;; UNCHANGED <<maxBal, maxVBal, maxVal>>
        (= maxBal_{no} maxBal_{no-1})
        (= maxVBal_{no} maxVBal_{no-1})
        (= maxVal_{no} maxVal_{no-1})
        (= m_{no} msg2a_{no})
    )
  )
)  
    """

    return text + text2 + text3 + text4


def phase_2b(no):
    text = \
    f"""
;; Step 1: Phase2b
;; \E m \in msgs
(declare-const m2b_{no} Msg)
;; a singleton set in Send
(declare-const singleton2b_{no} SetMsg)
(declare-const msg2b_{no} Msg)
;; [type |-> "2b", acc |-> a, bal |-> m.bal, val |-> m.val]
(assert (= msg2b_{no}
           (mk-msg type2b a_{no} (bal m2b_{no})
                   -1 -1 (val m2b_{no}))))
;; ((Msg (mk-msg (type MsgType) (acc Int) (bal Int) (mbal Int) (mval Int) (val Int)))))

;; {{ msg2b_{no} }}
(assert (= singleton2b_{no} (store emptyMsgs msg2b_{no} true)))

;; the assertions imposed by the action
(assert
  (or
    (not (= oracle_{no} 3))
    (and 
        ;; m.type = 2b"
        (= (type m2b_{no}) type2a)
        ;; m.bal > maxBal[a]
        (>= (select bal m2b_{no}) (select maxBal_{no-1} a_{no}))
        ;; maxBal' = [maxBal EXCEPT ![a] = m.bal]
        (= maxBal_{no} (store maxBal_{no-1} a_{no} (bal m2b_{no})))
        ;;  maxVBal' = [maxVBal EXCEPT ![a] = m.bal]
        (= maxVBal_{no} (store maxVBal_{no-1} a_{no} (bal m2b_{no})))
        ;;  maxVal' = [maxVal EXCEPT ![a] = m.val]
        (= maxVal_{no} (store maxVal_{no-1} a_{no} (val m2b_{no})))
        ;; Send([type |-> "1b", acc |-> a, bal |-> m.bal, 
        ;;       mbal |-> maxVBal[a], mval |-> maxVal[a]])
        (= msgs_{no} ((_ map or) msgs_{no-1} singleton1b_{no}))
        (= m_{no} msg1b_{no})
    )    
  )
)    
    """

    return text


if __name__ == "__main__":

    preambule = \
    """
(define-sort Set1 () (Array Int Bool))
(define-sort Set2 () (Array Set1 Bool))
(define-sort Fun () (Array Int Int))

(declare-sort MsgType)
(declare-const type1a MsgType)
(declare-const type1b MsgType)
(declare-const type2a MsgType)
(declare-const type2b MsgType)
(assert (distinct type1a type1b type2a type2b))

(declare-datatypes ()
    ((Msg (mk-msg (type MsgType) (acc Int)
                  (bal Int) (mbal Int) (mval Int) (val Int)))))
(define-sort SetMsg () (Array Msg Bool))

;; encode the constants

;; declare the empty set
(declare-const empty1 Set1)
(assert (= empty1 ((as const Set1) false)))

;; declare the members of the Quorum set
(declare-const s1 Set1)
(declare-const s2 Set1)
(declare-const s3 Set1)
(declare-const s4 Set1)

(assert (= s1
    (store
        (store
            (store empty1 1 true)
            2 true
        )
        3 true)))

(assert (= s2
    (store
        (store
            (store empty1 1 true)
            2 true
        )
        4 true)))

(assert (= s3
    (store
        (store
            (store empty1 1 true)
            3 true
        )
        4 true)))

(assert (= s4
    (store
        (store
            (store empty1 2 true)
            3 true
        )
        4 true)))

;; declare the Quorum set
(declare-const Quorum Set2)
(declare-const empty2 Set2)
(assert (= empty2 ((as const Set2) false)))

;; add the sets s1, s2, s3, s4 to Quorum
(assert (= Quorum
    (store
        (store
            (store
                (store empty2 s1 true)
                s2 true
            )
            s3 true
        )
        s4 true)))

; empty set {}
(declare-const emptyMsgs SetMsg)
(assert (= emptyMsgs ((as const SetMsg) false)))
    """

    print(preambule)

    init = """
;; Init

(declare-const maxBal_0 Fun)
(declare-const maxVBal_0 Fun)
(declare-const maxVal_0 Fun)
(declare-const msgs_0 SetMsg)

(assert (= maxBal_0 (store maxBal_0 1 -1)))
(assert (= maxBal_0 (store maxBal_0 2 -1)))
(assert (= maxBal_0 (store maxBal_0 3 -1)))
(assert (= maxBal_0 (store maxBal_0 4 -1)))

(assert (= maxVBal_0 (store maxVBal_0 1 -1)))
(assert (= maxVBal_0 (store maxVBal_0 2 -1)))
(assert (= maxVBal_0 (store maxVBal_0 3 -1)))
(assert (= maxVBal_0 (store maxVBal_0 4 -1)))

(assert (= maxVal_0 (store maxVal_0 1 -1)))
(assert (= maxVal_0 (store maxVal_0 2 -1)))
(assert (= maxVal_0 (store maxVal_0 3 -1)))
(assert (= maxVal_0 (store maxVal_0 4 -1)))

    """

    print(init)
    for k in range(1, 45 + 1):
        print(next_defs(k))
        print(phase_1a(k))
        print(phase_1b(k))
        print(phase_2a(k))
        print(phase_2b(k))
        print("(check-sat)")

    print("(check-sat)")


