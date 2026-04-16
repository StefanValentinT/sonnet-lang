(def x 0)

(def
    fib 
    (fun n ->
        (
            if (< n 2) 
            then 1 
            else (+ (fib (- n 1)) (fib (- n 2)))
        )
    )
)

(def a [2 true])
(def b [4 (fun x -> x)])
(def pick (fun x -> (if (eq? x true) then a else b)))


(def first (fun x -> (sel x 0)))

(def id (fun x -> x))

(def abc (let a 0 in (let b 1 in (let c 2 in (+ c (+ a b))))))

(def main abc )