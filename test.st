
(def map_some 
    (fun a -> (
        if (eq? a.opt.tag true) 
        then (a.f a.opt.val) 
        else  [tag=false val=[]] 
        )
    )
)
(def main (map_some [opt=[tag=true val=1] f=(fun x -> (+ x 1) )] ))