
(def map_some (fun a -> (if (eq? (sel (sel a opt) tag) true) then ((sel a f) (sel (sel a opt) val)) else ( [tag=false val=[] ]))))
(def main (map_some [opt=[tag=true val=1] f=(fun x -> (+ x 1) )] ))