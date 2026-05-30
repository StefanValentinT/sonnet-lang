(def src (or (get (dyn :args) 1) (os/exit 1)))
(def base (last (string/split "/" (string/replace ".c" "" src))))

(os/execute ["sh" "-c" (string "mill run " src " > /dev/null 2>&1")] :p)
(os/execute ["sh" "-c" (string "gcc build/" base ".s -o build/" base " > /dev/null 2>&1")] :p)

(os/exit (os/execute [(string "build/" base)] :p))