(var sym-table @{})
(var global-timer 0)


(defn error-exit [msg &opt detail]
	(eprintf "COMPILE ERROR: %s [%s]" msg (or detail ""))
	(os/exit 1))

(defn is-digit [c] (and c (>= c (chr "0")) (<= c (chr "9"))))

(defn get-sym [name &opt create]
	(if-let [s (get sym-table name)] s
		(when create
			(let [new-s @{:name name :last-seen -1 :reg-idx (length sym-table)}]
				(put sym-table name new-s)
				new-s))))

(defn rv-r [v] (string "s" (+ 1 (get (get-sym v) :reg-idx))))
(defn arm-r [v] (string "x" (+ 19 (get (get-sym v) :reg-idx))))


(var- input-buffer "")
(var- pos 0)

(defn peek-c [] (get input-buffer pos))
(defn next-c [] (let [c (peek-c)] (++ pos) c))
(defn is-space [c] (and c (<= c 32)))

(defn skip-ws []
	(while (let [c (peek-c)] (and c (or (is-space c) (= c (chr "#")))))
		(if (= (peek-c) (chr "#"))
			(while (let [c (next-c)] (and c (not= c (chr "\n")))))
			(next-c))))

(defn nxt-v []
	(skip-ws)
	(let [c (peek-c)]
		(if (or (not c) (= c (chr "(")) (= c (chr ")")))
			nil
			(let [start pos]
				(while (let [curr (peek-c)]
								 (and curr (not (is-space curr)) 
											(not= curr (chr "(")) (not= curr (chr ")"))))
					(next-c))
				(string/slice input-buffer start pos)))))

(defn parse []
	(skip-ws)
	(let [c (peek-c)]
		(cond
			(not c) nil
			(= c (chr "("))
			(do
				(next-c)
				(let [op (nxt-v)
							node @{:c @[] :id (++ global-timer)}]
					(cond
						(not op) (put node :t :T_PARAMS)
						(= op "fun")  (do (put node :t :T_FUN) (put node :v (nxt-v)))
						(= op "init") (put node :t :T_INIT)
						(= op "set")  (put node :t :T_SET)
						(= op "call") (put node :t :T_CALL)
						(= op "ret")  (put node :t :T_RET)
						(= op "loop") (put node :t :T_LOOP)
						(= op "add")  (put node :t :T_ADD)
						(= op "sub")  (put node :t :T_SUB)
						(= op "xor")  (put node :t :T_XOR)
						(= op "add-im") (put node :t :T_ADDI)
						(= op "sub-im") (put node :t :T_SUBI)
						(= op "xor-im") (put node :t :T_XORI)
						(do (put node :t :T_PARAMS)
								(array/push (get node :c) @{:t (if (string/has-prefix? "$" op) :T_REG :T_IDENT) :v op})))
					
					(while (do (skip-ws) (let [p (peek-c)] (and p (not= p (chr ")")))))
						(array/push (get node :c) (parse)))
					(next-c)

					(let [ch (node :c)]
						(case (node :t)
							:T_INIT (if (or (not= (length ch) 2) (not= ((ch 1) :t) :T_NUM)) 
												(error-exit "init requires $reg and immediate"))
							:T_SET  (if (or (not= (length ch) 2) (not= ((ch 1) :t) :T_REG)) 
												(error-exit "set requires two $regs"))
							:T_FUN  (if (or (= (length ch) 0) (not= ((ch 0) :t) :T_PARAMS)) 
												(error-exit "fun requires parameter list"))))
					node))
			(let [v (nxt-v)]
				@{:t (cond (string/has-prefix? "$" v) :T_REG
									 (or (is-digit (v 0)) (and (= (v 0) (chr "-")) (is-digit (v 1)))) :T_NUM
									 :T_IDENT) :v v :id (++ global-timer)}))))

(defn analyze [n]
	(unless n (break))
	(let [t (n :t) ch (get n :c @[])]
		(case t
			:T_PARAMS (each p ch (if (= (p :t) :T_REG) (get-sym (p :v) true)))
			:T_INIT   (get-sym ((ch 0) :v) true)
			:T_SET    (do (get-sym ((ch 0) :v) true) (get-sym ((ch 1) :v) true))
			:T_CALL   (get-sym ((ch 0) :v) true)
			:T_ADD    (get-sym ((ch 0) :v) true)
			:T_SUB    (get-sym ((ch 0) :v) true)
			:T_XOR    (get-sym ((ch 0) :v) true)
			:T_ADDI   (get-sym ((ch 0) :v) true)
			:T_SUBI   (get-sym ((ch 0) :v) true)
			:T_XORI   (get-sym ((ch 0) :v) true))
		(each c ch (analyze c))
		(if (= t :T_REG) (if-let [s (get-sym (n :v) false)] (put s :last-seen (n :id))))))


(defn gen-rv [n]
	(unless n (break))
	(let [ch (n :c)]
		(case (n :t)
			:T_FUN (do (set sym-table @{}) (analyze n)
							 (print "\n.global " (n :v) "\n" (n :v) ":")
							 (print "  addi sp, sp, -112\n  sd ra, 104(sp)")
							 (for i 1 12 (print "  sd s" i ", " (* (- i 1) 8) "(sp)"))
							 (each c ch 
								 (if (= (c :t) :T_PARAMS)
									 (let [ps (c :c)] (for j 0 (length ps) (print "  mv " (rv-r ((ps j) :v)) ", a" j)))
									 (gen-rv c)))
							 (if (= (n :v) "main") (print "  li a0, 0"))
							 (for i 1 12 (print "  ld s" i ", " (* (- i 1) 8) "(sp)"))
							 (print "  ld ra, 104(sp)\n  addi sp, sp, 112\n  ret"))
			:T_INIT (print "  li " (rv-r ((ch 0) :v)) ", " ((ch 1) :v))
			:T_SET  (print "  mv " (rv-r ((ch 0) :v)) ", " (rv-r ((ch 1) :v)))
			:T_ADD  (print "  add " (rv-r ((ch 0) :v)) ", " (rv-r ((ch 1) :v)) ", " (rv-r ((ch 2) :v)))
			:T_SUB  (print "  sub " (rv-r ((ch 0) :v)) ", " (rv-r ((ch 1) :v)) ", " (rv-r ((ch 2) :v)))
			:T_XOR  (print "  xor " (rv-r ((ch 0) :v)) ", " (rv-r ((ch 1) :v)) ", " (rv-r ((ch 2) :v)))
			:T_ADDI (print "  addi " (rv-r ((ch 0) :v)) ", " (rv-r ((ch 1) :v)) ", " ((ch 2) :v))
			:T_SUBI (print "  addi " (rv-r ((ch 0) :v)) ", " (rv-r ((ch 1) :v)) ", -" ((ch 2) :v))
			:T_XORI (print "  xori " (rv-r ((ch 0) :v)) ", " (rv-r ((ch 1) :v)) ", " ((ch 2) :v))
			:T_LOOP (let [id (n :id)] 
							 (print "L" id "s:") 
							 (let [cond-node (ch 0)]
								 (if (= (cond-node :t) :T_REG)
									 (print "  mv a0, " (rv-r (cond-node :v)))
									 (gen-rv cond-node)))
							 (print "  beqz a0, L" id "e") 
							 (each b (slice ch 1) (gen-rv b))
							 (print "  j L" id "s\nL" id "e:"))
			:T_RET  (print "  mv a0, " (rv-r ((ch 0) :v)))
			:T_CALL (do 
								(for i 2 (length ch) 
									(let [a (ch i)] 
										(print (if (= (a :t) :T_NUM) "  li a" "  mv a") (- i 2) ", " (if (= (a :t) :T_NUM) (a :v) (rv-r (a :v))))))
								(print "  call " ((ch 1) :v) "\n  mv " (rv-r ((ch 0) :v)) ", a0")))))

(defn gen-arm [n]
	(unless n (break))
	(let [ch (n :c)]
		(case (n :t)
			:T_FUN (do (set sym-table @{}) (analyze n)
							 (print "\n.global _" (n :v) "\n.p2align 2\n_" (n :v) ":")
							 (print "  stp x29, x30, [sp, #-112]!\n  mov x29, sp")
							 (for i 0 4 (print "  stp x" (+ 19 (* i 2)) ", x" (+ 20 (* i 2)) ", [sp, " (+ 16 (* i 16)) "]"))
							 (each c ch 
								 (if (= (c :t) :T_PARAMS)
									 (let [ps (c :c)] (for j 0 (length ps) (print "  mov " (arm-r ((ps j) :v)) ", x" j)))
									 (gen-arm c)))
							 (if (= (n :v) "main") (print "  mov w0, #0"))
							 (for i 0 4 (print "  ldp x" (+ 19 (* i 2)) ", x" (+ 20 (* i 2)) ", [sp, " (+ 16 (* i 16)) "]"))
							 (print "  ldp x29, x30, [sp], #112\n  ret"))
			:T_INIT (print "  mov " (arm-r ((ch 0) :v)) ", #" ((ch 1) :v))
			:T_SET  (print "  mov " (arm-r ((ch 0) :v)) ", " (arm-r ((ch 1) :v)))
			:T_ADD  (print "  add " (arm-r ((ch 0) :v)) ", " (arm-r ((ch 1) :v)) ", " (arm-r ((ch 2) :v)))
			:T_SUB  (print "  sub " (arm-r ((ch 0) :v)) ", " (arm-r ((ch 1) :v)) ", " (arm-r ((ch 2) :v)))
			:T_XOR  (print "  eor " (arm-r ((ch 0) :v)) ", " (arm-r ((ch 1) :v)) ", " (arm-r ((ch 2) :v)))
			:T_ADDI (print "  add " (arm-r ((ch 0) :v)) ", " (arm-r ((ch 1) :v)) ", #" ((ch 2) :v))
			:T_SUBI (print "  sub " (arm-r ((ch 0) :v)) ", " (arm-r ((ch 1) :v)) ", #" ((ch 2) :v))
			:T_XORI (print "  eor " (arm-r ((ch 0) :v)) ", " (arm-r ((ch 1) :v)) ", #" ((ch 2) :v))
			:T_LOOP (let [id (n :id)] 
							 (print "L" id "s:") 
							 (let [cond-node (ch 0)]
								 (if (= (cond-node :t) :T_REG)
									 (print "  mov x0, " (arm-r (cond-node :v)))
									 (gen-arm cond-node)))
							 (print "  cbz x0, L" id "e") 
							 (each b (slice ch 1) (gen-arm b))
							 (print "  b L" id "s\nL" id "e:"))
			:T_RET  (print "  mov x0, " (arm-r ((ch 0) :v)))
			:T_CALL (do 
								(for i 2 (length ch) 
									(let [a (ch i)]
										(print "  mov x" (- i 2) ", " (if (= (a :t) :T_NUM) (string "#" (a :v)) (arm-r (a :v))))))
								(print "  bl _" ((ch 1) :v) "\n  mov " (arm-r ((ch 0) :v)) ", x0")))))


(defn main [& args]
	(when (< (length args) 4)
		(error-exit "Usage: janet ginkoc.janet <filename> --target <riscv64|aarch64>"))
	
	(def filename (args 1))
	(def target-flag (args 2))
	(def target (args 3))

	(unless (= target-flag "--target")
		(error-exit "Expected --target flag"))

	(set input-buffer (slurp filename))
	(while (< pos (length input-buffer))
		(if-let [n (parse)] 
			(case target 
				"aarch64" (gen-arm n) 
				"riscv64" (gen-rv n)
				(error-exit "Bad target" target)))))