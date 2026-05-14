(var sym-table @{})
(var global-timer 0)
(var current-func-name "")

(defn error-exit [msg &opt detail]
	(eprintf "COMPILE ERROR: %s [%s]" msg (or detail ""))
	(os/exit 1))

(defn node-program [funcs] 
	@{:type :program :functions funcs})
(defn node-func [name params body] 
	@{:type :function :name name :params params :body body})

(defn node-set-im [dest val] 
	@{:type :set-im :dest dest :val val})
(defn node-set [dest src] 
	@{:type :set :dest dest :src src})
(defn node-call [dest func args] 
	@{:type :call :dest dest :func func :args args})
(defn node-ret [val] 
	@{:type :ret :val val})

(defn node-alloc [dest size-reg] 
	@{:type :alloc :dest dest :size-reg size-reg})
(defn node-alloc-im [dest size] 
	@{:type :alloc-im :dest dest :size size})

(defn node-store-i64 [src loc offset]
	@{:type :store :size :i64 :src src :loc loc :offset offset})
(defn node-store-i32 [src loc offset] # (store-i32 $val $location $offset)
	@{:type :store :size :i32 :src src :loc loc :offset offset})
(defn node-store-i16 [src loc offset]
	@{:type :store :size :i16 :src src :loc loc :offset offset})
(defn node-store-i8 [src loc offset]
	@{:type :store :size :i8 :src src :loc loc :offset offset})

(defn node-load-i64 [dest loc offset]
	@{:type :load :size :i64 :dest dest :loc loc :offset offset})
(defn node-load-i32 [dest loc offset]
	@{:type :load :size :i32 :dest dest :loc loc :offset offset})
(defn node-load-i16 [dest loc offset]
	@{:type :load :size :i16 :dest dest :loc loc :offset offset})
(defn node-load-i8 [dest loc offset]
	@{:type :load :size :i8 :dest dest :loc loc :offset offset})

(defn node-arith [op dest src1 src2] 
	@{:type op :dest dest :src1 src1 :src2 src2})
(defn node-arith-im [op dest src1 val] 
	@{:type op :dest dest :src1 src1 :val val})

(defn node-label [name] 
	@{:type :label :name name})
(defn node-jump [op src1 src2 label] 
	@{:type :jump :op op :src1 src1 :src2 src2 :label label})
(defn node-loop [cond-reg body] 
	@{:type :loop :cond cond-reg :body body})

(var input-buffer "")
(var pos 0)

(defn peek-c [] 
	(get input-buffer pos))
(defn next-c [] 
	(let [c (peek-c)] 
		(++ pos) 
		c))
(defn is-space [c] 
	(and c (<= c 32)))
(defn is-digit [c] 
	(and c (>= c (chr "0")) (<= c (chr "9"))))

(defn skip-ws []
	(while (let [c (peek-c)] (and c (or (is-space c) (= c (chr "#")))))
		(if (= (peek-c) (chr "#"))
			(while (let [c (next-c)] (and c (not= c (chr "\n")))))
			(next-c))))

(defn lex-token []
	(skip-ws)
	(let [c (peek-c)]
		(cond
			(not c) 
			nil
			(= c (chr "(")) 
			(do 
				(next-c) 
				@{:type :lparen :val "("})
			(= c (chr ")")) 
			(do 
				(next-c) 
				@{:type :rparen :val ")"})
			(let [start pos]
				(while (let [curr (peek-c)]
					(and curr (not (is-space curr))
						(not= curr (chr "(")) (not= curr (chr ")"))))
					(next-c))
				(let [v (string/slice input-buffer start pos)]
					(cond
						(string/has-prefix? "$" v) 
						@{:type :reg :val v}
						(string/has-prefix? ":" v) 
						@{:type :label :val (string/slice v 1)}
						(or (is-digit (v 0)) (and (= (v 0) (chr "-")) (> (length v) 1) (is-digit (v 1)))) 
						@{:type :num :val v}
						@{:type :ident :val v}))))))

(var current-token nil)
(defn advance [] 
	(set current-token (lex-token)))
(defn match-tok [t] 
	(if (and current-token (= (current-token :type) t)) 
		current-token 
		nil))

(defn expect [t &opt msg]
	(let [tok current-token]
		(if (and tok (= (tok :type) t))
			(do (advance) tok)
			(error-exit (string "Expected " t " but got " (if tok (tok :type) "EOF") 
				" with value: " (if tok (tok :val) "nil")) 
				(or msg "")))))

(defn expect-val [t &opt msg] 
	((expect t msg) :val))

(defn parse-instruction []
	(expect :lparen "instruction start")
	(let [op (expect-val :ident "instruction opcode")]
		(let [inst (case op
			"add"    (node-arith :add (expect-val :reg) (expect-val :reg) (expect-val :reg))
			"sub"    (node-arith :sub (expect-val :reg) (expect-val :reg) (expect-val :reg))
			"xor"    (node-arith :xor (expect-val :reg) (expect-val :reg) (expect-val :reg))
			"add-im" (node-arith-im :add-im (expect-val :reg) (expect-val :reg) (expect-val :num))
			"sub-im" (node-arith-im :sub-im (expect-val :reg) (expect-val :reg) (expect-val :num))
			"xor-im" (node-arith-im :xor-im (expect-val :reg) (expect-val :reg) (expect-val :num))

			"alloc"     (node-alloc (expect-val :reg) (expect-val :reg))
			"alloc-im"  (node-alloc-im (expect-val :reg) (expect-val :num))
			
			"store-i64" (node-store-i64 (expect-val :reg) (expect-val :reg) (expect-val :num))
			"store-i32" (node-store-i32 (expect-val :reg) (expect-val :reg) (expect-val :num))
			"store-i16" (node-store-i16 (expect-val :reg) (expect-val :reg) (expect-val :num))
			"store-i8"  (node-store-i8  (expect-val :reg) (expect-val :reg) (expect-val :num))
			
			"load-i64"  (node-load-i64  (expect-val :reg) (expect-val :reg) (expect-val :num))
			"load-i32"  (node-load-i32  (expect-val :reg) (expect-val :reg) (expect-val :num))
			"load-i16"  (node-load-i16  (expect-val :reg) (expect-val :reg) (expect-val :num))
			"load-i8"   (node-load-i8   (expect-val :reg) (expect-val :reg) (expect-val :num))
			
			"set-im" (node-set-im (expect-val :reg) (expect-val :num))
			"set"    (node-set (expect-val :reg) (expect-val :reg))

			"ret"    (node-ret (expect-val :reg))
			"call"   (let [dest (expect-val :reg)]
				(let [func (expect-val :ident)]
					(let [args @[]]
						(array/push args (expect-val :reg "call requires at least one register argument"))
						(while (not (match-tok :rparen))
							(array/push args (expect-val :reg "call arguments must ONLY be registers")))
						(expect :rparen)
						(node-call dest func args))))
			"loop"   (let [cond-reg (expect-val :reg)]
				(let [body @[]]
					(array/push body (parse-instruction))
					(while (not (match-tok :rparen))
						(array/push body (parse-instruction)))
					(expect :rparen)
					(node-loop cond-reg body)))
			"jump"         (node-jump :jmp nil nil (expect-val :label))
			"jump-zero"    (node-jump :jz  (expect-val :reg) nil (expect-val :label))
			"jump-eq"      (node-jump :jeq (expect-val :reg) (expect-val :reg) (expect-val :label))
			"jump-not-eq"  (node-jump :jne (expect-val :reg) (expect-val :reg) (expect-val :label))

			(error-exit "Unknown instruction opcode" op))]
			(if (or (= op "call") (= op "loop"))
				inst
				(do
					(expect :rparen "instruction end")
					inst)))))

(defn parse-function []
	(expect :lparen "function start")
	(let [op (expect-val :ident "expected fun")]
		(if (not= op "fun") 
			(error-exit "Expected 'fun' keyword"))
		(let [name (expect-val :ident "function name")]
			(let [params @[]]
				(let [body @[]]
					(expect :lparen "parameter list start")
					(while (not (match-tok :rparen))
						(array/push params (expect-val :reg "parameter must be a register")))
					(expect :rparen "parameter list end")
					(while (not (match-tok :rparen))
						(if-let [l (match-tok :label)]
							(do (advance) (array/push body (node-label (l :val))))
							(array/push body (parse-instruction))))
					(expect :rparen "function end")
					(node-func name params body))))))

(defn parse-program []
	(advance)
	(let [funcs @[]]
		(while current-token
			(array/push funcs (parse-function)))
		(node-program funcs)))

(defn get-sym [name &opt create-if-not-present]
	(if-let [s (get sym-table name)] 
		s
		(if create-if-not-present
			(let [new-s @{:name name :reg-idx (length sym-table)}]
				(put sym-table name new-s)
				new-s))))

(defn rv-r [v]
	(def sym (get-sym v))
	(def idx (sym :reg-idx))
	(if (> idx 11) (error-exit "Only 11 S-Registers, spill not yet implemented." "on riscv64"))
	(string "s" (+ 1 idx)))

(defn arm-r [v]
	(def sym (get-sym v))
	(def idx (sym :reg-idx))
	(if (> idx 11) (error-exit "Only 11 Callee-Registers, spill not yet implemented." "on aarch64"))
	(string "x" (+ 19 idx)))

(defn find-all-used-regs [n]
	(case (n :type)
		:function (do
			(each p (n :params) (get-sym p true))
			(each inst (n :body) (find-all-used-regs inst)))
		:set-im (get-sym (n :dest) true)
		:set    (do (get-sym (n :dest) true) (get-sym (n :src) true))

		:alloc-im (get-sym (n :dest) true)
		:alloc    (do (get-sym (n :dest) true) (get-sym (n :size-reg) true))
		
		:store (do (get-sym (n :src) true) (get-sym (n :loc) true))
		:load  (do (get-sym (n :dest) true) (get-sym (n :loc) true))

		:call   (do (get-sym (n :dest) true) (each a (n :args) (get-sym a true)))

		:add    (do (get-sym (n :dest) true) (get-sym (n :src1) true) (get-sym (n :src2) true))
		:sub    (do (get-sym (n :dest) true) (get-sym (n :src1) true) (get-sym (n :src2) true))
		:xor    (do (get-sym (n :dest) true) (get-sym (n :src1) true) (get-sym (n :src2) true))
		:add-im (do (get-sym (n :dest) true) (get-sym (n :src1) true))
		:sub-im (do (get-sym (n :dest) true) (get-sym (n :src1) true))
		:xor-im (do (get-sym (n :dest) true) (get-sym (n :src1) true))

		:loop   (do (get-sym (n :cond) true) (each inst (n :body) (find-all-used-regs inst)))
		:jump (do 
			(if (n :src1) (get-sym (n :src1) true))
			(if (n :src2) (get-sym (n :src2) true)))
		:label nil
		:ret    (get-sym (n :val) true)))

(defn printi [& args]
	(print "  " (string/join (map string args) " ")))

(defn gen-rv [n]
	(case (n :type)
		:program (each f (n :functions) (gen-rv f))
		:function (do 
			(set sym-table @{})
			(set current-func-name (n :name))
			(find-all-used-regs n)
			(print "\n.global " (n :name) "\n" (n :name) ":")
			(printi "addi sp, sp, -112\n  sd ra, 104(sp)")
			(printi "sd s0, 96(sp)")
			(printi "mv s0, sp")
			(for i 1 12 (print "  sd s" i ", " (* (- i 1) 8) "(sp)"))
			(let [ps (n :params)] 
				(for j 0 (length ps) (print "  mv " (rv-r (ps j)) ", a" j)))
			(if (= (n :name) "main") (print "  li a0, 0"))
			(each inst (n :body) (gen-rv inst)))

		:alloc-im (do
			(def raw-size (scan-number (n :size)))
			(def aligned (band (+ raw-size 15) (bnot 15)))
			(printi "addi sp, sp, -" aligned)
			(printi "mv " (rv-r (n :dest)) ", sp"))

		:alloc (do
			(def r (rv-r (n :size-reg)))
			(def d (rv-r (n :dest)))
			# Align the register value to 16 bytes
			(printi "addi " r ", " r ", 15")
			(printi "andi " r ", " r ", -16")
			(printi "sub sp, sp, " r)
			(printi "mv " d ", sp"))
	
		:store (do
			(def op (case (n :size) :i8 "sb" :i16 "sh" :i32 "sw" :i64 "sd"))
			(printi op " " (rv-r (n :src)) ", " (n :offset) "(" (rv-r (n :loc)) ")"))

		:load (do
			(def op (case (n :size) :i8 "lb" :i16 "lh" :i32 "lw" :i64 "ld"))
			(printi op " " (rv-r (n :dest)) ", " (n :offset) "(" (rv-r (n :loc)) ")"))
		
		:set-im (printi "li" (rv-r (n :dest)) ", " (n :val))
		:set (printi "mv" (rv-r (n :dest)) "," (rv-r (n :src)))

		:add (printi "add" (rv-r (n :dest)) "," (rv-r (n :src1)) "," (rv-r (n :src2)))
		:sub (printi "sub" (rv-r (n :dest)) "," (rv-r (n :src1)) "," (rv-r (n :src2)))
		:xor (printi "xor" (rv-r (n :dest)) "," (rv-r (n :src1)) "," (rv-r (n :src2)))
		:add-im (printi "addi" (rv-r (n :dest)) "," (rv-r (n :src1)) "," (n :val))
		:sub-im (printi "addi" (rv-r (n :dest)) "," (rv-r (n :src1)) "," (string "-" (n :val)))
		:xor-im (printi "xori" (rv-r (n :dest)) "," (rv-r (n :src1)) "," (n :val))

		:loop (let [id (++ global-timer)]
			(print "L" id "s:")
			(printi "mv a0," (rv-r (n :cond)))
			(printi "beqz a0" "," (string "L" id "e"))
			(each inst (n :body) (gen-rv inst))
			(print "  j L" id "s\nL" id "e:"))
		:call (do
			(for i 0 (length (n :args))
				(printi "mv" (string "a" i ",") (rv-r ((n :args) i))))
			(printi "call" (n :func))
			(printi "mv" (rv-r (n :dest)) ", a0"))
		:label (do 
			(printi "unimp")
			(print current-func-name "_" (n :name) ":"))

		:jump (let [s1 (if (n :src1) (rv-r (n :src1)))
			s2 (if (n :src2) (rv-r (n :src2)))
			lbl (string current-func-name "_" (n :label))]
			(case (n :op)
				:jmp (printi "j" lbl)
				:jz  (printi "beqz" s1 "," lbl)
				:jeq (printi "beq" s1 "," s2 ", " lbl)
				:jne (printi "bne" s1 "," s2 ", " lbl)))
		:ret (do 
			(printi "mv a0," (rv-r (n :val)))
			(printi "mv sp, s0") # Destroy all variables at end of scope (function)
			(for i 1 12 
					(printi "ld" (string "s" i ",") (string (* (- i 1) 8) "(sp)")))
			(printi "ld s0, 96(sp)")
			(printi "ld ra, 104(sp)\n  addi sp, sp, 112\n  ret"))))

(defn gen-arm [n]
	(case (n :type)
		:program  (each f (n :functions) (gen-arm f))
		:function (do
			(set sym-table @{})
			(set current-func-name (n :name))
			(find-all-used-regs n)
			(print ".global _" (n :name))
			(print ".p2align 2")
			(print "_" (n :name) ":")
			(print "  stp x29, x30, [sp, #-112]!\n  mov x29, sp")
			(for i 0 5 (print "  stp x" (+ 19 (* i 2)) ", x" (+ 20 (* i 2)) ", [sp, " (+ 16 (* i 16)) "]"))
			(let [ps (n :params)] 
				(for j 0 (length ps) (print "  mov " (arm-r (ps j)) ", x" j)))
			(if (= (n :name) "main") (printi "mov w0, #0"))
			(each inst (n :body) (gen-arm inst)))
		
		:alloc-im (do
			(def raw-size (scan-number (n :size)))
			(def aligned (band (+ raw-size 15) (bnot 15)))
			(print "  sub sp, sp, #" aligned)
			(print "  mov " (arm-r (n :dest)) ", sp"))

		:alloc (do
			(def r (arm-r (n :size-reg)))
			(def d (arm-r (n :dest)))
			(print "  add " r ", " r ", #15")
			(print "  and " r ", " r ", #0xfffffffffffffff0")
			(print "  sub sp, sp, " r)
			(print "  mov " d ", sp"))

		:store (do
			(def reg (arm-r (n :src)))
			(def val-reg (if (= (n :size) :i64) reg (string/replace "x" "w" reg)))
			(def op (case (n :size) :i8 "strb" :i16 "strh" :i32 "str" :i64 "str"))
			(print "  " op " " val-reg ", [" (arm-r (n :loc)) ", #" (n :offset) "]"))

		:load (do
			(def reg (arm-r (n :dest)))
			(def val-reg (if (= (n :size) :i64) reg (string/replace "x" "w" reg)))
			(def op (case (n :size) :i8 "ldrsb" :i16 "ldrsh" :i32 "ldr" :i64 "ldr"))
			(print "  " op " " val-reg ", [" (arm-r (n :loc)) ", #" (n :offset) "]"))
		
		:set-im   (print "  mov " (arm-r (n :dest)) ", #" (n :val))
		:set      (printi "mov" (arm-r (n :dest)) "," (arm-r (n :src)))

		:add      (printi "add" (arm-r (n :dest)) "," (arm-r (n :src1)) "," (arm-r (n :src2)))
		:sub      (printi "sub" (arm-r (n :dest)) ", " (arm-r (n :src1)) ", " (arm-r (n :src2)))
		:xor      (printi "eor" (arm-r (n :dest)) "," (arm-r (n :src1)) "," (arm-r (n :src2)))
		:add-im   (print "  add " (arm-r (n :dest)) ", " (arm-r (n :src1)) ", #" (n :val))
		:sub-im   (print "  sub " (arm-r (n :dest)) ", " (arm-r (n :src1)) ", #" (n :val))
		:xor-im   (print "  eor " (arm-r (n :dest)) ", " (arm-r (n :src1)) ", #" (n :val))

		:loop     (let [id (++ global-timer)]
			(print "L" id "s:")
			(print "  mov x0, " (arm-r (n :cond)))
			(print "  cbz x0, L" id "e")
			(each inst (n :body) (gen-arm inst))
			(print "  b L" id "s\nL" id "e:"))
		:call     (do
			(for i 0 (length (n :args))
				(print "  mov x" i ", " (arm-r ((n :args) i))))
			(print "  bl _" (n :func) "\n  mov " (arm-r (n :dest)) ", x0"))
		:label (do 
			(printi "brk #0")
			(print "_" current-func-name "_" (n :name) ":"))

		:jump (let [s1 (if (n :src1) (arm-r (n :src1)))
			s2 (if (n :src2) (arm-r (n :src2)))
			lbl (string "_" current-func-name "_" (n :label))]
			(case (n :op)
				:jmp (print "  b " lbl)
				:jz  (print "  cbz " s1 ", " lbl)
				:jeq (do (print "  cmp " s1 ", " s2)
						 (print "  b.eq " lbl))
				:jne (do (print "  cmp " s1 ", " s2)
				 (print "  b.ne " lbl))))
		:ret      (do 
			(print "  mov x0, " (arm-r (n :val)))
			(print "  mov sp, x29") # Same on aarch64
			(for i 0 5 (print "  ldp x" (+ 19 (* i 2)) ", x" (+ 20 (* i 2)) ", [sp, " (+ 16 (* i 16)) "]"))
			(print "  ldp x29, x30, [sp], #112\n  ret"))))

(defn main [& args]
	(when (< (length args) 4)
		(error-exit "Usage: janet ginkoc.janet <filename> --target <riscv64|aarch64>"))
	
	(def filename (args 1))
	(def target-flag (args 2))
	(def target (args 3))

	(unless (= target-flag "--target")
		(error-exit "Expected --target flag"))

	(set input-buffer (slurp filename))
	(def n (parse-program))
	(case target 
		"aarch64" (gen-arm n) 
		"riscv64" (gen-rv n)
		(error-exit "Bad target" target)))