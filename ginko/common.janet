(def build-dir "build")

(def arch-configs
	{
	:riscv64
	{:compile-cmd (fn [s-file obj-out] ["riscv64-unknown-elf-gcc" "-static" s-file "ginko-runtime.c" "-o" obj-out])
		:run-cmd     (fn [obj-out] ["spike" "pk" obj-out])}
	:aarch64
	{:compile-cmd (fn [s-file obj-out] ["gcc" s-file "ginko-runtime.c" "-o" obj-out])
		:run-cmd     (fn [obj-out] [(string "./" obj-out)])}})

(defn run-and-capture [args &opt verbose]
	(let [err-handle (if verbose (dyn :stderr) nil)
		proc (os/spawn args :px {:out :pipe :err err-handle})
		out-stream (get proc :out)
		output (:read out-stream :all)]

		(os/proc-wait proc)
		(:close out-stream)
		(string/trim (or (and output (string output)) ""))))

(defn compile-to-bin [filepath arch-key compile-fn verbose]
	(unless (os/stat build-dir) (os/mkdir build-dir))
	(let [target-str (string arch-key)
		base-name (last (string/split "/" filepath))
		asm-path (string build-dir "/" base-name "_" target-str ".s")
		bin-out (string build-dir "/bin_" base-name "_" target-str)]
		
		(let [asm (run-and-capture ["janet" "ginkoc.janet" filepath "--target" target-str] verbose)]
			(when verbose (print "\nGenerated assembly:\n" asm))
			(if (and asm (not (empty? asm)))
				(do
					(spit asm-path (string asm "\n"))
					(let [res (os/execute (compile-fn asm-path bin-out) :px)]
						(os/rm asm-path)
						(if (not= res 0) 
							(do (eprint "Failure of the assembly compiler. Possibly because of wrong asmebly emitted from the compiler.") nil)
							bin-out)))
				(do 
					(eprint "Compiler error or empty assembly for " filepath) 
					nil)))))