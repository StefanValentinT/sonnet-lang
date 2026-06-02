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
	(unless (os/stat build-dir) (os/mkdir build-dir))
	
	(def tmp-out-path (string build-dir "/.capture_out.txt"))
	(def tmp-err-path (string build-dir "/.capture_err.txt"))
	(def f-out (file/open tmp-out-path :w))
	(def f-err (file/open tmp-err-path :w))

	(def exit-code
		(try
			(os/execute args :p {:out f-out :err (if verbose (dyn :stderr) f-err)})
			([err] 1)))

	(file/close f-out)
	(file/close f-err)

	(def output (try (slurp tmp-out-path) ([_] "")))

	(os/rm tmp-out-path)
	(os/rm tmp-err-path)

	(if (= exit-code 0)
		(string/trim output)
		""))

(defn compile-to-bin [filepath arch-key compile-fn verbose &opt delete-asm]
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
					
					(def tmp-gcc-err (string build-dir "/.gcc_err.txt"))
					(def f-gcc-err (file/open tmp-gcc-err :w))
					
					(def exit-code
						(try
							(os/execute (compile-fn asm-path bin-out) :p 
								{:out (if verbose (dyn :out) f-gcc-err)
								:err (if verbose (dyn :stderr) f-gcc-err)})
							([err] 1)))
							
					(file/close f-gcc-err)
					(os/rm tmp-gcc-err)

					(if delete-asm (os/rm asm-path))
					
					(if (= exit-code 0) bin-out nil))
				nil))))