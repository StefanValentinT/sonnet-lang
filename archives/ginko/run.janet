(import ./common :as c)

(defn main [& args]
	(def verbose (not (nil? (find |(or (= $ "--verbose") (= $ "-v")) args))))
	(def filename (find |(not (string/has-prefix? "-" $)) (slice args 1)))
	
	(unless filename 
		(do 
			(eprint "Usage: janet run.janet <file>") 
			(os/exit 1)))

	(each arch (keys c/arch-configs)
		(print "On " arch ":")
		(let [arch-data (get c/arch-configs arch)
					compile-fn (get arch-data :compile-cmd)
					run-cmd-gen (get arch-data :run-cmd)]
			(if-let [bin (c/compile-to-bin filename arch compile-fn verbose false)]
				(do 
					(let [output (c/run-and-capture (run-cmd-gen bin) verbose)]
						(print output))
					(os/rm bin)
					)
				(print "Compilation failed!")))
		(print "")))