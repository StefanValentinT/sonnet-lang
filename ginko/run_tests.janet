(import ./common :as c)

(defn color-red [txt] (string "\e[31m" txt "\e[0m"))
(defn color-green [txt] (string "\e[32m" txt "\e[0m"))

(defn parse-tests-toml [filepath]
	(def tests @{})
	(var current-test-name nil)
	(if-let [f (os/open filepath :r)]
		(defer (:close f)
			(each line (string/split "\n" (:read f :all))
				(let [t (string/trim line)]
					(cond
						(or (empty? t) (string/has-prefix? "#" t)) nil
						(string/has-prefix? "[" t)
						(let [name (string/trim (string/slice t 1 -2))]
							(set current-test-name name)
							(put tests name @{}))
						current-test-name
						(let [parts (string/split "=" t 0 2)]
							(when (= (length parts) 2)
								(let [k (keyword (string/trim (parts 0)))
											v (string/trim (string/replace-all "\"" "" (string/trim (parts 1))))]
									(put (get tests current-test-name) k v))))))))
		(do (eprint "Error: Could not open " filepath) (os/exit 1)))
	tests)

(defn main [& args]
	(def verbose (not (nil? (find |(or (= $ "--verbose") (= $ "-v")) args))))
	(def tests (parse-tests-toml "tests/tests.toml"))
	
	(def failures @[])

	(eachp [test-name data] tests
		(print "Testing " test-name ":")
		(def filename (get data :file))
		
		(if (or (nil? filename) (= (get data :skip) "true"))
			(print "  Skipped")
			(do
				(def filepath (string "tests/" filename))
				(def expected (get data :expect))

				(each arch (keys c/arch-configs)
					(def arch-data (get c/arch-configs arch))
					(prin "  [" arch "] ... ")
					
					(if-let [bin (c/compile-to-bin filepath arch (arch-data :compile-cmd) verbose)]
						(let [output (c/run-and-capture ((arch-data :run-cmd) bin) verbose)]
							(os/rm bin)
							(if (= output expected)
								(print (color-green "PASSED"))
								(do 
									(print (color-red "FAILED") " (Expected '" expected "', got '" output "')")
									(array/push failures (string test-name " (" arch ")")))))
						(do 
							(print (color-red "COMPILE ERROR"))
							(array/push failures (string test-name " (" arch " - compile error)"))))))))

	(print (string/repeat "-" 34))
	(if (empty? failures)
		(print (color-green "All tests passed. Congratulations!"))
		(do
			(print "These tests didn't pass:")
			(each f failures
				(print "  - " (color-red f))))))