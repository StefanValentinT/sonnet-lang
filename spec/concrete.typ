#import "spec_template.typ": *

#set page(columns: 1)

#let include_skip(file, n: 0) = {
	let src = read(file)
		.split("\n")
		.slice(n)
		.join("\n")
	eval(src, mode: "markup")
}


#include_skip("concrete-semantics/ConcreteSemantics.lean", n: 3)