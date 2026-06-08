#import "spec_template.typ": *

#show: spec.with(
	title: "Specification of the Sonnet Programming Language",
	version: "0.1.0",
	author: "StefanValentinT",
	abstract: include "abstract.typ",
	bib: "refs.bib"
)

#include "introduction.typ"

#include "definitions.typ"

#include "syntax.typ"
