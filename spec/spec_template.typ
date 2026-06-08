#let dotted = repeat(h(0.25em) + text(fill:gray, ".") + h(0.25em))

#let new(key, desc, display: none) = context {
	let loc = here()
	let definition-label = label("g-def-" + key)
	let reference-label = label("g-ref-" + key)
	
	let previous = query(selector(definition-label).before(loc))
	let Wort = if display != none { display } else { key }
	
	if previous.len() == 0 {
		[#link(definition-label)[#text(style: "italic")[#Wort]]#reference-label#metadata(desc)#definition-label]
	} else {
		link(reference-label, Wort)
	}
}

#let print-glossary(title: "Glossary") = {
	heading(level: 1, numbering: none, title)
	
	context {
		let entries = query(selector(metadata)).filter(e => {
			e.has("label") and str(e.label).starts-with("g-def-")
		})
		
		let unique-entries = entries.sorted(key: e => str(e.label))
		
		for entry in unique-entries {
			let key = str(entry.label).slice(6)
			let ref-label = label("g-ref-" + key)
			let page-num = str(entry.location().page())
			let formatted-key = upper(key.at(0)) + key.slice(1)
			par(first-line-indent: 0pt, hanging-indent: 1.5em, justify: true)[
			#text(size: 11pt, weight: "regular", fill: black, link(ref-label, formatted-key))
			#text(size: 11pt)[ · #entry.value]
			#box(width: 1fr)[#dotted]
			#text(size: 11pt, fill: black, link(ref-label)[#page-num])
			]
			v(0.8em)
		}
	}
}

#let spec(
	title: "",
	version: "",
	author: "",
	abstract: [],
	glossary-title: "Glossary",
	bib: "",
	body
) = {
	set document(title: title)
	set text(
		font: "New Computer Modern",
		size: 11pt
	)
	set par(
		justify: true,
		leading: 0.7em,
	)
	set list(marker: ([–], [-]))

	set page(
		header: context {
			let page-num = counter(page).get().first()
			if page-num > 1 {
				align(right + horizon, title)
			} else {
				none
			}
		},
		numbering: "1",
		columns: 2,
	)
	
	set heading(numbering: "1.")

	place(
		top + center,
		float: true,
		scope: "parent",
		clearance: 2em,
	)[
		#align(center)[
			#text(size: 20pt, weight: "bold")[#title] \
			#v(1em)
			#text(size: 14pt)[Version #version] \
			#v(1em)
			#grid(
				columns: (1fr),
				align(center)[#author],
			)
			#v(1em)
		]
		#set par(justify: false)
		*Abstract* \
		#abstract
	]

	show heading.where(level: 1): set align(center)
	show heading.where(level: 1): set text(size: 13pt, weight: "bold")
	show heading.where(level: 1): smallcaps
	show heading.where(level: 1): set block(above: 1.5em, below: 1em)

	show heading.where(level: 2): set align(center)
	show heading.where(level: 2): set text(size: 13pt, weight: "medium")
	show heading.where(level: 2): set block(above: 1.2em, below: 0.8em)

	show heading.where(level: 3): it => {
		set block(spacing: 0em)
		text(size: 11pt, weight: "regular", style: "italic")[#it.body.]
		h(0.5em)
	}

	show outline.entry: it => {
		link(it.element.location())[
			#it.indented(
				it.prefix(),
				[
					#it.body()
					#box(width: 1fr)[#dotted]
					#it.page()
				]
			)
		]
	}

	outline(title: [Contents], indent: 1.5em)
	colbreak()

	body

	pagebreak()
	print-glossary()
	bibliography(bib, title: "References", style: "ieee")

}