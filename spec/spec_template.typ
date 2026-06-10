#let today = datetime.today()

#let note(c) = [
	#block(
		stroke: (left: 1pt + silver),
		inset: (left: 8pt),
		outset: (y: 2pt),
		[#smallcaps("Note") #c]
	)
]

#let example(c) = [
	#block(
		stroke: (left: 1pt + silver),
		inset: (left: 8pt),
		outset: (y: 2pt),
		[#smallcaps("Example") #c]
	)
]

#let prod(left, ..right) = [
	#v(0.25em)
	#left $->$ 

	#block(inset: (left: 1em))[
		#right.pos().map(item => [#item]).join(" | ")
	]
	#v(0.25em)
]

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
	show raw.where(block: true): it => block(
		width: 100%,
		stroke: 1pt + silver,
		inset: 10pt,
		layout(size => {
			set text(hyphenate: false)
			it
		})
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
			#text(size: 14pt)[Version #version dated
				#today.display("[day].[month].[year]")] \
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

	set underline(offset: 2pt)

	show heading.where(level: 1): set align(center)
	show heading.where(level: 1): set text(size: 13pt, weight: "bold")
	show heading.where(level: 1): smallcaps
	show heading.where(level: 1): set block(above: 1.5em, below: 1em)

	show heading.where(level: 2): set align(center)
	show heading.where(level: 2): set text(size: 13pt, weight: "medium")
	show heading.where(level: 2): set block(above: 1.2em, below: 0.8em)

	show heading.where(level: 3): it => {
		set block(spacing: 0em)
		text(size: 11pt, weight: "regular", style: "italic")[#underline[#it.body.]]
		h(0.5em)
	}

	show outline.entry.where(level: 1): set block(above: 1.0em)
	show outline.entry.where(level: 1): set text(weight: "bold")
	show outline.entry.where(level: 1): set outline.entry(fill: none)
	set outline.entry(fill: pad(left: 0.2em, right: 1em, repeat(gap: 0.50em, [.])))
	show outline.entry: it => link(
		it.element.location(),
		if it.prefix() == none {
			it.indented(none, it.inner())
		} else {
			it.indented(it.prefix() + h(0.4em), it.inner())
		},
	)

	outline(title: [Contents], depth: 2, indent: 1.0em)
	colbreak()

	body

	colbreak()
	bibliography(bib, title: "References", style: "ieee")

}