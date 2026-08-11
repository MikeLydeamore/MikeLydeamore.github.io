#set par(justify: false, leading: 0.58em)

#show heading.where(level: 1): it => block(
  above: 1.4em,
  below: 0.75em,
  text(size: 13pt, weight: "bold", upper(it.body)),
)

#show heading.where(level: 2): it => block(
  above: 1.15em,
  below: 0.6em,
  text(size: 11pt, weight: "bold", upper(it.body)),
)

#let cv-entry(body, dates) = block(
  breakable: false,
  below: 0.9em,
  grid(
    columns: (1fr, auto),
    column-gutter: 1em,
    {
      set par(leading: 0.72em)
      body
    },
    align(right, text(style: "italic", dates)),
  ),
)
