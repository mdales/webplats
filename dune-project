(lang dune 3.16)

(name webplats)

(generate_opam_files true)

(source
 (github mdales/webplats))

(authors "Michael Dales <michael@digitalflapjack.com>")

(maintainers "Michael Dales <michael@digitalflapjack.com>")

(license EUPL)

(documentation https://url/to/documentation)

(package
 (name webplats)
 (synopsis "An opinionated dynamic version of a static site generator.")
 (description "An opinionated dynamic version of a static site generator, which takes markdown+frontmatter files and renders them on demand.")
 (depends ocaml
  dune
  fpath
  dream
  cmarkit
  hilite
  astring
  metadata
  yaml
  uri
  camlimages
  pcre2)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
