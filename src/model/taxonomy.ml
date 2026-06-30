type t = { tag : string; title : string; sections : Section.t list }

let v tag title sections = { tag; title; sections }
let title t = t.title
let tag t = t.tag
let sections t = t.sections
let uri t = Uri.of_string ("/" ^ t.tag ^ "/")
