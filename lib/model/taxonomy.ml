type t = { title : string; sections : Section.t list }

let v title sections = { title; sections }
let title t = t.title
let sections t = t.sections
