# A [Forester](http://www.jonmsterling.com/jms-005P.xml) interpreter implementend in Elm

Currently, Forester is a [static site generator](https://en.wikipedia.org/wiki/Static_site_generator).
This repository is about exploring Forester as a hypermedia language in its own
right. 

The choice of using Elm for this might be puzzling for some. Amid the recent
buzz about hypermedia, why not use a hypermedia-oriented library such as
[HTMX](https://htmx.org) to enable richer interactivity in Forester? What about
[all these essays](https://htmx.org/essays/)? A JSON API for Forester?

I have in fact experimented with the hypermedia, and I still think it is a
viable path for building a richer, more reactive interface to Forester. In fact
it would be much simpler and faster than what I am doing here. However, the
base assumption that HTMX makes is that the hypermedia we use is HTML, which is
the limitation which I am trying to surpass. I am trying to understand
precisely the implications of an interactive UI to Forester. This is why I have
chosen Elm. While HTMX brings more browser APIs into the scope of HTML markup,
Elm abstracts these fully away APIs into the well known [Elm
Architecture](https://guide.elm-lang.org/architecture/). In a sense, I am
shallowly embedding a custom hypermedia client tailored to Forester inside of
the browser. Elm gives me control over all details. The Elm runtime handles the
entire interaction loop, whereas HTMX just wires up a small amount of
javascript into some custom tags.

Now about the title of this README: I have found that the natural structure
that this program is tending towards is an "interpreter" of 
[Forester's intermediate representation format](https://git.sr.ht/~jonsterling/ocaml-forester/tree/late-binding/item/lib/core/Xml_tree.ml).
Because we use the [repr library](https://mirage.github.io/repr/repr/Repr/index.html),
we get json representations for the ocaml values for free, which I can
parse in Elm to obtain a faithful representation of those types (Elm and OCaml
are both [ml-style languages](https://en.wikipedia.org/wiki/ML_(programming_language)))

Ultimately, the point of this exercise is not necessarily for it to become the
new way of interacting with Forester, but rather to inform a solid design for a
Forester hypermedia model.
