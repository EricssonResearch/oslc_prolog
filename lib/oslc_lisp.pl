/*
Copyright 2019 Ericsson AB

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

:- module(oslc_lisp, []).

:- use_module(library(oslc)).
:- use_module(library(oslc_client)).

:- multifile lisp:func/3.

lisp:func(copy, [FromIRI, Source, ToIRI, Sink], Result) :- !,
  lisp:lit_to_atom(Source, SourceA),
  lisp:lit_to_atom(Sink, SinkA),
  lisp:result(oslc:copy_resource(FromIRI, ToIRI, rdf(SourceA), rdf(SinkA)), Result).

lisp:func(copy, [FromIRI, Source, ToIRI, Sink, Options], Result) :- !,
  lisp:lit_to_atom(Source, SourceA),
  lisp:lit_to_atom(Sink, SinkA),
  maplist(lisp:lit_to_term, Options, OptionsT),
  lisp:result(oslc:copy_resource(FromIRI, ToIRI, rdf(SourceA), rdf(SinkA), OptionsT), Result).

lisp:func(delete, [IRI, Sink], Result) :- !,
  lisp:lit_to_atom(Sink, SinkA),
  lisp:result(oslc:delete_resource(IRI, rdf(SinkA)), Result).

lisp:func(delete, [IRI, Sink, Options], Result) :- !,
  lisp:lit_to_atom(Sink, SinkA),
  maplist(lisp:lit_to_term, Options, OptionsT),
  lisp:result(oslc:delete_resource(IRI, rdf(SinkA), OptionsT), Result).

lisp:func(send, [IRI, URI], Result) :- !,
  lisp:lit_to_atom(URI, URIA),
  lisp:result(oslc_client:post_resource(IRI, URIA, []), Result).

lisp:func(send, [IRI, URI, Options], Result) :- !,
  lisp:lit_to_atom(URI, URIA),
  maplist(lisp:lit_to_term, Options, OptionsT),
  lisp:result(oslc_client:post_resource(IRI, URIA, OptionsT), Result).

lisp:func(send_graph, [Graph, URI], Result) :- !,
  lisp:lit_to_atom(URI, URIA),
  lisp:result(oslc_client:post_graph(Graph, URIA, []), Result).

lisp:func(send_graph, [Graph, URI, Options], Result) :- !,
  lisp:lit_to_atom(Graph, GraphA),
  lisp:lit_to_atom(URI, URIA),
  maplist(lisp:lit_to_term, Options, OptionsT),
  lisp:result(oslc_client:post_graph(GraphA, URIA, OptionsT), Result).
