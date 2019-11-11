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

:- multifile lisp:func/2.

lisp:func([copy, FromIRI, Source, ToIRI, Sink], Result) :- !,
  lisp:result(oslc:copy_resource(FromIRI, ToIRI, rdf(Source), rdf(Sink)), Result).

lisp:func([copy, FromIRI, Source, ToIRI, Sink, Options], Result) :- !,
  lisp:result(oslc:copy_resource(FromIRI, ToIRI, rdf(Source), rdf(Sink), Options), Result).

lisp:func([delete, IRI, Sink], Result) :- !,
  lisp:result(oslc:delete_resource(IRI, rdf(Sink)), Result).

lisp:func([delete, IRI, Sink, Options], Result) :- !,
  lisp:result(oslc:delete_resource(IRI, rdf(Sink), Options), Result).

lisp:func([send, IRI, URI], Result) :- !,
  lisp:result(oslc_client:post_resource(IRI, URI, []), Result).

lisp:func([send, IRI, URI, Options], Result) :- !,
  lisp:result(oslc_client:post_resource(IRI, URI, Options), Result).

lisp:func([send_graph, Graph, URI], Result) :- !,
  lisp:result(oslc_client:post_graph(Graph, URI, []), Result).

lisp:func([send_graph, Graph, URI, Options], Result) :- !,
  lisp:result(oslc_client:post_graph(Graph, URI, Options), Result).
