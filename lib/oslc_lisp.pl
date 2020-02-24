/*
Copyright 2019-2020 Ericsson AB

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

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), [rdf_is_resource/1]).
:- use_module(library(oslc)).
:- use_module(library(oslc_client)).

:- multifile lisp:funct/3.

lisp:funct(copy, [FromIRI, Source, ToIRI, Sink], Result) :- !,
  lisp:funct(copy, [FromIRI, Source, ToIRI, Sink, []], Result).

lisp:funct(copy, [FromIRI, Source, ToIRI, Sink, Options], true) :- !,
  ( rdf_is_literal(FromIRI)
  -> ( rdf_is_resource(ToIRI)
     -> forall(
          rdf(S, P, ToIRI, Sink), (
            ( memberchk(merge, Options)
            -> true
            ; rdf_retractall(S, P, ToIRI, Sink),
              ( rdf_is_bnode(ToIRI)
              -> oslc:delete_resource(ToIRI, rdf(Sink))
              ; true
              )
            ),
            rdf_assert(S, P, FromIRI, Sink)
          )
        )
     )
  ; oslc:copy_resource(FromIRI, ToIRI, rdf(Source), rdf(Sink), Options)
  ).

lisp:funct(delete, [IRI, Sink], true) :- !,
  oslc:delete_resource(IRI, rdf(Sink)).

lisp:funct(delete, [IRI, Sink, Options], true) :- !,
  oslc:delete_resource(IRI, rdf(Sink), Options).

lisp:funct(send, [IRI, URI], true) :- !,
  oslc_client:post_resource(IRI, URI, []).

lisp:funct(send, [IRI, URI, Options], true) :- !,
  oslc_client:post_resource(IRI, URI, Options).

lisp:funct(send_graph, [Graph, URI], true) :- !,
  oslc_client:post_graph(Graph, URI, []).

lisp:funct(send_graph, [Graph, URI, Options], true) :- !,
  oslc_client:post_graph(Graph, URI, Options).
