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

lisp:func([copy, FromIRI, Source, ToIRI, Sink], true) :- !,
  copy_resource(FromIRI, ToIRI, rdf(Source), rdf(Sink), []).

lisp:func([delete, IRI, Sink], true) :- !,
  delete_resource(IRI, rdf(Sink)).

lisp:func([send, IRI, URI], true) :- !,
  ignore(post_resource(IRI, URI, [])).

lisp:func([send, IRI, URI, Graph], true) :- !,
  ignore(post_resource(IRI, URI, [graph(Graph)])).
