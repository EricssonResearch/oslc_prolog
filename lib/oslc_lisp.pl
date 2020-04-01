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

:- multifile lisp:funct/3.

lisp:funct(send, [IRI, URI], true) :- !,
  oslc_client:post_resource(IRI, URI, []).

lisp:funct(send, [IRI, URI, Options], true) :- !,
  oslc_client:post_resource(IRI, URI, Options).

lisp:funct(send_graph, [Graph, URI], true) :- !,
  oslc_client:post_graph(Graph, URI, []).

lisp:funct(send_graph, [Graph, URI, Options], true) :- !,
  oslc_client:post_graph(Graph, URI, Options).
