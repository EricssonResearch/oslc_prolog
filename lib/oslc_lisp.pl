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

:- use_module(library(oslc_client)).

:- multifile lisp:func/3.

lisp:func(send, [ResourceIRI, PostURI], true) :- !,
  oslc_client:post_resource(ResourceIRI, PostURI, []).

lisp:func(send, [ResourceIRI, PostURI, Options], true) :- !,
  map_content_type(Options, Options1),
  oslc_client:post_resource(ResourceIRI, PostURI, Options1).

lisp:func(send_graph, [GraphIRI, PostURI], true) :- !,
  oslc_client:post_graph(GraphIRI, PostURI, []).

lisp:func(send_graph, [GraphIRI, PostURI, Options], true) :- !,
  map_content_type(Options, Options1),
  oslc_client:post_graph(GraphIRI, PostURI, Options1).

map_content_type(Options, Options1) :-
  option(content_type(ContentType), Options),
  ( var(ContentType)
  -> Options1 = Options
  ; sub_string(ContentType, Before, _, After, "/"),
    sub_atom(ContentType, 0, Before, _, Type),
    sub_atom(ContentType, _, After, 0, Subtype),
    AtomContentType = Type/Subtype,
    merge_options([content_type(AtomContentType)], Options, Options1)
  ).