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

:- module(oslc_client, [post_graph/3,
                        post_resource/3
                       ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(http/http_client)).
:- use_module(library(oslc)).
:- use_module(library(oslc_rdf)).

:- rdf_meta post_resource(r, -),
            post_resource(r, -, -).

:- predicate_options(post_graph/3, 3, [content_type(any)]).
:- predicate_options(post_resource/3, 3, [content_type(any), graph(atom)]).

post_graph(Graph, URI, Options) :-
  option(content_type(ContentType), Options, text/turtle),
  rdf_graph_to_atom(Graph, Atom, ContentType),
  http_post(URI, atom(ContentType, Atom), _, [status_code(StatusCode),
                                              request_header('Accept'='text/turtle')
                                             ]),
  StatusCode == 200.

rdf_graph_to_atom(Graph, Atom, ContentType) :-
  must_be(atom, Graph),
  oslc_dispatch:serializer(ContentType, Serializer),
  with_output_to(
    atom(Atom),
    oslc_dispatch:serialize_response(stream(current_output), Graph, Serializer)
  ) .

post_resource(IRI, URI, Options) :-
  option(graph(Graph), Options, _),
  make_temp_graph(Tmp),
  call_cleanup((
      ( var(Graph)
      -> copy_resource(IRI, IRI, rdf, rdf(Tmp), Options)
      ; copy_resource(IRI, IRI, rdf(Graph), rdf(Tmp), Options)
      ),
      post_graph(Tmp, URI, Options)
    ),
    delete_temp_graph(Tmp)
  ).
