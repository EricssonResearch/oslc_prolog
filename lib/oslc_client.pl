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
  must_be(atom, Graph),
  option(content_type(ContentType), Options, text/turtle),
  oslc_dispatch:serializer(ContentType, Serializer),
  setup_call_cleanup(
    new_memory_file(File), (
      open_memory_file(File, write, Out),
      with_output_to(Out, oslc_dispatch:serialize_response(stream(current_output), Graph, Serializer)),
      close(Out),
      memory_file_to_atom(File, Codes),
      http_post(URI, atom(ContentType, Codes), _, [status_code(StatusCode),
                                                   request_header('Accept'='text/turtle,application/rdf+xml,application/n-triples')
                                                  ]),
      StatusCode == 200
    ),
    free_memory_file(File)
  ).

post_resource(IRI, URI, Options) :-
  option(graph(Graph), Options, _),
  setup_call_catcher_cleanup(
    make_temp_graph(Tmp),
    (
      ( var(Graph)
      -> copy_resource(IRI, IRI, rdf, rdf(Tmp), Options)
      ; copy_resource(IRI, IRI, rdf(Graph), rdf(Tmp), Options)
      ),
      post_graph(Tmp, URI, Options)
    ),
    _,
    delete_temp_graph(Tmp)
  ).
