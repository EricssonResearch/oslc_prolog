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
:- use_module(library(http/http_header)).
:- use_module(library(oslc)).
:- use_module(library(oslc_rdf)).

:- rdf_meta post_resource(r, -, -).

:- predicate_options(post_graph/3, 3, [content_type(any), response_graph(atom)]).
:- predicate_options(post_resource/3, 3, [graph(atom), response_graph(atom)]).

post_graph(Graph, URI, Options) :-
  debug(oslc_client, 'POSTing graph [~w] to [~w]', [Graph, URI]),
  % set defaults unless overridden by the user
  merge_options(Options, [ status_code(200),
                  request_header('Accept'='text/turtle,application/rdf+xml,application/n-triples')
                ], OptionsD),
  option(content_type(ContentType), Options, text/turtle),
  oslc_dispatch:serializer(ContentType, Serializer),
  term_to_atom(ContentType, PostContentType),
  % rewrite content_type/1 option
  merge_options([content_type(PostContentType)], OptionsD, Options1),
  setup_call_cleanup(
    new_memory_file(File), (
      open_memory_file(File, write, Out),
      oslc_dispatch:serialize_response(stream(Out), Graph, Serializer),
      close(Out),
      ( option(response_graph(ResponseGraph), Options)
      -> setup_call_cleanup(
           new_memory_file(ResponseFile), (
             open_memory_file(ResponseFile, write, ResponseStream, [encoding(octet)]),
             merge_options([headers(ResponseHeaders), to(stream(ResponseStream))], Options1, Options2),
             http_post(URI, memory_file(ContentType, File), _, Options2),
             close(ResponseStream),
             open_memory_file(ResponseFile, read, Stream, [encoding(utf8)]),
             ignore(read_response_body(ResponseHeaders, Stream, ResponseGraph)),
             close(Stream)
           ),
           free_memory_file(ResponseFile)
         )
      ; http_post(URI, memory_file(ContentType, File), _, Options1)
      )
    ),
    free_memory_file(File)
  ).

post_resource(IRI, URI, Options) :-
  debug(oslc_client, 'POSTing resource [~w] to [~w]', [IRI, URI]),
  option(graph(Graph), Options, _),
  setup_call_cleanup(
    make_temp_graph(Tmp),
    (
      ( var(Graph)
      -> copy_resource(IRI, IRI, rdf, rdf(Tmp), Options)
      ; copy_resource(IRI, IRI, rdf(Graph), rdf(Tmp), Options)
      ),
      post_graph(Tmp, URI, Options)
    ),
    delete_temp_graph(Tmp)
  ).

read_response_body(Fields, Stream, GraphOut) :-
  memberchk(content_length(ContentLength), Fields), % if there is response, try to parse it
  ContentLength > 0,
  memberchk(content_type(InContentType), Fields),
  http_parse_header_value(content_type, InContentType, media(SerializerType, _)),
  oslc_dispatch:serializer(SerializerType, Format),
  catch(
    rdf_load(stream(Stream), [graph(GraphOut), format(Format), silent(true), on_error(error), cache(false)]),
    error(E, stream(Stream, Line, Column, _)),
    (
      message_to_string(error(E, _), S),
      print_message(error, response_parsing_error(Line, Column, S))
    )
  ).

:- multifile prolog:message/3.

prolog:message(response_parsing_error(Line, Column, S)) -->
  [ 'Response parsing error (line ~w, column ~w): ~w'-[Line, Column, S] ].
