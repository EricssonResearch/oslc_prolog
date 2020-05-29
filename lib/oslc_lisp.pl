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

:- multifile lisp:funct/3.

call_and_catch(Goal, R) :-
  catch_with_backtrace((
      Goal,
      R = true
    ),
    E,
    ( print_message(error, E),
      R = false
    )).

lisp:funct(send, [ResourceIRI, PostURI], R) :- !,
  debug(lisp(oslc), 'POSTing resource [~w] to [~w]', [ResourceIRI, PostURI]),
  call_and_catch(oslc_client:post_resource(ResourceIRI, PostURI, []), R).

lisp:funct(send, [ResourceIRI, PostURI, Options], R) :- !,
  debug(lisp(oslc), 'POSTing resource [~w] to [~w]', [ResourceIRI, PostURI]),
  call_and_catch(oslc_client:post_resource(ResourceIRI, PostURI, Options), R).

lisp:funct(send_graph, [GraphIRI, PostURI], R) :- !,
  debug(lisp(oslc), 'POSTing graph [~w] to [~w]', [GraphIRI, PostURI]),
  call_and_catch(oslc_client:post_graph(GraphIRI, PostURI, []), R).

lisp:funct(send_graph, [GraphIRI, PostURI, Options], R) :- !,
  debug(lisp(oslc), 'POSTing graph [~w] to [~w]', [GraphIRI, PostURI]),
  call_and_catch(oslc_client:post_graph(GraphIRI, PostURI, Options), R).
