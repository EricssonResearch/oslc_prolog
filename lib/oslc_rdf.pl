/*
Copyright 2017-2019 Ericsson AB

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

:- module(oslc_rdf, [ make_temp_graph/1,
                      pass_temp_graph/2,
                      delete_temp_graph/1,
                      clean_temp_graphs/0,
                      autodetect_resource_graph/2,
                      resource_sha1/3 ] ).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(library(oslc_dict2)).
:- use_module(library(oslc_types)).

:- rdf_meta autodetect_resource_graph(r, -),
            resource_sha1(r, -, -).

:- dynamic temp_graph/1.

:- thread_local temp_graph/1.

:- multifile oslc:marshal_property/5,
             oslc:unmarshal_property/5,
             oslc:delete_property/3.

% ------------ RDF SOURCE / SINK

oslc:marshal_property(IRI, PropertyDefinition, Value, Type, rdf(Graph)) :-
  must_be(atom, IRI),
  must_be(atom, PropertyDefinition),
  must_be(ground, Value),
  must_be(atom, Graph),
  ( nonvar(Type),
    is_literal_type(Type)
  -> rdf_assert(IRI, PropertyDefinition, Value^^Type, Graph)
  ; rdf_assert(IRI, PropertyDefinition, Value, Graph)
  ).

oslc:unmarshal_property(IRI, PropertyDefinition, Value, Type, rdf) :-
  rdf(IRI, PropertyDefinition, Object),
  unmarshal_type(Object, Value, Type).

oslc:unmarshal_property(IRI, PropertyDefinition, Value, Type, rdf(Graph)) :-
  must_be(atom, Graph),
  rdf(IRI, PropertyDefinition, Object, Graph),
  unmarshal_type(Object, Value, Type).

oslc:delete_property(IRI, PropertyDefinition, rdf) :-
  must_be(atom, IRI),
  rdf_retractall(IRI, PropertyDefinition, _).

oslc:delete_property(IRI, PropertyDefinition, rdf(Graph)) :-
  must_be(atom, IRI),
  must_be(atom, Graph),
  rdf_retractall(IRI, PropertyDefinition, _, Graph).

oslc:marshal_property(IRI, PropertyDefinition, Value, Type, tmp(Graph)) :-
  ( var(Graph)
  -> make_temp_graph(Graph)
  ; true
  ),
  oslc:marshal_property(IRI, PropertyDefinition, Value, Type, rdf(Graph)).

oslc:unmarshal_property(IRI, PropertyDefinition, Value, Type, tmp(Graph)) :-
  ( var(Graph)
  -> fail
  ; oslc:unmarshal_property(IRI, PropertyDefinition, Value, Type, rdf(Graph))
  ).

oslc:delete_property(IRI, PropertyDefinition, tmp(Graph)) :-
  ( var(Graph)
  -> make_temp_graph(Graph)
  ; oslc:delete_property(IRI, PropertyDefinition, rdf(Graph))
  ).

%!  make_temp_graph(-Graph, +Prefix) is det.
%
%   Create a new non-persistent (RAM only) graph with unique name prefixed with an atom.

make_temp_graph(Graph, Prefix) :-
  must_be(var, Graph),
  uuid(Graph, Prefix),
  rdf_create_graph(Graph),
  rdf_persistency(Graph, false),
  assertz(temp_graph(Graph)).


%!  make_temp_graph(-Graph) is det.
%
%   Create a new non-persistent (RAM only) graph with unique name.

make_temp_graph(Graph) :-
  uuid_salt(Prefix),
  make_temp_graph(Graph, Prefix).

%!  pass_temp_graph(+Graph, +ThreadId) is det.
%
%   Remove temporary graph Graph registration from current thread
%   and register it in thread ThreadId. Current thread will not
%   remove this graph if delete_temp_graph or clean_temp_graphs
%   is called. Thread ThreadId becomes responsible for handling
%   temporary graph Graph and its cleanup. Fails silently if
%   current thread does not have temporary graph Graph.

pass_temp_graph(Graph, ThreadId) :-
  must_be(atom, Graph),
  ( temp_graph(Graph)
  -> retractall(temp_graph(Graph)),
     thread_signal(ThreadId, assertz(temp_graph(Graph)))
  ).

%!  delete_temp_graph(+Graph) is det.
%
%   Deletes temporary graph created with make_temp_graph.

delete_temp_graph(Graph) :-
  must_be(atom, Graph),
  temp_graph(Graph),
  rdf_unload_graph(Graph),
  retractall(temp_graph(Graph)).

%!  clean_temp_graphs is det.
%
%   Delete temporary graphs created with make_temp_graph/1 in this thread.

clean_temp_graphs :-
  forall(
    temp_graph(Graph),
    rdf_unload_graph(Graph)
  ),
  retractall(temp_graph(_)).

uuid_salt('$oslc_salt_').

uuid(Id) :-
  uuid_salt(Salt),
  uuid(Id, Salt).

uuid(Id, Salt) :-
  Max is 1<<128,
  random_between(0, Max, Num),
  atom_concat(Salt, Num, Id).

%!  autodetect_resource_graph(+IRI, +Graph) is det.
%
%   Tries to automatically detect Graph, in which resource IRI is defined.
%   Fails if resource has =|rdf:type|= property in more than one graph,
%   or doesn't have =|rdf:type|= property but is referrred to as a subject
%   in more that one graph. Ignores graphs created using make_temp_graph/1.

autodetect_resource_graph(IRI, Graph) :-
  must_be(ground, IRI),
  uuid_salt(Salt),
  once((
    findall(G, (
      rdf(IRI, rdf:type, _, G),
      \+ atom_concat(Salt, _, G)
    ), Graphs),
    sort(Graphs, SortedGraphs),
    [Graph] = SortedGraphs
  ; findall(G, (
      rdf(IRI, _, _, G),
      \+ atom_concat(Salt, _, G)
    ), Graphs),
    sort(Graphs, SortedGraphs),
    [Graph] = SortedGraphs
  )).

%!  resource_sha1(+IRI, +Graph, -Hash) is det.
%
%   Hash is SHA1 hash of resource IRI in graph Graph. Names of
%   blank nodes do not affect the hash.

resource_sha1(IRI, Graph, Hash) :-
  must_be(ground, IRI),
  must_be(atom, Graph),
  rdf_global_id(IRI, S),
  resource_dict(S, Dict, [graph(Graph), multi_bnodes(false)]),
  variant_sha1(Dict, Hash).
