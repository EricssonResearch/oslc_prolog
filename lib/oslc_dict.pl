/*
Copyright 2017 Ericsson AB

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

:- module(oslc_dict, [
  graph_dict/3,
  resource_dict/3,
  resource_dict_property/6,
  resource_object_key/3
]).

:- use_module(library(semweb/rdf11)).

:- rdf_meta graph_dict(r, -, t).
:- rdf_meta resource_dict(r, -, t).
:- rdf_meta resource_dict_property(r, r, t, -, -, -).
:- rdf_meta resource_object_key(r, -, t).

% TODO: fix handling of cyclic graphs <a,b,_bnode>, <_bnode,c,_bnode>.
%       rdf_create_bnode(B), rdf_assert(a,b,B), rdf_assert(B,c,B).
%       Maybe reuse oslc_rdf:read_resource_tree here instead.
graph_dict(Graph, Dict, Options) :-
  findall(Subject, (
    rdf(Subject, _, _, Graph),
    \+ rdf_is_bnode(Subject),
    ( memberchk(skip(SkipList), Options)
    -> \+ memberchk(s(Subject), SkipList)
    ; true
    )
  ), SubjectList),
  sort(SubjectList, Subjects),
  findall(Tag-NDict, (
    member(Subject, Subjects),
    resource_dict(Subject, NDict, [graph(Graph)|Options]),
    is_dict(NDict, Tag)
  ), TagDicts),
  dict_create(Dict, _, TagDicts).

resource_dict(Subject, Dict, Options) :-
  \+ memberchk(no_decode_list, Options),
  rdf_list(Subject), !,
  rdf_list(Subject, List),
  maplist(resource_dict_list_element(Options), List, Dict).

resource_dict(Subject, Dict, Options) :-
  findall(Key-Value, (
    ( memberchk(graph(Graph), Options)
    -> rdf(Subject, Predicate, Object, Graph)
    ; rdf(Subject, Predicate, Object)
    ),
    ( memberchk(skip(SkipList), Options)
    -> \+ memberchk(p(Predicate), SkipList),
       \+ memberchk(o(Object), SkipList)
    ; true
    ),
    ( memberchk(dict_property(Module, Custom), Options),
      T =.. [Custom, Subject, Predicate, Object, Key, Value, Options],
      call(Module:T)
    -> true
    ; resource_dict_property(Subject, Predicate, Object, Key, Value, Options)
    )
  ), KeyValues),
  keysort(KeyValues, SortedKeyValues),
  group_pairs_by_key(SortedKeyValues, GroupedKeyValues),
  flatten_singletons(GroupedKeyValues, FlattenKeyValues),
  ( rdf_is_bnode(Subject)
  -> true
  ; resource_object_key(Subject, Name, Options)
  ),
  dict_create(Dict0, Name, FlattenKeyValues),
  ( nonvar(Name),
    memberchk(id(Id), Options)
  -> Dict = Dict0.put(Id, Name)
  ; Dict = Dict0
  ).

flatten_singletons([], []) :- !.
flatten_singletons([K-[V]|T], [K-V|T2]) :- !,
  flatten_singletons(T, T2).
flatten_singletons([H|T], [H|T2]) :-
  flatten_singletons(T, T2).

resource_dict_property(_Subject, Predicate, Object, Key, Value, Options) :-
  rdf_is_literal(Object), !,
  resource_object_key(Predicate, Key, Options),
  once((
    Object = '^^'(Label, _)
  ; Object = '@'(Label, _)
  )),
  atom_string(Value, Label).

resource_dict_property(_Subject, Predicate, Object, Key, Value, Options) :-
  \+ memberchk(no_decode_list, Options),
  rdf_list(Object), !,
  rdf_list(Object, List),
  resource_object_key(Predicate, Key, Options),
  maplist(resource_dict_list_element(Options), List, Value).

resource_dict_property(_Subject, Predicate, Object, Key, Value, Options) :-
  \+ memberchk(no_inline_bnode, Options),
  rdf_is_bnode(Object), !,
  resource_object_key(Predicate, Key, Options),
  resource_dict(Object, Value, Options).

resource_dict_property(_Subject, Predicate, Object, Key, Value, Options) :-
  memberchk(inline(Inline), Options),
  memberchk(Predicate, Inline), !,
  resource_object_key(Predicate, Key, Options),
  resource_dict(Object, Value, Options).

resource_dict_property(_Subject, Predicate, Object, Key, Value, Options) :-
  resource_object_key(Predicate, Key, Options),
  resource_object_key(Object, Value, Options).

resource_dict_list_element(Options, Object, Value) :-
  resource_dict(Object, Value, Options).

resource_object_key(Object, Key, Options) :-
  ( memberchk(object_key(Module, Custom), Options),
    T =.. [Custom, Object, Key, Options],
    call(Module:T)
  -> true
  ; once((
      \+ memberchk(no_label, Options),
      rdf(Object, rdfs:label, Label^^xsd:string),
      atom_string(Key, Label)
    ; \+ memberchk(no_name, Options),
      rdf_global_id(_:Key, Object)
    ; Key = Object
    ))
  ).
