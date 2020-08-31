/*
Copyright 2020 Ericsson AB

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

:- module(oslc_dict2, [ graph_list/3,
                        resource_dict/3,
                        name_multireferenced_bnodes/1 ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).

:- predicate_options(graph_list/3, 3, [ skip(list),
                                        lists(boolean),
                                        subject(callable),
                                        property(callable),
                                        resource_key(callable),
                                        label(boolean),
                                        name(boolean),
                                        multi_bnodes(boolean)
                                      ]).

:- predicate_options(resource_dict/3, 3, [ graph(atom),
                                           skip(list),
                                           lists(boolean),
                                           subject(callable),
                                           property(callable),
                                           resource_key(callable),
                                           label(boolean),
                                           name(boolean),
                                           multi_bnodes(boolean)
                                         ]).

:- meta_predicate graph_list(-, -, :),
                  resource_dict(-, -, :).

:- rdf_meta graph_list(r, -, t),
            resource_dict(r, -, t),
            rdf_inlineable_list_(r),
            read_nested_list(r, -, -, -, -, -, -).

graph_list(Graph, List, Options0) :-
  must_be(atom, Graph),
  meta_options(is_meta, Options0, Options1),
  ( select(graph(_), Options1, Options2)
  -> Options = [graph(Graph)|Options2]
  ; Options = [graph(Graph)|Options1]
  ),
  read_subjects(Top, Rest, Options),
  empty_assoc(E),
  read_graph(Top, Rest, [], List, E, _, Options),
  name_multireferenced_bnodes(List, Options).

is_meta(subject).
is_meta(property).
is_meta(resource_key).

name_multireferenced_bnodes(List) :-
  name_multireferenced_bnodes(List, []).

name_multireferenced_bnodes(_, Options) :-
  option(multi_bnodes(MBNodes), Options, true),
  MBNodes \== true, !.

name_multireferenced_bnodes(Dict, _) :-
  term_variables(Dict, Vars),
  term_singletons(Dict, Singletons),
  exclude_singletons(Vars, Singletons, BNodes),
  name_bnodes(BNodes, 1).

exclude_singletons([], _, []) :- !.
exclude_singletons([H|T], Singletons, [H|T2]) :-
  var_not_in_list(H, Singletons), !,
  exclude_singletons(T, Singletons, T2).
exclude_singletons([_|T], Singletons, T2) :-
  exclude_singletons(T, Singletons, T2).

var_not_in_list(_, []) :- !.
var_not_in_list(V, [H|T]) :-
  V \== H,
  var_not_in_list(V, T).

name_bnodes([], _) :- !.
name_bnodes([H|T], Num) :-
  atom_concat('_:bn', Num, H),
  NextNum is Num + 1,
  name_bnodes(T, NextNum).

read_graph([], [], List, List, Seen, Seen, _) :- !.
read_graph([H|T], Rest, List0, List, Seen0, Seen, Options) :-
  read_graph_tops([H|T], List0, List1, Seen0, Seen1, Rest, Rest1, Options),
  ( Rest1 = [HR|TR]
  -> read_graph([HR], TR, List1, List, Seen1, Seen, Options)
  ; List = List1,
    Seen = Seen1
  ).

read_subjects(Top, Rest, Options) :-
  option(graph(Graph), Options, _),
  ( option(skip(SkipList), Options)
  -> true
  ; SkipList = []
  ),
  findall(Subject, (
    rdf(Subject, _, _, Graph),
    \+ memberchk(s(Subject), SkipList)
  ), SubjectList),
  sort(SubjectList, Subjects),
  classify_subjects(Subjects, Top, Rest, Graph).

classify_subjects([], [], [], _) :- !.
classify_subjects([H|T], [H|T2], Rest, Graph) :-
  \+ (
    rdf_is_bnode(H),
    rdf(_, _, H, Graph)
  ), !,
  classify_subjects(T, T2, Rest, Graph).
classify_subjects([H|T], Top, [H|T2], Graph) :-
  classify_subjects(T, Top, T2, Graph).

read_graph_tops([], List, List, Seen, Seen, Rest, Rest, _) :- !.
read_graph_tops([H|T], List0, List, Seen0, Seen, Rest0, Rest, Options) :-
  ( read_resource_dict(H, Dict, Seen0, Seen1, Rest0, Rest1, Options)
  -> read_graph_tops(T, [Dict|List0], List, Seen1, Seen, Rest1, Rest, Options)
  ; read_graph_tops(T, List0, List, Seen0, Seen, Rest0, Rest, Options)
  ).

resource_dict(Resource, Dict, Options) :-
  must_be(ground, Resource),
  meta_options(is_meta, Options, Options1),
  empty_assoc(E),
  read_resource_dict(Resource, Dict, E, _, [], _, Options1),
  name_multireferenced_bnodes(Dict, Options1).

read_resource_dict(S, _, _, _, _, _, Options) :-
  option(skip(SkipList), Options),
  memberchk(s(S), SkipList), !,
  fail.

read_resource_dict(S, Dict, Seen, Seen, Rest, Rest, _) :-
  get_assoc(S, Seen, Dict), !.

read_resource_dict(S, Dict, Seen0, Seen, Rest0, Rest, Options) :-
  option(graph(Graph), Options, _),
  findall(P-O, (
    ( var(Graph)
    -> rdf(S, P, O)
    ; rdf(S, P, O, Graph)
    ),
    ( option(skip(SkipList), Options)
    -> \+ memberchk(p(P), SkipList),
       \+ memberchk(o(O), SkipList)
    ; true
    )
  ), POs),
  ord_subtract(Rest0, [S], Rest1),
  ( rdf_is_bnode(S)
  -> true
  ; ( option(subject(Callback), Options),
      apply(Callback, [S, SV, Options])
    -> true
    ; resource_key(S, SV, Options)
    )
  ),
  put_assoc(S, Seen0, SV, Seen1),
  dict_create(Dict0, SV, []),
  read_resource_properties(S, POs, Dict0, Dict, Seen1, Seen, Rest1, Rest, Options).

read_resource_properties(_, [], Dict, Dict, Seen, Seen, Rest, Rest, _) :- !.

read_resource_properties(S, [P-O|T], Dict0, Dict, Seen0, Seen, Rest0, Rest, Options) :-
  option(property(Callback), Options),
  apply(Callback, [S, P, O, Key, Value, Options]), !,
  add_predicate_value(Key, Value, Dict0, Dict1),
  read_resource_properties(S, T, Dict1, Dict, Seen0, Seen, Rest0, Rest, Options).

read_resource_properties(S, [P-O|T], Dict0, Dict, Seen0, Seen, Rest0, Rest, Options) :-
  read_object(O, OV, Seen0, Seen1, Rest0, Rest1, Options), !,
  resource_key(P, PV, Options),
  add_predicate_value(PV, OV, Dict0, Dict1),
  read_resource_properties(S, T, Dict1, Dict, Seen1, Seen, Rest1, Rest, Options).

read_resource_properties(S, [_|T], Dict0, Dict, Seen0, Seen, Rest0, Rest, Options) :-
  read_resource_properties(S, T, Dict0, Dict, Seen0, Seen, Rest0, Rest, Options).

read_object(O, OV, Seen, Seen, Rest, Rest, _) :-
  read_literal(O, OV), !.

read_object(O, OV, Seen, Seen, Rest, Rest, _) :-
  get_assoc(O, Seen, OV), !.

read_object(O, OV, Seen0, Seen, Rest0, Rest, Options) :-
  rdf_inlineable_list(O, Options),
  read_nested_list(O, OV, Seen0, Seen, Rest0, Rest, Options), !.

read_object(O, OV, Seen0, Seen, Rest0, Rest, Options) :-
  rdf_is_bnode(O),
  read_resource_dict(O, OV, Seen0, Seen, Rest0, Rest, Options), !.

read_object(O, OV, Seen, Seen, Rest, Rest, Options) :-
  rdf_is_iri(O), !,
  ( option(subject(Callback), Options),
    apply(Callback, [O, OV, Options])
  -> true
  ; resource_key(O, OV, Options)
  ).

read_literal(^^(L, _), L).
read_literal(@(L, _), L).

rdf_inlineable_list(L, Options) :-
  ( option(lists(Lists), Options, true)
  -> Lists == true
  ; true
  ),
  rdf_inlineable_list_(L).
rdf_inlineable_list_(rdf:nil) :- !.
rdf_inlineable_list_(L) :-
  rdf_is_bnode(L),
  findall(F, rdf_has(L, rdf:first, F), [_]),
  findall(R, rdf_has(L, rdf:rest, R), [ER]),
  rdf_inlineable_list_(ER).

read_nested_list(rdf:nil, [], Seen, Seen, Rest, Rest, _).

read_nested_list(S, List, Seen, Seen, Rest, Rest, _) :-
  get_assoc(S, Seen, List), !.

read_nested_list(S, [H|T], Seen0, Seen, Rest0, Rest, Options) :-
  option(graph(Graph), Options, _),
  ( var(Graph)
  -> rdf(S, rdf:first, F),
     rdf(S, rdf:rest, R)
  ; rdf(S, rdf:first, F, Graph),
    rdf(S, rdf:rest, R, Graph)
  ), !,
  put_assoc(S, Seen0, [H|T], Seen1),
  ord_subtract(Rest0, [S], Rest1),
  read_object(F, H, Seen1, Seen2, Rest1, Rest2, Options),
  read_nested_list(R, T, Seen2, Seen, Rest2, Rest, Options).

add_predicate_value(P, O, Dict0, Dict) :-
  ( get_dict(P, Dict0, OldO)
  -> ( is_list(OldO)
     -> ord_add_element(OldO, O, Obj),
        put_dict(P, Dict0, Obj, Dict)
     ; ( OldO == O
       -> Dict = Dict0
       ; ord_add_element([OldO], O, Obj),
         put_dict(P, Dict0, Obj, Dict)
       )
     )
  ; put_dict(P, Dict0, O, Dict)
  ).

resource_key(Resource, Key, Options) :-
  ( option(resource_key(Callback), Options),
    apply(Callback, [Resource, Key, Options])
  -> true
  ; once((
      option(label(Label), Options, true),
      Label == true,
      rdf(Resource, rdfs:label, O^^xsd:string),
      atom_string(Key, O)
    ; option(name(Name), Options, true),
      Name == true,
      rdf_global_id(_:Key, Resource)
    ; Key = Resource
    ))
  ).
