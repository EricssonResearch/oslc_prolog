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

:- module(oslc_dict2, [ graph_dict/3 ]).

:- use_module(library(semweb/rdf11)).

:- predicate_options(graph_dict/3, 3, [ skip(list),
                                        lists(boolean),
                                        subject(callable),
                                        property(callable),
                                        resource_key(callable),
                                        label(boolean),
                                        name(boolean)
                                      ]).

:- meta_predicate graph_dict(-, -, :).

:- rdf_meta graph_dict(r, -, t),
            read_nested_list(r, -, -, -, -, -).

graph_dict(Graph, Dict, Options) :-
  meta_options(is_meta, Options, Options1),
  read_graph_forest(Graph, Dict, Options1),
  name_multireferenced_bnodes(Dict).

is_meta(subject).
is_meta(property).
is_meta(resource_key).

name_multireferenced_bnodes(Dict) :-
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

read_graph_forest(Graph, Forest, Options) :-
  findall(Subject, (
    ( rdf(Subject, _, _, Graph),
      \+ ( % TODO: also handle situation with dangling cyclic bnodes
       rdf_is_bnode(Subject),
       rdf(_, _, Subject, Graph)
      )
    ),
    ( option(skip(SkipList), Options)
    -> \+ memberchk(s(Subject), SkipList)
    ; true
    )
  ), SubjectList),
  sort(SubjectList, Subjects),
  empty_assoc(E),
  read_graph_forest_(Subjects, Graph, [], Forest, E, _, Options).

read_graph_forest_([], _, Forest, Forest, Seen, Seen, _) :- !.
read_graph_forest_([H|T], Graph, Forest0, Forest, Seen0, Seen, Options) :-
  read_resource_tree(H, Graph, Tree, Seen0, Seen1, Options),
  read_graph_forest_(T, Graph, [Tree|Forest0], Forest, Seen1, Seen, Options).

read_resource_tree(S, Graph, Tree, Seen0, Seen, Options) :-
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
  ( rdf_is_bnode(S)
  -> true
  ; ( option(subject(Callback), Options),
      apply(Callback, [S, Key, Options])
    -> SV = Key
    ; resource_key(S, SV, Options)
    )
  ),
  put_assoc(S, Seen0, SV, Seen1),
  dict_create(Tree0, SV, []),
  read_resource_tree_(S, POs, Graph, Tree0, Tree, Seen1, Seen, Options).

read_resource_tree_(S, [], _, Tree, Tree, Seen0, Seen, _) :- !,
  ( rdf_is_bnode(S),
    get_assoc(S, Seen0, _)
  -> put_assoc(S, Seen0, Tree, Seen)
  ; Seen = Seen0
  ).

read_resource_tree_(S, [P-O|T], Graph, Tree0, Tree, Seen0, Seen, Options) :-
  option(property(Callback), Options),
  apply(Callback, [S, P, O, Key, Value, Options]), !,
  add_predicate_value(Key, Value, Tree0, Tree1),
  read_resource_tree_(S, T, Graph, Tree1, Tree, Seen0, Seen, Options).

read_resource_tree_(S, [P-L|T], Graph, Tree0, Tree, Seen0, Seen, Options) :-
  read_literal(L, O), !,
  resource_key(P, PV, Options),
  add_predicate_value(PV, O, Tree0, Tree1),
  read_resource_tree_(S, T, Graph, Tree1, Tree, Seen0, Seen, Options).

read_resource_tree_(S, [P-O|T], Graph, Tree0, Tree, Seen0, Seen, Options) :-
  ( get_assoc(O, Seen0, OV)
  -> resource_key(P, PV, Options),
     add_predicate_value(PV, OV, Tree0, Tree1),
     Seen2 = Seen0
  ; put_assoc(O, Seen0, OV, Seen1),
    ( option(lists(Lists), Options, true),
      Lists == true,
      rdf_list(O)
    -> read_nested_list(O, OV, Graph, Seen1, Seen2, Options),
       resource_key(P, PV, Options),
       add_predicate_value(PV, OV, Tree0, Tree1)
    ; rdf_is_bnode(O)
    -> read_resource_tree(O, Graph, OV, Seen1, Seen2, Options),
       resource_key(P, PV, Options),
       add_predicate_value(PV, OV, Tree0, Tree1)
    ; Seen2 = Seen1,
      resource_key(P, PV, Options),
      resource_key(O, OV, Options),
      add_predicate_value(PV, OV, Tree0, Tree1)
    )
  ),
  read_resource_tree_(S, T, Graph, Tree1, Tree, Seen2, Seen, Options).

read_literal(^^(L, _), L).
read_literal(@(L, _), L).

read_nested_list(rdf:nil, [], _, Seen, Seen, _).
read_nested_list(RDFList, [H|T], Graph, Seen0, Seen, Options) :-
  ( ( var(Graph)
    -> rdf(RDFList, rdf:first, F),
       rdf(RDFList, rdf:rest, R)
    ; rdf(RDFList, rdf:first, F, Graph),
      rdf(RDFList, rdf:rest, R, Graph)
    )
  -> ( read_literal(F, H)
     -> Seen1 = Seen0
     ; ( read_nested_list(F, H, Graph, Seen0, Seen1, Options)
       -> true
       ; ( get_assoc(F, Seen0, H)
         -> Seen1 = Seen0
         ; read_resource_tree(F, Graph, H, Seen0, Seen1, Options)
         )
       )
     ),
     read_nested_list(R, T, Graph, Seen1, Seen, Options)
  ).

add_predicate_value(P, O, Tree0, Tree) :-
  ( get_dict(P, Tree0, OldO)
  -> ( is_list(OldO)
     -> ord_add_element(OldO, O, Obj),
        put_dict(P, Tree0, Obj, Tree)
     ; ( OldO == O
       -> Tree = Tree0
       ; ord_add_element([OldO], O, Obj),
         put_dict(P, Tree0, Obj, Tree)
       )
     )
  ; put_dict(P, Tree0, O, Tree)
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
