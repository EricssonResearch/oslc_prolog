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

:- module(json_ld, [
  json_serialize/2,
  json_deserialize/2
]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(oslc)).
:- use_module(library(pairs)).

:- rdf_meta json_serialize(r, -).
:- rdf_meta json_serialize0(r, -).
:- rdf_meta json_deserialize(-, r).
%:- rdf_meta iri_key(r, -).
:- rdf_meta find_type(r, r, -).
:- rdf_meta prefixed_iri(r, -, -).
:- rdf_meta predicate_object(r, -, r, -, -).

% Parser registration
:- multifile
    rdf_db:rdf_load_stream/3,
    rdf_db:rdf_file_type/2.

% Register parser
rdf_db:rdf_load_stream(jsonld, Stream, Options) :-
  rdf_db:graph(Options, Graph),
  ( var(Graph)
  -> Graph = user
  ; true
  ),
  rdf_transaction((
    load_json_stream(Stream, Options, Graph),
    rdf_set_graph(Graph, modified(false))
  ),
  loading_graph(Graph)).

rdf_db:rdf_file_type(jsonld,  jsonld).

load_json_stream(Stream, _Module:_Options, Graph) :-
  %option(graph(Graph), Options),
  rdf_graph(Graph),
  json_read_dict(Stream, Json),
  json_deserialize(Json, Graph).

% Register serialization
oslc_dispatch:serializer(application/'ld+json', jsonld).

oslc_dispatch:serialize_response(Out, Graph, jsonld) :-
  rdf_graph(Graph),
  once(
    (
      rdf(R, _, _, Graph),
      \+ rdf_is_bnode(R),
      json_serialize(R, Json),
      json_write(Out, Json, [width(0)])
    )
  ).

generate_context(Context, Prefixes) :-
  findall(P=IRI, (member(P, Prefixes), rdf_current_prefix(P, IRI)), Pairs),
  dict_create(Ctx, json, Pairs),
  Context = json{}.put('@context', Ctx).

json_serialize0(L, Output, _) :-
  rdf_is_literal(L),
  L = ^^(Output, _), !.

json_serialize0(L, Output, _) :-
  rdf_is_literal(L),
  L = @(Output, _), !.
 
json_serialize0(R, Output, _) :-
  rdf_is_iri(R),
  %iri_key(R, Id),
  prefixed_iri(R, _, Id),
  Output = json{}.put('@id', Id), !.

json_serialize0(R, Output, _) :-
  rdf_is_bnode(R),
  json_serialize(R, Output, false), !.

json_serialize(R, Output) :-
  json_serialize(R, Output, true).

json_serialize(R, Output, AddContext) :-
  rdf_global_id(rdf:type, IsType),
  findall(
    % wont be able to have different prefixes in key and value
    Key-Value-Prefix,
    ( 
      rdf(R, P, O),
      ( P = IsType % more readable than predicate_object
      -> Key = '@type', prefixed_iri(O, Prefix, Value)
      ; prefixed_iri(P, Prefix, Key), json_serialize0(O, Value, AddContext)
      )
    ),
    PropsPrefixes
  ),
  pairs_keys_values(PropsPrefixes, Props, Prefixes),
  sort(1, @=<, Props, Sorted),
  group_pairs_by_key(Sorted, Grouped),
  flatten_singletons(Grouped, Flat),
  list_to_set(Prefixes, Set),
  ( AddContext
  -> generate_context(Ctx, Set)
  ; Ctx = json{}
  ),
  ( rdf_is_bnode(R) 
  -> Id = Ctx
  ; Id = Ctx.put(_{'@id':R})
  ),
  Output = Id.put(Flat).

predicate_object(P, Key, O, Value, Prefix, AddContext) :-
  prefixed_iri(P, Prefix, Key),
  json_serialize0(O, Value, AddContext).

predicate_object(rdf:type, '@tyoe', O, Value, Prefix, _) :-
  prefixed_iri(O, Prefix, Value).

% type_decl(T, Output, Prefix) :-
%   rdf_iri(T),
%   rdf_global_id(Prefix:Local, T),
%   string_concat(Prefix, ":", P),
%   string_concat(P, Local, S),
%   atom_string(Output, S).

% type_decl(T, Output, AddContext) :-
%   rdf_is_bnode(T),
%   json_serialize0(T, Output, AddContext).

prefixed_iri(IRI, 'rdf', PK) :-
  PK = '@type',
  rdf_iri(IRI),
  rdf_global_id(rdf:type, IRI),
  !.

prefixed_iri(IRI, Prefix, PK) :-
  rdf_iri(IRI),
  rdf_global_id(Prefix:Local, IRI),
  string_concat(Prefix, ":", P),
  string_concat(P, Local, S),
  atom_string(PK, S),
  !.

%%%%%%%%%%%%%

% iri_key(P, K) :-
%   rdf_iri(P),
%   rdf_current_prefix(rdf, Prefix),
%   string_concat(Prefix, Name, P),
%   string_concat("@", Name, S),
%   atom_string(K, S),
%   !.

% iri_key(P, K) :-
%   rdf_iri(P),
%   rdf_current_prefix(Alias, Prefix),
%   string_concat(Prefix, Name, P),
%   string_concat(Alias, ":", Alias1),
%   string_concat(Alias1, Name, S),
%   atom_string(K, S),
%   !.

% iri_key(P, K) :-
%   P = K.

% % same as flatten singletons

% flatten_simple_lists([], []).

% %flatten_simple_lists([K-V|[]], K-V).

% flatten_simple_lists([K-[V]|T], [K-V|T1]) :-
%   flatten_simple_lists(T, T1), !.

% flatten_simple_lists([K-V|T], [K-V|T1]) :-
%   flatten_simple_lists(T, T1).

% %------- from oslc_dict

flatten_singletons([], []) :- !.
flatten_singletons([K-[V]|T], [K-V|T2]) :- !,
  flatten_singletons(T, T2).
flatten_singletons([H|T], [H|T2]) :-
  flatten_singletons(T, T2).

% %--------


% show_records([]).
% show_records([K=V|T]) :-
%   format('~w\t=\t~w\n',[K, V]),
%   show_records(T).


json_deserialize(_, _).

% json_deserialize(Json, Graph) :-
%   (get_dict('@context', Json, Ctx)
%   -> true
%   ; Ctx = json{}
%   ),
%   ( get_dict('@type', Json, _Clazz)
%   ->  %find_type(Ctx, Clazz, Class),
%       rdf_assert(Ctx, rdf:type, _Class, Graph)
%   ; true
%   ),
%   json_deserialize0(Ctx, Json, Graph).

% json_deserialize0(Context, Dict, Graph) :-
%   is_of_type(dict,Dict),
%   forall(
%     get_dict(Key, Dict, Value),
%     ( 
%       iri_key(P, Key),
%       json_deserialize0(P, Value, Graph),
%       create_property(Context, Key, Value, Graph)
%     )
%   ).

% json_deserialize0(Context, Value, _Graph) :-
%   format('~w\t=\t~w\n',[Context, Value]).


%%%%%%%%%%%%%%%%%%%%

json3(_, [], _, _).

json3(Id, [H|[]], _Graph, Context) :-
  json3(Id, H, _Graph1, Context).

json3([Id1|Ids], [H|T], _Graph, Context) :-
  json3(Id1, H, _Graph1, Context),
  json3(Ids, T, _Graph2, Context).

json3(Id, Dict, _Graph, Context) :-
  is_of_type(dict, Dict),
  ( get_dict('@context', Dict, Ctx)
  -> EffectiveContext = Ctx
  ; EffectiveContext = Context
  ),
  ( get_dict('@id', Dict, Id)
  -> true
  ; random(Rand), 
    Inf is Rand * 1000, 
    Ind is float_integer_part(Inf), 
    atom_concat(bnode, Ind, Id)
  ),
  json31(Id, Dict, Output, EffectiveContext),
  format('REZ:~w\n\n', [Output]).

json31(Subject, Dict, Output, Context) :-
  is_dict(Dict),
  findall(
    Subject-Predicate-Object,
    (
      get_dict(Key, Dict, Value),
      handle_key(Predicate, _Prefix, Key),
      ( Key = '@type'
      -> prefixed_iri(Object, _Prefix2, Value)
      ; is_dict(Value) 
      -> json3(Object, Value, _, Context)
      ; json31(Subject, Value, Object, Context)
      )
    ),
    SPOs
  ),
  printSPO(SPOs),
  Output = SPOs. 

json31(_Subject, L, Output, _) :-
  is_of_type(text, L),
  Output = ^^(L, xsd:string), !.
  
json31(_Subject, L, Output, _) :-
  is_of_type(integer, L),
  Output = ^^(L, xsd:integer), !.

json31(_Subject, L, Output, _) :-
  is_of_type(float, L),
  Output = ^^(L, xsd:double), !.

printSPO([]).
printSPO([H|[]]) :-
  format('~w\n', [H]).
printSPO([H|T]) :-
  format('~w\n', [H]),
  printSPO(T).

% json31(R, Output, _) :-
%   rdf_is_iri(R),
%   iri_key(R, Id),
%   Output = json{}.put('@id', Id).

% json31(R, Output, _) :-
%   rdf_is_bnode(R), !,
%   json30(R, Output, false).

handle_key(_, _, Key) :-
  Key = '@context', !,
  fail.

handle_key(IRI, Prefix, Key) :-
  prefixed_iri(IRI, Prefix, Key).

create_property(_, _, [], _) :- !.

create_property(S, P, [O|T], Graph) :-
  %rdf_assert(S, P, O, Graph),
  format('~w - ~w - ~w (~w)', [S, P, O, Graph]),
  create_property(S, P, T, Graph),
  !.

create_property(S, P, O, Graph) :-
  format('~w - ~w - ~w (~w)', [S, P, O, Graph]).
  %rdf_assert(S, P, O, Graph).

/*
load_turtle_stream(Stream, _Module:Options) :-
  rdf_db:graph(Options, Graph),
  atom_concat('_:', Graph, BNodePrefix),
  rdf_transaction((
    rdf_process_turtle(Stream, assert_triples,
                      [ anon_prefix(BNodePrefix)
                      | Options
                      ]),
                      rdf_set_graph(Graph, modified(false))
  ),
  parse(Graph)).


rdf_process_turtle(In, OnObject, Options) :-
  base_uri(In, BaseURI, Options),
  option(graph(Graph), Options, BaseURI),
  setup_call_cleanup(
      ( open_input(In, Stream, Close),
        create_turtle_parser(Parser, Stream, Options)
      ),
      ( process_turtle(Parser, Stream, OnObject, Graph,
                       [ parse(statement)
                       ]),
        post_options(Parser, Options)
      ),
      ( destroy_turtle_parser(Parser),
        call(Close)
      )).
*/