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

:- module(oslc_jsonld, []).

:- use_module(library(semweb/rdf11)).
:- use_module(library(http/json)).

:- multifile oslc_dispatch:serializer/2,
             oslc_dispatch:serialize_response/3,
             rdf_db:rdf_load_stream/3,
             rdf_db:rdf_file_type/2.

:- rdf_meta convert_po(r,-,-,-,-,-,-).

rdf_db:rdf_file_type(jsonld,  jsonld).

oslc_dispatch:serializer(application/'ld+json', jsonld).

oslc_dispatch:serialize_response(stream(Out), Graph, jsonld) :-
  rdf_graph(Graph),
  findall(S, (
    rdf(S, _, _, Graph),
    \+ rdf_is_bnode(S)
  ), List),
  sort(List, Subjects),
  maplist(ss_to_dict(Graph), Subjects, Dicts),
  json_write(Out, Dicts, [width(0)]).

ss_to_dict(Graph, S, D) :-
  s_to_dict(S, _{}, C, D1, Graph),
  D = D1.put('@context', C).

s_to_dict(S, C0, C, D, Graph) :-
  ( rdf_is_bnode(S)
  -> C1 = C0,
     D1 = _{}
  ; convert_resource(S, Id, C0, C1),
    D1 = _{'@id': Id}
  ),
  findall(P-O, rdf(S, P, O, Graph), POs),
  pos_to_dict(POs, C1, C, D1, D, Graph).

pos_to_dict([], C, C, D, D, _) :- !.
pos_to_dict([P-O|T], C0, C, D0, D, Graph) :-
  convert_po(P, P1, O, O1, C0, C1, Graph),
  ( O0 = D0.get(P1)
  -> ( is_list(O0)
     -> O2 = [O1|O0]
     ; O2 = [O1,O0]
     )
  ; O2 = O1
  ),
  D1 = D0.put(P1, O2),
  pos_to_dict(T, C1, C, D1, D, Graph).

convert_po(rdf:type, '@type', O0, O, C0, C, _) :- !,
  convert_resource(O0, O, C0, C).
convert_po(P0, P, O0, O, C0, C, Graph) :-
  convert_resource(P0, P, C0, C1),
  convert_o(O0, O, C1, C, Graph).

convert_o(^^(B, 'http://www.w3.org/2001/XMLSchema#boolean'), @(B), C, C, _) :- !.
convert_o(^^(O, _), O, C, C, _) :- !.
convert_o(@(O, _), O, C, C, _) :- !.
convert_o(O0, O, C0, C, Graph) :-
  rdf_list(O0), !,
  rdf_list(O0, L),
  convert_list_o(L, O, C0, C, Graph).
convert_o(O0, O, C0, C, Graph) :-
  rdf_is_bnode(O0), !,
  s_to_dict(O0, C0, C, O, Graph).
convert_o(O0, O, C0, C, _) :-
  convert_resource(O0, R, C0, C),
  O = _{'@id': R}.

convert_resource(R0, R, C0, C) :-
  rdf_global_id(Prefix:Local, R0),
  rdf_current_prefix(Prefix, PrefixIRI), !,
  ( get_dict('@vocab', C0, Vocab)
  -> ( Vocab = PrefixIRI
     -> R = Local, C = C0
     ;  format(atom(R), '~w:~w', [Prefix, Local]), 
        C = C0.put(Prefix, PrefixIRI) 
     )
  ; Vocab = _{'@vocab': PrefixIRI},
    C = C0.put(Vocab),
    R = Local
  ).
% No prefix exists
convert_resource(R, R, C, C) :-
  rdf_global_id(_, R).

% Other option is to store full @ids for each property in the context and use short names in the object.
% Downside is that context gets large -> could be referenced on a separate URI, see https://www.w3.org/2018/jsonld-cg-reports/json-ld/#the-context
  % Def = _{'@id': R0},
  % R = Local,
  % C = C0.put(Local, Def).

convert_list_o([], [], C, C, _) :- !.
convert_list_o([H0|T0], [H|T], C0, C, Graph) :-
  convert_o(H0, H, C0, C1, Graph),
  convert_list_o(T0, T, C1, C, Graph).

% NOTE: Loading is not fully compliant with serialization
% Loading json-ld

rdf_db:rdf_load_stream(jsonld, Stream, Options0) :-
  rdf_db:graph(Options0, Graph),
  ( var(Graph)
  -> rdf_default_graph(Graph)
  ; true
  ),
  strip_module(Options0, _, Options),
  option(register_namespaces(RegisterNS), Options, false),
  option(prefixes(Prefixes), Options, _),
  rdf_graph(Graph),
  json_read_dict(Stream, Json),
  rdf_transaction((
    json_deserialize(Graph, Json, Prefixes),
    ( RegisterNS == true
    -> maplist(register_prefix, Prefixes)
    ; true
    ),
    rdf_set_graph(Graph, modified(false))
  ), parse(Graph)).

register_prefix(Prefix-URI) :-
  rdf_register_prefix(Prefix, URI, keep(true)).

context_to_iri(PKey, IRI, Ctx) :-
  sub_string(PKey, NP, 1, NL, ":"),
  sub_string(PKey, 0, NP, _, Prefix),
  sub_string(PKey, _, NL, 0, Local),
  atom_string(Key, Prefix),
  NS = Ctx.get(Key), !,
  atom_concat(NS, Local, IRI).

context_to_iri(String, IRI, _) :-
  atom_string(IRI, String).

json_deserialize(Graph, List, Prefixes) :-
  is_list(List), !,
  maplist(json_deserialize(Graph), List, PrefixesList),
  merge_dicts(PrefixesList, [], Prefixes).

json_deserialize(Graph, Dict, Prefixes) :-
  is_dict(Dict),
  ( Ctx = Dict.get('@context')
  -> true
  ; Ctx = _{}
  ),
  dict_to_res(Graph, Dict, _, Ctx),
  dict_pairs(Ctx, _, Prefixes).

merge_dicts([], P, P) :- !.
merge_dicts([H|T], D, MP) :-
  findall(K-V, (
    member(K-V, H),
    \+ member(K-V, D)
  ), New),
  append(D, New, ND),
  merge_dicts(T, ND, MP).

dict_to_res(Graph, Dict, S, Ctx) :-
  ( Id = Dict.get('@id')
  -> context_to_iri(Id, S, Ctx)
  ; rdf_create_bnode(S)
  ),
  dict_pairs(Dict, _, KVs),
  kvs_to_po(Graph, S, KVs, Ctx).

kvs_to_po(_, _, [], _) :- !.

kvs_to_po(Graph, S, [K-V|T], Ctx) :-
  kv_to_po(Graph, Ctx, S, K, V),
  kvs_to_po(Graph, S, T, Ctx).

kv_to_po(Graph, Ctx, S, K, V) :-
  is_list(V), !,
  maplist(kv_to_po(Graph, Ctx, S, K), V).

kv_to_po(_, _, _, '@id', _) :- !.

kv_to_po(_, _, _, '@context', _) :- !.

kv_to_po(Graph, Ctx, S, '@type', V) :- !,
  context_to_iri(V, Type, Ctx),
  rdf_assert(S, rdf:type, Type, Graph).

kv_to_po(Graph, Ctx, S, K, V) :-
  is_dict(V), !,
  context_to_iri(K, P, Ctx),
  dict_to_res(Graph, V, O, Ctx),
  rdf_assert(S, P, O, Graph).

kv_to_po(Graph, Ctx, S, K, O) :-
  context_to_iri(K, P, Ctx),
  % We have a value, should be processed more elaborately
  rdf_assert(S, P, O, Graph).
