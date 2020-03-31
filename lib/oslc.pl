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

:- module(oslc, [ create_resource/4,
                  create_resource/5,
                  oslc_resource/3,
                  copy_resource/4,
                  copy_resource/5,
                  delete_resource/2,
                  delete_resource/3,
                  unmarshal_list_property/5,
                  marshal_list_property/5 ] ).

%!  unmarshal_property(+IRI, +PropertyDefinition, -Value, -Type, +Source) is nondet.
%!  unmarshal_list_property(+IRI, +PropertyDefinition, -Value, -Type, +Source) is det.
%
%   Interface to define a procedure of reading Value and Type of property
%   PropertyDefinition of resource IRI from Source.

:- multifile unmarshal_property/5.

%!  marshal_property(+IRI, +PropertyDefinition, +Value, +Type, +Sink) is det.
%!  marshal_list_property(+IRI, +PropertyDefinition, +Value, +Type, +Sink) is det.
%
%   Interface to define a procedure of writing Value and Type of property
%   PropertyDefinition of resource IRI to Sink.

:- multifile marshal_property/5.

%!  delete_property(+IRI, +PropertyDefinition, +Sink) is det.
%
%   Interface to define a procedure of deleting all values of property
%   PropertyDefinition from resource IRI from Sink.

:- multifile delete_property/3.

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), [rdf_is_resource/1]).
:- use_module(library(oslc_types)).
:- use_module(library(oslc_shape)).
:- use_module(library(oslc_error)).

:- rdf_meta create_resource(r, t, t, -),
            create_resource(r, t, t, t, -),
            oslc_resource(r, t, -),
            copy_resource(t, t, -, -),
            copy_resource(t, t, -, -, -),
            copy_resource1(-, -, r, -, -, -, -, -, -),
            delete_resource(r, -),
            delete_resource(r, -, -),
            unmarshal_property(r, r, -, -, -),
            unmarshal_list_property(r, r, -, -, -),
            marshal_property(r, r, -, -, -),
            marshal_list_property(r, r, -, -, -),
            delete_property(r, r, -).

:- thread_local copied/1, deleted/1.

%!  create_resource(+IRI, +Types, +Properties, +Sink) is det.
%
%   Create OSLC resource IRI of given Types with given Properties in Sink.
%   Associate all resource shapes that are applicable to Types with the resource.
%   See create_resource/5 for manual specification of resource shapes.

create_resource(IRI, Types, Properties, Sink) :-
  applicable_shapes(Types, Shapes),
  create_resource(IRI, Types, Shapes, Properties, Sink).

%!  create_resource(+IRI, +Types, +Shapes, +Properties, +Sink) is det.
%
%   Similar to create_resource/4, but the associate only resource shapes
%   from given list of Shapes, which can be empty.

create_resource(IRI, Types, Shapes, Properties, Sink) :-
  must_be(list(atom), Types),
  must_be(list(atom), Shapes),
  must_be(list(ground), Properties),
  rdf_transaction((
    delete_resource(IRI, Sink),
    marshal_list_property(IRI, rdf:type, Types, _, Sink),
    check_property(IRI, oslc_shapes:oslcInstanceShape, _, Shapes, _),
    marshal_list_property(IRI, oslc:instanceShape, Shapes, _, Sink),
    create_shapes_dict(Shapes, Dict),
    oslc_resource0(IRI, Dict, Properties, Sink),
    check_resource(IRI, Dict, Sink)
  )).

%!  oslc_resource(+IRI, +Properties, +SourceSink) is det.
%
%   Read and/or write properties of an OSLC resource IRI from/to SourceSink.
%   Check rules from all applicable resource shapes associated with IRI.
%   Properties is a list of terms =|Key=Value|=, where =Key= must
%   correspond to a subject of =oslc:name= in the corresponding property's
%   shape, or be a property IRI. If =Value= is a variable, it will be
%   unified with the value (if exists) of the property =Key=. If =Value=
%   is grounded it will be set as a value of property =Key=. If =Value=
%   is an empty list, property =Key= will be removed from resource IRI.
%   It is allowed to combine read and write in a single call, however,
%   each property may appear in Properties only once.

oslc_resource(IRI, Properties, SourceSink) :-
  rdf_transaction((
    applicable_shapes(IRI, Shapes, SourceSink),
    create_shapes_dict(Shapes, Dict),
    oslc_resource0(IRI, Dict, Properties, SourceSink)
  )).

oslc_resource0(_, _, [], _) :- !.

oslc_resource0(Id, Dict, [Property=Value|RemainingProperties], SourceSink) :-
  must_be(atom, Property),
  once((
    once((
      atom_string(Property, Dict.PropertyDefinition.name)
    ; rdf_equal(Property, Dict.PropertyDefinition)
    )),
    ( var(Value)
    -> unmarshal_list_property(Id, PropertyDefinition, InternalValue, _, SourceSink),
       check_property(Id, Dict.PropertyDefinition.resource, _, InternalValue, Value)
    ; delete_property(Id, PropertyDefinition, SourceSink),
      check_property(Id, Dict.PropertyDefinition.resource, Type, InternalValue, Value),
      marshal_list_property(Id, PropertyDefinition, InternalValue, Type, SourceSink)
    )
  ) ; (
    ( var(Value)
    -> unmarshal_some_property(Id, Property, Value, _, SourceSink)
    ; delete_property(Id, Property, SourceSink),
      marshal_some_property(Id, Property, Value, _, SourceSink)
    )
  )),
  oslc_resource0(Id, Dict, RemainingProperties, SourceSink).

% ------------ READ PROPERTY

unmarshal_property(_, _, _, _, []) :- !.

unmarshal_property(IRI, PropertyDefinition, V, Type, [Source|OtherSources]) :-
  unmarshal_property(IRI, PropertyDefinition, V, Type, Source),
  unmarshal_property(IRI, PropertyDefinition, V, Type, OtherSources).

unmarshal_list_property(IRI, PropertyDefinition, Values, Type, Source) :-
  findall(V,
    unmarshal_property(IRI, PropertyDefinition, V, Type, Source)
  , Values).

unmarshal_some_property(IRI, PropertyDefinition, Values, Type, Source) :-
  unmarshal_list_property(IRI, PropertyDefinition, Object, Type, Source),
  once((
    Object = [Values]
  ; Object = Values
  )).

% ------------ WRITE PROPERTY

marshal_property(_, _, _, _, []) :- !.

marshal_property(IRI, PropertyDefinition, V, Type, [Sink|OtherSinks]) :-
  marshal_property(IRI, PropertyDefinition, V, Type, Sink),
  marshal_property(IRI, PropertyDefinition, V, Type, OtherSinks).

marshal_list_property(_, _, [], _, _) :- !.

marshal_list_property(IRI, PropertyDefinition, [V|T], Type, Sink) :-
  marshal_property(IRI, PropertyDefinition, V, Type, Sink),
  marshal_list_property(IRI, PropertyDefinition, T, Type, Sink).

marshal_some_property(IRI, PropertyDefinition, Value, Type, Sink) :-
  ( is_list(Value)
  -> marshal_list_property(IRI, PropertyDefinition, Value, Type, Sink)
  ; marshal_property(IRI, PropertyDefinition, Value, Type, Sink)
  ).

%!  copy_resource(+IRIFrom, +IRITo, +Source, +Sink) is det.
%
%   Equivalent to =|copy_resource(+IRIFrom, +IRITo, +Source, +Sink, [])|=.

copy_resource(IRIFrom, IRITo, Source, Sink) :-
  copy_resource(IRIFrom, IRITo, Source, Sink, []).

%!  copy_resource(+IRIFrom, +IRITo, +Source, +Sink, +Options) is det.
%
%   Copy OSCL resource IRIFrom in Source to IRITo in Sink recursively,
%   i.e. including all of its local resources (blank nodes). If no
%   =merge= option is specified, resource IRITo (and all recursively
%   referred resources, if =neighbours= option is specified) existed in
%   Sink before copying is deleted. If IRIFrom and IRITo are lists of
%   resources of equal sizes, all resources from IRIFrom are copied to
%   corresponding resources from IRITo in a single transaction. A list
%   of Options may contain:
%
%    * inline(InlineSource)
%    If specified, properties of resource IRIFrom described in a shape
%    as having representaton =oslc:Inline= are dereferenced from
%    InlineSource and inlined as blank nodes in resource IRITo.
%
%    * properties(Properties)
%    Copy only properties specified in Properties. The format of the
%    Properties structure is defined by the =properties= term in the
%    following BNF grammar:
%    ==
%      properties   ::= property ("," property)*
%      property     ::= identifier | wildcard | nested_prop
%      nested_prop  ::= (identifier | wildcard) "{" properties "}"
%      wildcard     ::= "*"
%      identifier   ::= prefix ":" name
%      prefix, name ::= /* everything until one of :*,{} */
%    ==
%
%    * prefix(Prefix)
%    Define a set of prefixes to be used in _properties_ option. The
%    syntax of Prefix is defined by the =prefix_defs= term in the
%    following BNF grammar:
%    ==
%      prefix_defs ::= prefix_def ("," prefix_def)*
%      prefix_def  ::= prefix "=" uri_ref_esc
%      prefix      ::= /* everything until = */
%      uri_ref_esc ::= /* an angle bracket-delimited URI reference
%                      in which > and \ are \-escaped. */
%    ==
%
%    * neighbours
%    Recursively copy all refererred resources (i.e. the ones that
%    appear as objects in the IRIFrom) residing in the Source.
%    Reference loops are detected and resolved.
%
%    * merge
%    Do not delete existing IRITo resource from Sink, i.e. merge
%    it with the resource copied from IRIFrom. This also applies
%    to all recursively copied referred resources if =neighbours=
%    option is specified.



copy_resource(IRIFrom, IRITo, Source, Sink, Options) :-
  ( is_list(IRIFrom),
    is_list(IRITo)
  -> rdf_transaction(
       maplist(copy_resource_(Options, Source, Sink), IRIFrom, IRITo)
     )
  ; rdf_transaction(
      copy_resource_(Options, Source, Sink, IRIFrom, IRITo)
    )
  ).

copy_resource_(Options, Source, Sink, IRIFrom, IRITo) :-
  must_be(ground, Source),
  rdf_is_resource(IRIFrom),
  rdf_is_resource(IRITo),
  ( selectchk(prefix(Prefix), Options, O1)
  -> parse_prefix(Prefix, PrefixList)
  ; O1 = Options
  ),
  ( selectchk(properties(Properties), O1, RestOptions2)
  -> parse_properties(Properties, PropertyList, PrefixList),
     O2 = [properties(PropertyList)|RestOptions2]
  ; O2 = O1
  ),
  call_cleanup((
    applicable_shapes(IRIFrom, Shapes, Source),
    create_shapes_dict(Shapes, Dict),
    copy_resource0(IRIFrom, IRITo, Dict, Source, Sink, O2)
  ),
    retractall(copied(_))
  ).

parse_prefix(Prefix, Structure) :-
  atom_chars(Prefix, CPrefix),
  prefixes(Structure, CPrefix, []).

prefixes([P|Ps]) --> prefix_def(P), ( [','], prefixes(Ps) ; [], { Ps = [] } ).
prefix_def([P,U]) --> prefix(P), ['='], uri(U).
prefix(P) --> prefix_letters(Pl), { atom_chars(P, Pl), ! }.
prefix_letters([L|Ls]) --> prefix_letter(L), prefix_letters(Ls).
prefix_letters([]) --> [].
prefix_letter(L) --> [L], { L \== '=' }.

uri(U) --> ['<'], uri_letters(Ul), { atom_chars(U, Ul), ! }, ['>'].
uri_letters([L|Ls]) --> uri_letter(L), uri_letters(Ls).
uri_letters([]) --> [].
uri_letter('>') --> ['\\'], ['>'].
uri_letter('\\') --> ['\\'], ['\\'].
uri_letter(L) --> [L], { L \== '>' }.

parse_properties(Properties, Structure, Prefixes) :-
  atom_chars(Properties, CProperties),
  properties(Structure, Prefixes, CProperties, []).

properties([P|Ps], Prefixes) --> property(P, Prefixes), ( [','], properties(Ps, Prefixes) ; [], { Ps = [] } ).
property(P, Prefixes) --> identifier(P, Prefixes) ; wildcard(P) ; nested_prop(P, Prefixes).
nested_prop(NP, Prefixes) --> ( identifier(N, Prefixes) ; wildcard(N) ), ['{'], properties(Ps, Prefixes), ['}'],
                    { NP =.. [N,Ps] }.
wildcard('*') --> ['*'].
identifier(I, Prefixes) --> word(Prefix), [':'], word(Name),
                            { once((
                                nonvar(Prefixes),
                                member([Prefix,Uri], Prefixes)
                              ; rdf_current_prefix(Prefix, Uri)
                              )),
                              atom_concat(Uri, Name, I)
                            }.
word(W) --> word_letters(Wl), { atom_chars(W, Wl), ! }.
word_letters([L|Ls]) --> letter(L), word_letters(Ls).
word_letters([]) --> [].
letter(L) --> [L], { \+ member(L, [':','*',',','{','}']) }.

copy_resource0(IRIFrom, IRITo, Dict, Source, Sink, Options) :-
  ( memberchk(neighbours, Options)
  -> assertz(copied(IRIFrom))
  ; true
  ),
  ( memberchk(merge, Options)
  -> true
  ; delete_resource(IRITo, Sink)
  ),
  check_resource(IRIFrom, Dict, Source),
  ( selectchk(properties(PropertyList), Options, RestOptions)
  -> true
  ; RestOptions = Options
  ),
  forall(
    unmarshal_property(IRIFrom, PropertyDefinition, Value, Type, Source)
  , copy_resource1(Dict, Value, Type, Source, Sink, IRITo, PropertyDefinition, PropertyList, RestOptions)
  ).

copy_resource1(_, Value, oslc:'LocalResource', Source, Sink, IRITo, PropertyDefinition, PropertyList, RestOptions) :- !,
  copy_bnode(Value, Source, Source, Sink, IRITo, PropertyDefinition, PropertyList, RestOptions).

copy_resource1(Dict, Value, oslc:'Resource', Source, Sink, IRITo, PropertyDefinition, PropertyList, RestOptions) :-
  memberchk(inline(InlineSource), RestOptions),
  rdf_equal(Dict.get(PropertyDefinition).get(representation), oslc:'Inline'), !,
  copy_bnode(Value, Source, InlineSource, Sink, IRITo, PropertyDefinition, PropertyList, RestOptions).

copy_resource1(_, Value, oslc:'Resource', Source, Sink, IRITo, PropertyDefinition, _, RestOptions) :-
  memberchk(neighbours, RestOptions),
  once(unmarshal_property(Value, _, _, _, Source)), !,
  ( copied(Value)
  -> true
  ; applicable_shapes(Value, ResourceShapes, Source),
    create_shapes_dict(ResourceShapes, ResourceDict),
    copy_resource0(Value, Value, ResourceDict, Source, Sink, RestOptions)
  ),
  marshal_property(IRITo, PropertyDefinition, Value, _, Sink).

copy_resource1(_, Value, Type, _, Sink, IRITo, PropertyDefinition, PropertyList, _) :-
  copy_property(Value, Sink, IRITo, PropertyDefinition, Type, PropertyList).

copy_bnode(Value, ShapeSource, Source, Sink, IRITo, Property, PropertyList, RestOptions) :-
  ( ( var(PropertyList),
      NewOptions = RestOptions
    ; ( P =.. [Property, SubList],
        memberchk(P, PropertyList)
      ; memberchk('*'(SubList), PropertyList)
      ),
      NewOptions = [properties(SubList)|RestOptions]
    )
  -> rdf_create_bnode(Bnode),
     applicable_shapes(Value, BnodeShapes, ShapeSource),
     create_shapes_dict(BnodeShapes, BnodeDict),
     copy_resource0(Value, Bnode, BnodeDict, Source, Sink, NewOptions),
     marshal_property(IRITo, Property, Bnode, _, Sink)
  ; true
  ).

copy_property(Value, Sink, IRITo, Property, Type, PropertyList) :-
  ( ( var(PropertyList)
    ; once((
        memberchk(Property, PropertyList)
      ; memberchk('*', PropertyList)
      ))
    )
  -> marshal_property(IRITo, Property, Value, Type, Sink)
  ; true
  ).

%!  delete_resource(+IRI, +Sink) is det.
%
%   Equivalent to =|delete_resource(IRI, Sink, [])|=.

delete_resource(IRI, Sink) :-
  delete_resource(IRI, Sink, []).

%!  delete_resource(+IRI, +Sink, +Options) is det.
%
%   Delete OSCL resource IRI from Sink recursively, i.e. including all
%   of its local resources (blank nodes). If IRI is a list, all
%   resources from the list are deleted a single transaction.
%   If option =neighbours= is specified, recursively removes all
%   referred resources from Sink. A list of Options may contain:
%
%    * neighbours
%    Recursively delete all refererred resources (i.e. the ones that
%    appear as objects in IRI) residing in the Sink.

delete_resource(IRI, Sink, Options) :-
  ( is_list(IRI)
  -> rdf_transaction(
       maplist(delete_resource_(Options, Sink), IRI)
     )
  ; rdf_transaction(
      delete_resource_(Options, Sink, IRI)
    )
  ).

delete_resource_(Options, Sink, IRI) :-
  call_cleanup(
    ( assertz(deleted(IRI)),
      ignore(delete_resource0(IRI, Sink, Options))
    ),
    retractall(deleted(_))
  ).

delete_resource0(IRI, Sink, Options) :-
  rdf_is_resource(IRI),
  forall((
    unmarshal_property(IRI, _, Value, Type, Sink),
    once((
      rdf_equal(oslc:'LocalResource', Type)
    ; memberchk(neighbours, Options),
      rdf_equal(oslc:'Resource', Type)
    )),
    \+ deleted(Value)
  ), (
    assertz(deleted(Value)),
    delete_resource0(Value, Sink, Options)
  )),
  delete_property(IRI, _, Sink).
