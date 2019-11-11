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

:- module(oslc_shape, [ applicable_shapes/2,
                        create_shapes_dict/2,
                        check_resource/3,
                        applicable_shapes/3,
                        check_property/5 ] ).

:- use_module(library(semweb/rdf_db), [rdf_is_resource/1]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(oslc_types)).
:- use_module(library(oslc_error)).

:- rdf_meta applicable_shapes(t, -),
            check_resource(r, -, -),
            applicable_shapes(r, -, -),
            check_property(r, r, r, -, -),
            format_value(r, -, -),
            check_value(r, r, -, r).

%!  applicable_shapes(+Types, -Shapes) is det.
%
%   True if Shapes is a list of resource shapes applicable to OSLC
%   resources of given list of Types.

applicable_shapes(Types, Shapes) :-
  must_be(list(atom), Types),
  findall(ResourceShape, (
    member(Type, Types),
    rdf(ResourceShape, rdf:type, oslc:'ResourceShape'),
    rdf(ResourceShape, oslc:describes, Type)
  ), Shapes).

create_shapes_dict(Shapes, Dict) :-
  findall(ShapeData, (
    member(Shape, Shapes),
    (
      findall(K=V, (
        rdf(Shape, oslc:property, PropertyResource),
        once((
          rdf(PropertyResource, oslc:propertyDefinition, K),
          rdf(PropertyResource, oslc:occurs, Occurs),
          rdf(PropertyResource, oslc:name, Name^^xsd:string)
        ; oslc_error('Error while processing resource shape [~w]', [Shape])
        )),
        Va = _{resource:PropertyResource, name:Name, occurs:Occurs},
        once((
          rdf(PropertyResource, oslc:representation, Representation),
          V = Va.put(representation, Representation)
        ; V = Va
        ))
      ),
      ShapeData)
    )
  ), ShapesData),
  flatten(ShapesData, DictData),
  catch(
    dict_create(Dict, _, DictData),
    error(duplicate_key(X), _),
    oslc_error('Dulicate definition of property [~w] in resource shapes ~w', [X, Shapes])
  ).

check_resource(IRI, Dict, Source) :-
  forall((
    Dict.PropertyDefinition.resource = PropertyResource
  ), (
    oslc:unmarshal_list_property(IRI, PropertyDefinition, Values, Type, Source),
    check_property(IRI, PropertyResource, Type, Values, _),
    ( rdf_equal(Dict.get(PropertyDefinition).get(representation), oslc:'Inline')
    -> forall(
         member(Value, Values),
         (
           applicable_shapes(Value, BnodeShapes, Source),
           create_shapes_dict(BnodeShapes, BnodeDict),
           check_resource(Value, BnodeDict, Source)
         )
       )
    ; true
    )
  )).

%!  applicable_shapes(+IRI, -Shapes, +Source) is det.
%
%   True if Shapes is a list of resource shapes applicable to OSLC
%   resource IRI from Source.

applicable_shapes(IRI, Shapes, Source) :-
  must_be(atom, IRI),
  oslc:unmarshal_list_property(IRI, oslc:instanceShape, ReadShapes, _, Source),
  oslc:unmarshal_list_property(IRI, rdf:type, Types, _, Source),
  findall(ResourceShape, (
    member(ResourceShape, ReadShapes),
    once((
      \+ rdf(ResourceShape, oslc:describes, _)
    ; member(Type, Types),
      rdf(ResourceShape, oslc:describes, Type)
    ))
  ), Shapes).

% ------------ CHECK PROPERTY

check_property(IRI, PropertyResource, Type, InternalValue, Value) :-
  once((
    rdf(PropertyResource, oslc:occurs, Occurs),
    ( format_value(Occurs, InternalValue, Value)
    ; rdf(PropertyResource, oslc:propertyDefinition, PropertyDefinition),
      oslc_error("Property [~w] of resource [~w] must have cardinality [~w]", [PropertyDefinition, IRI, Occurs])
    )
  )),
  check_value_type(IRI, PropertyResource, InternalValue, Type).

format_value(oslc:'Zero-or-one', [], V) :-
  var(V) ; V == [].
format_value(oslc:'Zero-or-many', [], V) :-
  var(V) ; V == [].
format_value(oslc:'Zero-or-one', [V], V) :-
  nonvar(V), \+ is_list(V).
format_value(oslc:'Exactly-one', [V], V) :-
  nonvar(V), \+ is_list(V).
format_value(oslc:'Zero-or-many', [V|T], [V|T]).
format_value(oslc:'One-or-many', [V|T], [V|T]).

% ------------ CHECK VALUE TYPE

check_value_type(_, _, [], _) :- !.

check_value_type(IRI, PropertyResource, [V|T], Type) :-
  (once(rdf(PropertyResource, oslc:valueType, Type))
  -> once((
       check_value(IRI, PropertyResource, V, Type),
       check_allowed_values(IRI, PropertyResource, V, Type)
     ; rdf(PropertyResource, oslc:propertyDefinition, PropertyDefinition),
       oslc_error("Property [~w] of resource [~w] must be of type [~w]", [PropertyDefinition, IRI, Type])
     )),
     check_value_type(IRI, PropertyResource, T, Type)
  ; true
  ).

check_value(IRI, PropertyResource, Value, oslc:'LocalResource') :-
  rdf_is_bnode(Value),
  check_range(IRI, PropertyResource, Value).

check_value(IRI, PropertyResource, Value, oslc:'Resource') :-
  \+ rdf_is_bnode(Value),
  rdf_is_resource(Value),
  check_range(IRI, PropertyResource, Value).

check_value(IRI, PropertyResource, Value, oslc:'AnyResource') :-
  rdf_is_resource(Value),
  check_range(IRI, PropertyResource, Value).

check_value(_, _, Value, xsd:boolean) :-
  member(Value, [true, false]).

check_value(_, _, Value, xsd:float) :-
  float(Value).

check_value(_, _, Value, xsd:dateTime) :-
  float(Value).

check_value(_, _, Value, xsd:dateTime) :-
  xsd_time_string(Value, 'http://www.w3.org/2001/XMLSchema#dateTime', _).

check_value(_, _, Value, xsd:decimal) :-
  float(Value).

check_value(_, _, Value, xsd:double) :-
  float(Value).

check_value(_, _, Value, xsd:integer) :-
  integer(Value).

check_value(IRI, PropertyResource, Value, rdf:'XMLLiteral') :-
  string(Value),
  check_max_size(IRI, PropertyResource, Value).

check_value(IRI, PropertyResource, Value, xsd:string) :-
  atomic(Value),
  check_max_size(IRI, PropertyResource, Value).

check_max_size(IRI, PropertyResource, Value) :-
  ( once(rdf(PropertyResource, oslc:maxSize, MaxSize^^xsd:integer))
  -> string_length(Value, Length),
     once((
       Length =< MaxSize
     ; rdf(PropertyResource, oslc:propertyDefinition, PropertyDefinition),
       oslc_error("String property [~w] of resource [~w] must not be more than ~w characters long", [PropertyDefinition, IRI, MaxSize])
     ))
  ; true
  ).

check_range(IRI, PropertyResource, Value) :-
  ( findall(R, rdf(PropertyResource, oslc:range, R), Ranges),
    Ranges \== []
  -> once((
       once((
         member(Class, Ranges),
         once((
           rdf_equal(oslc:'Any', Class)
         ; rdf(Value, rdf:type, Class)
         % According to https://tools.oasis-open.org/version-control/svn/oslc-core/trunk/specs/resource-shape.html#range
         % oslc:range can only refer to rdf:type(s), not do inferencing through rdfs:subClassOf
         % this is why rdfs_individual_of(Value, Class) would be too relaxed here.
         ))
       ))
     ; rdf(PropertyResource, oslc:propertyDefinition, PropertyDefinition),
       oslc_error("Property [~w] of resource [~w] must be of one of the following types: ~w", [PropertyDefinition, IRI, Ranges])
     ))
  ; true
  ).

check_allowed_values(IRI, PropertyResource, Value, Type) :-
  ( findall(UAV, (
      rdf(PropertyResource, oslc:allowedValue, AV),
      unmarshal_type(AV, UAV, _),
      check_allowed_value_type(IRI, PropertyResource, UAV, Type)
    ), AVs),
    findall(UAVOV, (
      rdf(PropertyResource, oslc:allowedValues, AVO),
      rdf(AVO, oslc:allowedValue, AVOV),
      unmarshal_type(AVOV, UAVOV, _),
      check_allowed_value_type(IRI, PropertyResource, UAVOV, Type)
    ), AVOVs),
    append(AVs, AVOVs, AllowedValues),
    AllowedValues \== []
  -> once((
       member(Value, AllowedValues)
     ; rdf(PropertyResource, oslc:propertyDefinition, PropertyDefinition),
       oslc_error("The allowed values for property [~w] of resource [~w] are: ~w", [PropertyDefinition, IRI, AllowedValues])
     ))
  ; true
  ).

check_allowed_value_type(IRI, PropertyResource, AllowedValue, Type) :-
  once((
    catch(
      check_value(IRI, PropertyResource, AllowedValue, Type),
      oslc_error(_),
      fail
    )
  ; rdf(PropertyResource, oslc:propertyDefinition, PropertyDefinition),
    oslc_error("The allowed value [~w] of resource [~w] does not satisfy the type of property [~w]", [AllowedValue, IRI, PropertyDefinition])
  )).
