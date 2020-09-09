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

:- module(oslc_types, [ literal_types/1,
                        is_literal_type/1,
                        unmarshal_type/3 ] ).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), [rdf_is_resource/1]).

:- rdf_meta literal_types(t),
            is_literal_type(r),
            unmarshal_type(-, -, r).

% ------------ LITERAL TYPES

literal_types([ rdf:'XMLLiteral',
                xsd:string,
                xsd:integer,
                xsd:float,
                xsd:double,
                xsd:decimal,
                xsd:dateTime,
                xsd:boolean,
                xsd:date,
                xsd:dateTime,
                xsd:gMonthDay,
                xsd:gYearMonth,
                xsd:time
              ]).

is_literal_type(Type) :-
  literal_types(LT),
  memberchk(Type, LT).

% ------------ VALUE UNMARSHALLING

unmarshal_type(Value^^Type, Value, Type) :- !,
  is_literal_type(Type).

unmarshal_type(Value@_, Value, xsd:string) :- !.

unmarshal_type(Value, Value, oslc:'LocalResource') :-
  rdf_is_bnode(Value), !.

unmarshal_type(Value, Value, oslc:'Resource') :-
  rdf_is_resource(Value).
