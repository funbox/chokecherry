-module(chokecherry_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    Forms2 = [add_calling_info(E) || E <- Forms],
    Forms2.

add_calling_info({attribute,_Line,module,Module} = Match) ->
    put(module,Module),
    Match;
add_calling_info({function, Line, FunName, Arrity, Clauses}) ->
    put(function, FunName),
    Clauses2 = [transform_clause(C) || C <- Clauses],
    {function, Line, FunName, Arrity, Clauses2};
add_calling_info(Match) ->
    Match.

transform_clause({clause, Line, Args, Opts, Body}) ->
    Body2 = [transform_statement(St) || St <- Body],
    {clause, Line, Args, Opts, Body2};
transform_clause(C) -> C.

transform_statement({call, Line1, {remote, Line2, {atom, Line3, chokecherry}, {atom, Line4, info}}, Args}) ->
    Module = get(module),
    Args2 = [{atom, Line4, Module} | Args],
    {call, Line1, {remote, Line2, {atom, Line3, chokecherry}, {atom, Line4, info}}, Args2};
transform_statement(Stmt) when is_tuple(Stmt) ->
    list_to_tuple(transform_statement(tuple_to_list(Stmt)));
transform_statement(Stmt) when is_list(Stmt) ->
    [transform_statement(S) || S <- Stmt];
transform_statement(Stmt) -> Stmt.
