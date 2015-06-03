%% -----------------------------------------------------------------------------
%%
%% The MIT License (MIT)
%%
%% Copyright (c) 2015 Hynek Vychodil
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%
%% -----------------------------------------------------------------------------
%%
%% @author Hynek Vychodil <vychodil.hynek@gmail.com>
%% @doc Generic Digraph Algorithms and Behaviour
%%
%% Provides generic digraph algorithms and behaviour definition. The module also
%% provides wrappers for all callbacks to simplify polymorphism. For example
%% you can use `gen_digraph:vertices(G)' as virtual method dispatcher for
%% `your_digraph:vertices({your_digraph, _} = G)'.
%%
%% Callback `from_edgelist` is required as simplest constructor for testing.
%%
%% @copyright 2015 Hynek Vychodil
%% @end
%%
%% -----------------------------------------------------------------------------
-module(gen_digraph).

-export_type([gen_digraph/0, vertex/0]).

-export([ to_edgelist/1
        , no_edges/1
        , vertices/1
        , no_vertices/1
        , in_neighbours/2
        , out_neighbours/2
        , in_degree/2
        , out_degree/2
        , sources/1
        , sinks/1
        , delete/1
        , has_edge/3
        , has_path/2
        , get_path/3
        , get_cycle/2
        ]).

-export([ gen_no_edges/1
        , gen_vertices/1
        , gen_no_vertices/1
        , gen_in_neighbours/2
        , gen_out_neighbours/2
        , gen_in_degree/2
        , gen_out_degree/2
        , gen_sources/1
        , gen_sinks/1
        , gen_has_edge/3
        , gen_has_path/2
        , gen_get_path/3
        , gen_get_cycle/2
        ]).

-ifdef(TEST).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([ digraph/0
        , acyclic_digraph/0
        , prop_edgelist/1
        , prop_no_edges/1
        , prop_vertices/1
        , prop_no_vertices/1
        , test_empty_vertices/1
        , prop_neighbours/1
        , prop_has_edge/1
        , prop_has_path/1
        , prop_get_path/1
        , prop_get_cycle/1
        , gen_properties_tests/1
        , gen_properties_tests/2
        , gen_tests/1
        ]).

-endif.

%% -----------------------------------------------------------------------------
%% Behaviour
%% -----------------------------------------------------------------------------

-type gen_digraph() :: {atom(), term()}.

-type vertex() :: term().

-callback from_edgelist([{vertex(), vertex()}]) -> gen_digraph().

-callback to_edgelist(Graph :: gen_digraph()) -> [{vertex(), vertex()}].

-callback no_edges(Graph :: gen_digraph()) -> non_neg_integer().

-callback vertices(Graph :: gen_digraph()) -> [vertex()].

-callback no_vertices(Graph :: gen_digraph()) -> non_neg_integer().

-callback in_neighbours(Graph :: gen_digraph(), V :: vertex()) -> [vertex()].

-callback out_neighbours(Graph :: gen_digraph(), V :: vertex()) -> [vertex()].

-callback in_degree(Graph :: gen_digraph(), V :: vertex()) -> non_neg_integer().

-callback out_degree(Graph :: gen_digraph(), V :: vertex()) -> non_neg_integer().

-callback sources(Graph :: gen_digraph()) -> [vertex()].

-callback sinks(Graph :: gen_digraph()) -> [vertex()].

-callback delete(Graph :: gen_digraph()) -> true.

-callback has_edge(Graph :: gen_digraph(), V1 :: vertex(), V2 :: vertex()) ->
    boolean().

-callback has_path(Graph :: gen_digraph(), [V1 :: vertex()]) -> boolean().

-callback get_path(Graph :: gen_digraph(), V1 :: vertex(), V2 :: vertex()) ->
    [vertex()] | false.

-callback get_cycle(Graph :: gen_digraph(), V :: vertex()) -> [vertex()] | false.

%% -----------------------------------------------------------------------------
%% Callback wrappers
%% -----------------------------------------------------------------------------

-define(G, {M, _} = G).

to_edgelist(?G) -> M:to_edgelist(G).

no_edges(?G) -> M:no_edges(G).

vertices(?G) -> M:vertices(G).

no_vertices(?G) -> M:no_vertices(G).

in_neighbours(?G, V) -> M:in_neighbours(G, V).

out_neighbours(?G, V) -> M:out_neighbours(G, V).

in_degree(?G, V) -> M:in_degree(G, V).

out_degree(?G, V) -> M:out_degree(G, V).

sources(?G) -> M:sources(G).

sinks(?G) -> M:sinks(G).

delete(?G) -> M:delete(G).

has_edge(?G, V1, V2) -> M:has_edge(G, V1, V2).

has_path(?G, P) -> M:has_path(G, P).

get_path(?G, V1, V2) -> M:get_path(G, V1, V2).

get_cycle(?G, V) -> M:get_cycle(G, V).

%% -----------------------------------------------------------------------------
%% Generic implementations
%% -----------------------------------------------------------------------------

gen_no_edges(G) ->
    length(to_edgelist(G)).

gen_vertices(G) ->
    % Note sources and sinks can has implementation different form gen_* so
    % it can return vertices in any particular order.
    lists:umerge([ lists:usort(Vs) || Vs <- [sources(G), sinks(G)] ]).

gen_no_vertices(G) ->
    length(vertices(G)).

gen_in_neighbours(G, V) ->
    lists:usort([ V1 || {V1, V2} <- to_edgelist(G), V2 =:= V ]).

gen_out_neighbours(G, V) ->
    lists:usort([ V2 || {V1, V2} <- to_edgelist(G), V1 =:= V ]).

gen_in_degree(G, V) ->
    length(in_neighbours(G, V)).

gen_out_degree(G, V) ->
    length(out_neighbours(G, V)).

gen_sources(G) ->
    lists:usort([ V1 || {V1, _} <- to_edgelist(G) ]).

gen_sinks(G) ->
    lists:usort([ V2 || {_, V2} <- to_edgelist(G) ]).

gen_has_edge(G, V1, V2) ->
    lists:member({V1, V2}, to_edgelist(G)).

gen_has_path(G, [V]) -> has_edge(G, V, V);
gen_has_path(G, [V|P]) -> gen_has_path(G, V, P).

gen_has_path(_, _, []) -> true;
gen_has_path(G, V1, [V2|P]) ->
    has_edge(G, V1, V2) andalso gen_has_path(G, V2, P).

gen_get_path(G, V1, V2) ->
    get_one_path(G, V2, [], out_neighbours(G, V1), [V1], [V1]).

gen_get_cycle(G, V) -> get_path(G, V, V).

-spec get_one_path(Graph :: gen_digraph(), Traget :: vertex(),
                   Stack :: [ToDo :: [vertex()]],
                   Neighbours :: [vertex()],
                   Seen  :: [vertex()], Path :: [vertex()]) ->
    [vertex()] | false.
get_one_path(_, T, _, [T|_], _, P) -> lists:reverse(P, [T]);
get_one_path(G, T, S, [V|Ns], Seen, P) ->
    case lists:member(V, Seen) of
        true  -> get_one_path(G, T, S, Ns, Seen, P);
        false ->
            S2  = [Ns|S],
            Ns2 = out_neighbours(G, V),
            get_one_path( G, T, S2, Ns2, [V|Seen], [V|P])
    end;
get_one_path(G, T, [Ns|S], [], Seen, P) ->
    get_one_path(G, T, S, Ns, Seen, tl(P));
get_one_path(_, _, [], [], _, _) -> false.

%% -----------------------------------------------------------------------------
%% Generic properties and generators
%% -----------------------------------------------------------------------------

-ifdef(TEST).

edge() ->
    ?SIZED(S, begin
                  V = round(math:sqrt(S)),
                  {integer(0, V-1), integer(0, V)}
              end
          ).

acyclic_edge() -> ?LET({A, B}, edge(), {B, A+B+1}).

digraph(Edge) -> list(Edge).

digraph() -> digraph(edge()).

acyclic_digraph() -> digraph(acyclic_edge()).

is_simple(false) -> true;
is_simple(P) ->
    LoP = length(P),
    LoU = length(lists:usort(P)),
    LoP =:= LoU orelse
    hd(P) =:= lists:last(P) andalso LoP =:= LoU + 1. % cycle

%% Warning! This macro can be used only as the innermost macro of a property
%% because it deletes graph `G'. In other words, If action `Do' returns
%% `test()', it has to perform all actions using `G' in direct code execution
%% of `Do'.
-define(WITH_G(L, Do),
        begin
            G = Module:from_edgelist(L),
            try Do after delete(G) end
        end
       ).

prop_edgelist(Module) ->
    ?FORALL(
       L, digraph(),
       ?WITH_G(
          L,
          begin
              El = lists:sort(to_edgelist(G)),
              Sl = lists:sort(L),
              Ul = lists:usort(Sl),
              ?WHENFAIL(
                 io:format("~p and ~p =/= ~p~n",
                           [Sl, Ul, El]),
                 Sl =:= El orelse Ul =:= El
                )
          end
         )
      ).

prop_no_edges(Module) ->
    ?FORALL(
       L, digraph(),
       ?WITH_G(L, equals(length(to_edgelist(G)), no_edges(G)))
      ).

prop_vertices(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       ?FORALL(
          {V1, V2}, oneof(L),
          ?WITH_G(L,
                  conjunction(
                    [{source,     lists:member(V1, vertices(G))},
                     {sink,       lists:member(V2, vertices(G))},
                     {in_sources, lists:member(V1, sources(G))},
                     {in_sinks,   lists:member(V2, sinks(G))}
                    ]
                   )
                 )
         )
      ).

prop_no_vertices(Module) ->
    ?FORALL(
       L, digraph(),
       ?WITH_G(L, equals(length(vertices(G)), no_vertices(G)))
      ).

test_empty_vertices(Module) ->
    ?_test(
       ?WITH_G(
          [],
          begin
              ?assertEqual([], vertices(G)),
              ?assertEqual([], sources(G)),
              ?assertEqual([], sinks(G))
          end
         )
      ).

prop_neighbours(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       ?FORALL(
          {V1, V2}, oneof(L),
          ?WITH_G(
             L,
             conjunction(
               [{in,  lists:member(V1,  in_neighbours(G, V2))},
                {out, lists:member(V2, out_neighbours(G, V1))},
                {in_degree,
                 equals(length(in_neighbours(G, V2)), in_degree(G, V2))},
                {out_degree,
                 equals(length(out_neighbours(G, V1)), out_degree(G, V1))}
               ]
              )
            )
         )
      ).

prop_has_edge(Module) ->
    ?FORALL(
       {{V1, V2} = E, L}, {edge(), digraph()},
       ?WITH_G(
          L,
          begin
              Expect = lists:member(E, L),
              collect(Expect, equals(Expect, has_edge(G, V1, V2)))
          end
         )
      ).

prop_has_path(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       begin
           R = edgelist_digraph:from_edgelist(L),
           ?FORALL(
              P, non_empty(list(oneof(vertices(R)))),
              ?WITH_G(
                 L,
                 begin
                     Expect = gen_has_path(R, P),
                     collect(Expect andalso {length(P), length(L)}, equals(Expect, has_path(G, P)))
                 end
                )
             )
       end
      ).

prop_get_path(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       begin
           R = edgelist_digraph:from_edgelist(L),
           ?FORALL(
              {V1, V2}, {oneof(sources(R)), oneof(sinks(R))},
              ?WITH_G(
                 L,
                 begin
                     Expect = gen_get_path(R, V1, V2),
                     Path   = get_path(G, V1, V2),
                     Class  = case Expect of
                                  false -> false;
                                  _ -> {Expect =:= Path, length(Expect)}
                              end,
                     ?WHENFAIL(
                        io:format("Path = ~p~n", [Path]),
                        collect(
                          Class,
                          is_simple(Path) andalso (Expect =:= Path orelse
                                                   gen_has_path(R, Path))
                         )
                       )
                 end
                )
             )
       end
      ).

prop_get_cycle(Module) ->
    ?FORALL(
       L, non_empty(digraph()),
       begin
           R = edgelist_digraph:from_edgelist(L),
           ?FORALL(
              V, oneof(vertices(R)),
              ?WITH_G(
                 L,
                 begin
                     Expect = gen_get_cycle(R, V),
                     Cycle  = get_cycle(G, V),
                     Class  = case Expect of
                                  false -> false;
                                  _ -> {Expect =:= Cycle, length(Expect)}
                              end,
                     ?WHENFAIL(
                        io:format("Cycle = ~p~n", [Cycle]),
                        collect(
                          Class,
                          is_simple(Cycle) andalso (Expect =:= Cycle orelse
                                                    gen_has_path(R, Cycle))
                         )
                       )
                 end
                )
             )
       end
      ).

gen_properties_tests(Module) ->
    gen_properties_tests(Module, []).

gen_properties_tests(Module, Opts) ->
    [{atom_to_list(X), ?_assert(proper:quickcheck(Prop, Opts))}
     || X <- [  prop_edgelist
              , prop_no_edges
              , prop_vertices
              , prop_no_vertices
              , prop_neighbours
              , prop_has_edge
              , prop_has_path
              , prop_get_path
              , prop_get_cycle
             ],
        Prop <- [?MODULE:X(Module)]
    ].

gen_tests(Module) ->
    [{atom_to_list(X), Test}
     || X <- [test_empty_vertices],
        Test <- [?MODULE:X(Module)]
    ].

gen_has_path_test() ->
    R = edgelist_digraph:from_edgelist([{0, 0}, {0, 1}]),
    ?assertEqual(true, gen_has_path(R, [0, 1])).

gen_get_path_simple_path_test() ->
    R = edgelist_digraph:from_edgelist([{0, 0}, {0, 1}]),
    ?assertEqual([0,1], gen_get_path(R, 0, 1)).

-endif. %% TEST
