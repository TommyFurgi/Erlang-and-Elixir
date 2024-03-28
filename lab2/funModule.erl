-module(funModule).
-author("tomci").

-export([replace/1, count_div3/1, get_data/2]).


replace(List) ->
  Replace = fun
              F([]) -> [];
              F([$a | T]) -> [$e | F(T)]; F([$e | T]) -> [$o | F(T)];
              F([H | T]) -> [H | F(T)]
            end,
  Replace(List).


count_div3(L) ->
  Count_div3 = fun
                 (X) when X rem 3 == 0 -> true;
                 (_) -> false
               end,
  Filtered = lists:filter(Count_div3, L),
  length(Filtered).



get_data(ListaPomiarow, Param) ->
  Res1 = lists:map(fun({_, _, A}) -> A end, ListaPomiarow),
  Res2 = lists:foldl(fun(A, B) -> A ++ B end, [], Res1),
  Res3 = [X || {A, X} <- Res2, A == Param],
  Res3.
