%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Cache server for public crest api
%%% @end
%%% Created : 23 Feb 2016 by Redpate
%%% Copyright © 2016 Redpate
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%-------------------------------------------------------------------

-module(crest_cache).

-compile(export_all).
-include("config.hrl").

-define(DEFAULT_DEP,0).


req(Req)->
  req(Req, ?DEFAULT_DEP, []).
req(Req, Deps) when is_integer(Deps)->
  req(Req, Deps, []);
req(Req, ExtractPoint) when is_list(ExtractPoint)->
  req(Req, ?DEFAULT_DEP, ExtractPoint).
req(Req, Deps, ExtractPoint)->
  case ets:lookup(?CREST_VARIABLES, ?REDIS_ETS_NAME) of
    []->
      #{};
    [{redis_pid, Redis}]->
      {ok, Res}=worker_forecone(Req, Redis, Deps, ExtractPoint), Res
  end.

worker(Req, State, Deps)->
  io:format("get record:~p~n",[Req]),
  case eredis:q(State, ["TYPE", Req]) of
    {ok, <<"none">>}->
      %%io:format("No record:~p~n", [Req]),
      {ok, req3(Req, State, Deps)};
    {ok, <<"hash">>}->
      %io:format("Hash record:~p~n", [Req]),
      case eredis:q(State, ["HGETALL", Req]) of
        {ok, Result} ->
          {ok, worker_run(Result, #{}, State, Deps)};
        _->
          {ok, req3(Req, State, Deps)}
      end;
    {ok, <<"list">>}->
      {ok, Result} = eredis:q(State, ["LRANGE", Req, 0, -1]),
      {ok, [worker_run_dirty(X, State, Deps)||X<-Result]};
    {ok, _Type}->
      eredis:q(State, ["GET", Req])
  end.

get_sub_hash(Req, Redis, Deps, [])->
  {'EXIT', badarg};
get_sub_hash(Req, Redis, Deps, ExtractPoint)->
  {ok, Value}=eredis:q(Redis, ["HGET", Req, ExtractPoint]),
  {ok, worker_run_dirty(Value, Redis, Deps)}.

create_sub_hash(Req, Redis, Deps, [])->
     {'EXIT', badarg};
create_sub_hash(Req, Redis, Deps, ExtractPoint)->
  Value = maps:get(list_to_binary(ExtractPoint), req3(Req, Redis, ?DEFAULT_DEP)),
  {ok, worker_run_dirty(Value, Redis, Deps)}.

worker_forecone(Req, State, Deps, ExtractPoint)->
    case eredis:q(State, ["TYPE", Req]) of
      {ok, <<"none">>}->
        case catch create_sub_hash(Req, State, Deps, ExtractPoint) of
          {'EXIT', _}->{ok, req3(Req, State, Deps)};
          {ok, Attr}->{ok, Attr}
        end;
      {ok, <<"hash">>}->
        case ExtractPoint of
          []->
            case eredis:q(State, ["HGETALL", Req]) of
              {ok, Result} ->
                if
                  9>length(Result) ->
                    {ok, req3(Req, State, Deps)};
                  true->
                    {ok, worker_run(Result, #{}, State, Deps+1)}
                end;
              _->
                {ok, req3(Req, State, Deps+1)}
            end;
          _->
            get_sub_hash(Req, State, Deps, ExtractPoint)
        end;
      {ok, <<"list">>}->
        {ok, Result} = eredis:q(State, ["LRANGE", Req, 0, -1]),
        {ok, [worker_run_dirty(X, State, Deps+1)||X<-Result]};
      {ok, _Type}->
        eredis:q(State, ["GET", Req])
    end.
worker_run([], Acc, _State, _Deps)->
  Acc;
worker_run([Key, <<47, _SubKey/binary>>=Value|Tail], Acc, State, 0)->
  worker_run(Tail, Acc#{Key=>Value}, State, 0);
worker_run([Key, <<47, _SubKey/binary>>=Value|Tail], Acc, State, Deps)->
  Res = element(2, worker(Value, State, Deps-1)),
  worker_run(Tail, Acc#{Key=>Res}, State, Deps);
worker_run([Key, <<131, _/binary>>=Value|Tail], Acc, State, Deps)->
  Res = case catch binary_to_term(Value) of
    {'EXIT', _}->binary:copy(Value, 1);
    _Res->
      _Res
  end,
  worker_run(Tail, Acc#{Key=>Res}, State, Deps);
worker_run([Key, Value|Tail], Acc, State, Deps)->
  worker_run(Tail, Acc#{Key=>Value}, State, Deps).

worker_run_dirty(Value, _State, -1)->
  Value;
worker_run_dirty(<<47, _SubKey/binary>>=Value, State, Deps)->
  element(2, worker(Value, State, Deps-1));
worker_run_dirty(<<131, _/binary>>=Value, State, Deps)->
  Res = case catch binary_to_term(Value) of
    {'EXIT', _}->binary:copy(Value, 1);
    Map when is_map(Map)->
      compile_map(Map, Deps);
    List when is_list(List)->
      [worker_run_dirty(X, State, Deps)||X<-List];
    _Res->
      _Res
  end;
worker_run_dirty(Value, _State, _Deps)->
  Value.

compile_map(Map, Deps)->
  case maps:get(<<"href">>, Map, undefined) of
    <<?PUBLIC_CREST_HOST, Href/binary>>->
      if
        Deps > 0 ->
          crest_cache:req(binary_to_list(Href), Deps-1);
        true->
          Map
      end;
    _->
      Map
  end.



  %% ----------------
  %% cached req (callback for crest_cache)
  req3(Endpoint, Redis, Deps) when is_binary(Endpoint)->
    req3(binary_to_list(Endpoint), Redis, Deps);
  req3(Endpoint, Redis, Deps)->
    Res=pub_crest:req_map(Endpoint),
    case maps:get(<<"href">>, Res, undefined) of
      <<?PUBLIC_CREST_HOST, Href/binary>>->
        parse_map_dirty(Href, Res, Redis, Deps);
      _->
        parse_map_dirty(list_to_binary(Endpoint), Res, Redis, Deps)
    end.

  parse_map(Key, Map, Redis, Deps)->
    case maps:get(<<"href">>, Map, undefined) of
      <<?PUBLIC_CREST_HOST, Href/binary>>->
        if
          Deps > 0 ->
            crest_cache:req(binary_to_list(Href), Deps), Href;
          true->
            parse_map_dirty(Href, Map, Redis, Deps)
        end;
      _->
        case maps:get(<<"id">>, Map, undefined) of
          undefined-> term_to_binary(Map);
          ID->
            ID_STR= integer_to_list(ID),
            parse_map_dirty(<<Key/binary, ID_STR/binary>>, Map, Redis, Deps)
        end
    end.
  map_key(_ParentKey, _Map, -1)->undefined;
  map_key(ParentKey, Map, Deps)->
    case maps:get(<<"href">>, Map, undefined) of
      <<?PUBLIC_CREST_HOST, Href/binary>>->if
        Deps > 0 ->
          {req, Href};
        true->
          Href
        end;
      _->
        case maps:get(<<"id">>, Map, undefined) of
          undefined-> undefined;
          ID->
            ID_STR= integer_to_list(ID),
            <<ParentKey/binary, ID_STR/binary>>
        end
    end.

  parse_map_dirty(Key, Map, Redis, Deps)->
    LinkerPID = spawn_link(?MODULE, insert_map, [self(), Redis, Key, []]),
    NewMap=maps:map(fun(K, V)->
      if
        is_list(V)->
          LinkerPID ! {self(), K, <<Key/binary , K/binary>>}, parse_list( <<Key/binary , K/binary>>, V, [], [], Redis, Deps);
        is_map(V)->
          case map_key(K, V, Deps-1) of
            {req, Href}->
              LinkerPID ! {self(), K, Href},
              {ok, Res}=crest_cache:worker(binary_to_list(Href), Redis, Deps-1), Res;
            undefined->
              Res=term_to_binary(V),
              LinkerPID ! {self(), K, Res}, V;
            MapKey->
              LinkerPID ! {self(), K, MapKey}, parse_map_dirty(MapKey, V, Redis, Deps-1)
          end;
        is_float(V) or is_integer(V)->
          LinkerPID ! {self(), K, term_to_binary(V)}, V;
        true->
          %io:format("ezParsed:~p => ~p~n",[K,V]),
          LinkerPID ! {self(), K, V}, V
      end
    end, Map),
    LinkerPID ! {self(), insert},
    NewMap.

  insert_map(From, Redis, Key, Acc)->
    receive
      {From, K, V} ->
        insert_map(From, Redis, Key, Acc++[K, V]);
      {From, insert} ->
          %%io:format("Added ~p hash~n", [Key]),
          eredis:qp(Redis, [["HMSET", Key | Acc], ["EXPIRE", Key, 120]]), exit(normal);
      _->
        insert_map(From, Redis, Key, Acc)
    after
      60000 ->
        exit(normal)
    end.

  parse_list(K, [], Acc, Acc2, Redis, _Deps)->eredis:qp(Redis, [["RPUSH", K | Acc], ["EXPIRE", K, 120]]), %io:format("Added ~p list~n", [K]),
    Acc2;
  parse_list(K, [Map|Tail], Acc, Acc2, Redis, Deps) when is_map(Map)->
    {Value, Value2} = case map_key(K, Map, Deps) of
      {req, Href}->
        {Href, crest_cache:req(binary_to_list(Href), Deps)};
      undefined->
        {term_to_binary(Map), Map};
      MapKey->
        {MapKey, parse_map_dirty(MapKey, Map, Redis, Deps-1)}
    end,
    parse_list(K, Tail, Acc++[Value], Acc2++[Value2], Redis, Deps);
  parse_list(K, [Map|Tail], Acc, Acc2, Redis, Deps) ->
    parse_list(K, Tail, Acc++[Map], Acc2++[Map], Redis, Deps).
