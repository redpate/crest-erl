%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Pure interface for public CREST
%%% @end
%%% Created : 20 Feb 2016 by Redpate
%%% Copyright © 2016 Redpate
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%-------------------------------------------------------------------
-module(pub_crest).

-include("config.hrl").
-compile(export_all).
-export([req/1, req_map/1, req/2, init/0, get_variable/1]).

%% --------------------------
%% init

init()-> %% init all requered variables
  {ok, Redis} = eredis:start_link(), %% for less then 5k requests per second its pointless to spawn couple of redis workers.
  ets:new(?CREST_VARIABLES, [named_table , {read_concurrency, true}, public, {write_concurrency, true}]), %% shared ets for toring all vaiables used in api
  ets:insert_new(?CREST_VARIABLES, {?REDIS_ETS_NAME, Redis}), %% store redis pid
  {PropList} = pub_crest:req("/"),
  store_endpoints_href(PropList).

store_endpoints_href(PropList)->
  lists:foreach(fun({Key, _Value})->
    Value = if
      is_tuple(_Value)->
        proplists:get_value(<<"href">>, element(1, _Value) , <<"">>) ;
      true->
        _Value %% store whole value if no href found
    end,
    ets:insert_new(?CREST_VARIABLES, {Key, binary_to_list(Value)}) end, %% store endpoints
  PropList).

get_variable(ID)->
  get_variable(ID, []).
get_variable(ID, Default)->
  Res = ets:lookup(?CREST_VARIABLES, ID),
  case Res of
    [] ->
      Default;
    [{ID, Value}]-> Value
  end.

%% --------------------------
%% base request

-define(JIFFY_OPTIONS,[return_maps]).
-define(JIFFY_FILTER(Options),  lists:filter(fun(X)-> lists:member(X,?JIFFY_OPTIONS) end, Options)).

req(Endpoint, Options)->
  Res =httpc:request(?PUBLIC_CREST_HOST++Endpoint),
  case Res of
    {ok, {{_, 200, _}, _Headers, Body}}->
      case lists:member(all, Options) of
        true->
          req_all(jiffy:decode(Body, ?JIFFY_FILTER(Options)), lists:delete(all, Options), []);
        false->
          jiffy:decode(Body, ?JIFFY_FILTER(Options))
      end;
    {ok, {{_, _Code, _}, _Headers, _Body}}->
      {[]};
    {error, _}->
      {[]}
  end.

req_nohost(Endpoint, Options)->
  Res =httpc:request(Endpoint),
  case Res of
    {ok, {{_, 200, _}, _Headers, Body}}->
      jiffy:decode(Body, ?JIFFY_FILTER(Options));
    {ok, {{_, _Code, _}, _Headers, _Body}}->
      {[]};
    {error, _}->
      {[]}
  end.

%% all pages request

req_all(Res, Options, Acc)->
  case lists:member(return_maps, Options) of
    true -> %% if return_maps is an option - return map anyway
      proc_next_page(Res, Options, Acc, map);
    false->
      proc_next_page(Res, Options, Acc, list)
  end.

proc_next_page(Page, Options, Acc, map)->
  NextPage = maps:get(<<"next">>, Page, undefined),
  NewAcc = Acc++maps:get(<<"items">>, Page, []),
  case NextPage of
    undefined->
      NewAcc;
    #{<<"href">> := NextPageUrl}->
      Body = req_nohost(NextPageUrl, Options),
      proc_next_page(Body, Options, NewAcc, map)
  end;
proc_next_page({Page}, Options, Acc, list)->
  NextPage = proplists:get_value(<<"next">>, Page, undefined),
  NewAcc = Acc++proplists:get_value(<<"items">>, Page, []),
  case NextPage of
    undefined->
      NewAcc;
    {[{<<"href">>, NextPageUrl}]}->
      Body = req_nohost(NextPageUrl, Options),
      proc_next_page(Body, Options, NewAcc, list)
  end;
proc_next_page(_Res, _Options, _Acc, _Type)->
  erlang:error(badarg).
%% ------------
%% api requests

req(Endpoint)->
  req(Endpoint, []).
req_map(Endpoint)->
  req(Endpoint, [return_maps]).%% result is map
req_cache(Endpoint)->
  crest_cache:req(Endpoint,[]).
req_cache(Endpoint, Deps)->
  crest_cache:req(Endpoint, Deps).
req_cache(Endpoint, Deps, ExtractPoint)->
  crest_cache:req(Endpoint, Deps, ExtractPoint).
