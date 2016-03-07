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

-export([req/1, req2/1,req2/2, req/2, init/1, get_variable/1]).

-define(CREST_VARIABLES, crest_variables).


init(PropList)->
  ets:new(?CREST_VARIABLES, [named_table ,{read_concurrency, true}, public, {write_concurrency, true}]),
  lists:foreach(fun({Key, _Value})->
    Value = if
      is_tuple(_Value)->
        proplists:get_value(<<"href">>, element(1, _Value) ,<<"">>) ;
      true->
        _Value
    end,
    ets:insert_new(?CREST_VARIABLES, {Key, binary_to_list(Value)}) end,
  PropList).

get_variable(ID)->
  Res = ets:lookup(?CREST_VARIABLES, ID),
  case Res of
    [] ->
      [];
    [{ID,Value}]-> Value
  end.


req2(Endpoint)->
  Res =httpc:request(?PUBLIC_CREST_HOST++Endpoint),
  case Res of
  	{ok,{{_,200,_}, _Headers, Body}}->
  		jiffy:decode(Body, [return_maps]);
  	{ok,{{_,_Code,_}, _Headers, _Body}}->
  		#{};
    {error,_}->
      #{}
  end.

req(Endpoint)->
  Res =httpc:request(?PUBLIC_CREST_HOST++Endpoint),
	case Res of
		{ok,{{_,200,_}, _Headers, Body}}->
			jiffy:decode(Body);
		{ok,{{_,_Code,_}, _Headers, _Body}}->
			{[]};
    {error,_}->
      {[]}
	end.

  req(Endpoint, all)->
    {Page}=pub_crest:req(Endpoint),
    NextPage = proplists:get_value(<<"next">>, Page, none),
    case NextPage of
      none->
        proplists:get_value(<<"items">>, Page, []);
      {[{<<"href">>, NextPageUrl}]}->
        req_dirty(binary_to_list(NextPageUrl), all, proplists:get_value(<<"items">>, Page, []))
    end.

req2(Endpoint, all)->
  Page=pub_crest:req2(Endpoint),
  NextPage = maps:get(<<"next">>, Page, none),
  case NextPage of
    none->
      maps:get(<<"items">>, Page, #{});
    #{<<"href">> := NextPageUrl}->
      req_dirty2(binary_to_list(NextPageUrl), all, maps:get(<<"items">>, Page, []))
  end.

req_dirty(Endpoint, all, Acc)->
  Res =httpc:request(Endpoint),
	case Res of
		{ok,{{_,200,_}, _Headers, Body}}->
      {Page}=jiffy:decode(Body),
      NextPage = proplists:get_value(<<"next">>, Page, none),
      case NextPage of
        none->
          Acc++proplists:get_value(<<"items">>, Page, []);
        {[{<<"href">>, NextPageUrl}]}->
          req_dirty(binary_to_list(NextPageUrl), all, Acc++proplists:get_value(<<"items">>, Page, []))
      end;
		{ok,{{_,_Code,_}, _Headers, _Body}}->
		  Acc
	end.

req_dirty2(Endpoint, all, Acc)->
  Res =httpc:request(Endpoint),
	case Res of
		{ok,{{_,200,_}, _Headers, Body}}->
      Page=jiffy:decode(Body, [return_maps]),
      NextPage = maps:get(<<"next">>, Page, none),
      case NextPage of
        none->
          Acc++ maps:get(<<"items">>, Page, []);
        #{<<"href">> := NextPageUrl}->
          req_dirty2(binary_to_list(NextPageUrl), all, Acc++maps:get(<<"items">>, Page, []))
      end;
		{ok,{{_,_Code,_}, _Headers, _Body}}->
		  Acc
	end.
