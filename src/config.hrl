%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Main config
%%% @end
%%% Created : 21 Feb 2016 by Redpate
%%% Copyright © 2016 Redpate
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%-------------------------------------------------------------------
%global define
-define(SEC, erlang:system_time(seconds)).
-record(crest,{access_token, token_type, owner_hash, character_name, character_id, expires_on, location_id, location_name, jumped_id, jumped_name,
			expires_in=300, callback, in_wh=true, last_call=0, last_update=?SEC,
			refresh_token}).

-record(event,{id,tags,text,time,system,expire=erlang:monotonic_time(seconds)}).

-define(CREST_VARIABLES, crest_variables).
-define(REDIS_ETS_NAME, redis_pid).
%% CREST
%%-define(CREST_SERVER,"crest-tq.eveonline.com").
-define(CREST_AUTH, pub_crest:get_variable(<<"authEndpoint">>)).
-ifndef(PUBLIC_CREST_HOST).
-define(PUBLIC_CREST_HOST,"https://public-crest.eveonline.com").
-endif.
-define(PUBLIC_CREST_HOST_BIN,list_to_binary(?PUBLIC_CREST_HOST)).
-ifndef(CREST_HOST).
-define(CREST_HOST,"https://crest-tq.eveonline.com").
-endif.
-define(REDIRECT_URL, pub_crest:get_variable(redirect_url)).
-define(APPLICATION_ID, pub_crest:get_variable(application_id)).%% client id from https://developers.eveonline.com
-define(AUTH_TOKEN, pub_crest:get_variable(auth_token)). %% precompiled  base64:encode(ClientID++":"++SecretKey)
