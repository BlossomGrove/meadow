%%%-------------------------------------------------------------------
%%% @author Johan Blom
%%% @copyright (C) 2017, Johan Blom
%%% @doc
%%%  Simple log utility.
%%% @end
%%% Created :  5 May 2017 by Johan Blom <johan@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(emd_log).

%% API
-export([error/1,error/2,error/3,
	 warning/1,warning/2,warning/3,
	 info/1,info/2,info/3,
	 debug/1,debug/2,debug/3
	]).

%%%===================================================================
%%% API
%%%===================================================================
error(Format) ->
    format("ERROR:",Format,[],[]).

error(Format,Args) ->
    format("ERROR:",Format,Args,[]).

error(Format,Args,Add) ->
    format("ERROR:",Format,Args,Add).

warning(Format) ->
    format("W:",Format,[],[]).

warning(Format,Args) ->
    format("W:",Format,Args,[]).

warning(Format,Args,Add) ->
    format("W:",Format,Args,Add).

info(Format) ->
    format("I:",Format,[],[]).

info(Format,Args) ->
    format("I:",Format,Args,[]).

info(Format,Args,Add) ->
    format("I:",Format,Args,Add).

debug(Format) ->
    format("D:",Format,[],[]).

debug(Format,Args) ->
    format("D:",Format,Args,[]).

debug(Format,Args,Add) ->
    format("D:",Format,Args,Add).





%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
format(Pre,Format,Args,[]) ->
    io:format(Pre++Format++"~n",Args);
format(Pre,Format,Args,Add) ->
    io:format(Pre++" in ~p, ~p/~p, line ~p"++Format++"~n",Add++Args).
    
