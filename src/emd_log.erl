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
-export([error/1,error/2,
	 warning/1,warning/2,
	 info/1,info/2,
	 debug/1,debug/2
	]).

%%%===================================================================
%%% API
%%%===================================================================
error(Format) ->
    io:format("ERROR:"++Format++"~n",[]).

error(Format,Args) ->
    io:format("ERROR:"++Format++"~n",Args).

warning(Format) ->
    io:format("W:"++Format++"~n",[]).

warning(Format,Args) ->
    io:format("W:"++Format++"~n",Args).

info(Format) ->
    io:format("I:"++Format++"~n",[]).

info(Format,Args) ->
    io:format("I:"++Format++"~n",Args).

debug(Format) ->
    io:format("D:"++Format++"~n",[]).

debug(Format,Args) ->
    io:format("D:"++Format++"~n",Args).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
