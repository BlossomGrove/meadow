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
    io:format("E:"++Format,[]).

error(Format,Args) ->
    io:format("E:"++Format,Args).

warning(Format) ->
    io:format("W:"++Format,[]).

warning(Format,Args) ->
    io:format("W:"++Format,Args).

info(Format) ->
    io:format("I:"++Format,[]).

info(Format,Args) ->
    io:format("I:"++Format,Args).

debug(Format) ->
    io:format("D:"++Format,[]).

debug(Format,Args) ->
    io:format("D:"++Format,Args).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
