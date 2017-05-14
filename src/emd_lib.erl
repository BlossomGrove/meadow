%%% @author  <>
%%% @copyright (C) 2015, 
%%% @doc
%%%
%%% @end
%%% Created : 20 Aug 2015 by  <>

-module(emd_lib).


-export([%% List operations
	 umerge/1,remove_duplicates/1,append/2,merge_lists/3,

	 %% OS operations
	 open_file/2,write_data/2,mkdir/1,

	 %% Erlang source compilation
	 compile/3,
	 compile_cb/4,compile_remote/3,compile_remote/4,

	 %% Misc stuff
	 load_beam/3,
	 add_app_paths/2,
	 add_paths/2,
	 start_apps/1,stop_app/0

	]).

-include("meadow_internal.hrl").

%%% ============================================================================
%% List operations
%%% Merges a list of lists, by doing List1++List2++...++Listn but removing all
%%% duplicates. Keeps ordering. 
umerge(Lists) ->
    umerge0(Lists,[]).

umerge0([],Out) ->
    Out;
umerge0([List|Rest],Out) when is_list(List) ->
    NewOut=umerge1(List,Out),
    umerge0(Rest,NewOut).

umerge1([],Out) ->
    Out;
umerge1([H|Rest],Out) ->
    NewOut=case lists:member(H,Out) of
	       false ->
		   Out++[H];
	       true ->
		   Out
	   end,
    umerge1(Rest,NewOut).



merge_lists([],Fields1,Out) ->
    Out++Fields1;
merge_lists([H={K,_}|Rest],Fields1,Out) ->
    case proplists:get_value(K,Fields1) of
	undefined ->
	    merge_lists(Rest,Fields1,[H|Out]);
	H ->
	    Fields2=proplists:delete(K,Fields1),
	    merge_lists(Rest,Fields2,[H|Out])
    end.


%%% Remove any duplicated element from List
remove_duplicates([]) ->
    [];
remove_duplicates([H]) ->
    [H];
remove_duplicates(List) ->
    remove_duplicates_list(List,[]).

remove_duplicates_list([],Out) ->
    lists:reverse(Out);
remove_duplicates_list([Hd|Rest],Out) ->
    case lists:member(Hd,Out) of
 	false ->
 	    remove_duplicates_list(Rest,[Hd|Out]);
 	true ->
 	    remove_duplicates_list(Rest,Out)
    end.


append(L1,L2) when is_list(L1),is_list(L2) ->
    L1++L2;
append(L1,_) when is_list(L1) ->
    L1;
append(_,L2) when is_list(L2) ->
    L2;
append(_,_) ->
    [].

%%% ============================================================================
%% Operating System operations
open_file(Name,Data) ->
    case catch file:open(Name,[write]) of
	{ok,IODev} when is_pid(IODev) ->
	    case write_data(IODev,Data) of
		ok ->
		    IODev;
		Error0 ->
		    emd_log:error("Opened file, but when writing data on ~p,"
				  " got ~p~n Data=~p",
				  [Name,Error0,Data]),
		    throw(Error0)
	    end;
	Error1 ->
	    emd_log:error("~p:open_file ~p got ~p",[?MODULE,Name,Error1]),
	    throw({error,bad_file})
    end.


write_data(IOdev,Data) ->
    case catch file:write(IOdev,Data) of
	ok ->
	    ok;
	Error0={'EXIT',{badarg,_}} ->
	    Data0=lists:flatten(Data),
	    open_file_check_bad_data(Data0),
	    throw(Error0);
	{error,Reason} ->
	    emd_log:error("Writing data on ~p, got ~p~n Data=~p",
			   [IOdev,Reason,Data]),
	    exit({error,Reason})
    end.

open_file_check_bad_data([]) ->
    ok;
open_file_check_bad_data([H|Rest]) when is_integer(H),H>=0,H=<255 ->
    open_file_check_bad_data(Rest);
open_file_check_bad_data([H|_]) ->
    emd_log:error("~p:open_file_check_bad_data Found ~p",[?MODULE,H]).


mkdir(Dir) ->
    os:cmd(["mkdir -p ",Dir]).


%%% ============================================================================
%%% Erlang source compilation

%% Compile Erlang file
compile(TSnameDir,TSnameStr,Opt) ->
    case compile2(TSnameDir,TSnameStr,Opt) of
	error ->
	    throw({error,cannot_compile});
	{error,Errors,_Warnings} ->
	    throw({error,Errors});
	_Other ->
	    ok
    end.
	    



compile2(TSnameDir,TSnameStr,Inc) when is_list(Inc)->
    FileName=filename:join([TSnameDir,TSnameStr]),
    LibDir="",
    IncDirs=[{i,TSnameDir}|compile_cb_inc(Inc,LibDir,[])],
    compile:file(FileName,[verbose,report_errors,report_warnings,
			   {outdir,TSnameDir}|IncDirs]);
compile2(TSnameDir,TSobsStr,observer) ->
    Source=filename:join([code:priv_dir(?APP_NAME),"../src","em_observer.hrl"]),
    Cmd=["cp ",Source," ",TSnameDir],
    os:cmd(Cmd),
    FileName=filename:join([TSnameDir,TSobsStr]),
    compile:file(FileName,[verbose,report_errors,% report_warnings,
			   {outdir,TSnameDir},{i,TSnameDir}]).

%% Used by environment node to compile call back module.
%% Additionally adds paths to be able to also execute to the call back module.
compile_cb(CB,Inc,Ebins,AppName) when is_atom(CB) ->
    LibDir=code:lib_dir(AppName),
    File=filename:join([LibDir,"test/sut_if",atom_to_list(CB)]),
    OutDir=LibDir, % FIXME Find some better place to store these
    code:add_patha(OutDir),
    Opts=[return_errors,{outdir,OutDir}]++compile_cb_inc(Inc,LibDir,[]),
    code:add_patha(LibDir), 
    add_paths(Ebins,"."),
    compile:file(File,Opts);
compile_cb({Dir,CB},Inc,Ebins,_AppName) when is_list(Dir),
				       is_atom(CB) ->
    File=filename:join([Dir,"src",atom_to_list(CB)]),
    OutDir=Dir, % FIXME Find some better place to store these
    code:add_patha(OutDir),
    Opts=[return_errors,{outdir,OutDir}|
	  compile_cb_inc(Inc,Dir,[])],
    add_paths(Ebins,Dir),
    compile:file(File,Opts).

%% Expand include directories.
compile_cb_inc([],_LibDir,Out) ->
    lists:reverse(Out);
compile_cb_inc([I|Rest],LibDir,Out) when is_list(I) ->
    Dir=filename:join([LibDir,"../../",I]),
    compile_cb_inc(Rest,LibDir,[{i,Dir}|Out]);
compile_cb_inc([{Dir0,I}|Rest],LibDir,Out) when is_list(I),
						is_list(Dir0) ->
    Dir=filename:join([Dir0,I]),
    compile_cb_inc(Rest,LibDir,[{i,Dir}|Out]).



%% Compile (and load) modules on a running remote host (SUT).
%% Note that the SUT may use another Erlang version
compile_remote(SUTnode,Mod,Mod) ->
    compile_remote2(SUTnode,Mod,Mod,Mod:module_info());
compile_remote(SUTnode,ReplMod,Mod) ->
    load_beam(node(),ReplMod,Mod),
    compile_remote2(SUTnode,ReplMod,Mod,ReplMod:module_info()).

compile_remote2(SUTnode,ReplMod,Mod,ModInfo) ->
    CompInfo=proplists:get_value(compile,ModInfo),
    Src=proplists:get_value(source,CompInfo),
    Incs0=case proplists:get_value(options,CompInfo) of
	     undefined ->
		 [];
	     I1 when is_list(I1) -> 
		 [I0 || {i,I0} <- I1]
	 end,
    Incs=[filename:dirname(Src) | Incs0],
    PreDefs=[{'EM_NODE',node()}],
    {ok,Parsed}=rpc:call(SUTnode,epp,parse_file,[Src,Incs,PreDefs]),
    {ok,_,Bin}=rpc:call(SUTnode,compile,forms,
			[Parsed,[verbose,report_errors,report_warnings,
				 {outdir,"/tmp"}]]),

    BeamFile="/tmp/"++atom_to_list(Mod)++".beam", % Just a fake name
    case rpc:call(SUTnode,code,load_binary,[ReplMod,BeamFile,Bin]) of
	{module,_} -> ok;
	Error -> Error
    end.
	     


compile_remote(Mod,RemInc,RemLibDir,SUTnode) ->
    CompInfo=proplists:get_value(compile,Mod:module_info()),
    Src=proplists:get_value(source,CompInfo),
    Incs=case proplists:get_value(options,CompInfo) of
	     undefined ->
		 [I || {i,I} <- compile_cb_inc(RemInc,RemLibDir,[])];
	     I1 when is_list(I1) -> 
		  [I0 || {i,I0} <- I1]
	 end,

    PreDefs=[{'EM_NODE',node()}],
    {ok,Parsed}=rpc:call(SUTnode,epp,parse_file,[Src,Incs,PreDefs]),
    {ok,_,Bin}=rpc:call(SUTnode,compile,forms,
			[Parsed,[verbose,report_errors,report_warnings,
				 {outdir,"/tmp"}]]),

    BeamFile="/tmp/"++atom_to_list(Mod)++".beam", % Just a fake name
%    RemPath=rpc:call(SUTnode,code,which,[Mod]),
    rpc:call(SUTnode,code,load_binary,[Mod,BeamFile,Bin]).

    


%%% ============================================================================
%%% Misc stuff

%%% Load an Erlang module
load_beam(Node,ReplMod,Mod) ->
    BeamFile=code:which(Mod),
    case file:read_file(BeamFile) of
	{ok, BeamBin} ->
	    case rpc:call(Node,code,load_binary,[ReplMod, BeamFile, BeamBin]) of
		{module, _M} ->
		    ok;
		Error ->
		    emd_log:error("Problem loading ~p beam on ~p, got ~p",
				  [Mod,Node,Error]),
		    Error
	    end;
	Error ->
	    emd_log:error("Problem reading beam, got ~p",[Error]),
	    Error
    end.

%%% Start a list of applications, including all dependent applications.
%% First add paths to applications dynamically from the given list, then start 
%% LibDir  = For application in AppList without a full path, join with LibDir 
%% AppList = [App or {Dir,App}] where App is an application and Dir a directory.
%%      If Dir is an atom it is assumed to *not* be the full path
%%      If Dir is a list it is assumed to be the full path
%% Note:
%% - An application depending on another application that is not yet started
%%   {not_started} will try to start that application.
%% - This is very similar to application:ensure_all_started/1, but additionally
%%   tries to guess code paths and add accordingly.
start_apps(AppList0) ->
    emd_log:debug("start_apps AppList0=~p",[AppList0]),
    AppList=[to_app(AE) || AE <- AppList0],
    start_apps2(AppList).

% add_app_paths(AppList0,LibDir,[],[]),

to_app({App,_}) -> App;
to_app(App) -> App.
    

start_apps2([]) ->
    ok;
start_apps2(L=[App|Rest]) ->
    try case application:start(App) of
	    ok ->
		start_apps2(Rest);
	    {error,{already_started,App}} ->
		start_apps2(Rest);
	    {error,{not_started,NewApp}} ->
		emd_log:info("DEP: ~p",[NewApp]),
		start_apps([NewApp]),
		start_apps2(L)
	end
    catch
	_:Reason ->
	    emd_log:error("EXIT ~p:start_apps/2 App=~p Reason=~p~n"
			  " Stacktrace=~p",
			  [?MODULE,App,Reason,erlang:get_stacktrace()])
    end.

%% Iterates over a list that contain:
%% - Names (atom) of applications. These are assumed to be located in LibDir.
%% - Tuples, on the form {Dir,App} where Dir is a directory, that may
%%   be different from LibDir.
%% Creates and adds paths to all applications to start. The list of names to
%% these applications are returned.
add_app_paths(AppList,LibDir) ->
    add_app_paths(AppList,LibDir,[]).

add_app_paths([],LibDir,Out1) ->
    PathList=lists:reverse(Out1),
    add_paths(PathList,LibDir);
add_app_paths([{Dir,App}|Rest],LibDir,Out1) when is_list(Dir) ->
    Path=atom_to_list(App)++"/ebin",
    add_app_paths(Rest,LibDir,[{Dir,Path}|Out1]);
add_app_paths([App|Rest],LibDir,Out1) when is_atom(App) ->
    Path=atom_to_list(App)++"/ebin",
    add_app_paths(Rest,LibDir,[Path|Out1]).



%% Add new paths:
%% - Path       -> LibDir/Path, and
%% - {Dir,Path} -> Dir/Path
add_paths([],_LibDir) -> 
    ok;
add_paths([Path|Rest],LibDir) when is_list(Path) ->
    emd_log:debug("JB-1 add_paths Path=~p LibDir=~p",[Path,LibDir]),
    Dir=filename:join([LibDir,Path]),
    try true=code:add_patha(Dir)
    catch
	_:Reason ->
	    emd_log:warning("Could not add path ~p, got ~p",[Dir,Reason])
    end,
    emd_log:debug("JB-1c add_paths Dir=~p",[Dir]),
    add_paths(Rest,LibDir);
add_paths([{Dir0,Path}|Rest],LibDir) ->
    emd_log:debug("JB-2 add_paths Path=~p",[Path]),
    Dir=filename:join([Dir0,Path]),
    code:add_patha(Dir),
    add_paths(Rest,LibDir).



%%%% Stop all runnning applications.
stop_app() ->
    stop_apps(application:which_applications()),
    halt().

stop_apps([]) ->
    ok;
stop_apps([{App,_,_}|Rest]) when App==kernel;
				 App==stdlib ->
    stop_apps(Rest);
stop_apps([{App,_,_}|Rest]) ->
    application:stop(App),
    stop_apps(Rest).
