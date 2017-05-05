%%% @author Johan Blom  <>
%%% @copyright (C) 2015, 
%%% @doc
%%%
%%% @end
%%% Created : 20 Aug 2015 by  <>

-module(emd_cfg).

-include("emd_erlang.hrl").
-include("logic.hrl").
-include("meadow_internal.hrl").


-export([
	 %% Configuration operations
	 get_cfg_a/2,get_cfg_a/3,

	 %% Configuration utils
	 read_cfg/1,read_cfg/2,parse/1,parse_simple_expr/1,
	 compile/3,
	 compile_cb/4,

	 create_record_list/4
	]).



%%% ============================================================================
%% Configuration access operations
get_cfg_a(AppName,Cfg) ->
    {ok,R}=application:get_env(AppName,Cfg),
    R.

get_cfg_a(AppName,Cfg,Def) ->
    case application:get_env(AppName,Cfg) of
	{ok,R} ->
	    R;
	undefined ->
	    Def
    end.


%%% ----------------------------------------------------------------------------
%% Read, parse and store configuration
%% To support records etc. in configuration files
read_cfg(AppName) ->
    read_cfg(AppName,AppName).

read_cfg(AppName,CfgName) ->
    PrivDir=code:priv_dir(AppName),
    FileName=filename:join([PrivDir,atom_to_list(CfgName)++".cfg"]),
    Cfgs=parse(FileName),
    Cfgs2=emd_erlang:erlsyn_to_inter(Cfgs),
    store_cfgs(Cfgs2).


%% Parse Erlang file
parse(undefined) ->
    undefined;
parse(InName) when is_list(InName) ->
    io:format("parse_file Name=~p~n",[InName]),
    case epp:parse_file(InName,[], []) of
	{ok,FL0} ->
	    FL=erl_recomment:recomment_forms(erl_syntax:form_list(FL0),
					     erl_comment_scan:file(InName)),
	    FlatFormList=erl_syntax:flatten_form_list(FL),
	    erl_syntax:form_list_elements(FlatFormList);
	Err ->
	    Err
    end.

%% Parse a simple Erlang expression and translate to internal representation.
parse_simple_expr(Str) ->
    {ok, Tokens, _EndLine1}=erl_scan:string(Str),
    {ok, ParseTree}=erl_parse:parse_exprs(Tokens),
    [Tree]=erl_syntax:form_list_elements(erl_syntax:form_list(ParseTree)),
    emd_erlang:erlsyn_to_inter(Tree).



%% Store configuration in standard application environment
%% The special name 'config' is the starting point for all configurations.
%% This *must* only contain function calls on form:
%% - definitions/0 and
%% - configs/1,
%% where the argument (A) to configs/1 has the additional ability to point to
%% a module A_lib.erl with additional record definitions.
store_cfgs(Funcs) ->
    Body=lookup_clause_body(config,undefined,Funcs),
    store_cfgs0(Body,[],Funcs).

store_cfgs0([],_Env,_Funcs) ->
    ok;
store_cfgs0([#application{op=definitions,args=[]}|Rest],_Env0,Funcs) ->
    case lists:keysearch(definitions,#em_userfun.op,Funcs) of
	{value,#em_userfun{clauses=[#clause_expr{body=Body}]}} ->
	    %% Config MAY include a definitions/0 function
	    {Env,_}=expand_body(Body,[],Funcs,[]),
	    store_cfgs0(Rest,Env,Funcs);
	Other ->
	    io:format("Missing definitions/0 configuation, got ~p~n",[Other]),
	    {error,bad_configuration}
    end;
store_cfgs0([#application{op=configs,args=[AppName]}|Rest],Env,Funcs) ->
    case lists:keysearch(configs,#em_userfun.op,Funcs) of
	{value,#em_userfun{clauses=Clauses}} ->
	    %% Config MUST include at least one configs/1 function
	    expand_configs(Clauses,AppName,Env,Funcs),
	    store_cfgs0(Rest,Env,Funcs);
	Other ->
	    io:format("Missing configs/1 configuation, got ~p~n",[Other]),
	    {error,bad_configuration}
    end.


%% Lookup clause in configs/1 function matching AppName (MUST only be one)
%% Note:
%% - An #application{} in Body may add additional parameter to
%%   configuration key.
expand_configs([],_AppName,_Env,_Funcs) ->
     ok;
expand_configs([#clause_expr{pattern=[AppName],body=Body}|_],
	      AppName,Env,Funcs) ->
    Cfgs=expand_configs1(Body,Env,Funcs,[]),
    store_config(Cfgs,AppName);
expand_configs([_|Rest],AppName,Env,Funcs) ->
    expand_configs(Rest,AppName,Env,Funcs).

%% Note:
%% - The configuration field app_depend has the implicit action that paths 
%%   to these dependent application are guessed here.
expand_configs1([],_Env,_Funcs,Out) ->
    Out;
expand_configs1([#match_expr{l={N1,N2},r=R0}|Rest],Env,Funcs,Out)
  when is_atom(N1),is_atom(N2) ->
    {_,R}=expand_expr(R0,Env,Funcs),
    expand_configs1(Rest,Env,Funcs,Out++[{{N1,N2},R}]);
expand_configs1([#match_expr{l=app_depend,r=AppList0}|Rest],Env,Funcs,Out) ->
%    LibDir=filename:join(code:lib_dir(?APP_NAME),".."),
%    {PathList,AppList}=build_pathlist(AppList0,[],[]),
%    emd_lib:add_paths(PathList,LibDir),
    expand_configs1(Rest,Env,Funcs,Out++[{app_depend,AppList0}]);
expand_configs1([#match_expr{l=Name,r=R0}|Rest],Env,Funcs,Out)
  when is_atom(Name) ->
    {_,R}=expand_expr(R0,Env,Funcs),
    expand_configs1(Rest,Env,Funcs,Out++[{Name,R}]);
expand_configs1([#application{op=K,args=Args}|Rest],Env,Funcs,Out) ->
    Par=case Args of
	    [Par0] -> Par0;
	    [] -> undefined
	end,
    Body=lookup_clause_body(K,Par,Funcs),
    {_,Cfgs0}=expand_body(Body,Env,Funcs,[]),
    Cfgs1=expand_configs1_insert(Cfgs0,Par,[]),
    expand_configs1(Rest,Env,Funcs,Out++Cfgs1).

expand_configs1_insert([],_Par,Out) ->
    lists:reverse(Out);
expand_configs1_insert([{K,V}|Rest],undefined,Out) ->
    expand_configs1_insert(Rest,undefined,[{K,V}|Out]);
expand_configs1_insert([{{K1,K2},V}|Rest],Par,Out) ->
    expand_configs1_insert(Rest,Par,[{{K1,Par,K2},V}|Out]);
expand_configs1_insert([{K1,V}|Rest],Par,Out) ->
    expand_configs1_insert(Rest,Par,[{{K1,Par},V}|Out]).

lookup_clause_body(K,undefined,Funcs) ->
    case lists:keysearch(K,#em_userfun.op,Funcs) of
	{value,#em_userfun{clauses=[#clause_expr{body=Body}]}} ->
	    Body;
	false ->
	    io:format("ERROR: Function ~p not defined~n",[K]),
	    throw({error,bad_configuration})
    end;
lookup_clause_body(K,Pattern,Funcs) ->
    Clauses=case lists:keysearch(K,#em_userfun.op,Funcs) of
		{value,#em_userfun{clauses=Clauses0}} ->
		    Clauses0;
		false ->
		    io:format("ERROR: Function ~p not defined~n",[K]),
		    throw({error,bad_configuration})
	    end,
    case lookup_clause_body2(Clauses,Pattern) of
	Error={error,no_matching_clause} ->
	    io:format("Error pattern ~p not found for ~p~n",[Pattern,K]),
	    throw(Error);
	Body ->
	    Body
    end.

lookup_clause_body2([],_Pattern) ->
    {error,no_matching_clause};
lookup_clause_body2([#clause_expr{pattern=[Pattern],body=Body}|_],Pattern) ->
    Body;
lookup_clause_body2([_|Rest],Pattern) ->
    lookup_clause_body2(Rest,Pattern).
    

store_config([],_AppName) ->
    ok;
store_config([{K,V0}|Rest],AppName) ->
    V=map_records(V0,AppName),
    application:set_env(AppName,K,V),
    store_config(Rest,AppName).




%% Expands a body of a clause
expand_body([],Env,_Funcs,Out) ->
    {Env,Out};
expand_body([#clause_expr{pattern=_,body=Body}|Rest],
	    Env,Funcs,Out) ->
    %% Pattern matching of clauses curently not supported !
    {Env1,H}=expand_body(Body,Env,Funcs,[]),
    expand_body(Rest,Env1,Funcs,Out++H);
expand_body([#match_expr{l=#variable{name=Name},r=R0}|Rest],
	    Env0,Funcs,Out) ->
    {_,R}=expand_expr(R0,Env0,Funcs),
    Env=[{Name,R}|Env0],
    expand_body(Rest,Env,Funcs,Out);
expand_body([#match_expr{l=Name,r=R0}|Rest],Env,Funcs,Out) when is_atom(Name) ->
    {_,R}=expand_expr(R0,Env,Funcs),
    expand_body(Rest,Env,Funcs,Out++[{Name,R}]);
expand_body([#match_expr{l={N1,N2},r=R0}|Rest],Env,Funcs,Out)
  when is_atom(N1),is_atom(N2) ->
    {_,R}=expand_expr(R0,Env,Funcs),
    expand_body(Rest,Env,Funcs,Out++[{{N1,N2},R}]);

expand_body([Expr|Rest],Env,Funcs,Out) when is_integer(Expr);
					    is_atom(Expr) ->
    expand_body(Rest,Env,Funcs,Out++[Expr]);
expand_body([Expr|Rest],Env,Funcs,Out) when is_record(Expr,application);
					    is_record(Expr,variable);
					    is_record(Expr,record_expr) ->
    case expand_expr(Expr,Env,Funcs) of
	{Env1,[]} -> %% Only side effects
	    expand_body(Rest,Env1,Funcs,Out);
	{Env1,Expr1} ->
	    expand_body(Rest,Env1,Funcs,Out++[Expr1])
    end;
expand_body([Expr|Rest],Env,Funcs,Out) when is_list(Expr) ->
    {_,Expr1}=expand_body(Expr,Env,Funcs,[]),
    expand_body(Rest,Env,Funcs,Out++[Expr1]);
expand_body([Expr|Rest],Env,Funcs,Out) when is_tuple(Expr) ->
    {_,Expr1}=expand_body(tuple_to_list(Expr),Env,Funcs,[]),
    expand_body(Rest,Env,Funcs,Out++[list_to_tuple(Expr1)]).
    



expand_expr(Expr=#application{op=Op,args=[Arg1,Arg2]},Env,Funcs) when Op=='+';
								      Op=='-';
								      Op=='*' ->
    {_,L}=expand_expr(Arg1,Env,Funcs),
    {_,R}=expand_expr(Arg2,Env,Funcs),
    NewExpr=case Op of
		'+' when is_integer(L),is_integer(R) -> L+R;
		'-' when is_integer(L),is_integer(R) -> L-R;
		'*' when is_integer(L),is_integer(R) -> L*R;
		_ -> Expr#application{args=[L,R]}
	    end,
    {Env, NewExpr};
expand_expr(#application{op=K,args=[]},Env,Funcs) ->
    %% Pattern matching of clauses curently not supported !
    case lists:keysearch(K,#em_userfun.op,Funcs) of
	{value,#em_userfun{clauses=[#clause_expr{body=Body}]}} ->
	    expand_body(Body,Env,Funcs,[]);
	Other ->
	    io:format("Cannot expand function ~p, got ~p~n",[K,Other]),
	    throw({error,bad_configuration})
    end;
expand_expr(#variable{name=Name},Env,_Funcs) ->
    %% Variable MUST be bound
    case proplists:get_value(Name,Env) of
	undefined ->
	    io:format("ERROR ~p not bound~n",[Name]),
	    throw({error,bad_configuration});
	Val ->
	    {Env,Val}
    end;
expand_expr(V0=#record_expr{arg=Arg,type=Type,fields=Fields0},Env,Funcs) ->
    Fields2=case Arg of
		#variable{name=Name} ->
		    %% Variable MUST be bound
		    case lists:keysearch(Name,1,Env) of
			{value,{_,#record_expr{fields=Fields1,type=Type}}} ->
			    emd_lib:merge_lists(Fields0,Fields1,[]);
		        false->
			    io:format("Variable ~p unbound~n in ~p~n",
				      [Name,Env]),
			    throw({error,bad_configuration})
		    end;
		undefined ->
		    Fields0
	    end,
    {_,Fields}=expand_expr_fields(Fields2,Env,Funcs,[]),
    {Env,V0#record_expr{fields=Fields}};
expand_expr(#infix_expr{l=L0,op='++',r=R0},Env,Funcs) ->
    {_,L}=expand_expr(L0,Env,Funcs),
    {_,R}=expand_expr(R0,Env,Funcs),
    {Env,L++R};
expand_expr(Expr,Env,Funcs) when is_tuple(Expr) ->
    {_,Expr1}=expand_body(tuple_to_list(Expr),Env,Funcs,[]),
    {Env,list_to_tuple(Expr1)};
expand_expr(Expr,Env,Funcs) when is_list(Expr) ->
    expand_body(Expr,Env,Funcs,[]);
expand_expr(Expr,Env,_Funcs) when is_atom(Expr);is_integer(Expr) ->
    {Env,Expr}.

				 
expand_expr_fields([],Env,_Funcs,Out) ->
    {Env,lists:reverse(Out)};
expand_expr_fields([{FN,FV}|Rest],Env,Funcs,Out) ->
    {_,NewFV}=expand_expr(FV,Env,Funcs),
    expand_expr_fields(Rest,Env,Funcs,[{FN,NewFV}|Out]).



map_records(V,AppName) when is_list(V) ->
    map_record_list(V,AppName,[]);
map_records(V,AppName) when is_record(V,record_expr) ->
    map_record(V,AppName);
map_records(V,_AppName) ->
    V.

map_record_list([],_AppName,Out) ->
    lists:reverse(Out);
map_record_list([H|Rest],AppName,Out) when is_record(H,record_expr) ->
    NewH=map_record(H,AppName),
    map_record_list(Rest,AppName,[NewH|Out]);
map_record_list([H|Rest],AppName,Out) when is_list(H) ->
    NewH=map_record_list(H,AppName,[]),
    map_record_list(Rest,AppName,[NewH|Out]);
map_record_list([H|Rest],AppName,Out) ->
    map_record_list(Rest,AppName,[H|Out]);
map_record_list(Other,_AppName,_Out) ->
    Other.

%% All records used in configuration must be included here
%% Note:
%% - We really want the record mapping here to be independent meadow
%% - Use AppName to create callback to module with definition.
map_record(#record_expr{type=RecordType,fields=Fields0},AppName) ->
    Fields=[{K,map_records(V,AppName)} || {K,V} <- Fields0],
    io:format("~p:map_record ~p -> ~p~n",[?MODULE,RecordType,AppName]),
    try AppName:map_record(RecordType,Fields)
    catch
	Reason ->
	    io:format("ERROR ~p~n",[Reason])
    end.
			
	%% --- Move to smtp module ---

	%% smtp_server_conf ->
	%%     DecFields=record_info(fields,smtp_server_conf),
	%%     Defaults=[{socktype,?SMTP_DEFAULT_SOCKTYPE},
	%% 	      {port,?SMTP_DEFAULT_PORT},
	%% 	      {timeout_inactivity,undefined},
	%% 	      {timeout_response,?SMTP_RESPONSE_TIMEOUT},
	%% 	      {test_indicator,0}],
	%%     FieldList=create_record_list(DecFields,Fields,Defaults,[]),
	%%     list_to_tuple([Type|FieldList]);
	%% --- Move to smpp module ---
	%% smpp_esme_client ->
	%%     DecFields=record_info(fields,smpp_esme_client),
	%%     Defaults=[{socktype,?SMPP_DEFAULT_SOCKTYPE},
	%% 	      {port,?SMPP_DEFAULT_PORT},
	%% 	      {timeout_session,?SMPP_SESSIONINIT_TIMEOUT},
	%% 	      {timeout_inactivity,?SMPP_INACTIVITY_TIMEOUT},
	%% 	      {timeout_response,?SMPP_RESPONSE_TIMEOUT},
	%% 	      {timeout_app,?SMPP_APP_TIMEOUT},
	%% 	      {bindtype,?SMPP_DEFAULT_BINDTYPE},
	%% 	      {system_type,?SMPP_EMPTYLIST},
	%% 	      {window_size,?SMPP_MAX_SLWSIZE},
	%% 	      {addr_ton,unknown},
	%% 	      {addr_npi,unknown},
	%% 	      {addr_range,?SMPP_NULL},
	%% 	      {max_open_sessions,?SMPP_MAX_OPENSESSIONS},
	%% 	      {version,?SMPP_DEFAULT_VERSION},
	%% 	      {test_indicator,0}],
	%%     FieldList=create_record_list(DecFields,Fields,Defaults,[]),
	%%     list_to_tuple([Type|FieldList]);



create_record_list([],_Fields,_Defaults,Out) ->
    lists:reverse(Out);
create_record_list([F|Rest],Fields,Defaults,Out) ->
    NewOut=case lists:keysearch(F,1,Fields) of
	       false ->
		   %% Value not set, check defaults
		   V=lookup_default_val(F,Defaults),
		   [V|Out];
	       {value,{_,V}} ->
		   [V|Out]
	   end,
    create_record_list(Rest,Fields,Defaults,NewOut).

lookup_default_val(F,Defaults) ->
    case lists:keysearch(F,1,Defaults) of
	false ->
	    undefined;
	{value,{_,V}} ->
	    V
    end.





%% Compile Erlang file
compile(TSnameDir,TSnameStr,Opt) ->
    case compile2(TSnameDir,TSnameStr,Opt) of
	error ->
	    {error,cannot_compile};
	{error,Errors,_Warnings} ->
	    {error,Errors};
	_Other ->
	    ok
    end.
	    



compile2(TSnameDir,TSnameStr,Inc) when is_list(Inc)->
    FileName=filename:join([TSnameDir,TSnameStr]),
    LibDir="",
    IncDirs=[{i,TSnameDir}|compile_cb_inc(Inc,LibDir,[])],
    io:format("IncDirs=~p~n",[IncDirs]),
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
    emd_lib:add_paths(Ebins,LibDir),
    compile:file(File,Opts);
compile_cb({Dir,CB},Inc,Ebins,_AppName) when is_list(Dir),
				       is_atom(CB) ->
    File=filename:join([Dir,"src",atom_to_list(CB)]),
    OutDir=Dir, % FIXME Find some better place to store these
    code:add_patha(OutDir),
    Opts=[return_errors,{outdir,OutDir}|
	  compile_cb_inc(Inc,Dir,[])],
    emd_lib:add_paths(Ebins,Dir),
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


build_pathlist([],Out1,Out2) ->
    {lists:reverse(Out1),lists:reverse(Out2)};
build_pathlist([{Dir,App}|Rest],Out1,Out2) ->
    Path=if
	     is_atom(Dir) ->
		 atom_to_list(Dir)++"/"++atom_to_list(App)++"/ebin";
	     is_list(Dir) ->
		 {Dir,atom_to_list(App)++"/ebin"}
	 end,
    build_pathlist(Rest,[Path|Out1],[App|Out2]);
build_pathlist([App|Rest],Out1,Out2) when is_atom(App) ->
    Path=atom_to_list(App)++"/ebin",
    build_pathlist(Rest,[Path|Out1],[App|Out2]).    
