%%% @private
%%% Description : Translates to/from Erlang clauses suitable for syntax_tools
%%%     (Erlang pretty printing)
%%% Created : 23 Feb 2004 by Johan Blom 

-module(emd_erlang).

-export([create_record_defs/2,
	 erlsyn_to_inter/1,erlsyn_to_inter_g/1,inter_to_erlsyn/1,
	 pp_inter/2

	]).

-include("emd_erlang.hrl").
-include("logic.hrl").


%%% ============================================================================
%%% Handling of Erlang expressions on the "erl_syntax" format

%% Transforms Erlang expressions from erl_syntax to a "simple" internal form
erlsyn_to_inter(List) when is_list(List) ->
    [erlsyn_to_inter(L) || L <- List];
erlsyn_to_inter(none) ->
    undefined;
erlsyn_to_inter({type,_,union,TypeUnion}) when is_list(TypeUnion) ->
    erlsyn_to_inter(TypeUnion);
erlsyn_to_inter({type,_,record,[TypeRecordType|TypeRecordFields]}) ->
    #record_expr{type=erlsyn_to_inter(TypeRecordType),
		 fields=erlsyn_to_inter(TypeRecordFields)};
erlsyn_to_inter({type,_,field_type,[TypeFieldName,TypeFieldType]}) ->
    {erlsyn_to_inter(TypeFieldName),erlsyn_to_inter(TypeFieldType)};
erlsyn_to_inter({type,_,'fun',[TypeIn,TypeOut]}) ->
    {erlsyn_to_inter(TypeIn),erlsyn_to_inter(TypeOut)};
erlsyn_to_inter({type,_,product,TypeList}) ->
    erlsyn_to_inter(TypeList);
erlsyn_to_inter({type,_,range,[TypeStart,TypeStop]}) ->
    Start=erlsyn_to_inter(TypeStart),
    Stop=erlsyn_to_inter(TypeStop),
    if
	Stop>Start ->
	    {range,Start,Stop};
	true ->
	    throw({error,bad_type_range})
    end;
erlsyn_to_inter({type,_Line,TypeName,[]})  when is_atom(TypeName) ->
    %% Usage of a previous type declaration, not yet supported by syntax_tools
    TypeName;
erlsyn_to_inter({user_type,_Line,TypeName,[]})  when is_atom(TypeName) ->
    %% Usage of a previous type declaration
    TypeName;
erlsyn_to_inter({ann_type,_,[AnnTypePar,AnnTypeList]}) ->
    {erlsyn_to_inter(AnnTypePar),erlsyn_to_inter(AnnTypeList)};


erlsyn_to_inter(Tree) ->
    case erl_syntax:type(Tree) of
	atom ->
	    erl_syntax:atom_value(Tree);
	binary ->
	    L=[erlsyn_to_inter(T) || T <- erl_syntax:binary_fields(Tree)],
	    list_to_binary(L);
	binary_field ->
	    erlsyn_to_inter(erl_syntax:binary_field_body(Tree));
	integer ->
	    erl_syntax:integer_value(Tree);
	string ->
	    erl_syntax:string_value(Tree);
	variable ->
	    #variable{name=erl_syntax:variable_name(Tree)};
	underscore ->
	    #variable{name='_'};
	application ->
	    Op=erlsyn_to_inter(erl_syntax:application_operator(Tree)),
	    Args=erlsyn_to_inter(erl_syntax:application_arguments(Tree)),
	    #application{op=Op,args=Args};
	operator ->
	    erl_syntax:operator_name(Tree);
	match_expr ->
	    #match_expr{l=erlsyn_to_inter(erl_syntax:match_expr_pattern(Tree)),
			r=erlsyn_to_inter(erl_syntax:match_expr_body(Tree))};
	infix_expr ->
	    Op=erlsyn_to_inter(erl_syntax:infix_expr_operator(Tree)),
	    if
		Op=='and';Op=='or' ->
		    L=erlsyn_to_inter_g(erl_syntax:infix_expr_left(Tree)),
		    R=erlsyn_to_inter_g(erl_syntax:infix_expr_right(Tree)),
		    case Op of
			'and' ->
			    flatten_logiclist(my_and,L,R);
			'or' ->
			    flatten_logiclist(my_or,L,R)
		    end;
		Op=='+';Op=='-';Op=='*';Op=='/' ->
		    L=erlsyn_to_inter(erl_syntax:infix_expr_left(Tree)),
		    R=erlsyn_to_inter(erl_syntax:infix_expr_right(Tree)),
		    #application{op=Op,args=[L,R]};
		true ->
		    L=erlsyn_to_inter(erl_syntax:infix_expr_left(Tree)),
		    R=erlsyn_to_inter(erl_syntax:infix_expr_right(Tree)),
		    case Op of
			'==' when (not is_atom(L)) and (R==false) ->
			    {my_not,#bool{var=L}};
			'==' when (not is_atom(L)) and (R==true) ->
			    #bool{var=L};
			'=/=' when (not is_atom(L)) and (R==true) ->
			    {my_not,#bool{var=L}};
			'=/=' when (not is_atom(L)) and (R==false) ->
			    #bool{var=L};
			'=/='  ->
			    {my_not,#infix_expr{l=L,op='==',r=R}};
			'=='  ->
			    #infix_expr{l=L,op='==',r=R};
			_  when Op=='>';Op=='>=';
				Op=='<';Op=='=<';
				Op=='++' ->
			    #infix_expr{l=L,op=Op,r=R}
		    end
	    end;

	case_expr ->
	    Arg=erlsyn_to_inter(erl_syntax:case_expr_argument(Tree)),
	    Clauses=erlsyn_to_inter(erl_syntax:case_expr_clauses(Tree)),
	    #case_expr{arg=Arg,clauses=Clauses};
	if_expr ->
	    Clauses0=erlsyn_to_inter(erl_syntax:if_expr_clauses(Tree)),
	    %% Rewrite to #case_expr{}
	    Clauses1=[C0#clause_expr{pattern=[#variable{name='_'}]} ||
			 C0 <- Clauses0],
	    #case_expr{clauses=Clauses1};
	clause ->
	    Patterns=erlsyn_to_inter(erl_syntax:clause_patterns(Tree)),
	    Pos=erl_syntax:get_pos(Tree),
	    Guard=case erlsyn_to_inter_g(erl_syntax:clause_guard(Tree)) of
		      undefined -> #expr{x=true};
		      G -> #expr{x=G}
		  end,
	    #clause_expr{pattern=Patterns,
			 guard=Guard,
			 body=erlsyn_to_inter(erl_syntax:clause_body(Tree)),
			 pos=Pos};
	record_access ->
	    Arg=erlsyn_to_inter(erl_syntax:record_access_argument(Tree)),
	    Type=erlsyn_to_inter(erl_syntax:record_access_type(Tree)),
	    Field=erlsyn_to_inter(erl_syntax:record_access_field(Tree)),
	    #record_access{arg=Arg,type=Type,field=Field};
	record_expr ->
	    Arg=erlsyn_to_inter(erl_syntax:record_expr_argument(Tree)),
	    Type=erlsyn_to_inter(erl_syntax:record_expr_type(Tree)),
	    Fields=erlsyn_to_inter(erl_syntax:record_expr_fields(Tree)),
	    #record_expr{arg=Arg,type=Type,fields=Fields};
	record_field ->
	    L=erlsyn_to_inter(erl_syntax:record_field_name(Tree)),
	    R=erlsyn_to_inter(erl_syntax:record_field_value(Tree)),
	    {L,R};
	tuple ->
	    List=erlsyn_to_inter(erl_syntax:tuple_elements(Tree)),
	    list_to_tuple(List);
	list ->
	    erlsyn_to_inter(erl_syntax:list_elements(Tree));
	module_qualifier ->
	    M=erlsyn_to_inter(erl_syntax:module_qualifier_argument(Tree)),
	    F=erlsyn_to_inter(erl_syntax:module_qualifier_body(Tree)),
	    {M,F};
	attribute ->
	    N=erlsyn_to_inter(erl_syntax:attribute_name(Tree)),
	    A=erlsyn_to_inter(erl_syntax:attribute_arguments(Tree)),
	    {N,A};
	nil ->
	    [];
	comment ->
	    erl_syntax:comment_text(Tree);
	function ->
	    Op=erlsyn_to_inter(erl_syntax:function_name(Tree)),
	    Arity=erl_syntax:function_arity(Tree),
%	    Arity=erlsyn_to_inter(erl_syntax:function_arity(Tree)),
	    Clauses=erlsyn_to_inter(erl_syntax:function_clauses(Tree)),
	    #em_userfun{op=Op,arity=Arity,clauses=Clauses};
	eof_marker ->
	    eof_marker;
	Other ->
	    io:format("ERROR ~p:erlsyn_to_inter~n"
		      " Unexpected expression (~p) in ~n Tree=~p~n",
		      [?MODULE,Other,Tree]),
	    throw({error,bad_spec})
    end.


erlsyn_to_inter_g(List) when is_list(List) ->
    [erlsyn_to_inter_g(L) || L <- List];
erlsyn_to_inter_g(none) ->
    undefined;
erlsyn_to_inter_g(Tree) ->
%    io:format("Type=~p~n Tree=~p~n",[erl_syntax:type(Tree),Tree]),
    case erl_syntax:type(Tree) of
	atom ->
	    erl_syntax:atom_value(Tree);
	application ->
	    Op=erlsyn_to_inter(erl_syntax:application_operator(Tree)),
	    Args=erlsyn_to_inter(erl_syntax:application_arguments(Tree)),
	    #bool{var=#application{op=Op,args=Args}};
	variable ->
	    #bool{var=#variable{name=erl_syntax:variable_name(Tree)}};
	infix_expr ->
	    Op=erlsyn_to_inter(erl_syntax:infix_expr_operator(Tree)),
	    if
		Op=='and';Op=='or' ->
		    L=erlsyn_to_inter_g(erl_syntax:infix_expr_left(Tree)),
		    R=erlsyn_to_inter_g(erl_syntax:infix_expr_right(Tree)),
		    case Op of
			'and' ->
			    flatten_logiclist(my_and,L,R);
			'or' ->
			    flatten_logiclist(my_or,L,R)
		    end;
		true ->
		    L=erlsyn_to_inter(erl_syntax:infix_expr_left(Tree)),
		    R=erlsyn_to_inter(erl_syntax:infix_expr_right(Tree)),
		    case Op of
			'==' when (not is_atom(L)) and (R==false) ->
			    {my_not,#bool{var=L}};
			'==' when (not is_atom(L)) and (R==true) ->
			    #bool{var=L};
			'=/=' when (not is_atom(L)) and (R==true) ->
			    {my_not,#bool{var=L}};
			'=/=' when (not is_atom(L)) and (R==false) ->
			    #bool{var=L};
			'=/='  ->
			    {my_not,#infix_expr{l=L,op='==',r=R}};
			'=='  ->
			    #infix_expr{l=L,op='==',r=R};
			_  when Op=='>';Op=='>=';
				Op=='<';Op=='=<' ->
			    #infix_expr{l=L,op=Op,r=R}
		    end
	    end;
	prefix_expr ->
	    Arg=erlsyn_to_inter_g(erl_syntax:prefix_expr_argument(Tree)),
	    case erlsyn_to_inter(erl_syntax:prefix_expr_operator(Tree)) of
		'not' ->
		    case Arg of
			{my_not,A} -> A;
			_ -> {my_not,Arg}
		    end;
		Op ->
		    io:format("ERROR: ~p(~p) is not supported!",[Op,Arg]),
		    throw({error,unsupported_prefix_expr})
	    end;
	conjunction ->
	    case erlsyn_to_inter_g(erl_syntax:conjunction_body(Tree)) of
		[Expr] ->
		    Expr;
		AndList ->
		    {my_and,AndList}
	    end;
	disjunction ->
	    case erlsyn_to_inter_g(erl_syntax:disjunction_body(Tree)) of
		[Expr] ->
		    Expr;
		OrList ->
%		    io:format("disjunction OrList=~p~n",[OrList]),
		    {my_or,OrList}
	    end;
	Other ->
	    io:format("ERROR ~p:erlsyn_to_inter_g~n"
		      " Unexpected expression (~p) in guard~n Tree=~p~n",
		      [?MODULE,Other,Tree]),
	    throw({error,bad_spec})
    end.


flatten_logiclist(Type,L,R) ->
    NewL=case L of
	     {Type,AL} -> AL;
	     _ -> [L]
	 end,
    NewR=case R of
	     {Type,AR} -> AR;
	     _ -> [R]
	 end,
    {Type,NewL++NewR}.


pp_inter(module,Mod) ->
    Tree=inter_to_erlsyn(#module_def{name=Mod}),
    [erl_prettypr:format(Tree,[{ribbon,80}]),$\n,$\n];
pp_inter(export,[]) ->
    [];
pp_inter(export,Exports) ->
    Tree=inter_to_erlsyn(#export_def{args=Exports}),
    [erl_prettypr:format(Tree,[{ribbon,80}]),$\n,$\n];
pp_inter(include,[]) ->
    [];
pp_inter(include,Includes) ->
    Tree=inter_to_erlsyn(#include_def{args=Includes}),
    [erl_prettypr:format(Tree,[{ribbon,80}]),$\n,$\n].



%%% ============================================================================
%%% Handling of Erlang expressions on the "simple" format



%% Transforms Erlang expressions from a "simple" internal form to erl_syntax.
%% 
%% Note:
%% - This is intended to be used when transforming from the internal
%%   representation to Erlang source. Thus certain structures has a "meaning".
inter_to_erlsyn(#macro_expr{op=Op,args=Args}) ->
    MacName=erl_syntax:text(Op),
    MacArgs=if
		Args==undefined -> none;
		true -> [inter_to_erlsyn(Arg) || Arg <- Args]
	    end,
    erl_syntax:macro(MacName,MacArgs);    
inter_to_erlsyn(#export_def{args=Args}) ->
    E1=[erl_syntax:arity_qualifier(inter_to_erlsyn(V),inter_to_erlsyn(A))
	|| {V,A} <- Args],
    Exports=[erl_syntax:list(E1)],
    erl_syntax:attribute(erl_syntax:atom(export),Exports);
inter_to_erlsyn(#include_def{args=Args}) ->
    [erl_syntax:attribute(erl_syntax:atom(include),inter_to_erlsyn(A))
     || A <- Args];
inter_to_erlsyn(#case_expr{arg=Arg,clauses=Clauses}) ->
    TreeList=[inter_to_erlsyn(C) || C <- Clauses],
    erl_syntax:case_expr(inter_to_erlsyn(Arg),TreeList);
    %% case Arg of
    %% 	undefined ->
    %% 	    erl_syntax:if_expr(TreeList);
    %% 	_ ->
    %% 	    erl_syntax:case_expr(inter_to_erlsyn(Arg),TreeList)
    %% end;
inter_to_erlsyn(#if_expr{clauses=Clauses}) ->
    TreeList=[inter_to_erlsyn(C) || C <- Clauses],
    erl_syntax:if_expr(TreeList);
inter_to_erlsyn(#clause_expr{pattern=ArgList,
			     guard=Guard,
			     body=BodyList,
			     comments=CommentsTree}) ->
    GuardTree=case Guard of
		  undefined -> none;
		  #expr{x=true} -> none;
		  #expr{x=X} -> inter_to_erlsyn(X);
		  _ -> inter_to_erlsyn(Guard)
	      end,
    BodyTree=[inter_to_erlsyn(Expr) || Expr <- BodyList],
    ClauseTree=case ArgList of
		   undefined ->
		       erl_syntax:clause(GuardTree,BodyTree);
		   _ ->
		       PatternTree=[inter_to_erlsyn(Expr) || Expr <- ArgList],
		       erl_syntax:clause(PatternTree,GuardTree,BodyTree)
	       end,
    case CommentsTree of
	undefined ->
	    ClauseTree;
	_ ->
	    erl_syntax:set_precomments(ClauseTree,CommentsTree)
    end;
inter_to_erlsyn({clause,ArgList,GuardTree,BodyTree}) ->
    PatternTree=[inter_to_erlsyn(Expr) || Expr <- ArgList],
    erl_syntax:clause(PatternTree,GuardTree,BodyTree);
inter_to_erlsyn({clause,ArgList,GuardTree,BodyTree,Comments}) ->
    PatternTree=[inter_to_erlsyn(Expr) || Expr <- ArgList],
    CommentsTree=[erl_syntax:comment(Comments)],
    ClauseTree=erl_syntax:clause(PatternTree,GuardTree,BodyTree),
    erl_syntax:set_precomments(ClauseTree,CommentsTree);
inter_to_erlsyn(#syntax_expr{x=X}) ->
    format_expr_syntax(X);
inter_to_erlsyn(#module_def{name=Name}) ->
    erl_syntax:attribute(erl_syntax:atom(module),[erl_syntax:atom(Name)]);
inter_to_erlsyn(#record_def{type=Type,fields=Fields}) ->
    FieldTreeList=format_expr_fieldlist(Fields,[]),
    erl_syntax:attribute(erl_syntax:atom(record),
			 [erl_syntax:atom(Type),
			  erl_syntax:tuple(FieldTreeList)]);
inter_to_erlsyn(#record_expr{arg=Arg,type=Type,fields=Fields}) ->
    FieldTreeList=format_expr_fieldlist(Fields,[]),
    case Arg of
	undefined ->
     	    erl_syntax:record_expr(erl_syntax:atom(Type),FieldTreeList);
	_ ->
	    erl_syntax:record_expr(inter_to_erlsyn(Arg),
				   erl_syntax:atom(Type),
				   FieldTreeList)
    end;
inter_to_erlsyn(#record_access{arg=Arg,type=Type,field=Field}) ->
    erl_syntax:record_access(inter_to_erlsyn(Arg),
			     erl_syntax:atom(Type),
			     erl_syntax:atom(Field));
inter_to_erlsyn(#match_expr{l=L,r=R,comments=CommentsStr}) ->
    case CommentsStr of
	[] ->
	    erl_syntax:match_expr(inter_to_erlsyn(L),inter_to_erlsyn(R));
	_ ->
	    Comment=erl_syntax:comment(-1,CommentsStr),
	    Tree=erl_syntax:match_expr(inter_to_erlsyn(L),inter_to_erlsyn(R)),
	    erl_syntax:add_precomments([Comment],Tree)
    end;
inter_to_erlsyn(#em_userfun{op=Op,clauses=Clauses}) ->
    FunName=inter_to_erlsyn(Op),
    FunClauses=[inter_to_erlsyn(Clause) || Clause <- Clauses],
    erl_syntax:function(FunName,FunClauses);
inter_to_erlsyn(#application{op='catch',args=[Arg]}) ->
    %% Note: This is really cheating as we thus cannot have a function named
    %%       catch...
    erl_syntax:catch_expr(inter_to_erlsyn(Arg));
inter_to_erlsyn(#application{op=Op,args=ArgList}) ->
    ArgTree=[inter_to_erlsyn(Expr) || Expr <- ArgList],
    if
	Op=='+';Op=='-';Op=='*';Op=='/' ->
	    to_infix(erl_syntax:operator(Op),ArgTree);
	is_atom(Op);is_list(Op) ->
	    erl_syntax:application(erl_syntax:atom(Op),ArgTree);
	true ->
	    {Mod,FunName}=Op,
	    erl_syntax:application(erl_syntax:atom(Mod),
				   erl_syntax:atom(FunName),ArgTree)
    end;
inter_to_erlsyn(#variable{name=Name}) ->
    erl_syntax:variable(Name);
inter_to_erlsyn(#infix_expr{l=L,op=Op,r=R}) ->
    NewL=inter_to_erlsyn(L),
    NewR=inter_to_erlsyn(R),
    erl_syntax:infix_expr(NewL,erl_syntax:operator(Op),NewR);
inter_to_erlsyn(#bool{var=L}) ->
    NewL=inter_to_erlsyn(L),
    NewR=inter_to_erlsyn(true),
    erl_syntax:infix_expr(NewL,erl_syntax:operator('=='),NewR);
inter_to_erlsyn({my_not,#bool{var=L}}) ->
    NewL=inter_to_erlsyn(L),
    NewR=inter_to_erlsyn(false),
    erl_syntax:infix_expr(NewL,erl_syntax:operator('=='),NewR);
inter_to_erlsyn({my_or,OrList}) ->
    case OrList of
	[L,R] ->
	    NewL=inter_to_erlsyn(L),
	    NewR=inter_to_erlsyn(R),
	    erl_syntax:infix_expr(NewL,erl_syntax:operator('or'),NewR);
	[L|Rest] ->
	    NewL=inter_to_erlsyn(L),
	    NewR=inter_to_erlsyn({my_or,Rest}),
	    erl_syntax:infix_expr(NewL,erl_syntax:operator('or'),NewR)
    end;
inter_to_erlsyn({my_and,AndList}) ->
    case AndList of
	[L,R] ->
	    NewL=inter_to_erlsyn(L),
	    NewR=inter_to_erlsyn(R),
	    erl_syntax:infix_expr(NewL,erl_syntax:operator('and'),NewR);
	[L|Rest] ->
	    NewL=inter_to_erlsyn(L),
	    NewR=inter_to_erlsyn({my_and,Rest}),
	    erl_syntax:infix_expr(NewL,erl_syntax:operator('and'),NewR)
    end;
inter_to_erlsyn(Int) when is_integer(Int) ->
    erl_syntax:integer(Int);
inter_to_erlsyn(List) when is_list(List) ->
    TreeList=[inter_to_erlsyn(Expr) || Expr <- List],
    erl_syntax:list(TreeList);
inter_to_erlsyn(Tuple) when is_tuple(Tuple) ->
    TupleList=tuple_to_list(Tuple),
    TupleTree=[inter_to_erlsyn(Expr) || Expr <- TupleList],
    erl_syntax:tuple(TupleTree);
inter_to_erlsyn(Expr) when is_atom(Expr) ->
    erl_syntax:atom(Expr);
inter_to_erlsyn(Pid) when is_pid(Pid) ->
    Expr=lists:flatten(io_lib:format("~p",[Pid])),
    erl_syntax:atom(Expr);
inter_to_erlsyn(Other) ->
    io:format("inter_to_erlsyn ERROR=~p~n",[Other]),
    throw({error,unsupported_syntax}).
    

format_expr_fieldlist([],Out) ->
    lists:reverse(Out);
format_expr_fieldlist([{Field,Expr}|Rest],Out) ->
    Tree=erl_syntax:record_field(erl_syntax:atom(Field),inter_to_erlsyn(Expr)),
    format_expr_fieldlist(Rest,[Tree|Out]);
format_expr_fieldlist([Field|Rest],Out) ->
    Tree=erl_syntax:record_field(erl_syntax:atom(Field)),
    format_expr_fieldlist(Rest,[Tree|Out]).


format_expr_syntax(X) when is_integer(X);
			   is_atom(X);
			   is_pid(X) ->
    inter_to_erlsyn(X);
format_expr_syntax(List) when is_list(List) ->
    TreeList=[format_expr_syntax(Expr) || Expr <- List],
    erl_syntax:list(TreeList);
format_expr_syntax(Tuple) when is_tuple(Tuple) ->
    TupleList=tuple_to_list(Tuple),
    TupleTree=[format_expr_syntax(Expr) || Expr <- TupleList],
    erl_syntax:tuple(TupleTree).

to_infix(OpTree,[Arg1,Arg2]) ->
    erl_syntax:infix_expr(Arg1,OpTree,Arg2);
to_infix(OpTree,[Arg1|Rest]) ->
    erl_syntax:infix_expr(Arg1,OpTree,to_infix(OpTree,Rest)).


%%% ----------------------------------------------------------------------------

%%% Record definitions
create_record_defs([],Out) ->
    lists:reverse(Out);
create_record_defs([H0|Rest],Out) ->
    H1=inter_to_erlsyn(H0),
    RecordDef=erl_prettypr:format(H1)++io_lib:format("~n~n",[]),
    create_record_defs(Rest,[RecordDef|Out]).


