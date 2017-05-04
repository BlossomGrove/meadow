-define(logic_false,0).
-define(logic_true,1).


%%% Wrapper around a logical expression
-record(expr,{
	  x,       % The logical expression
	  form=any,% (atom) Form on expression, supported forms are:
	           %   dnf - Disjunctive Normal Form
	           %   bdd - ROBDD (Binary Decision Diagram)
	           %   mdd - ROMDD (Multi-Level Decision Diagram)
	           %   any - Undecided symbolic (Default) 
	  env % Form specific environment
	           %   dnf - undefined
	           %   bdd - #bddenv{}
	           %   mdd - #mddenv{}
	           %   any - undefined
	 }).

%%% ............................................................................
%%% Symbolic specifics

-record(infix_expr,{
	  l,      % (erl_syntax or my_expt) Expression
	  op='==',% ('==', '=/=') Operator
	  r       % (erl_syntax or my_expt) Expression
	 }).


%%% A boolean is an infix_expr L==R where L is a variable and R is true 
-record(bool,{
	  var   % (#variable{}) Boolean variable 
	 }).


%%% An integer domain specificaton 
-record(integer_dom,{
	  min,
	  max
	 }).



%% ---var_type---     ---val_type---
%% eventvar    - #invar_i{} 
%% localvar    - undefined | #invar_i{} 
%% statevar    - undefined | #invar_i{} 
%% event_trace - #trace_i{}
%% abstraction - undefined | #invar_entry{} 
%%                 The latter if original expression contains event parameters 
%% obslocvar   - #invar_i{}
%% The var_type indicates the type of variable
%% The val_type indicates the type of the value of the variable
-record(variable,{
	  name,    % (atom) Name. Note: Used as key
	      %% spectool_eval:eval_atomic_expr/3 ...
	  var_type, % (localvar | statevar | invar | abstraction) 
	  val_type  % Type dependent info
	 }).


%% State/Local variable
-record(var_i,{
	  domain=2,   % (integer) Domain size, default is a boolean [0,1]
	  domain_vals=boolean % List with enum values (if any)
	 }).

%% Event parameter
-record(invar_i,{
	  index,      % (integer) Last Index, when multiple copies of same
		      %    inparameter name.
	  domain=2,   % (integer) Domain size, default is a boolean [0,1]
	  domain_vals=boolean,% List with enum values (if any)
	  event_type  % (atom)
	 }).

%% Event parameter, that represents a record
-record(trace_i,{
	  index=0,  % (integer) Last Index
	  domain,   % (integer) Domain size, ie number of possible inevents
	  domain_vals=boolean % List with enum values (if any)
	 }).

%% Configuration functions
-record(conf_i,{
	  domain=2,   % (integer) Domain size, default is a boolean [0,1]
	  domain_vals=boolean % List with enum values (if any)
	 }).


%%% ............................................................................
%%% Backend specifics

-record(mddenv,{
	  t, % U -> {I,Type,ChildList}
	  h, % {I,Type,ChildList} -> U
	  gand, % Results of conjunctions between nodes (U1 and U2 -> Res)
	  gor,  % Results of disjunctions between nodes (U1 or U2 -> Res)
	  vardb,% Enumeration of all variables occuring
%	  recdb,% List with all record definitions
		%   [{RecName,[Field1,Field2,...]},...]
	  cache % Expr -> U
	 }).

%%% Experimental: For backward linking 
%% -record(node,{
%% 	  u,
%% 	  backlink=[]
%% 	  }).

-record(bddenv,{
	  t, % U -> {I,Type,ChildList}
	  h, % {I,Type,ChildList} -> U
	  gand, % Results of conjunctions between nodes (U1 and U2 -> Res)
	  gor,  % Results of disjunctions between nodes (U1 or U2 -> Res)
	  vardb,% Enumeration of all variables occuring
	  cache % Expr -> U
	 }).


-record(dnfenv,{
	  vardb % Enumeration of all variables occuring
	 }).
