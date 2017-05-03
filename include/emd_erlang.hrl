%%% A number of simplified records, to represent Erlang expressions.


%% Module definition
-record(module_def,{
	  name
	 }).

%% Export definition
-record(export_def,{
	  args
	 }).

%% Include definition
-record(include_def,{
	  args
	 }).

%% Type definition
-record(type_def,{
	  name,
	  values
	 }).

%% Record definition
-record(record_def,{
	  type,
	  fields
	 }).


-record(syntax_expr,{
	  x
	 }).


%% Represents a function where the clauses have two possible representations
%% Note:
%% - User defined functions are pre-proccesed. After this pre-proccesing
%%   clauses are represented on spec_rule format
-record(em_userfun,{
	  op,
	  arity,
	  clauses
%	  format=clause_expr % (clause_expr | spec_rule) Clause representation
	 }).


%% Internal Erlang function
-record(application,{
	  op,    % (atom | {atom,atom}) Function name or {Module,Function} names
	  args,  % (list) Arguments
	  val_type  % Type dependent info
	 }).


-record(clause_expr,{
	  pattern,
	  guard,
	  body=[],
	  comments,
	  pos       % Start line 
	 }).

-record(case_expr,{
	  arg,
	  clauses
	 }).

-record(if_expr,{
	  clauses
	 }).


%%% "Pattern = Body".
-record(match_expr,{
	  l,     % (#variable{}) Expression (Pattern)
	  r,     % (#variable{}, erl_syntax or atom) Expression (Body)
	  comments=[]
	 }).

%%% arg#type{fields}
-record(record_expr,{
	  arg,
	  type,
	  fields
	 }).

%%% arg#type.field
-record(record_access,{
%	  indexvar, % #variable Set by stop db to disinguish on which variable
%		    % it depends
	  arg,
	  type,
	  field,
	  domain=2,   % (integer) Domain size, default is a boolean [0,1]
	  domain_vals=boolean % List with enum values (if any)
	 }).

-record(infix_expr,{
	  l,      % (erl_syntax or my_expt) Expression
	  op='==',% ('==', '=/=') Operator
	  r       % (erl_syntax or my_expt) Expression
	 }).

%% --var_type--  ---val_type---
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
