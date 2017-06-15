-define(error(Format,Args),
	emd_log:error(Format,Args,
		      [?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY,?LINE])).
-define(error(Format),
	emd_log:error(Format,[],
		      [?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY,?LINE])).
-define(warning(Format,Args),
	emd_log:warning(Format,Args,
			[?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY,?LINE])).
-define(warning(Format),
	emd_log:warning(Format,[],
			[?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY,?LINE])).
-define(info(Format,Args),
	emd_log:info(Format,Args,
		     [?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY,?LINE])).
-define(info(Format),
	emd_log:info(Format,[],
		     [?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY,?LINE])).
-define(debug(Format,Args),
	emd_log:debug(Format,Args,
		      [?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY,?LINE])).
-define(debug(Format),
	emd_log:debug(Format,[],
		      [?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY,?LINE])).