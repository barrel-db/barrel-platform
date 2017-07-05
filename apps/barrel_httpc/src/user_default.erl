-module(user_default).
-compile(export_all).


setup() ->
    sync:go(),
		   RunTests = fun(Mods) ->

													[eqc:module(Mod) || Mod <- Mods, erlang:function_exported(Mod, test, 0)]
									end,
    sync:onsync(RunTests),
		ok.


eqc() ->
		eqc:module(barrel_httpc_eqc).
