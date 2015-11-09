-module(barrel_config).

-export([init/1]).
-export([handle_config_change/1]).

-export([subscribe/0, unsubscribe/0]).

-include("config.hrl").

subscribe() ->
	econfig:subscribe(?CFGNAME).

unsubscribe() ->
	econfig:unsubscribe(?CFGNAME).

init(IniFiles) ->
	ok = wait_for_econfig(),
	econfig:register_config(?CFGNAME, IniFiles, [autoreload,
	                        				     {change_fun, {barrel_config, handle_config_change}}]).

handle_config_change({config_updated, ?CFGNAME, {Type, {Section, Key}}}) ->
	hooks:run(config_key_update, [Section, Key, Type]);
handle_config_change({config_updated, ?CFGNAME, Type}) ->
	hooks:run(config_update, Type);
handle_config_change(_) ->
	ok.

%% @private
wait_for_econfig() ->
	case whereis(econfig_sup) of
		undefined ->
			timer:sleep(10),
			wait_for_econfig();
		_Pid ->
			ok
	end.