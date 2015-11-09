%% config macros

-ifndef(__CONFIG_HRL__).
-define(__CONFIG_HRL__, true).


-define(CFGNAME, barrel).

-define(cfall(), econfig:cfg2list(?CFGNAME)).

-define(cfget(Section), econfig:get_value(?CFGNAME, Section)).
-define(cfget(Section, Key), econfig:get_value(?CFGNAME, Section, Key)).
-define(cfget(Section, Key, Default), econfig:get_value(?CFGNAME, Section, Key, Default)).

-define(cfset(Section, KVs), econfig:set_value(?CFGNAME, KVs)).
-define(cfset(Section, Key, Value), econfig:set_value(?CFGNAME, Section, Key, Value)).
-define(cfset(Section, Key, Value, Persist), econfig:set_value(?CFGNAME, Section, Key, Value, Persist)).

-define(cfdel(Section, Key), econfig:delete_value(?CFGNAME, Section, Key)).
-define(cfdel(Section, Key, Persist), econfig:delete_value(?CFGNAME, Section, Key, Persist)).

-define(cfget_bool(Section, Key), econfig:get_boolean(?CFGNAME, Section, Key)).
-define(cfget_bool(Section, Key, Default), econfig:get_boolean(?CFGNAME, Section, Key, Default)).

-define(cfget_int(Section, Key), econfig:get_integer(?CFGNAME, Section, Key)).
-define(cfget_int(Section, Key, Default), econfig:get_integer(?CFGNAME, Section, Key, Default)).

-define(cfget_float(Section, Key), econfig:get_float(?CFGNAME, Section, Key)).
-define(cfget_float(Section, Key, Default), econfig:get_float(?CFGNAME, Section, Key, Default)).

-define(cfget_list(Section, Key), econfig:get_list(?CFGNAME, Section, Key)).
-define(cfget_list(Section, Key, Default), econfig:get_list(?CFGNAME, Section, Key, Default)).

-define(cfget_bin(Section, Key), econfig:get_binary(?CFGNAME, Section, Key)).
-define(cfget_bin(Section, Key, Default), econfig:get_binary(?CFGNAME, Section, Key, Default)).

-endif.