-module(cowboy_swagger_redirect_handler).

%% Cowboy callbacks
-export([init/3]).

%% Handlers
-export([resource_exists/2, previously_existed/2, moved_permanently/2]).

-type state() :: #{}.
-type options() :: any().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cowboy Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init({atom(), atom()}, cowboy_req:req(), options()) ->
  {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handlers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec resource_exists(Req::cowboy_req:req(), State::state()) ->
  {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
  {false, Req, State}.

%% @hidden
-spec previously_existed(Req::cowboy_req:req(), State::state())->
  {boolean(), cowboy_req:req(), state()}.
previously_existed(Req, State) ->
  {true, Req, State}.

%% @hidden
-spec moved_permanently(Req::cowboy_req:req(), State::state()) ->
  {{boolean(), string()}, cowboy_req:req(), state()}.
moved_permanently(Req, State) ->
  {{true, "/api-docs/index.html"}, Req, State}.
