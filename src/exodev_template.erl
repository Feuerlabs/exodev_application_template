%% Copyright (C) 2011, Feuerlabs, Inc. All rights reserved.
%% Redistribution and use in any form, with or without modification, is strictly prohibited.
%%

-module(exodev_template).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    inpevt_sup:start_link().

stop(_State) ->
    ok.
