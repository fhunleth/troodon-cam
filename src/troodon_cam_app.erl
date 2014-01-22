-module(troodon_cam_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% One-time initialization of the system to load the troodon-cam
    %% device tree overlay
    file:write_file("/sys/devices/bone_capemgr.9/slots", <<"troodon-cam">>),

    troodon_cam_sup:start_link().

stop(_State) ->
    ok.
