-module(troodon_cam).

-export([init/0]).

init() ->
    file:write_file("/sys/devices/bone_capemgr.9/slots", <<"troodon-cam">>),
    gpio:start_link({48, output}),
    gpio:write(48, 1).
