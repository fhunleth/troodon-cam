-module(troodon_cam).

-export([init/0, doit/0]).

init() ->
    file:write_file("/sys/devices/bone_capemgr.9/slots", <<"troodon-cam">>),
    gpio:start_link({48, output}),
    gpio:write(48, 1).

doit() ->
    subprocess:cmdpp(code:priv_dir(troodon_cam) ++ "/troodon_cam").
