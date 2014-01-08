DIALYZER_OPTS = -Wrace_conditions -Wunderspecs

all: compile

# Help rebar figure out the C file dependencies
c_src/camera.c: c_src/pru_camera.p c_src/pru_camera.hp
	touch c_src/camera.c

compile: c_src/camera.c
	rebar compile

DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt
ERLANG_APPS=erts kernel stdlib

$(DEPSOLVER_PLT):
		dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
					--apps $(ERLANG_APPS) 

dialyzer: $(DEPSOLVER_PLT)
		dialyzer --plt $(DEPSOLVER_PLT) $(DIALYZER_OPTS) --src src

typer: $(DEPSOLVER_PLT)
		typer --plt $(DEPSOLVER_PLT) -r ./src

clean:
	rebar clean

.PHONY: dialyzer typer clean compile
