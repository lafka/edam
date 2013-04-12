-record(dep, {
	  name  :: binary()
	, repo = [] :: [{binary(), URL::binary()}] %% Repos containing a match
	, repos = [] :: [binary()] %% Repos allowed to search in
	, version = any :: string() | any
	, ref = any :: string() | any
	, deps = [] :: [#dep{}]
	}).

-record(cfg, {
	  repos = [] :: [{binary(), binary()}]
	, deps = [] :: [#dep{}]
	, paths = [] :: tuple(binary())
	}).

-define(epmpp(Path), "bin/epmpp " ++ Path).

-define(backends, [epm_backend_github, epm_backend_git, epm_backend_file]).

-define(plugins, [epm_plugin_conf, epm_plugin_rebar]).
