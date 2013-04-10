-record(dep, {
	  name  :: binary()
	, repo = [] :: [{binary(), URL::binary()}] %% Repos containing a match
	, repos = [] :: [binary()] %% Repos allowed to search in
	, version = any :: string() | any
	, ref = any :: string() | any
	}).

-record(cfg, {
	  repos = [] :: [{binary(), binary()}]
	, deps = [] :: [#dep{}]
	}).

-define(epmpp, "bin/epmpp").

-define(backends, [epm_backend_github, epm_backend_git, epm_backend_file]).
