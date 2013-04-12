%% consistency.incomplete -> no suitable publishers found
%% consistency.missing    -> remote found, no local copy
%% consistency.unknown    -> local copy found, needs cloning
%% consistency.stale      -> local copy found, remote has update
%% consistency.consistent -> local copy found, is up to date
-record(dep, {
	  name  :: binary()
	, repo = [] :: [{binary(), URL::binary()}] %% Repos containing a match
	, repos = [] :: [binary()] %% Repos allowed to search in
	, version = any :: string() | any
	, ref = any :: string() | any
	, deps = [] :: [#dep{}]
	, consistency = incomplete :: incomplete | unknown | stale | consistent
	}).

-record(cfg, {
	  repos = [] :: [{binary(), binary()}]
	, deps = [] :: [#dep{}]
	, paths = [] :: tuple(binary())
	}).

-define(epmpp(Path), "bin/epmpp " ++ Path).

-define(backends, [epm_backend_github, epm_backend_git, epm_backend_file]).

-define(plugins, [epm_plugin_conf, epm_plugin_rebar]).
