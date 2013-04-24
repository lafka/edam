-module(epm_agent_void).

-export([
	  name/0
	, init/1
	, fetch/2
	, sync/2
	, status/2
	]).

name() ->
	<<"void">>.

init(Pkg) ->
	epm_pkg:set(path, buildpath(Pkg), Pkg).

-spec fetch(epm_pkg:pkg(), epm:cfg()) -> ok.
fetch(_Pkg, _Cfg) ->
	ok.

-spec sync(epm_pkg:pkg(), epm:cfg()) -> ok.
sync(_Pkg, _Cfg) ->
	ok.

-spec status(epm_pkg:pkg(), epm:cfg()) -> ok.
status(_Pkg, _Cfg) ->
	ok.

buildpath(Pkg) ->
	% case epm:get(append_versions, true, Cfg) of
	Suffix = case epm:env(append_versions, true) of
		true ->
			case epm_pkg:get(version, Pkg) of
				any -> <<>>;
				Version -> <<$-, Version/binary>> end;
		false ->
			<<>> end,

	filename:join([epm:env(libdir, <<"lib">>)
		, <<((epm_pkg:get(name, Pkg)))/binary, Suffix/binary>>
		]).
