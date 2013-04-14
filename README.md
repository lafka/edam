# Introduction

Todays state of erlang dependency manager is, work in progress...
EPM will try to handle working dependencies with dependencies in a more
controlled way.

https://hyperthunk.wordpress.com/2012/05/28/does-erlangotp-need-a-new-package-management-solution/
http://lists.basho.com/pipermail/riak-users_lists.basho.com/2013-April/011777.html
http://erlang.org/pipermail/erlang-questions/2012-October/069825.html

Features:

+ Deterministic dependencies
+ Dynamic handling of repository/dependency remotes, branches etc
+ Handles catalogs of dependencies vs single dependency
+ When specifying a version make sure that version will be used
	(even for recursive dependencies) instead of random error because of
	update-deps tried to checkout a non existing ref and now you don't
  have an .app file.....
+ Support for multiple repository backends (only github for now)
+ Compiles to single binary without requirement for erlang (maybe?)

Will probably add support for dependency snapshots, which would lock
dependencies at commit time (only git support?).

## Catalogs

All `packages` resides within 1 catalog, the catalog is managed by a
developer or an organization. Catalogs contains a list of `packages`
and additionally a set of keys used for signing (not implemented).

Catalogs must export 4 functions:

`name/0 :: binary()`
Unique name used for path generation

`match/1 :: (binary()) -> boolean()`
Match function determining if a resource belongs to the catalog.

`resource/2 :: (#pkg{}, #ctl{}) -> URL :: binary()`
Build a URL used for fetching remote

`fetch/1 :: (A :: binary()) -> #catalog{}`
Fetches list of packages from remote server

`search/1 :: (A :: binary()) -> [#pkg{}]`
Search the remote catalog for package, returns a list of matching
packages.

`sync/1 :: (Resource :: binary(), _) -> boolean()`
Syncs a package to the local store. The second argument is reserved
for `source handler` hinting.

## Packages

A remote resource containing some source code. Once synced additional
information can be retrieved, mainly the version number and a list of
additional dependencies. This information is stored in one or more
configuration files (ie. `rebar.config` and `*.app`).

## Parsers

To be able to handle a multitude of configuration variants several
parsers are bundled with EPM. Each parser acts as an adapter to other
build tools/package managers.

A parser must export the following functions:

`parse/2 :: (file:filename(), epm_pkg:pkg()) ->
	{[epm_catalog:catalog()], [epm_pkg:pkg()]} | false`
Parses the configuration for a file, returns the catalogs/pkgs found
or false if a error occured (if a path is not valid for that config,
it's not considered an error and should return `{[], []}`.

## Agents

All the action is handled by a plug-in agent, there will typically
be 1 agent pr. version control system. GIT is the only supported
backend at the moment. In the future a plug-in for local filesystem
and HTTP might be added.

Agents must export the following functions:

`name/0` :: () -> binary()`
Return the name of the agent (ie `git`, `http`)

`init/1 :: (epm_pkg:pkg()) -> epm_pkg:pkg()`
Initiate the agent for a package, this will be called during config
parsing and can be used to set any additional options for the pkg.

`fetch/2 :: (epm_pkg:pkg(), epm:cfg()) -> ok | {error, Reason :: term()}`
Checks if there are any available updates for this package.

`sync/2 :: (epm_pkg:pkg(), epm:cfg()) -> ok | {error, Reason :: term()}`
Syncs the local copy of the package with any remote updates.o

`status/2` :: (epm_pkg:pkg(), epm:cfg()) -> ok | stale | unknown`
Check the current known state of the package - sideeffect free.

## env/config notes

+ __opt:ignore_codepath__: Flag to tell backends to not change _codepath_,
  useful when only checking to see if there are updates (git fetch)
+ __env:dryrun__: Don't do any hard work, check should be implemented
  in backend private methods.
+ __env:autofetch__: Automatically fetch everything that's needed.
+ __env:cachedir__: The root cache directory
+ __env:libdir__: Where to store those precious libraries.
+ __env:verbosity__: What level of output to give

# Examples

## Usage (Erlang API)

### Lists all package versions

```erlang
1> {ok, Cfg} = epm:parse("."),
1> epm_pkg:map(fun(Pkg) -> epm_pkg:get([name, version], Pkg) end, Cfg).
[[<<"lager">>,<<"1.2.1">>],[<<"cowboy">>,<<"0.8.3">>]]
```

### Sync all dependencies recursively

```erlang
%% epm:parse/1 is side-effect free, therefor a explicit call to fetch
%% is required.
2> Fetch = fun(Pkg, Fun) ->
2>	epm_pkg:sync(Pkg),
2>	{ok, Cfg} = epm:parse(epm_pkg:get(path, Pkg)),
2>	Fun(Cfg)
2> end,
2> epm_pkg:foreach(fun(Pkg) -> Fetch(Pkg, Fetch) end, Cfg).
ok
```

### Find conflicting packages (the naive way)

The following example expands the dependencies to their version which
can easily be used to lookup possible version conflicts.

```erlang
{ok, Cfg} = epm:parse("."),
Pkgs0 = epm_pkg:flatten(Cfg),
Pkgs1 = [epm_pkg:get([name, catalog, version], P) || P <- Pkgs0],
Pkgs = lists:usort(Pkgs1), % Fixes identical duplicates
{_, Duplicates0} = lists:foldl(
		fun([Name|_] = P, {Acc0, Acc1}) ->
			case lists:keyfind(Name, 1, Acc0) of
				{Name, Prev} -> {Acc0, [P,Prev | Acc1]};
				false -> {[{Name, P} | Acc0], Acc1}
			end
		end, {[], []}, Pkgs),
Duplicates = lists:usort(Duplicates0). % Fold copies first value multiple times
```

## Usage (CLI)

__Drop in replacement for Rebar:__

Check what it tells you from a rebar config, this does nothing but fetch
a list of available repositories from Basho's organization on github.

```bash
/tmp> git clone git://github.com/basho/riak.git && cd riak
master /tmp/riak> git checkout riak-1.3.1
Note: checking out 'riak-1.3.1'.
.....
66706a0 /tmp/riak> ~/src/epm/bin/epm deps print

Config:
=======

Repositories:
= github.basho (git://github.com/basho/riaknostic); 118 packages

Dependencies:
= riaknostic:any  [<<"github.basho">>]; requires remote fetch: remote -> git://github.com/basho/riaknostic.git
= riak_control:any  [<<"github.basho">>]; requires remote fetch: remote -> git://github.com/basho/riak_control.git
= riak_search:any  [<<"github.basho">>]; requires remote fetch: remote -> git://github.com/basho/riak_search.git
= riak_kv:any  [<<"github.basho">>]; requires remote fetch: remote -> git://github.com/basho/riak_kv.git
= cluster_info:any  [<<"github.basho">>]; requires remote fetch: remote -> git://github.com/basho/cluster_info.git
= lager_syslog:any  [<<"github.basho">>]; requires remote fetch: remote -> git://github.com/basho/lager_syslog.git
```

Now we can sync and see what happens, this will pull down all deps
recursively. It will try to match specific version if specified in the
rebar config:

```bash
66706a0 /tmp/riak> ~/src/epm/bin/epm deps sync
[info] 03:02:01 -> dep: github.basho:riaknostic, checking out master copy @git://github.com/basho/riaknostic.git
[info] 03:02:03 -> checking out local copy of github.basho:riaknostic=v1.1.0
[info] 03:02:03 -> dep: github.basho:riak_control, checking out master copy @git://github.com/basho/riak_control.git
[info] 03:02:08 -> checking out local copy of github.basho:riak_control=1.3.1
....
[info] 03:03:15 -> picking 'riak_kv' from github.basho @ git://github.com/basho/riak_kv.git
[info] 03:03:16 -> picking 'cluster_info' from github.basho @ git://github.com/basho/cluster_info.git
```

We can now see we filled up the `lib/` directory:

```bash
66706a0 /tmp/riak> ls -l lib 
total 30
drwxr-xr-x  6 olav users 240 Apr 14 03:03 basho_stats-1.0.3
drwxr-xr-x  4 olav users 160 Apr 14 03:03 bear-0.1.2
drwxr-xr-x 10 olav users 380 Apr 14 03:03 bitcask-1.6.1
....
```

A final call to print will now tell us a bit more about our project:

```bash
Config:
=======

Repositories:
= github.Vagabond (git://github.com/Vagabond/erlang-syslog); 26 packages
= github.basho (git://github.com/basho/lager_syslog); 118 packages
= github.boundary (git://github.com/boundary/bear.git); 32 packages
= github.eproxus (git://github.com/eproxus/meck); 21 packages
= github.esl (git://github.com/esl/edown.git); 47 packages
= github.evanmiller (git://github.com/evanmiller/erlydtl.git); 30 packages
= github.jcomellas (git://github.com/jcomellas/getopt.git); 20 packages
= github.uwiger (git://github.com/uwiger/sext); 20 packages

Dependencies:
= syslog:1.0.1 (lager_syslog) ; no suitable publishers found
= lager:1.2.2 (lager_syslog) [<<"github.basho">>];
= erlydtl:c18f2a00 (riak_control) [<<"github.evanmiller">>];
....
= lager:1.2.2 (riaknostic) [<<"github.basho">>];
```
