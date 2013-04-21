# Introduction

Todays state of erlang dependency manager is not optimal.
EPM was designed to handle dependencies in more controlled way.

https://hyperthunk.wordpress.com/2012/05/28/does-erlangotp-need-a-new-package-management-solution/
http://lists.basho.com/pipermail/riak-users_lists.basho.com/2013-April/011777.html
http://erlang.org/pipermail/erlang-questions/2012-October/069825.html

## Features:

+ Drop-in replacement for Rebar dependency management
+ Pr. project configuration
+ Deterministic checkout, no need to deps at specific position in config
+ Modular codebase with hooks for most common operations
+ Supports remote catalogs for simplifying dependency management

## Features coming to you soon:

+ Variations of dependencies based on env, allows for using package
  different versions during development/testing/prod/etc
+ Compiles to single binary without requirement for erlang (maybe?)
+ Release signing / key trust for packages
+ Isolated checkout, dependency will checkout required deps on it's
  own. Good when building multiple different applications


### Needs hacking:

#### Parsing
+ Add a `facts` to provide default options like libdir, append_versions (config is always parsed for deps, but facts are only processed for isolate/root pkgs)
+ Add support for overriding pkg config by their abspath: `tavern/cowboy [version = "0.8.3"]` or `*//cowboy[version = "0.8.3"]` for all cowboy deps
+ Validate if should be in global ETS storage.
+ Add async operations for checkout and friends, build 'action list' returned
  by parse/{0,1} which should define operations required for fixing
  consistency errors before main operation can process (<- think twice)

#### Checkout
+ Make libdir/append_versions pkg specific (in config).
+ Add isolate option to build pr.pkg local depedency tree
+ Fix automatic catalog selection AND catalog priority
+ Make cached version of git use mirror to forward remote branches

#### Performance
+ Flatten and merge deps list before iterating

##### Conflicts
+ Find and warn about packages with version conflicts.
+ Find and warn about auto configured packages with multiple available backends

# Various resolve functionality
+ Fix cache expiry of github catalogs
+ Add HTTP agent
+ Add local agent
+ Add gitorious catalog
+ Add void catalog (1:1 repo mapping)
+ Add bitbuck catalog
+ Add localfs catalog
+ Add search functionality to github catalog


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
agent at the moment. In the future a plug-in for local filesystem
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
1> lists:usort(epm_pkg:map(fun(Pkg) -> epm_pkg:get([name, version], Pkg) end, epm_pkg:flatten(Cfg))).
[[<<"lager">>,<<"1.2.1">>],[<<"cowboy">>,<<"0.8.3">>],[<<"ranch">>,<<"0.8.1">>]]
```

### Sync all known dependencies

```erlang
%% epm:parse/1 is side-effect free, therefor a explicit call to fetch
%% is required.
2> epm_pkg:foreach(fun(Pkg) -> epm_pkg:sync(Pkg, Cfg) end, epm_pkg:flatten(Cfg)).
ok
```

### Find conflicting versions of packages (the naive way)

The following example expands the dependencies to their version which
can easily be used to lookup possible version conflicts.

```erlang
42> {ok, Cfg} = epm:parse("."),
42> Pkgs0 = epm_pkg:flatten(Cfg),
42> Pkgs1 = [epm_pkg:get([name, version], P) || P <- Pkgs0],
42> Pkgs = lists:usort(Pkgs1), % Remove identical duplicates
42> {_, Duplicates0} = lists:foldl(
42> 		fun([Name|_] = P, {Acc0, Acc1}) ->
42> 			case lists:keyfind(Name, 1, Acc0) of
42> 				{Name, Prev} -> {Acc0, [P,Prev | Acc1]};
42> 				false -> {[{Name, P} | Acc0], Acc1}
42> 			end
42> 		end, {[], []}, Pkgs),
42> lists:usort(Duplicates0).
[[<<"cowboy">>,<<"0.8.3">>],
 [<<"cowboy">>,<<"0.6.1">>]]
```

## Usage (CLI)

__Drop in replacement for Rebar:__

There is no need to write a new config file to use EPM, it supports
rebar out of the box. We can at any time print out the current known
configuration:

```bash
olav@nyx /tmp/riak » ~/src/epm/bin/epm deps print -v=info  
    [info] 22:13:45 -> agent:git: github.basho:lager_syslog=1.2.2 not checked out
    [info] 22:13:45 -> agent:git: github.basho:cluster_info=1.2.3 not checked out
    [info] 22:13:45 -> agent:git: github.basho:riak_kv=1.3.1 not checked out
    [info] 22:13:45 -> agent:git: github.basho:riak_search=1.3.1 not checked out
    [info] 22:13:45 -> agent:git: github.basho:riak_control=1.3.1 not checked out
    [info] 22:13:45 -> agent:git: github.basho:riaknostic=v1.1.0 not checked out
Catalogs (1):
= github.basho.https://github.com/basho (github: 118 packages)

Dependencies (6/6):
= github.basho:cluster_info=1.2.3
= github.basho:lager_syslog=1.2.2
= github.basho:riak_control=1.3.1
= github.basho:riak_kv=1.3.1
= github.basho:riak_search=1.3.1
= github.basho:riaknostic=v1.1.0
```

The `print` operation is sideeffect free - except for fetching the
initial catalog - therefor we need an explicit call to fetch our
dependencies (the `--rebar` flag sets `libdir` and `append_version` env variables):

```bash
olav@nyx /tmp/riak » epm deps sync -v=notice --rebar
  [info] 22:15:25 -> epm: setting rebar compatability
  [notice] 22:15:26 -> agent:git: syncing riak_pipe=1.3.1 (riak_pipe) from [<<"github.basho">>]
  [notice] 22:15:27 -> agent:git: syncing sext=1.1 (sext) from [<<"github.uwiger">>]
  [notice] 22:15:27 -> agent:git: syncing edown=HEAD (edown) from [<<"github.esl">>]
  [notice] 22:15:28 -> agent:git: syncing eleveldb=1.3.0 (eleveldb) from [<<"github.basho">>]
	...
  [notice] 22:16:47 -> agent:git: syncing lager=1.2.2 (lager) from [<<"github.basho">>]
  [notice] 22:16:48 -> agent:git: syncing meck=master (meck) from [<<"github.eproxus">>]
```

We can now see we filled up the `deps/` directory:

```bash
olav@nyx /tmp/riak » ls -l deps                                                                                                                                       1 ↵
drwxr-xr-x  6 olav users 240 Apr 21 22:15 basho_stats
drwxr-xr-x  4 olav users 160 Apr 21 22:15 bear
....
drwxr-xr-x  5 olav users 160 Apr 21 22:15 syslog
drwxr-xr-x 12 olav users 500 Apr 21 22:15 webmachine

```

A final call to print will now tell us a bit more about our project:

```bash
olav@nyx /tmp/riak » ~/src/epm/bin/epm deps print

Catalogs (5):
= github.basho.https://github.com/basho (github: 118 packages)
= github.boundary.https://github.com/boundary (github: 32 packages)
= github.eproxus.https://github.com/eproxus (github: 21 packages)
= github.evanmiller.https://github.com/evanmiller (github: 30 packages)
= github.jcomellas.https://github.com/jcomellas (github: 20 packages)

Dependencies (6/22):
= github.basho:cluster_info=1.2.3
= github.basho:lager_syslog=1.2.2
= github.basho:riak_control=1.3.1
  = github.evanmiller:erlydtl=c18f2a00
  = github.basho:riak_core=1.3.1
    = github.basho:basho_stats=1.0.3
    = github.basho:folsom=0.7.3p1
      = github.boundary:bear=0.1.2
      = github.eproxus:meck=0.7.2-2-gd6230ae
    = github.basho:lager=1.2.2
    = github.basho:poolboy=0.8.1-14-g0e15b5d
    = github.basho:protobuffs=0.8.0
      = github.eproxus:meck=master (defined previously)
    = github.basho:riak_sysmon=1.1.3
    = github.basho:webmachine=1.9.3
      = github.basho:mochiweb=1.5.1p3
  = github.basho:webmachine=1.9.3 (defined previously)
= github.basho:riak_kv=1.3.1
= github.basho:riak_search=1.3.1
  = github.basho:merge_index=1.3.0
  = github.basho:riak_api=1.3.1
  = github.basho:riak_kv=1.3.1 (defined previously)
  = github.basho:riak_pb=1.3.0
    = github.basho:protobuffs=0.8.0 (defined previously)
= github.basho:riaknostic=v1.1.0
  = github.jcomellas:getopt=v0.4.3
  = github.basho:lager=1.2.2 (defined previously)
  = github.eproxus:meck=master (defined previously)
```
