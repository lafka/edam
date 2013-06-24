# Introduction

Edam[1] is a dependency management tool aimed to simplify the routines
around creation, publication of code artifacts as well as resolving
dependant source code/artifacts.

The motivation for this project comes from me finding rebar[2] a
unsatisfactory solution for future Erlang build environments.

## High-level features

+ Profile specific dependencies (e.g. compile-time or test dependencies)
+ Support for source dependencies as well as code artifacts
+ Digital signing of builds
+ Supports local and global cache for dependencies
+ Isolation of specific packages to allow multiple different dependency graphs

[1] Edam: I planned to rename epm to dtt (short for Dithiothreitol),
           a chemical compound which happens to be an unusually strong
           reducing agent. Instead i ended up with disli which i
           derived from disulfide: the structural unit composed of
           a linked pair of sulfur atoms. Diden't realy care to much
           for it so i decided Distel instead but was to close to
           distel. Finaly I ended up with EDM (Erlang Dependency Manager)
           which ofcourse was to close to EPM so I added an 'A' and
           then we got cheese!

[2] rebar: The current de-facto build tool for Erlang projects


The state of a _edam_ configuration consists of multiple _Catalogs_
each with multiple _Packages_. These are explained in more detail below.


## Concepts

### Catalogs

All `packages` resides within 1 catalog, the catalog is managed by a
developer or an organization. Catalogs contains a list of `package templates`.

### Packages

A - possibly remote - resource containing some source code. Once synced
additional information can be retrieved, mainly the version number and
a list of additional dependencies. This information is stored in one or
more configuration files (ie. `*.app` files).

### Parsers

Parsers are abstractions for different type of configuration. It
allows for integration with other 3rd party tools.

We have chosen to minimize this to 2 variants configuration files:
`.app` files and `rebar.config` files. This is solely to allow drop-in
replacement for existing tools to ease adoption.

### Agents

Agents are used to retrieve dependencies from some location. Typically
there is one _agent_ pr. protocol e.g. _git_, _filesystem_ or _HTTP_.

**Note:** A catalog will typically use only one agent, e.g. _github_
catalog will use the _git_ agent, whilst _otp_ catalog will use the
_localfs_ agent.


# Configuration

Edam have been built with a _convention over configuration_ mindset,
therefore configuration can be done very easily. The most basic way
uses the existing tools (`rebar.config` or a `*.app` file[1]).

Both solutions have limitations (rebar lacks proper versioning, app
file does not provide any contraints). When you require additional
configuration the edm profile files come in very handy.

The state of each EDM configuration contains a set of catalogs each
containing a set of template packages. `deps` and `build` commands
operates on a single target package. The target package contains a
list of dependencies each with individual constrains and additional
meta data. When resolving a dependency all possible candidates are
retrieved from the enabled catalogs, each of these are matched against
the dependency constraints; ending with a list of possible packages.

[1] The app file will only let you define Erlang application
    dependencies. Refer to `*.profile` files for most advanced funcionality

## Edam Profiles

A profile files are stored in the `build/` directory using the format `<profile>.dt`.
When running Edam the profile can be specified by specifying a profile
file or by the name (e.g. `edam -c ./build/prod.dt` or `edam -p prod` respectivly).


The profiles MUST contain valid erlang terms.

```erlang
%% Example profile

%% Some catalogs can be parameterized:
{catalogs, [
    {github, "basho"}
  , {github, "extend"}
  , {local, "~/dist/"}
]}.

%% {dependencies, [{pkg_name(), pkg_constraints(), pkg_opts()}]}
%% Automatically resolve riakc to highest tag, lager to specific tag
{dependencies, [
    {riakc, [], [{pkgname, "riak-erlang-client"}]}
  , {lager, [{prefix, "2.0."}], []} % Use {prefix,_} for specific major/min
  , {ranch, [{eq, "0.8.4"}]}
]}.

%% Include ct_helper for testing only
{dependencies, test, [
	  {ct_helper, [any], [{{agent, remote}, git://github.com/extend/ct_helper.git}]}
]}.
```

# Extending

## Catalogs

Catalogs must export these functions:

```erlang
% Unique name used for path generation
-spec name() ->  binary().

% Match function determining if a resource belongs to the catalog.
-spec match(binary()) -> boolean().

% Build a URL refering to the location of a package within a catalog.
-spec resource(package(), catalog()) -> URL :: binary().

% Update the catalog() record with packages found in catalog
-spec fetch(A) -> A when A :: catalog().
```

## Parsers

```erlang
% Parse a specific file, returns {ok, ConfigName} when successfull
% false is returned if parser was unable to find any terms.
-spec parse(file:filename(), Cfg) ->
	{ok, Cfg} | false | {error, Reason :: term()} when Cfg :: config().
```

## Agents

All the action is handled by a plug-in agent, there will typically
be 1 agent pr. version control system. GIT is the only supported
agent at the moment. In the future a plug-in for local filesystem
and HTTP might be added.

Agents must export the following functions:

```erlang
% Return the name of the agent (ie <<"git">>, <<"http">>)
-spec name() -> binary().

% Initiate the agent for a package, this will be called during config
% parsing and can be used to set any additional options for the pkg.
-spec init(package()) -> package().


% Fetches any remote update and stores in cache
-spec fetch(epm_pkg:pkg()) -> ok | {error, Reason :: term()}

% Syncs the local copy of the package with any remote updates
-spec sync(package()) -> ok | {error, Reason :: term()}

% Check the current known state of the package - MUST BE sideeffect free
-spec status(package()) -> ok | stale | unknown
```

# Examples

## Usage (Erlang API)

### Lists all package versions

```erlang
1> {ok, Cfg} = edm_cfg:parse("."),
1> Fun = fun(Pkg) -> edm_pkg:get([name, version], Pkg) end,
1> lists:usort(iter:map(Fun, iter:flatten(Cfg))).
[[<<"lager">>,<<"1.2.1">>],[<<"cowboy">>,<<"0.8.3">>],[<<"ranch">>,<<"0.8.1">>]]
```

### Sync all known dependencies

```erlang
2> Fun = fun(Pkg) -> edm_pkg:sync(Pkg, Cfg) end,
2> iter:foreach(Fun, lists:usort(iter:flatten(Cfg))).
ok
```

### Find conflicting versions of packages (the naive way)

The following example expands the dependencies to `[Name, Version]` which
can easily be used to lookup possible version conflicts.

```erlang
42> Pkgs = lists:usort(iter:pick([name, version], iter:flatten(Cfg))),
42> lists:foldl(fun([Name|Vsn], Acc) ->
42>   case lists:keyfind(Name, 1, Acc0) of
42>     {Name, T} -> lists:keystore(Name, 1, Acc, {Name, [Vsn|T]});
42>     false     -> Acc;
42>   end
42> end, [], Pkgs).
[{<<"cowboy">>,[<<"0.6.1">>, <<"0.8.3">>]}]
```

## Usage (CLI)

__Drop in replacement for Rebar:__

There is no need to write a new config file to use EPM, it supports
rebar out of the box. We can at any time print out the current known
configuration:

```bash
olav@nyx /tmp » git clone git://github.com/basho/riak && cd riak
olav@nyx /tmp/riak » edam deps show
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

The `show` operation is sideeffect free - some catalogs may do an
initial fetch - therefore we need to call sync.

**Note:** _--rebar-compat_ sets rebar compatability mode, meaning we
store all dependencies in _deps/_ WITHOUT semantic versioning.

```bash
olav@nyx /tmp/riak » edam deps sync --rebar-compat
(22:15:26) agent:git: syncing riak_pipe=1.3.1 (riak_pipe) from [<<"github.basho">>] (info)
(22:15:27) agent:git: syncing sext=1.1 (sext) from [<<"github.uwiger">>] (info)
(22:15:27) agent:git: syncing edown=HEAD (edown) from [<<"github.esl">>] (info)
(22:15:28) agent:git: syncing eleveldb=1.3.0 (eleveldb) from [<<"github.basho">>] (info)
....
(22:16:47) agent:git: syncing lager=1.2.2 (lager) from [<<"github.basho">>] (info)
(22:16:48) agent:git: syncing meck=master (meck) from [<<"github.eproxus">>] (info)
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
olav@nyx /tmp/riak » edam deps show

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
