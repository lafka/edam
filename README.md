# EPM

Todays state of erlang dependency manager is, work in progress...
EPM will try to handle working dependencies with dependencies in a more
controlled way.

https://hyperthunk.wordpress.com/2012/05/28/does-erlangotp-need-a-new-package-management-solution/
http://lists.basho.com/pipermail/riak-users_lists.basho.com/2013-April/011777.html
http://erlang.org/pipermail/erlang-questions/2012-October/069825.html

Features:

+ Deterministic dependencies
+ Dynamic handling of repository/dependency remotes, branches etc
+ Handles repositories of dependencies vs single dependency
+ When specifying a version make sure that version will be used
	(even for recursive dependencies) instead of random error because of
	update-deps not working correctly.
+ Support for multiple repository backends (only github for now)
+ Compiles to single binary

Will probably add Support for dependency snapshots, which would lock
dependencies at commit time (only git support?).


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

## Usage (Erlang API)

__Print out the current known configuration:__

```erlang
epm_config:print(epm_config:parse(".")).
```

__Fetch/Update all depenencies:__

```erlang
%% Fetches a local copy to .cache/<publisher>/<dep> and then makes a
%% local clone to lib/<dep>-<vsn>.
[epm_deps:update(Dep) || Dep <- epm_config:deps(epm_config:parse("."))].
```
