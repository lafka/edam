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
