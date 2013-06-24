{application, testapp,
 [{description, "Test application"},
  {vsn, "1.3.1"},
  {modules, [
             testapp
             testapp_sup
            ]},
  {applications, [
                  kernel,
                  stdlib,
                  'dep-a'
                 ]},
  {registered, []},
  {env, []}.

