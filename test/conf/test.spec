{logdir, "log"}.
{config, "test.cfg"}.
{define, 'TestDir', ".."}.
{suites, 'TestDir', all}.
{ct_hooks, [{cth_surefire, [{path, "report.xml"}]}]}.
