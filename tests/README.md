# Integration tests

A poor-man's snapshot testing framework for integration tests is prepared for you in the `tests` directory.
It is a shell script that runs your implementation with various programs and checks the output and exit code against some expected, pre-recorded values (snapshots - hence the name snapshot testing).
If the output / exit code is different it will show a diff using either `diff` or [delta](https://github.com/dandavison/delta).
The tests are divided into `suites` found under [`tests/suites`](tests/suites).
The expected outputs are in [`tests/expected`](tests/expected).

For example, the following

```sh
./tests/test.sh --microc ../microc-cpp/build/microc --suite all
```

will test all available test suites using the `../microc-cpp/build/microc` executable.

Each task has its own suite, i.e., `task-1`, `task-2`, ... `task-4`.

The path to the executable can be set either using the `--uc` option or the `UC` environment variable.

See the `./tests/test.sh --help` for more info.
