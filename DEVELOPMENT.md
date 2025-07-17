# Running Tests

Most tests are now behind features, to prevent compile failures.

Run all tests with:
```shell
cargo test --all-features
```

You can run base tests with no features, or only run client-side / server-side test, by setting `--features=client` or `--features=server` respectively.