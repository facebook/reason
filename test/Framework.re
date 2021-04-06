include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: "test/fixtures/_snapshots",
      projectDir: "test",
    });
});
