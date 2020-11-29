let () =
  Revery.App.initConsole ();
  Timber.(App.enable (Reporter.console ~enableColors:true ()));
  Timber.App.setLevel Timber.Level.perf;

  Bonsai_revery.Start.start_standalone Chat.app ~initial_input:()
