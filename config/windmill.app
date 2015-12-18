{
    application, windmill,
    [
        {description, ""},
        {vsn, "1.0"},
        {modules, []},
        {registered, [windmill_sup]},
        {mod, {windmill_app, []}},
        {applications, [kernel, stdlib, sasl]},
        {env, []},
        {start_phases, []}
    ]
}.
