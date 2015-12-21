{application, pattern,
    [
        {description, "Pattern Dialog"},
        {vsn, "0.2"},
        {registered, [pattern_fsm]},
        {applications, [
            kernel,
            stdlib,
            sasl
        ]},
        {mod, {pattern_app, []}},
        {env, []},
        {modules,[pattern, pattern_app, pattern_sup, pattern_fsm, pattern_api]}
    ]}.
