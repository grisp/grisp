-record(device, {
    slot,
    driver,
    driver_opts,
    pid,
    monitor,
    access = public,
    owner,
    owner_monitor,
    status = initializing}
).
