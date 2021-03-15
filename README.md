# tempo
erlang rate limiter

## Example usage:

### counting/totals backend setup

Supervised child process with args for 10 calls within 500ms allowed.

```
#{
    id       => tempo,
    start    => {tempo, start_link, [?SERVER_NAME, _Calls=10, _WithinTimeRageMs=500]},
    restart  => permanent,
    shutdown => brutal_kill,
    type     => worker,
    modules  => [tempo]
}
```

### Client/Usage 

```
tempo:can_make_call(whereis(?SERVER_NAME))
```

returns either true/false.