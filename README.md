defshef16
=====

Accompanying presentation with notes is here:
https://github.com/jwheare/defshef16/tree/master/talk/defshef16-notes.pdf

A demo OTP application

Requirements: https://rebar3.org

Build step 1
------------
    $ git reset --hard 1
    $ rebar3 release
    $ _build/default/rel/defshef16/bin/defshef16 start
    
    $ tail -F _build/default/rel/defshef16/log/erlang.log.1
    $ _build/default/rel/defshef16/bin/defshef16 remote_console
    
    1> defshef16_sup:children().
    2> defshef16_int_server:get().
    3> defshef16_int_server:increment().
    4> defshef16_int_server:decrement().
    5> defshef16_int_server:set(15).
    
Try setting a non integer, then increment it to make the process crash. Then fix it with a [guard sequence](http://erlang.org/doc/reference_manual/expressions.html#id81912) and `when`

Build step 2
------------
    $ git reset --hard 2
    $ rebar3 release relup tar
    $ mv _build/default/rel/defshef16/defshef16-2.tar.gz _build/default/rel/defshef16/releases/2/defshef16.tar.gz
    $ _build/default/rel/defshef16/bin/defshef16 upgrade 2
    
    6> 15 = defshef16_int_server:get().
    7> {ok, Pid} = defshef16_user_sup:start_user("James", "Wheare").
    8> defshef16_user:get_full_name(Pid).
    9> rr(dict).
    10> sys:get_state(Pid).

Build step 3
------------
    $ git reset --hard 3
    $ rebar3 release relup tar
    $ mv _build/default/rel/defshef16/defshef16-3.tar.gz _build/default/rel/defshef16/releases/3/defshef16.tar.gz
    $ _build/default/rel/defshef16/bin/defshef16 upgrade 3
    
    11> 15 = defshef16_int_server:get().
    12> supervisor:which_children(defshef16_user_sup).
    13> true = defshef16_user:first_name_starts_with(Pid, "j").
    14> rr(defshef16_user).
    15> sys:get_state(Pid).
