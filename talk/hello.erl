% usage: Pid = hello:start().

-module(hello).

-export([start/0]).

start() ->
  Pid = spawn(fun loop/0),
  % Use ! to send messages to a process
  Pid ! hello,
  Pid ! {hello, defshef16},
  Pid.

loop() ->
  receive
    hello ->
      % say hi
      io:format("Hello world!~n"),
      loop();
    {hello, Name} ->
      % say hi to someone
      io:format("Hello ~s!~n", [Name]),
      loop();
    Unrecognised ->
      % dunno
      io:format("huh? what’s ~s?~n", [Unrecognised]),
      loop()
  end.
