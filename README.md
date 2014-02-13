# canbcm -- Linux SocketCAN BCM Interface for Erlang

Linux provides support for cyclic transmission of CAN messages and filtering of
received messages through the CAN Broadcast Manager (BCM). This Erlang
application provides access to this interface.

# Examples

```erlang
1> canbcm:start_link(can0).
{ok,<0.172.0>}
1> canbcm:subscribe(can0, 10, 0).   %% subscribe to messages with can_id 0 with a period of 10 seconds
ok
2> canbcm:send(can0, 0, <<0, 1, 2, 4, 5>>).   %% send message with can_id 0 and content 0,1,2,4,5
ok
3> flush().
Shell got {can_frame,0,<<0,1,2,4,5>>}
ok
4> canbcm:release(can0).
ok
```
