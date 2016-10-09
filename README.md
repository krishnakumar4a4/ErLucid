# ErLucid
See through the erlang VM with ease

I meant, an implementation which can give you a runtime memory consumption of erlang functions.


The current implementation would grab all the function calls and corresponding memory at every call and return.
This would record every 500 traces into a file. if you stop in middle the last traces would be missed.
Need to think of a solution for not missing even one.

Still it traces every module, going recursively infinite.
So, next step would be to isolate OTP calls, atleast io_lib and group_leader calls that may give infinite recursiveness.

May be xref and tags module might help to have selective tracing.

Still need to work hard on visualizing concurrent processes,function calls and memory consumption without loosing the essence.
