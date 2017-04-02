# ErLucid
See through the erlang VM with ease

I meant, an implementation which can give runtime memory consumption of erlang functions.


The current implementation would grab all the function calls and their corresponding memory at every call and return.
# Limitations
- Batch processing while writing to the file, if stopped in middle last batch messages would be lost.
- Recursive looping is not automatically prevented, need to take care while tracing OTP modules.

# Good things
- OTP calls are by default isolated, and their tracing should be set explicitly.
- Used xref for validation and event manager for seamless message processing.
- Trace file consumption happens now in "rust programming language" for performance reasons.

# Roadmap
Still need to work hard on visualizing concurrent processes,function calls and memory consumption without loosing the essence.
