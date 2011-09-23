# luaimap4

A client based IMAP library that aims to be RFC3501 compliant.  An IMAP object
is created using a server/ port settings and then IMAP commands are implemented
as methods of the object.  All command formatting is handled by each method.
Normal operation returns all responses to an IMAP command up to the tagged
completion response.  IMAP pipelining is also supported.

# Installation

As of now, the default installation location for the `imap4/` directory and it's
files is `/usr/local/share/lua/5.1/`.  This directory is known to work on a
Debian/testing based setup as that path is part of the standard module search
path for lua.

If it is desired to locate the library elsewhere, then the `init.lua` file will
need to be modified appropriately to reflect that location.  Specifically, there
is a path information line that should specify the full directory where the
`imap4.lua` file will be located.  Also, make sure that directory is in the lua
search path.

For instance, if the desired install location is `/home/user/lua/libs` then the
path line in `init.lua` should be changed to read(assuming that the `imap4`
directory name is kept):

    path = "/home/user/lua/libs/imap4"

# Usage

    local imaplib = require("imap4")
               .
               .
               .
    local imap = imaplib:new(server_str, port_n, ssl_opts_tbl)

    imap:capability()


