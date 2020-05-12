#!/usr/bin/env lua
--[[
Copyright (c) <2010> <Gerry LaMontagne>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--]]

--[[
Test for largish server responses.  The response to the search here is very
large and would break the low-level receiving in the library.    Run this test
to make sure that hasn't changed.  The folder specified should be a VERY large
one- thousands of emails ideally.  If this runs without coughing up and error,
congrats, test passed.

    Command line arguments:
        mailserver url
        user
        password
        folder

    Example usage: test4.lua my.mail.server user secret folder

    author: G.LaMontagne 5/2020

--]]

-- make sure we are using local library for testing
package.path = "./?/init.lua;" .. package.path

local imaplib = require("imap4")
local string = require("string")

function chk_result(r)
    if r:getTaggedResult() ~= 'OK' then
        imap:shutdown()
        print("Imap command failed")
    end
    return r
end

-- start by creating the imap object
imap = imaplib.IMAP4:new(arg[1])

-- make sure server supports IMAP4rev1
r = chk_result(imap:CAPABILITY())
capability = r:getUntaggedContent('CAPABILITY')[1]
if not capability:find('IMAP4rev1') then
    imap:shutdown()
    print("Server does not support IMAP4Rev1")
end
if not capability:find('STARTTLS') then
    print([[Server does not support STARTTLS, aborting to avoid sending login
            credentials in the clear.]])
end
chk_result(imap:STARTTLS())
chk_result(imap:LOGIN(arg[2], arg[3]))

-- logged in, now select the mb
rs = chk_result(imap:SELECT(arg[4]))
rs = chk_result(imap:SEARCH('ALL'))

-- this logs us out and shuts down the network connection
imap:shutdown()
