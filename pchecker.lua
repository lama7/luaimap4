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
    Sample program to check for mail on an IMAP4Rev1 server.

    Command line arguments:
        mailserver url
        user
        password

    Example usage: checker.lua my.mail.server user secret

    author: G.LaMontagne 10/2010

--]]

package.path = "./?/init.lua;"..package.path

if not arg[1] and not arg[2] and not arg[3] then
    print("Usage: checker.lua mailserver username password")
    return
end

local imaplib = require("imap4")
local string = require("string")


function chk_result(r)
    if r:getTaggedResult() ~= 'OK' then
        imap:shutdown()
        error("Imap command failed")
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
    error("Server does not support IMAP4Rev1")
end
if not capability:find('STARTTLS') then
    error([[Server does not support STARTTLS, aborting to avoid sending login
            credentials in the clear.]])
end
chk_result(imap:STARTTLS())
chk_result(imap:LOGIN(arg[2], arg[3]))

print("Checking for new mail...")
r = chk_result(imap:LIST('""', '*'))  -- list of all mailboxes
local tag
imap:startPipeline()
for i,v in ipairs(r:getUntaggedContent('LIST')) do
    local mb = v:match([[.* %"(.-)%"$]])
    tag = imap:STATUS(mb, 'UNSEEN')
end
imap:endPipeline()

for r in imap:readResponses(tag) do
    local mbstat = r:getUntaggedContent('STATUS')[1] 
    local mb, newmail = mbstat:match([[^%"(.-)%"%s+%(UNSEEN (%d+)%)$]])
    if newmail ~= '0' then
        print(string.format("\t%s:\t%u", mb, newmail))
    end
end

-- this logs us out and shuts down the network connection
imap:shutdown()
