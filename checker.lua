#!/usr/bin/env lua

--[[
    Sample program to check for mail on an IMAP4Rev1 server.

    Command line arguments:
        mailserver url
        user
        password

    Example usage: checker.lua my.mail.server user secret

    author: G.LaMontagne 10/2010

--]]

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
r = chk_result(imap:LIST('""', '*'))
newmail = {}
for i,v in ipairs(r:getUntaggedContent('LIST')) do
    local mb = v:match([[.* %"(.-)%"$]])
    local rs = chk_result(imap:STATUS(mb, 'UNSEEN'))
    local mbstat = rs:getUntaggedContent('STATUS')[1] 
    local newmail = mbstat:match([[%(UNSEEN (%d+)%)$]])
    if newmail ~= '0' then
        print(string.format("\t%s:\t%u", mb, newmail))
    end
end

-- this logs us out and shuts down the network connection
imap:shutdown()
