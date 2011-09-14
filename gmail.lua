#!/usr/bin/env lua

package.path = "./?/init.lua;"..package.path

local imaplib = require("imap4")
local string = require("string")


function chk_result(r)
    if r:getTaggedResult() ~= 'OK' then
        imap:shutdown()
        r:__print()
        error("Imap command failed")
    end
    return r
end

imap = imaplib.IMAP4:new("imap.gmail.com", nil, {protocol = 'sslv3'})
local r = chk_result(imap:capability())
capability = r:getUntaggedContent('CAPABILITY')[1]
if not capability:find('IMAP4rev1') then
    imap:shutdown()
    error("Server does not support IMAP4Rev1")
end
chk_result(imap:LOGIN(arg[1], arg[2]))

print("Checking for new mail...")
r = chk_result(imap:LIST('""', '*'))
newmail = {}
for i,v in ipairs(r:getUntaggedContent('LIST')) do
    if not v:find('\\Noselect') then
        local mb = v:match([[.* %"(.-)%"$]])
--        local rs = chk_result(imap:STATUS(nil, 'UNSEEN', {mb_name = mb }))
        local rs = chk_result(imap:STATUS(mb, 'UNSEEN'))
        local mbstat = rs:getUntaggedContent('STATUS')[1] 
        local newmail = mbstat:match([[%(UNSEEN (%d+)%)$]])
        if newmail ~= '0' then
            print(string.format("\t%s:\t%u", mb, newmail))
        end
    end
end

imap:shutdown()


