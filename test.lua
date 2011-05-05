#!/usr/bin/env lua

package.path = "./?/init.lua;"..package.path

if not arg[1] and not arg[2] and not arg[3] then
    print("Usage: test.lua mailserver username password")
    return
end

local imaplib = require("imap4")

local message = "Date: Mon, 7 Feb 1994 21:52:25 -0800 (PST)\r\nFrom: Fred Foobar<foobar@Blurdybloop.COM>\r\nSubject: afternoon meeting\r\nTo:mooch@owatagu.siam.edu\r\nMessage-Id:<B27397-0100000@Blurdybloop.COM>\r\nMIME-Version: 1.0\r\nContent-Type:TEXT/PLAIN; CHARSET=US-ASCII\r\n\r\nHello Joe, do you think we can meet at 3:30 tomorrow?"

local imap = imaplib.IMAP4:new(arg[1])
imap.__welcome:__print()
local r = imap:capability()
r:__print()
r = imap:NOOP()
r:__print()
r = imap:starttls()
r:__print()

r = imap:login(nil, arg[3], {arg[2]})
r:__print()
r = imap:select()
r:__print()
r = imap:search('ALL')
r:__print()
r = imap:search('ANSWERED', 'UTF-8')
r:__print()
r = imap:fetch('4', 'BODY\[TEXT\]')
r:__print()
r = imap:examine('Dad')
r:__print()
r = imap:select('sahd')
r:__print()
r = imap:uid('SEARCH', 'UTF-8', 'ALL')
r:__print()
r = imap:uid('fetch', '1278', 'BODY\[TEXT\]')
r:__print()
r = imap:xatom('UNSELECT')
r:__print()
r = imap:close()
r:__print()
r = imap:create('Dad')
r:__print()
r = imap:delete('oogedy')
r:__print()
r = imap:create('test')
r:__print()
r = imap:rename('test', 'test2')
r:__print()
r = imap:delete('test2')
r:__print()
r = imap:list('""', 'D*')
r:__print()
r = imap:list('Work', '%')
r:__print()
r = imap:lsub()
r:__print()
r = imap:status('sahd', 'UNSEEN')
r:__print()
r = imap:status('blah', 'MESSAGES')
r:__print()
r = imap:APPEND('Junk', message, [[\Deleted \Seen]]) 
r:__print()
r = imap:select('Junk')
r:__print()
r = imap:store('1', '-FLAGS', [[\Deleted]])
r:__print()
r = imap:expunge()
r:__print()
r = imap:store('1', '+FLAGS', [[\Deleted]])
r:__print()
r = imap:expunge()
r:__print()
r = imap:close()
r:__print()
if r:getTaggedResult() ~= 'OK' then print('FAILED') end
r = imap:examine('Junk')
r:__print()
r = imap:logout()
r:__print()

imap:shutdown()


