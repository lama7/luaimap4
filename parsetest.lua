#!/usr/bin/env lua

package.path = "./imap4/?.lua;"..package.path

local parser = require('parser')

local function test(test_tbl)
    for i, cmd_t in ipairs(test_tbl) do
        if cmd_t[2] then
            assert(cp(cmd_t[1]), test_err..cmd_t[1].."'")
        else
            assert(not cp(cmd_t[1]), test_err..cmd_t[1].."'")
        end
    end
end

cp = parser.command_parse

test_err = "Error: '"
assert(cp("capability"), test_err.."capability'")
assert(not cp(" capability"), test_err.." capability'")
assert(not cp("NOOP "), test_err.."NOOP '")
assert(cp("NOOP"), test_err.."NOOP'")
assert(cp("logOUt"), test_err.."logOUt'")
assert(not cp("logOUT "), test_err.."logOUT '")

xtest = "xtest2 lkjsdf 1:* lskjfk"
assert(cp(xtest), test_err..xtest.."'")
assert(not cp(xtest..' '), test_err..xtest.." '")

login_c = "login user secret"
assert(cp(login_c), test_err..login_c.."'")

login_c = [[login "user" secret]]
assert(cp(login_c), test_err..login_c.."'")

login_c = [[login user "\"sec\\ret"]]
assert(cp(login_c), test_err..login_c.."'")

login_c = [[login user "\"sec\ret"]]
assert(not cp(login_c), test_err..login_c.."'")

login_c = [[login {5}]]
assert(cp(login_c), test_err..login_c.."'")

login_c = [[login {5} ]]
assert(not cp(login_c), test_err..login_c.."'")

login_c = [[login user {6}]]
assert(cp(login_c), test_err..login_c.."'")

auth_c = [[authenticate cram-md5]]
assert(cp(auth_c), test_err..auth_c.."'")

auth_c = [[authenticate bogus test]]
assert(not cp(auth_c), test_err..auth_c.."'")

single_mb = 
{
    { [[create boogers]], true },
    { [[create {7}]], true},
    { [[create {7} blah]], false },
    { [[create "blurdy-bloop"]], true },
    { [[create "blurdy*bloop"]], true },
    { [[create blurdy*bloop]], false },
}
test(single_mb)
    
rename_cmds =
{
    { [[rename boogers snot]], true },
    { [[rename mb1 {5}]], true },
    { [[rename {6}]], true },
    { [[rename {6} mb2]], false },
}
test(rename_cmds)

copy_cmds = 
{
    { [[Copy 2:4,8,9,10,20:* here]], true },
    { [[Copy here 2:4,8,9,10,20:*]], false },
    { [[Copy * here]], true },
    { [[Copy 2:420:* here]], false },
    { [[Copy 2:420,*:100 here]], true },
    { [[Copy 2:420,*:100 {4}]], true },
    { [[Copy 2:420,*:100 {4} ]], false },
    { [[Copy 2:420,*:100 here ]], false },
}
test(copy_cmds)

store_cmds = 
{ 
  { [[store 2:4 FLAGS \Deleted \Experimental]], true },
  { [[store * +FLAGS (\blah )]], false },
  { [[store 2:10,12,15:* flags.silent \flagged]], true },
  { [[store + FLAGS deleted]], false },
  { [[store 5 FLAGS deleted]], true },
  { [[store 100 -fLaGs.sIlEnT (\answered \seen)]], true },
  { [[store 100 .silent (\answered)]], false },
  { [[store 100 +.silent (\answered)]], false },
  { [[store 100:* -.flags (\flagged)]], false }
}
test(store_cmds)

list_cmds = 
{
  { [[list "" *]], true },
  { [[list "" ""]], true },
  { [[list #news.comp.mail.misc ""]], true },
  { [[list /usr/staff/jones ""]], true },
  { [[list ~/Mail/ %]], true },
  { [[lsub "#news." "comp.mail.*"]], true},
  { [[lsub "#news." "comp.%"]], true},
  { [[list news\comp\mail\misc ""]], false },
  { [[list "news\comp\mail\misc" ""]], false },
}
test(list_cmds)

uid_cmds = 
{
    { [[uid Copy 2:4,8,9,10,20:* here]], true },
    { [[uid Copy here 2:4,8,9,10,20:*]], false },
    { [[uid Copy * here]], true },
    { [[uid Copy 2:420:* here]], false },
    { [[uid Copy 2:420,*:100 here]], true },
    { [[uid Copy 2:420,*:100 {4}]], true },
    { [[uid Copy 2:420,*:100 {4} ]], false },
    { [[uid Copy 2:420,*:100 here ]], false },
    { [[uid starttls]], false },
}
test(uid_cmds)


append_cmds =
{
  { [[append INBOX {50}]], true },
  { [[append spam (\Seen) {200}]], true },
  { [[append {7}]], true },
  { [[append foo "23-10-2011 14:24:32 -0006" {500}]], false },
  { [[append foo "23-OCT-2011 14:24:32 -0006" {500}]], true },
  { [[append bar (\Seen \Answered) "02-OCT-2011 21:44:32 -0006" {1024}]], true },
  { [[append bar (\Seen \Answered) "2-10-2011 21:44:32 -0006" {1024}]], false },
  { [[append bar "2-OCT-2011 21:44:32 -0006" {1024}]], false },
  { [[append bar "02-OCT-2011 21:44:32 -006" {1024}]], false },
  { [[append spam (\Seen) {200} ]], false },
  { [[APPEND test2 (Deleted Seen) {304}]], true },
}
test(append_cmds)

fetch_cmds = 
{
  { [[fetch 2,3,4 ALL]], true },
  { [[fetch 2,3,4 FULL]], true },
  { [[fetch 2,3,4 FAST]], true },
  { [[fetch 10:* RFC822]], true },
  { [[fetch 1:* RFC822.HEADER]], true },
  { [[fetch 10:* RFC822.SIZE]], true },
  { [[fetch 10:* RFC822.TEXT]], true },
  { [[fetch 10:* BODYSTRUCTURE]], true },
  { [[fetch * UID]], true },
  { [[fetch 10:* internaldate]], true },
  { [[fetch 10:* envelope]], true },
  { [[fetch 10:* envelope internaldate]], false },
  { [[fetch 10:* (envelope internaldate)]], true },
  { [[fetch 2:4 (FLAGS BODY[HEADER.FIELDS (DATE FROM)])]], true },
  { [[fetch 2:4 (FLAGS BODY[HEADER.FIELDS (DATE FROM)]<2.4>)]], true },
  { [[fetch 2:4 FLAGS BODY[HEADER.FIELDS (DATE FROM)]<2.4>]], false },
  { "fetch 1:* BODY[HEADER.FIELDS (DATE FROM SUBJECT)]", true },
  { "fetch 1:* BODY[HEADER.FIELDS (DATE FROM SUBJECT)]<5.10>", true },
  { [[fetch 4 (BODY[TEXT])]], true },
}
test(fetch_cmds)

search_cmds =
{
  { [[search DELETED FROM "SMITH" SINCE 1-FEB-1994]], true },
  { [[search flagged since 1-feb-1994 not from "smith"]], true },
  { [[search or flagged deleted or or unanswered deleted unseen]], true },
  { [[search or deleted (flagged or unanswered unseen)]], true },
  { [[search or deleted (flagged not (answered seen))]], true },
  { [[search CHARSET UTF-8 DELETED FROM "SMITH" SINCE 1-FEB-1994]], true },
  { [[search CHARSET UTF-8 flagged since 1-feb-1994 not from "smith"]], true },
  { [[search CHARSET UTF-8 or flagged deleted or or unanswered deleted unseen]], true },
  { [[search CHARSET UTF-8 or deleted (flagged or unanswered unseen)]], true },
  { [[search CHARSET UTF-8 or deleted (flagged not (answered seen))]], true },
  { [[search CHARSET UTF-8 or header subject {3}]], true },
  { [[search CHARSET UTF-8 or header subject foo header from blah]], true },
}
test(search_cmds)
