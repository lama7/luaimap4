#!/usr/bin/env lua

local string = require('string')
local table = require('table')

SP = ' '
CRLF = '\r\n'
ATOM_SPECIAL = " {\"%c%(%)%%%*%]"


-- Per RFC3501: "X" atom <experimental command arguments>
x_command = {
            }

-- Per RFC3501:  command_any = "CAPABILITY" / "LOGOUT" / "NOOP" / x-command
command_any = {
               ['CAPABILITY'] = function(t) 
                                   return not t:next()
                                end,
               ['LOGOUT'] = function(t)
                                return not t:next()
                            end,
               ['NOOP'] = function(t)
                              return not t:next()
                          end,
              }
setmetatable(command_any, {__index = x_command})

command_auth = {
               }

command_nonauth = {
                  }

command_select = {
                 }

command = {}
command.__index = function(t,k)
                      local f = command_any[k:upper()]
                      if not f then 
                          return function(t) return false end
                      end
                      return command_any[k:upper()]
                  end
local parse_tbl = {}
setmetatable(parse_tbl, command)
 
--[[
    Token Object definiton
--]]
local token = {}
token.__index = token  -- make token and object

function token.next(self)
    if #self.token_t ~= 0 then
        return table.remove(self.token_t, 1)
    else
        return nil
    end
end

function token.__split(self,s)
--[[
    Splits a command string into tokens on SPACE character.
--]]
    local starts = s:find(' ' ,1)
    if not starts then
        table.insert(self.token_t, s:sub(1, -1))
    else
        table.insert(self.token_t, s:sub(1, starts - 1))
        self:__split(s:sub(starts + 1, -1))
    end
end

function token.new(self, s)
    local o = {}
    setmetatable(o, self)

    o.token_t = {}
    o:__split(s)

    return o
end

function command_parse(imapcmd)
--[[
    Entry point for parsing a command.
    The parameter should consist of the everything except the tag and space
    following the tag and the trailing CRLF
    
    According to RFC3501:
      command = tag SP (command-any / command-auth / command-nonauth /
                command-select) CRLF
    
    So we're picking up from after the "tag SP"
--]]
    local tok_o = token:new(imapcmd)
    print(parse_tbl[tok_o:next()](tok_o))
end

-- test code

tok_o = token:new("test string for  parsing")
repeat
    local t = tok_o:next()
    if t then print(t) end
until not t

command_parse("capability")
command_parse(" capability")
command_parse("NOOP ")
command_parse("NOOP")
command_parse("logOUt")
command_parse("logOUT ")

-- end test code

-- export parse object
parser = { command_parse = command_parse
         }
return parser
