local string = require('string')
local table = require('table')

local SP = ' '
local CRLF = '\r\n'
-- The following are all per RFC3501
local LIST_WILDCARDS = "%%%*"
local QUOTED_SPECIALS = [["\]]
local RESP_SPECIALS = "%]"
local ATOM_SPECIAL = "%(%){ %c"..LIST_WILDCARDS..QUOTED_SPECIALS..RESP_SPECIALS

local p_CHAR = "[^%z]" -- any char but NUL
local p_ASTRING_CHAR = "[^%(%){ %c"..LIST_WILDCARDS..QUOTED_SPECIALS.."]"
local p_ATOM_SPECIAL = "["..ATOM_SPECIAL.."]"
local p_ATOM_CHAR = "[^%z"..ATOM_SPECIAL.."]"
local p_ATOM = p_ATOM_CHAR.."+"
local p_LIST_CHAR = "[^%z%(%){ %c"..QUOTED_SPECIALS.."]"

--[[
    lexer object definiton
--]]
local lex = {}
lex.__index = lex  -- make lexer and object
lex.commands = { capability = 1, logout = 1, noop = 1, append = 1,
                 create = 1, delete = 1, examine = 1, list = 1, 
                 lsub = 1, rename = 1, ['select'] = 1, status = 1,
                 subscribe = 1, unsubscribe = 1, login = 1,
                 authenticate = 1, starttls = 1, check = 1, close = 1,
                 expunge = 1, copy = 1, fetch = 1, store = 1, 
                 uid = 1, search = 1 }
setmetatable(lex.commands, 
             { __index = function(t,k)
                             if k:upper():find("^X"..p_ATOM) then
                                 return 1
                             else
                                 return rawget(t, k)
                             end
                         end
             }
            )

lex.store = { flags = 1, silent = 1 }
lex.fetchmacros = { all = 1, full = 1, fast = 1 }
lex.fetch = { envelope = 1, flags = 1, internaldate = 1, rfc822 = 1, body = 1,
              bodystructure = 1, uid = 1, size = 1, text = 1, peek = 1 }
lex.section = { header = 1, fields = 1, ['not'] = 1, text = 1, mime = 1 }
lex.searchnoarg = { all = 1, answered = 1, deleted = 1, flagged = 1,  
                    new = 1, old = 1, recent = 1, seen = 1, unanswered = 1,
                    undeleted = 1, unflagged = 1, unseen = 1, draft = 1,
                    undraft = 1, charset = 1 }
lex.searchonearg = { bcc = 1, before = 1, body = 1, cc = 1, from = 1, 
                     keyword = 1, on = 1, since = 1, subject = 1, text = 1,
                     to = 1, unkeyword = 1, larger = 1, ['not'] = 1, 
                     sentbefore = 1, senton = 1, sentsince = 1, smaller = 1,
                     uid = 1, }
lex.searchtwoarg = { header = 1, ['or'] = 1, }
lex.date_month = { jan = 1, feb = 1, mar = 1, apr = 1, may = 1, jun = 1, 
                   jul = 1, aug = 1, sep = 1, oct = 1, nov = 1, dec = 1 }
lex.flags = { answered = 1, flagged = 1, deleted = 1, seen = 1,
              draft = 1, }
lex.status = { messages = 1, uidnext = 1, recent = 1, uidvalidity = 1,
               unseen = 1 }
lex.keywords =  {}
setmetatable(lex.keywords, 
             { __index = function(t,k)
                           k = k:lower()
                           if lex.commands[k] then return 'command_wd' end
                           if lex.fetchmacros[k] then return 'fetchmacro_wd' end
                           if lex.fetch[k] then return 'fetch_wd' end
                           if lex.searchnoarg[k] or
                              lex.searchonearg[k] or
                              lex.searchtwoarg[k] then return 'search_wd' end
                           if lex.date_month[k] then return 'date_month_wd' end
                           if lex.flags[k] then return 'flag_wd' end
                           if lex.section[k] then return 'section_wd' end
                           if lex.store[k] then return 'store_wd' end
                           if lex.status[k] then return 'status_wd' end
                         end
             }
            )

function lex.token(self)
--[[ 
    returns and removes next token from table
--]]
    if #self.tok_t == 0 then
        return nil
    else
        return table.remove(self.tok_t, 1)
    end
end

function lex.push(self, t)
--[[
    Puts a token back into table
--]]
    assert(type(t) ~= 'table' or #t ~= 1,
           "Attempt to push invalid token onto stack")
    table.insert(self.tok_t, 1, t)
end

function lex.peek(self)
--[[
    Return token without removing it from table
--]]
    if #self.tok_t < 1 then
        return nil
    else
        return self.tok_t[1]
    end
end

function lex.new(self, s)
    local o = {}
    setmetatable(o, self)

    local t = ''
    local states = {
                     ['new_t'] = function(c)
                                   if not c then return end
                                   t = c
                                   if c:match('[%a]') then
                                     return 'word_t'
                                   elseif c:match('[%d]') then
                                     return 'number_t'
                                   elseif c == SP then
                                     table.insert(o.tok_t, {c, 'space'})
                                     return 'new_t'
                                   elseif c:match(p_ATOM_SPECIAL) then
                                     table.insert(o.tok_t, {c, 'atomspecial'})
                                     return 'new_t'
                                   else
                                     table.insert(o.tok_t, {c, c})
                                     return 'new_t'
                                   end
                                 end,
                     ['word_t'] = function(c)
                                    if c and c:match('[%w]') then
                                      t = t..c
                                      return 'word_t'
                                    else
                                      local t_type = o.keywords[t]
                                      if not t_type then t_type = 'word' end
                                      table.insert(o.tok_t, {t, t_type})
                                      return 'new_t', c
                                    end
                                  end,
                     ['number_t'] = function(c)
                                      if c and c:match('%d') then
                                        t = t..c
                                        return 'number_t'
                                      else
                                        table.insert(o.tok_t, 
                                                     {t, 'number'})
                                        return 'new_t', c
                                      end
                                    end,
                   }
    o.tok_t = {}
   
    local next_state = 'new_t'
    for c in s:gmatch(".") do
        if c:match('%z') then 
            error("IMAP commands can't have NUL (0x00) char")
        end
        while c do
            next_state, c = states[next_state](c)
        end
    end
    states[next_state]() -- this is to make sure the final token is processed
                         -- unfortunately the way the loop is constructed, it
                         -- won't complete processing of the final token
    return o
end

-- define functions to handle parsing 2 arguments- creates a closure
-- using the supplied functions
local parse2Args = function(pFunc1, pFunc2)
                       return function(l)
                                return l:token()[2] == 'space' and pFunc1(l) and
                                       l:token()[2] == 'space' and pFunc2(l)
                              end
                   end

-- define function to return closure that parses 2 arguments where either
-- argument can be a literal
local parse2ArgsWithLiterals = function(pFunc1, pFunc2)
                                   return function(l)
                                            if l:token()[2] ~= 'space' then
                                              return false 
                                            end
                                            local r = pFunc1(l)
                                            if not r then return false
                                            elseif r == '}' then return true end
                                            return l:token()[2] == 'space' and
                                                   pFunc2(l)
                                          end
                               end

-- define function to handle a single 'mailbox' argument
local parse1Arg = function(pFunc)
                      return function(l)
                               return l:token()[2] == 'space' and pFunc(l)
                             end
                  end

--define function to handle case of no arguments
local argNone = function(l)
                    return true
                end

local function parseQString(l)
--[[
    per RFC3501- quoted = DQUOTE *QUOTED-CHAR DQUOTE
     Already parsed a '"', valid characters are anything but CR, LF
     and '\' or '"' unless they are backslash-escaped, ie: '\\' or '\"'
     final character is an unescaped '"'
--]]
    local t = l:token()
    while t and t[1] ~= '"' do
        if t[1] == '\\' then
            -- check for a '"' or '\' as next token
            local pt = l:peek()
            if pt[1] == '"' or pt[1] == '\\' then
                l:token()
                t = l:token()
            else
                return false
            end
        elseif t[1]:match('[\\"%c%z]') then
            return false
        end
        t = l:token()
    end
    -- yep, a valid quoted string
    return true
end

local function parseLiteral(l)
    -- parse a '{', so what follows should be a number followed by a '}'
    local t = l:token()
    if t[2] ~= 'number' then return false end
    local n = tonumber(t[1])
    if n < 0 and n >= 4294967296 then return false end
    t = l:token()
    if t[1] ~= '}' then return false end
    -- a literal is ALWAYS the last part of a command, or it should be
    if not l:peek() then
        return '}'
    else
        -- another token followed the '}', bad bad bad
        return false
    end
end

local function parseString(l)
--[[
    per RFC3501-
      string = quoted / literal
      quoted = DQUOTE *QUOTED-CHAR DQUOTE
      literal = "{" number "}" CRLF *CHAR8
      number = 1*DIGIT
--]]
    local pt = l:peek()
    if pt[1] == '"' then
        l:token()
        return parseQString(l)
    elseif pt[1] == '{' then
        l:token()
        return parseLiteral(l)
    end
    return false
end

local function parsePattern(l, p)
    local rval = false
    while true do
        local pt = l:peek()
        if not pt then break end
        local m = pt[1]:match(p)
        if not m or #m ~= #pt[1] then break end
        rval = true
        l:token()
    end
    return rval
end

local function parseAtom(l)
--[[
    The lexer is very simple, breaking up a string into tokens based
    on space, alpha-numerics, or punctuation chars.  An ATOM, as defined
    by RFC3501 can contain any one of a number of punctuation chars
    that the lexer will separate into distinct tokens (as opposed to a long
    unbroken token string).  Thus, we have to loop over tokens and accept 
    anything that is an ATOM_CHAR.
--]]
    return parsePattern(l, p_ATOM)
end

local function parseAString(l)
    -- per RC3501: astring = 1*ASTRING-CHAR / string
    return parsePattern(l, p_ASTRING_CHAR..'+') or parseString(l)
end

local function parseNumber(l)
    return parsePattern(l, "%d+")
end

local function parseDate(l)
    local function parseDateText(l)
        return l:peek()[2] == 'number' and #(l:token()[1]) <= 2 and
               l:token()[1] == '-' and lex.date_month[l:token()[1]:lower()] and
               l:token()[1] == '-' and l:peek()[2] == 'number' and
               #(l:token()[1]) == 4
    end
    local pt = l:peek()
    if pt[1] == '"' then
        l:token()
        return parseDateText(l) and l:token()[1] == '"'
    else
        return parseDateText(l)
    end
end

local function parseMailbox(l)
    -- per RFC3501- mailbox = "INBOX" / astring
    -- where "INBOX" is case insensitive ("iNbOx" is stupid, but valid)
    local mb = l:peek()[1]
    if mb:upper() == 'INBOX' then 
        l:token()
        return true 
    end
    -- can return true, false or '}' indicating a literal argument 
    return parseAString(l)
end

local function parseSeqSet(l)
--[[
    per RFC3501- seq-set = (seq-number / seq-range) *("," seq-set)
                 seq-number = nz-number / "*"
                 seq-range = seq-number ":" seq-number
                 nz-number = digit-nz *DIGIT
                 digit-nz = 1-9
--]]
    local function chkNumber(t)
        if t[1] == '*' or 
           (t[2] == 'number' and t[1]:match("^[123456789]%d*")) then
            return true
        end
        return false
    end

    local function pSeqRange(l)
        local t = l:token()
        if not chkNumber(t) then return false end
        t = l:peek()
        if t[2] == 'space' then return true
        elseif t[2] == ',' then 
            l:token()
            return parseSeqSet(l)
        end
        return false
    end
    
    local function pSeqNumber(l)
        local t = l:token()
        if not chkNumber(t) then
            return false 
        end
        t = l:peek()
        -- sequence sets can be specified in search commands, in which case
        -- they may be the last element in the command
        if not t then return true end
        if t[2] == 'space' then return true end
        t = l:token()
        if t[2] == ':' then
            return pSeqRange(l)
        elseif t[2] == ',' then
            return pSeqNumber(l)
        else
            return false
        end
    end

    return pSeqNumber(l)
end

local function parseStatusAtt(l)
    local t = l:token()
    if t[1] ~= '(' then return false end
    while true do
        t = l:token()
        if not lex.status[t[1]] then return false end
        t = l:token()
        if t[1] == ')' then break end
        if t[1] ~= 'space' then return false end
    end
    return true
end

local function parseFlags(l)
--[[
    per RFC3501:
      flag-list = "(" [flag *(SP flag)] ")"
      flag =  "\Answered" / "\Flagged" / "\Deleted" / "\Seen" /
              "\Draft" / flag-keyword / flag-extension
                ; Does not include "\Recent"
      flag-keyword = atom
      flag-extension = "\" atom

    NOTE: This doesn't parse the '(' or ')' of the flag list
          That should be handled by the caller, as they can  be optional in a 
          STORE but not an APPEND
--]]
    local pt = l:peek()
    -- check for flag-keyword case
    if pt[1] ~= '\\' then
        if not parseAtom(l) then return false end
        l:token()
    else
        l:token() -- "pop" the '\'
        -- must be a defined flag or a flag-extension
        pt = l:peek()
        if lex.flags[pt[1]:lower()] then
            l:token() -- "pop" the flag token
        elseif not parseAtom(l) then
            return false
        end
    end
    -- OK, parsed a valid flag, but we can't make any
    -- assumptions about end of command, so return true if the
    -- next token isn't a space, otherwise continue parsing flags
    if l:peek() and l:peek()[2] == 'space' then
        l:token()
        return parseFlags(l)
    end
    return true 
end

local function parseStoreAttFlags(l)
--[[
    per RFC3501:
      store-att-flags = (["+" / "-"] "FLAGS" [".SILENT"]) SP 
                        (flag-list / (flag *(SP flag)))
--]]
    local t = l:token()
    if t[1] == '-' or t[1] == '+' then t = l:token() end
    if t[1]:upper() ~= 'FLAGS' then return false end
    t = l:token()
    if t[1] == '.' then
        t = l:token()
        if t[1]:upper() ~= 'SILENT' then return false end
        t = l:token()
    end
    if t[2] ~= 'space' then return false end
    if l:peek()[1] == '(' then
        l:token()
        if not parseFlags(l) then return false end
        if l:token()[1] ~= ')' then return false end
        return true
    else 
        return parseFlags(l)
    end
end

local function parseListMailbox(l)
--[[
    per RFC3501:
      list-mailbox = 1*list-char / string
      list-char = ATOM-CHAR / list-wildcards / resp-specials
      list-wildcards = "%" / "*"
      resp-specials = "]"
--]]
    return parseString(l) or parsePattern(l, p_LIST_CHAR.."+")
end

local function parseDateTime(l)
--[[
    per RFC3501:
      date-time = DQUOTE date-day-fixed "-" date-month "-" date-year SP time SP
                  zone DQUOTE
      date-day-fixed = (SP DIGIT) / 2DIGIT
      date-month = "Jan" / "Feb" / "Mar" / "Apr" / "May" / "June" /
                   "Jul" / "Aug" / "Sep" / "Oct" / "Nov" / "Dec"
      date-year = 4DIGIT
      time = 2DIGIT ":" 2DIGIT ":" 2DIGIT
      zone = ("+" / "-") 4DIGIT

      It is assumed that the opening DQUOTE has already been parsed
--]]
    -- first entity is 2 characters, space filled if 1 digit
    local t = l:token()
    if t[2] == 'space' then
        t = l:token()
        if t[2] ~= 'number' or #t[1] ~= 1 then return false end
    elseif t[2] ~= 'number' or #t[1] ~= 2 then
        return false 
    end
    -- now check the month and year
    if l:token()[1] ~= '-' or not lex.date_month[l:token()[1]:lower()] or
       l:token()[1] ~= '-' or l:peek()[2] ~= 'number' or #(l:token()[1]) ~= 4 or
       l:token()[2] ~= 'space' then
        return false
    end
    -- check time syntax
    for i, n in ipairs( {23, 59, 59} ) do
        t = l:token()
        if t[2] ~= 'number' or #t[1] ~= 2 or tonumber(t[1]) > n then
            return false
        end
        if i ~= 3 then
            if l:token()[1] ~= ':' then return false end
        end
    end
    if l:token()[2] ~= 'space' then return false end
    -- check zone syntax
    t = l:token()
    if (t[1] ~= '+' and t[1] ~= '-') then return false end
    t = l:token()
    if t[2] ~= 'number' or #t[1] ~= 4 then return false end
    return l:token()[1] == '"'
end

local function parseHeaderList(l)
--[[
    per RFC3501:
      header-list = "(" header-fld-name *(SP header-fld-name) ")"
      header-fld-name = astring
--]]
    if not parseAString(l) then return false end
    if l:peek()[2] == 'space' then
        l:token()
        return parseHeaderList(l)
    end
    return l:token()[1] == ')'
end

local function parseSection(l)
--[[
    per RFC3501:
    section = "[" [section-spec] "]"
    section-spec = section-msgtext / (section-part ["." section-text])
    section-msgtext = "HEADER" / "HEADER.FIELDS" [".NOT"] SP header-list /
                       "TEXT"
    section-part = nz-number *("." nz-number)
    section-text = section-msgtext / "MIME"
--]]
    local function pSectionMsgText(l)
        local t = l:peek()
        if t[1]:lower() == 'text' then return true end
        if t[1]:lower() ~= 'header' then return false end
        l:token()
        t = l:peek()
        if t[1] ~= '.' then return true end
        l:token()
        if l:token()[1]:upper() ~= 'FIELDS' then return false end
        t = l:token()
        if t[1] == '.' then
            if l:token()[1]:upper() ~= 'NOT' then 
                return false
            end
            t = l:token()
        end
        if t[2] ~= 'space' or l:token()[1] ~= '(' then return false end
        return parseHeaderList(l)
    end 

    local function pSectionPart(l)
        local t = l:peek()
        if t[2] ~= 'number' or t[1]:match("^[0]%d*") then
            return false
        end
        l:token()
        t = l:peek()
        if t[1] ~= '.' then return true end
        l:token()
        return pSectionPart(l) 
    end

    local function pSectionSpec(l)
        if pSectionMsgText(l) then return true end
        if not pSectionPart(l) then return false end
        local t = l:peek()
        if t[1] ~= '.' then return true end
        l:token()
        if pSectionMsgText(l) then return true end
        if l:peek()[1]:lower() == 'MIME' then
            l:token()
            return true
        end
        return false
    end

    -- remarkably, a section can contain nothing (just "[]"), so check for it
    if l:peek()[1] == ']' then 
        l:token()
        return true
    end
    return pSectionSpec(l) and l:token()[1] == ']'
end

local function parseFetchAtt(l)
--[[
    Per RFC3501:
      fetch-att = "ENVELOPE" / "FLAGS" / "INTERNALDATE" / 
                  "RFC822" [".HEADER" / ".SIZE" / ".TEXT."] /
                  "BODY" ["STRUCTURE"] / "UID" /
                  "BODY" section ["<" number "." nz-number ">"] /
                  "BODY.PEEK" section ["<" number "." nz-number ">"]
--]]
    local t = l:token()
    if not lex.fetch[t[1]:lower()] then return false end
    local fa = t[1]:upper()
    if fa == 'RFC822' then
        -- we've already parsed the RFC822, so we're good, the only thing
        -- that matters now is if it's followed by a '.', if not then just
        -- return true and the caller will take care of the rest
        if not l:peek() or l:peek()[1] ~= '.' then
            return true
        end
        l:token() -- clear the '.'
        t = l:token()
        local rfc822_t = { header = 1, size = 1, text = 1 }
        return rfc822_t[t[1]:lower()] == 1
    elseif fa == 'BODY' then
        t = l:token()
        if t[1] == '.' and l:token()[1]:upper() ~= 'PEEK' then
            return false 
        end
        if t[1] ~= '[' or not parseSection(l) then return false end
        -- this checks for the end of the command, or the ')' that matches
        -- the '(' found by the caller(if one was found...)
        t = l:peek()
        if not t or t[1] == ')' then return true end
        t = l:token()
        if t[1] ~= '<' then return false end
        return l:token()[2] == 'number' and l:token()[1] == '.' and
               l:peek()[2] == 'number' and 
               l:token()[1]:match("^[123456789]%d*") and l:token()[1] == '>'
    end
    return true
end

local function parseSearchKey(l)
--[[
    The list is long, but basically consists of single word tokens or a series
    of space separated single word tokens.
--]]
    local searchonearg = { bcc = parse1Arg(parseAString),
                           before = parse1Arg(parseAString),
                           body = parse1Arg(parseAString), 
                           cc = parse1Arg(parseAString), 
                           from = parse1Arg(parseAString), 
                           keyword = parse1Arg(parseAtom), -- flag-keyword
                           on = parse1Arg(parseDate),
                           since = parse1Arg(parseDate),
                           subject = parse1Arg(parseAString),
                           text = parse1Arg(parseAString),
                           to = parse1Arg(parseAString),
                           unkeyword = parse1Arg(parseAtom), --flag-keyword
                           larger = parse1Arg(parseNumber),
                           ['not'] = parse1Arg(parseSearchKey), 
                           sentbefore = parse1Arg(parseDate),
                           senton = parse1Arg(parseDate),
                           sentsince = parse1Arg(parseDate),
                           smaller = parse1Arg(parseNumber),
                           uid = parse1Arg(parseSeqSet), 
                         }
    local searchtwoarg = { header = parse2ArgsWithLiterals(parseAString,
                                                           parseAString,
                                                           false),
                           ['or'] = parse2ArgsWithLiterals(parseSearchKey, 
                                                           parseSearchKey,
                                                           false),
                         }


    function pSearchKeyHelper(l, t)
        if not t then 
            -- key is not followed by anything
            if lex.searchnoarg[l:peek()[1]:lower()] then
                l:token()
                return true
            else
                return false
            end
        end
        local pf = t[l:peek()[1]:lower()]
        if not pf then return false end
        l:token()
        return pf(l)
    end

    if l:peek()[1] == '(' then
        l:token()
        while true do
            local r = parseSearchKey(l)
            if not r then return false
            elseif r == '}' then return true end
            local pt = l:peek()
            if not pt then return false end
            if pt[1] == ')' then 
                l:token() 
                return true
            elseif pt[2] ~= 'space' then
                return false
            end
            l:token()
        end
    end
    return pSearchKeyHelper(l) or 
           pSearchKeyHelper(l, searchonearg) or 
           pSearchKeyHelper(l, searchtwoarg) or
           parseSeqSet(l)
end

local parseList = function(l)
                  -- LIST SP mailbox SP list-mailbox
                      if l:token()[2] ~= 'space' then
                          return false
                      end
                      local r = parseMailbox(l)
                      if not r then return false
                      elseif r == '}' then return true end
                      if l:token()[2] ~= 'space' then
                          return false
                      end
                      return parseListMailbox(l)
                  end

local uid_commands =
{
    ['COPY'] = parse2Args(parseSeqSet, parseMailbox),
    ['FETCH'] = function(l)
                -- FETCH SP sequence-set SP ("ALL / "FULL" / "FAST" / 
                -- fetch-att / "(" fetch-att *(SP fetch-att) ")")
                  if l:token()[2] ~= 'space' or not parseSeqSet(l) or
                     l:token()[2] ~= 'space' then
                    return false
                  end
                  local pt = l:peek()
                  if pt[1] == '(' then
                    l:token()
                    while true do
                      if not parseFetchAtt(l) then return false end
                      pt = l:token()
                      if pt[1] == ')' then break
                      elseif pt[2] ~= 'space' then return false end
                    end
                  else
                    if pt[2] == 'fetchmacro_wd' then
                      l:token()
                    elseif not parseFetchAtt(l) then
                      return false
                    end
                  end
                  return true
                end,
    ['STORE'] = parse2Args(parseSeqSet, parseStoreAttFlags),
    ['SEARCH'] = function(l)
                 -- SEARCH [SP "CHARSET" SP astring] 1*(SP search-key)
                   if l:token()[2] ~= 'space' then return false end
                   local t = l:peek()
                   if t[1]:upper() == 'CHARSET' then
                     l:token()
                     if l:token()[2] ~= 'space' then return false end
                     local r = parseAString(l)
                     if not r then return false
                     elseif r == '}' then return true end
                     if l:token()[2] ~= 'space' then return false end
                   end
                   while true do
                       local r = parseSearchKey(l)
                       if not r then return false
                       elseif r == '}' then return true end
                       if not l:peek() or l:peek()[2] ~= 'space' then
                           return true
                       end
                       l:token()
                   end
                 end,
}
setmetatable(uid_commands, 
             {__index = function(t,k)
                          local f = rawget(t,k)
                          if not f then
                            return function(l) return false end
                          end
                          return f
                        end
             }
            )             

local command = 
{
-- Per RFC3501:  command_any = "CAPABILITY" / "LOGOUT" / "NOOP" / x-command
    ['CAPABILITY'] = argNone,
    ['LOGOUT'] = argNone,
    ['NOOP'] = argNone,
--[[
    Per RFC3501- command_auth = append / create / delete / examine / list /
                                lsub / rename / select / status / subscribe /
                                unsubscribe
--]]
    ['APPEND'] = function(l)
                   -- APPEND SP mailbox [SP flag-list] [SP date-time] SP
                   -- literal  
                   if l:token()[2] ~= 'space' then return false end
                   local r = parseMailbox(l)
                   if not r then return false
                   elseif r == '}' then return true end
                   if l:token()[2] ~= 'space' then return false end
                   if l:peek()[1] == '(' then
                     l:token()
                     if not parseFlags(l) or
                        l:token()[1] ~= ')' or
                        l:token()[2] ~= 'space' then 
                       return false 
                     end
                   end
                   if l:peek()[1] == '"' then
                     l:token()
                     if not parseDateTime(l) or    
                        l:token()[2] ~= 'space' then
                       return false
                     end
                   end
                   if l:token()[1] ~= '{' then return false end
                   return parseLiteral(l) == '}'
                 end,
    ['CREATE'] = parse1Arg(parseMailbox),
    ['DELETE'] = parse1Arg(parseMailbox),
    ['EXAMINE'] = parse1Arg(parseMailboxtrue),
    ['LIST'] = parseList,
    ['LSUB'] = parseList,
    ['RENAME'] = parse2ArgsWithLiterals(parseMailbox, parseMailbox),
    ['SELECT'] = parse1Arg(parseMailbox),
    ['STATUS'] = parse2ArgsWithLiterals(parseMailbox, parseStatusAtt),
    ['SUBSCRIBE'] = parse1Arg(parseMailbox),
    ['UNSUBSCRIBE'] = parse1Arg(parseMailbox),
-- Per RFC3501: command-nonauth = login /authenticate / "STARTTLS"
    ['LOGIN'] = parse2ArgsWithLiterals(parseAString, parseAString),
    ['AUTHENTICATE'] = parse1Arg(parseAtom),
    ['STARTTLS'] = argNone,
--[[
    Per RFC3501- command-select = "CHECK" / "CLOSE" / "EXPUNGE" / copy / fetch /
                                  store / uid / search
--]]
    ['CHECK'] = argNone,
    ['CLOSE'] = argNone,
    ['EXPUNGE'] = argNone,
    ['UID'] = function(l)
                -- "UID" SP (copy / fetch / search / store)
                return l:token()[2] == 'space' and
                       uid_commands[l:token()[1]:upper()](l)
              end,
}
setmetatable(command,
             { __index = function(t,k)
                            if k[2] ~= 'command_wd' then
                              return function(l) return false end
                            end
                            k = k[1]:upper()
                            local f = rawget(t, k)
                            if f then 
                              return f 
                            elseif k:find("^X"..p_ATOM) then
                              return function(l)
                                       local my_t
                                       while l:peek() do
                                         my_t = l:token()
                                       end
                                       return my_t[2] ~= 'space'
                                     end
                            else
                              return uid_commands[k]
                            end
                          end
             }
            )

local function command_parse(imapcmd)
--[[
    Entry point for parsing a command.
    The parameter should consist of the everything except the tag and space
    following the tag and the trailing CRLF
    
    According to RFC3501:
      command = tag SP (command-any / command-auth / command-nonauth /
                command-select) CRLF
    
    So we're picking up from after the "tag SP" and excluding the concluding
    CRLF
--]]

    local lexer = lex:new(imapcmd)
    if not command[lexer:token()](lexer) then return false end
    return not lexer:token()
end

-- export parse function
parser = { command_parse = command_parse
         }
return parser
