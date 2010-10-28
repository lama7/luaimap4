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
    author: Gerry LaMontagne
    date created: 10/2010

    Conventions: 
    Seeing as this is a lua program, there are really no "private" entities in
    objects.  Everything within the object is available for manipulation by the
    programmer.  However, methods and variables that are not intended for public
    usage are prefixed with a "__".

    All objects are created using a "new" method.

    All objects names start with a capital letter.  Object methods use all
    lower-case with underscores separating words.

    In general, if an IMAP4 command is supported, it will be a lower-case method
    of an IMAP4 object.  For example, assuming an IMAP4 object 'imap,' IMAP4
    commands 'LOGIN', 'NOOP', and 'CAPABILITY' are as follows:

        imap.login('user', 'secret')
        imap.noop()
        imap.capability()

    DISCLAIMER:
    This library is intended to take care of the drudgery involved in
    interacting with an IMAP server.  It makes no attempt to enforce coding or
    design practices or protect a using program from itself.  So, for example,
    if you decide to login over an unsecured channel using this library, any
    problems that arise as a result are on you, the developer of said program.

    Anyone who uses this library should take the time to read the RFC3501 spec
    to understand exactly what these commands do and  what the recommendations
    are when considering designing an IMAP client.

--]]

-- create local environment
local table = require("table")
local string = require("string")
local socket = require("socket")
if not socket then
    error([[Unable to import socket library.  Imaplib requires lua-socket-
            please make sure it is installed and visible to the lua
            interpreter.]])
end
local ssl = require("ssl")
local mime = require("mime")
local md5 = require("md5")

--local __debug__ = print
local __debug__ = function() end

--[[

    CONSTANTS

--]]
local CRLF = "\r\n"
local IMAP4_port = 143

local IMAP4_states = { "NOT_AUTHENTICATED", "AUTHENTICATED", "SELECTED", "LOGOUT"}
local NONAUTH = IMAP4_states[1]
local AUTH = IMAP4_states[2]
local SELECT = IMAP4_states[3]
local LOGOUT = IMAP4_states[4]

local VALID_RESP_TOKENS = { ['+'] = true, ['*'] = true }
local VALID_RESPONSE_CODES = {
                              ALERT = true,
                              BADCHARSET = true,
                              CAPABILITY = true,
                              PARSE = true,
                              PERMANENTFLAGS = true,
                              ['READ-ONLY'] = true,
                              ['READ-WRITE'] = true,
                              TRYCREATE = true,
                              UIDNEXT = true,
                              UIDVALIDITY = true,
                              UNSEEN = true,
                             }
local RESP_COMPLETION = { OK = true, NO = true, BAD = true }

local UNIVERSAL = { [NONAUTH] = 1, [AUTH] = 1, [SELECT] = 1, [LOGOUT] = 1 }
local NOT_AUTHENTICATED = { [NONAUTH] = 1 }
local AUTHENTICATED = { [AUTH] = 1, [SELECT] = 1}
local SELECTED = { [SELECT] = 1 }
local ALLOWED_STATES = {
                        APPEND = AUTHENTICATED,
                        AUTHENTICATE = NOT_AUTHENTICATED,
                        CAPABILITY = UNIVERSAL,
                        CHECK = SELECTED,
                        CLOSE = SELECTED,
                        COPY = SELECTED,
                        CREATE = AUTHENTICATED,
                        DELETE = AUTHENTICATED,
                        EXAMINE = AUTHENTICATED,
                        EXPUNGE = SELECTED,
                        FETCH = SELECTED,
                        LIST = AUTHENTICATED,
                        LOGIN = NOT_AUTHENTICATED,
                        LOGOUT = UNIVERSAL,
                        LSUB = AUTHENTICATED,
                        NOOP = UNIVERSAL,
                        RENAME = AUTHENTICATED,
                        SEARCH = SELECTED,
                        SELECT = AUTHENTICATED,
                        STARTTLS = NOT_AUTHENTICATED,
                        STATUS = AUTHENTICATED,
                        STORE = SELECTED,
                        SUBSCRIBE = AUTHENTICATED,
                        UID = SELECTED,
                        UNSUBSCRBE = AUTHENTICATED,
                       }

-- UID Command tables
local copy_args = {}
local fetch_args = {}
local store_args = {}
local search_args = {}

-- String Patterns
local RESPONSE_TYPES_PAT = {"^(%*) ", "^(%+) ", "^(%a%d+) "}
local CONTINUE_PAT = "(.*)\r\n"
local RESPONSE_PAT1 = "^(%u+) (.*)\r\n"
local RESPONSE_PAT2 = "^(%d+) (%u+) ?(.*)\r\n"

--[[
 
    MISCELLANEOUS FUNCTIONS
 
--]]
local function keys(t)
    --[[
        Returns table of non-numeric key values in a table.

        Arguments:
            t:  any table

        Returns:
            Table of non-numeric(index?) key values
    --]]
    if not t then return nil end
    local key_t = {}
    for k in pairs(t) do 
        if type(k) ~= 'number' then 
            table.insert(key_t, k) 
        end
    end
    return key_t
end

local function issubset(t1, t2)
    --[[
        Return true if t1 is a subset of t2, works only for indexed tables

        Arguments:
            t1:  indexed table
            t2:  indexed table that establishes the set

        Returns:
            true if t1 is a subset of t2, false otherwise
    --]]
    if not t1 then return true end
    local set_t = {}
    for i,v in ipairs(t2) do set_t[v] = true end
    for i,v in ipairs(t1) do 
        if not set_t[v] then return false end
    end
    return true
end

local function makeordered(t, order)
    --[[
        Returns a table whose values come from a key-value table and in a
        specified order.

        Arguments:
            t: a key-value based table

            order: a table which lists the keys of table 't' in the order they
                   should be inserted into the return table

        Returns:
            Indexed table whose values are those of table 't' and is ordered
            according to the key order specified in 'order' or nil if t is nil
    --]]
    if not t then return t end
    local new_t = {}
    for i,v in ipairs(order) do
        if t[v] then table.insert(new_t, t[v]) end
    end
    if #new_t ~= 0 then 
        return new_t 
    else
        return t
    end
end

local function arg_error(argt, literals)
    local err_msg = ''
    local literal_i = 1
    for i,v in ipairs(argt) do
        if v[1] == ''  then
            if literals and not literals[literal_i] then
                err_msg = err_msg..v[1]..CRLF
            else
                literal_i = literal_i + 1
            end
        end
    end
    error(err_msg)
end

--[[
    Authentication Table

    This table contains the supported authenication mechanisms.  It consists of
    a mechanism as the key and then a function which takes a table as an
    argument and returns a function that that takes a server response as an
    argument.  No pre-processing of server responses is done prior to calling
    the function.

    The server response processing function should return 2 values: a string to
    be sent as the client response to the server, and a function to respond to
    subsequent server responses.  If none are expected, then return 'nil'.
--]]
AUTH_T = { ['CRAM-MD5'] = function(argt)
                          return function (s_resp)
                                     local key = argt['pw']
                                     local user = argt['user']
                                     local text = mime.unb64(s_resp)

                                     if #key > 64 then key = md5.sum(key) end
                                     if #key < 64 then 
                                         key = key..string.char(0):rep(64-#key)
                                     end
                                     local ixor = md5.exor(key,
                                             string.char(54):rep(64))
                                     local oxor = md5.exor(key,
                                             string.char(92):rep(64))
                                     local digest =
                                                   md5.sumhexa(oxor..md5.sum(ixor..text))
                                     return mime.b64(user.." "..digest), nil
                                 end
                      end
         }


--[[
    Response Object

    An object for holding and accessing responses from an IMAP server.

    tagged responses-   an indexed table with the tag, result and text as seperate
                        entities within the table.  
    
    untagged responses- a keyed table with the initial atom of the response used
                        as the key.

    response codes- these are a portion of either a tagged or untagged response
                    that appear in square brackets.  It is provided as a
                    courtesy to the using program so that response content can
                    be more quickly parsed if expecting certain information.
--]]
local Response = {}
Response.__index = Response  -- make table usable as an object

function Response.__print(self)
    print("RESPONSES:")
    if next(self.__tagged) then
        print("\tTAGGED:")
        for k,v in pairs(self.__tagged) do
            print("\t\t"..k..":  "..v)
        end
    end
    if next(self.__untagged) then
    print("\tUNTAGGED:")
        for k,v in pairs(self.__untagged) do
            local resp = ''
            for i,s in ipairs(v) do
                if s[1] ~= '' then
                    resp = resp..s[1].."\r\n\t\t\t"    
                end
                if s[2] then
                    resp = resp..s[2].."\r\n\t\t\t"
                end
            end
            print("\t\t"..k..":\t"..resp)
        end
    end
    if next(self.__codes) then
        print("\tCODES:")
        for k,v in pairs(self.__codes) do
            local resp = ''
            for i,s in ipairs(v) do
                resp = resp..s.."\r\n\t\t\t"
            end
            print("\t\t"..k..":\t"..resp)
        end
    end
end

function Response.__check_response_codes(self, content)
    local ct = self.__codes
    local rcode, data = content:match("%[([A-Z-]+) ?(.*)%]")
    if rcode and VALID_RESPONSE_CODES[rcode] then
        if not ct[rcode] then ct[rcode] = {} end
        table.insert(ct[rcode], data)
    end
end

function Response.__getitem(self, t, i)
    local ret_t = {}
    for i,m in ipairs(t) do table.insert(ret_t, m[i]) end
    return ret_t
end

--[[
    PUBLIC METHODS
--]]
function Response.add_tagged(self, tag, data)
    --[[
        For adding tagged responses to the response object- not needed by a
        Response object user.

        arguments:
        tag:  tag of tagged response
        data:  the data portion of the response, includes result code and other 
               content

        returns: nothing
    --]]
    local result, content = data:match(RESPONSE_PAT1)
    if not content then content = '' end
    self.__tagged['tag'] = tag
    self.__tagged['result'] = result
    self.__tagged['content'] = content
    self:__check_response_codes(content)
end

function Response.add_untagged(self, data)
    --[[
        For adding untagged responses from the server to a response object.
        Untagged responses can be a result of a command, or responses read from
        the line which were unsolicited.

        arguments:
        data:  response string for parsing

        returns: nothing
    --]]
    __debug__("Response.add_untagged:")
    __debug__(data)
    local ut = self.__untagged
    local msg_num, typ, content = data:match(RESPONSE_PAT2)
    if not msg_num then
        typ, content = data:match(RESPONSE_PAT1)
    end
    if not ut[typ] then ut[typ] = {} end
    if not content then content = '' end
    table.insert(ut[typ], { content, msg_num })
    self.__last_untagged = typ
    self:__check_response_codes(content)
end

function Response.add_continuation(self, data)
    --[[
        Appends continuation data received from the server to a continuation
        string.
    --]]
    self.__continuation = self.__continuation..data
end

function Response.add_literal(self, data)
    local t = self.__untagged[self.__last_untagged] 
    t = t[#t]
    t[1] = t[1]..CRLF..data
end

function Response.getContinuation(self)
    return self.__continuation
end

function Response.getTaggedTag(self)
    --[[
        Returns the tag responded to be the server.

        Arguments: none

        Returns:  The actual tag received from the server in response to a
                  tagged command to the server.
    --]]
    return self.__tagged.tag
end

function Response.getTaggedResult(self)
    --[[
        For getting the result code of the tagged server response.

        Arguments: none
        
        Returns:  The result code for a tagged command, typically OK, BAD, NO
    --]]
    return self.__tagged.result
end

function Response.getTaggedContents(self)
    --[[
        Returns content of a tagged response- basically everything after the
        result code.

        Arguments: none

        Returns:  All tagged response content following the result code.  This
                  is typically something about the command completed, or why it
                  failed or what error occurred.  This will also contain
                  bracketed response codes.
    --]]
    return self.__tagged.content
end

function Response.getResponseCodes(self)
    --[[
        Returns all response codes from both tagged and untagged response.

        Arguments: none

        Returns:  A table with all IMAP4Rev1 defined response codes received in
                  the tagged and untagged responses.  Server implementation
                  dependent response codes are not included here.
    --]]
    if self.__codes then
        return keys(self.__codes)
    else
        return nil
    end
end

function Response.getResponseCodeContent(self, code)
    --[[
        Returns the content associated with a response code.

        Arguemnts: the response code whose content to look up

        Return: the content corresponding to the supplied code, or nil if the
                code is not in this response.
    --]]
    if self.__codes[code] then
        return self.__codes[code]
    else
        return nil
    end
end

function Response.getUntaggedResponse(self)
    --[[
        Returns all untagged responses

        Arguments:  none

        Return:  A table of with all response codes received in the response.
    --]]
    if next(self.__untagged) then
        return keys(self.__untagged)
    else
        return nil
    end
end

function Response.getUntaggedContent(self, typ)
    --[[
        Returns all content for specified untagged response code.

        Arguments: untagged response type, such as OK, or LIST

        Returns: a table containing all responses corresponding to the supplied
                 type
    --]]
    if not self.__untagged[typ] then
        return nil
    end
    local ret_t = {}
    for i,v in ipairs(self.__untagged[typ]) do
        table.insert(ret_t, v[1])
    end
    return ret_t
end

function Response.getUntaggedNum(self, typ)
    --[[
        Returns a numeric value associated with certain response codes.

        Arguments: untagged response type, such as OK or LIST

        Returns: The number corresponding to the untagged response type.  For
                 example, if the type is EXISTS the number returned is the
                 number of existing messages in a mailbox.
    --]]
    if self.__untagged[typ][2] then  
        return self.__untagged[typ][2]
    else
        return nil
    end
end

function Response.new(self)
    -- create the object
    local o = {}
    setmetatable(o, self)

    -- now do some initialization
    o.__tagged = {}
    o.__untagged = {}
    o.__codes = {}
    o.__continuation = ''

    return o
end




--[[
    IMAP4 Object

    Handles command-response exchanges with an IMAP4Rev1 server (RFC3501).  The
    main goal is to provide an interface so the drudgrey of the under-the-hood
    related IMAP protocol is handled but still gives an app developer the
    control to deal with responses as they see fit.

    This code makes no attempt to enforce good client design practices.  It
    is merely meant to serve as a starting point.  Outside of dropped
    connections and properly formed commands to and from the server, error
    handling and IMAP4Rev1 best practices are left to the user of this library.

    To create an object, use IMAP4.new(hostname, [port]).  Then, use the object
    to transmit commands to the server.  For instance, a 'NOOP' command is sent
    using the 'noop' method:
    
        imap = IMAP4:new('my.mail.server')
        imap:noop()

    All IMAP4Rev1 command methods are case insensitive.  So the `SELECT` command
    can be invoked using either of the following forms:

        imap:select()
          - or -
        imap:SELECT()

    All command methods return a Response object.  This object contains all
    responses from the server up to the tag completion response.  So server
    initiated responses will be buried in the "untagged" portion of the
    Response object.

    Commands which require arguments all support the IMAP literal form.  In
    fact, it is possible to mix and match the named arguments with literal ones.
    Commands which require arguments name those arguments for the simplest usage.
    Alternatively, those arguments can be placed into a table and passed into
    the method using the `literals` argument.  The IMAP4 object will handle the
    rest.

    If mixing named and literal arguments, the rule is simple- literals are used
    sequentially for each required named command.  For example, it is possible
    to login as follows:
     
        imap:login('', 'secret', { 'user' })

    The 'user' argument will be sent in its literal form and the password
    argument will be sent in string form.

    Use of literals allows for more character support in command arguments (the
    space character is a good example).  The cost is in extra line turn around,
    since the server has to be told how many bytes to expect, and those bytes
    cannot be sent until the server is ready for them.  Thus the above login
    example takes 2 client transmissions.  Essentially, each literal requires
    it's own transmission.

    Literals can be supplied in two ways- an indexed table or a key-value table.
    The key-value table is internally converted to an indexed table where values
    are placed in the appropriate order for the command.  For commands that have
    optional arguments, you SHOULD use the key-value form as the command results
    cannot be guaranteed if the indexed form is used in these cases.

    For the key-value form of the literal table, the keys MUST be the same as
    the named argument equivalent.  So, for example the `login` function is
    defined with arguments `user` and `pw`.  So the command would look like:

        imap:login(nil, nil, {user = 'me', pw = 'secret'})

--]]
local IMAP4 = {}
local RECEIVE_SIZE = 4096
-- The following makes IMAP4 table usable as an object and allows for using the 
-- IMAP methods as all caps. 
-- For example: 
--     imap:NOOP() is mapped to imap:noop().
IMAP4.__index = function (t,k)
                    if ALLOWED_STATES[k] then
                        return IMAP4[k:lower()]
                    else
                        return IMAP4[k]
                    end
                end
--[[
    
    PRIVATE METHODS

--]]
function IMAP4.__open_connection(self)
    self.__connection = assert(socket.connect(self.host, self.port), 
                               'Unable to establish connection with host.\r\n')
    self.__connection:settimeout(0)
end

function IMAP4.__receive(self)
    --[[
        Buffers data in `__received_data` and returns as soon as data is
        available.
    --]]
    while true do
        local data, emsg, partial = self.__connection:receive(RECEIVE_SIZE)
        if data then self.__received_data = self.__received_data..data end
        if partial then self.__received_data = self.__received_data..partial end
        if emsg == 'timeout' and #self.__received_data ~= 0 then
            return
        elseif emsg == 'wantread' then
            -- 'wantread' is a timeout indicator for an SSL/TLS connection 
            if not data and not partial then
                socket.select({ self.__connection }, nil)
            else
                return
            end 
        elseif emsg == 'closed' then
            if #self.__received_data == 0 then
                error('Remote closed connection unexpectedly.')
            end
            return
        end
    end
end

function IMAP4.__read(self, cnt)
    --[[
        Reads `cnt` bytes from the received buffer.  If the request is for more
        than is available, read until enough is available to satisfy the
        request.  Otherwise, pulls out `cnt` bytes from the buffer to return and
        adjusts the buffer to reflect the removed bytes
    --]]
    -- add 1 because index starts at 1, plus 2 more to get trailing CRLF
    cnt = cnt + 3
    while #self.__received_data <= cnt do
        self:__receive()
    end
    local ret_str = self.__received_data:sub(1, cnt)
    self.__received_data = self.__received_data:sub(cnt + 1, -1)
    return ret_str
end

function IMAP4.__readline(self)
    --[[
        Return a CRLF terminated string from the receive buffer.  If no CRLF
        exists in the buffer, then read until there is.
    --]]
    local line = ''
    while #self.__received_data == 0 and not self.__received_data:find(CRLF) do
        self:__receive()
    end

    -- IMAP4 spec states all responses must end in CRLF
    line, self.__received_data = self.__received_data:match("(.-\r\n)(.*)")
    return line
end

function IMAP4.__get_response(self)
    __debug__("__get_response")
    local resp = self:__readline()
    if not resp then return nil end

    local r_type, s = self:__get_type(resp)
    if r_type == '*' then
        -- untagged response
        self.__current_response:add_untagged(s)
        -- check for literal at the end of the response
        local m = s:match(".*%{(%d+)%}\r\n")
        if m then 
            s = self:__read(m)
            self.__current_response:add_literal(s)
        end
    elseif r_type == '+' then
        -- continuation response
        self.__current_response:add_continuation(s)
        return r_type
    else
        -- tagged completion
        if not self.__tags[r_type] then
            error("Unknown tag from server: "..r_type)
        end
        self.__current_response:add_tagged(r_type, s)
    end
    return r_type
end

function IMAP4.__send_command(self, tag, cmd, args)
    local out = ''
    if args then
        out = cmd..args
    else
        out = cmd
    end
    self.__tags[tag] = out
    assert(self.__connection:send(tag..' '..out..CRLF))
end

function IMAP4.__send_continuation(self)
    local out
    if self.__literal_func then
        out, self.__literal_func =
                  self.__literal_func(self.__current_response:getContinuation())
    else
        local arg
        local flag
        out = self.__literals[1]
        table.remove(self.__literals, 1)
        if #self.__argt ~= 0 then
            repeat
                arg = self.__argt[1][1]
                flag = self.__argt[1][3]
                table.remove(self.__argt, 1)
            until arg ~= ' ' 
            if arg ~= '' then
                if flag == 'l' then
                    table.insert(self.__literals, arg)
                else 
                    out = out.." "..arg 
                end
            end
        end
        if #self.__literals ~= 0 then out = out.." "..self:__add_literal() end
    end
    assert(self.__connection:send(out..CRLF))
end

function IMAP4.__synchronous_cmd(self, cmd, args)
    states = assert(ALLOWED_STATES[cmd], "Invalid command requested: "..cmd)
    assert(states[self.__state], 
           "Command '"..cmd.."' not valid in "..self.__state.." state.")
    local tag = self:__next_tag()
    self:__new_response()
    self:__send_command(tag, cmd, args)
    repeat
        local rtype = self:__get_response(tag)
        if rtype == '+' then
            self:__send_continuation()
        end
    until rtype == tag

    return self.__current_response
end

function IMAP4.__get_type(self, s)
    for i, re in ipairs(RESPONSE_TYPES_PAT) do
        local m = s:match(re)
        if m then
            -- The 2 below is needed because indexing starts at 1, plus a 
            -- ' ' always follows the response identifier contained in m
            s = s:sub(2+#m, -1)
            return m, s
        end
    end
    error("Invalid server response: " .. s)
end

function IMAP4.__add_literal(self)
    return string.format("{%u}", #self.__literals[1])
end

function IMAP4.__next_tag(self)
    self.__tag_num = self.__tag_num + 1
    return string.format("%s%04u", self.__tagpre, self.__tag_num)
end

function IMAP4.__new_response(self)
    self.__current_response = Response:new()
    table.insert(self.__responses, self.__current_response)
    return self.__current_response
end

function IMAP4.__check_args(self, cmdargs, literals, ...)
    --[[
        Checks command arguments.  There are seveeral things to consider:
        1.  The imap command arguments are present
        2.  That literal substitution for named commands are accounted for
        3.  That any option commands are accoutned for
        4.  Named literals must be checked and turned into an indexed table

        Arguments:
            cmdargs:  this is a list of the arguments that go with an IMAP4Rev1
                      command.  The list should be specified in the order that
                      the IMAP4Rev1 command expects
            literals: a table, either indexed or key-value, of literal
                      substitutions for cmd arguments
            ...: A series of tables with the following entries:
                  1.  the parameter for a command argument or a blank indicating
                      literal substitution, or a space indicating an optional
                      argument that should not be used for literal substitution
                  2.  An error message explaining what argument error occured
                  3.  A flag field used when processing arguments, used for 
                      creating parenthesized lists('()'), or if a required
                      argument should be processes as a literal ('l')

        Returns:
            Nothing, but there are a number of side effects:
            1. If a key-value literal table is supplied it is converted to an 
               indexed table with values in the appropriate order
            2. The `__literals` member of the object is populated if any
               literals are specified
            3. The `__argt` table is assigned with the arguments as supplied
               by the calling method.

    --]]
    local named_argt = {...}
    local n_args = #named_argt
    local maxargs = #cmdargs
    local n_literals = 0
    local named_args = 0
    if literals then 
        assert(issubset(keys(literals), cmdargs), 
               "Invalid literal key in literals")
        literals = makeordered(literals, cmdargs)
        n_literals = #literals 
    end
    for i,v in ipairs(named_argt) do
        if v[1] ~= '' then named_args = named_args + 1 end
    end
    local arg_total = named_args + n_literals
    if maxargs ~= n_args then
        if arg_total < n_args then 
            error("Too few command args provided")
        elseif arg_total > maxargs then
            error("Too many command args provided")
        end
    elseif named_args + n_literals ~= n_args then
        arg_error(named_argt, literals)
    end
    self.__literals = literals or {}
    self.__argt = named_argt
end

function IMAP4.__build_arg_str(self)
    local arg_str = ''
    while #self.__argt ~= 0 do
        local arg = self.__argt[1][1]
        local flag = self.__argt[1][3]
        table.remove(self.__argt, 1)
        if arg ~= ' ' then 
            if arg ~= '' then
                if flag =='l' then
                    table.insert(self.__literals, arg)
                    arg_str = arg_str..' '..self:__add_literal()
                    break
                elseif flag == '()' then 
                    arg = '('..arg..')' 
                elseif flag == 'C' then
                    arg = 'CHARSET '..arg
                end
                arg_str = arg_str..' '..arg
            elseif #self.__literals ~= 0 then
                arg_str = arg_str..' '..self:__add_literal()
                break
            end
        end
    end
    return arg_str
end

--[[

    PUBLIC METHODS

--]]
function IMAP4.append(self, mb_name, msg_literal, opt_flags, opt_datetime)
    --[[
        Sends an IMAP4Rev1 APPEND command.  Please note that using the `literals`
        argument may not provide the expected result with some servers.  The
        best way to get the desired result is to use the named arguments.
    --]]
    self:__check_args({'mb_name', 'opt_flags', 'opt_datetime', 'msg_literal'},
                      nil, 
                      { mb_name or '',
                        "You must supply a mailbox name when using 'APPEND'"},
                      { msg_literal or '',
                        "You must supply a string to append to the mailbox", 
                        'l' } )

    table.insert(self.__argt, 2, { opt_flags or ' ', '', '()'} )
    table.insert(self.__argt, 3, { opt_datetime or ' ', '' } )
    return self:__synchronous_cmd('APPEND', self:__build_arg_str())
end

function IMAP4.authenticate(self, auth_mech, argt)
    --[[
        `auth_mech` is the text that appears after the "AUTH=" in server
        capabilities
        `argt` is a key-value table used to create a closure over the
        authenticating function
    --]]
    assert(md5, [[The md5 library for lua is not installed.  Please install it
                  in order to make use of CRAM-MD5 authentication.]])
    local auth_func =  assert(AUTH_T[auth_mech], 
                              "Authorization type not supported.")
    self.__literal_func = auth_func(argt)

    local r = self:__synchronous_cmd('AUTHENTICATE', " "..auth_mech)
    if r:getTaggedResult() == 'OK' then self.__state = AUTH end
    return r
end

function IMAP4.capability(self)
    return self:__synchronous_cmd('CAPABILITY')
end

function IMAP4.check(self)
    return self:__synchronous_cmd('CHECK')
end

function IMAP4.close(self)
    local r = self:__synchronous_cmd('CLOSE')
    if r:getTaggedResult() == 'OK' then self.__state = AUTH end
    return r
end

function IMAP4.copy(self, seq_set, mb_name, literals)
    self:__check_args({ 'seq_set', 'mb_name' },
                      literals,
                      { seq_set or '',
                        "You must supply a sequence set when using 'COPY'" },
                      { mb_name or '',
                        "You must supply a mailbox name when using 'COPY'" } )
    return self:__synchronous_cmd('COPY', self:_build_arg_str())
end

function IMAP4.create(self, mb_name, literals)
    self:__check_args({ 'mb_name' }, 
                      literals,
                      { mb_name or '', 
                        "You must supply a mailbox name when using 'CREATE'" } )
    local mb_name = self:__build_arg_str()
    return self:__synchronous_cmd('CREATE', mb_name)
end

function IMAP4.delete(self, mb_name, literals)
    self:__check_args({'mb_name'}, 
                      literals,
                      { mb_name or '', 
                        "You must supply a mailbox name when using 'DELETE'" } )
    local mb_name = self:__build_arg_str()
    return self:__synchronous_cmd('DELETE', mb_name)
end

function IMAP4.examine(self, mb_name, literals)
    self:__check_args({'mb_name'},
                      literals,
                      { mb_name or 'INBOX', '' } )
    local mb = self:__build_arg_str()
    local r = self:__synchronous_cmd('EXAMINE', mb)
    if r:getTaggedResult() == 'OK' then
        self.__state = SELECT
    else
        self.__state = AUTH
    end
    return r
end

function IMAP4.expunge(self)
    return self:__synchronous_cmd('EXPUNGE')
end

function IMAP4.fetch(self, seq_set, data, literals)
    self:__check_args({'seq_set', 'data'},
                      literals,
                      {seq_set or '',
                       "You must provide a sequence set to fetch"},
                      {data or '',
                       "You must provide data times to fetch from messages" } )
    return self:__synchronous_cmd('FETCH', self:__build_arg_str())
end

function IMAP4.list(self, reference, mb_pattern, literals)
    self:__check_args({'reference', 'mb_pattern'},
                      literals,
                      { reference or '""', '' },
                      { mb_pattern or '*', '' } )
    return self:__synchronous_cmd('LIST', self:__build_arg_str())
end

function IMAP4.login(self, user, pw, literals)
    self:__check_args({'user', 'pw'}, literals,
                   { user or '', "You must supply a username to login with." },
                   { pw or '', "You must supply a password to login with. " })
    args = self:__build_arg_str()
    local r = self:__synchronous_cmd('LOGIN', args)
    if r:getTaggedResult() == 'OK' then self.__state = AUTH end
    return r
end

function IMAP4.logout(self)
    self.__state = LOGOUT
    return self:__synchronous_cmd('LOGOUT')
end

function IMAP4.lsub(self, reference, mb_pattern, literals)
    self:__check_args({'reference', 'mb_pattern'}, 
                      literals,
                      { reference or '""', '' },
                      { mb_pattern or '*', '' } )
    return self:__synchronous_cmd('LSUB', self:__build_arg_str())
end

function IMAP4.noop(self)
    return self:__synchronous_cmd('NOOP')
end

function IMAP4.rename(self, existing_name, new_name, literals)
    self:__check_args({'existing_name', 'new_name'},
                      literals,
                      { existing_name or '', 
                        "You must supply an existing mailbox name" },
                      { new_name or '',
                        "You must supply a new name for the mailbox" } )
    return self:__synchronous_cmd('RENAME', self:__build_arg_str())
end

function IMAP4.search(self, criteria, opt_charset, literals)
    --[[
        Use of `literals` to send `opt_charset` is not supported
    --]]
    self:__check_args({'opt_charset', 'criteria'},
                      literals,
                      { criteria or '', 
                        "You must specify search criteria" } )

    table.insert(self.__argt, 1, {opt_charset or ' ', '', 'C' } )
    return self:__synchronous_cmd('SEARCH', self:__build_arg_str())
end

function IMAP4.select(self, mb_name, literals)
    self:__check_args({'mb_name'},
                      literals,
                      { mb_name or 'INBOX', '' } )
    local mb = self:__build_arg_str()
    local r = self:__synchronous_cmd('SELECT', mb)
    if r:getTaggedResult() == 'OK' then
        self.__state = SELECT
    else
        self.__state = AUTH
    end    
    return r
end

function IMAP4.shutdown(self)
    if self.__state ~= LOGOUT then self:logout() end
    self.__connection:close()
end

function IMAP4.starttls(self)
    if not ssl then
        error([[The imaplib `STARTTLS` command requires `ssl` from the luasec
                library- please make sure it is installed and visible to the lua
                interpreter.]])
    end
    local r = self:__synchronous_cmd('STARTTLS')
    if r:getTaggedResult() == 'OK' then
        self.__connection:settimeout(nil)
        self.__connection = ssl.wrap(self.__connection, self.__sslparams)
        assert(self.__connection:dohandshake())
        self.__connection:settimeout(0)
    end
    return r
end

function IMAP4.status(self, mb_name, stat_item, literals)
    local stat_items = { MESSAGES = 1,
                         RECENT = 1,
                         UIDNEXT = 1,
                         UIDVALIDITY = 1,
                         UNSEEN = 1 }
    assert(stat_items[stat_item], "Invalid status item requested")
    self:__check_args({ 'mb_name', 'stat_item' }, 
                      literals,
                      { mb_name or '', 
                       "You must supply a mailbox name when using 'STATUS'" },
                      { stat_item, '', '()' } )
    return self:__synchronous_cmd('STATUS', self:__build_arg_str())
end

function IMAP4.store(self, seq_set, msg_data_item, value, literals)
    self:__check_args({ 'seq_set', 'msg_data_item', 'value' },
                      literals,
                      { seq_set or '',
                        "You must supply a sequence set when using 'STORE'" },
                   { msg_data_item or '',
                     "You must supply a message data item when using 'STORE'" },
                     { value or '',
                       [[You must supply a value for the message data item when
                       using 'STORE']], '()'} )
    return self:__synchronous_cmd('STORE', self:__build_arg_str())
end

function IMAP4.subscribe(self, mb_name, literals)
    self:__check_args({'mb_name'},
                      literals,
                   { mb_name or '', 
                     "You must supply a mailbox name when using 'SUBSCRIBE'" } )
    local mb_name = self:__build_arg_str()
    return self:__synchronous_cmd('SUBSCRIBE', mb_name)
end

function IMAP4.uid(self, cmd, ...)
    local valid_uid_cmd = { COPY = 2, FETCH = 2, STORE = 3, SEARCH = 1 }
    cmd = cmd:upper()
    local n_cmd_args = valid_uid_cmd[cmd]
    if not n_cmd_args then
        error("UID command does not support "..cmd)
    end
    local argt = {...}
    if (cmd == 'SEARCH' and #argt ~= 1 and #argt ~= 2) or
       (cmd ~= 'SEARCH' and #argt ~= n_cmd_args) then
        error("Invalid arguments for UID command '"..cmd.."'")
    end

    if cmd == 'SEARCH' and #argt == 2 then
        self.__argt[1] = { argt[1], '', 'C' }
        self.__argt[2] = { argt[2], '' }
    else
        for i,v in ipairs(argt) do
            if cmd == 'STORE' and i == 3 then
                self.__argt[i] = { v, '', '()' }
            else
                self.__argt[i] = { v, '' }
            end
        end
    end
    table.insert(self.__argt, 1, { cmd, '' } )
    return self:__synchronous_cmd('UID', self:__build_arg_str())
end

function IMAP4.unsubscribe(self, mb_name, literals)
    self:__check_args({'mb_name'},
                      literals,
                 { mb_name or '', 
                   "You must supply a mailbox name when using 'UNSUBSCRIBE'" } )
    local mb_name = self:__build_arg_str()
    return self:__synchronous_cmd('UNSUBSCRIBE', mb_name)
end

function IMAP4.xatom(self, cmd, argstr)
    --[[ 
        This is to support experimental command extensions.  As such, the user
        of the function is responsible for passing in a valid argument string
        for the command.
    --]]
    -- just make sure argstr has a leading ' '
    if argstr and not argstr:find([[$ .*]]) then
        argstr = ' '..argstr
    end
    -- we'll add the command to the ALLOWED_STATES table as universal, since we
    -- don't have anything else to go on
    ALLOWED_STATES[cmd] = UNIVERSAL
    return self:__synchronous_cmd(cmd, argstr)
end

function IMAP4.new(self, hostname, port)
    -- the following 'magic' lines make this usable as an object
    local o = {}
    setmetatable(o, self)

    -- now handle object initialization
    o.host = hostname or 'localhost'
    o.port = port or IMAP4_port
    o.__tagpre = 'a'
    o.__tag_num = 1
    o.__responses = {}
    o.__tags = {}
    o.__literals = {}
    o.__argt = {}
    o.__received_data = ''
    o.__sslparams = {
                     mode = "client",
                     protocol = "tlsv1",
                    }
    o.__connection = assert(socket.connect(o.host, o.port), 
                            'Unable to establish connection with host.\r\n')
    o.__connection:settimeout(0)
    
    -- get greeting
    o:__open_connection()
    o:__new_response()
    if not o:__get_response() then
        error("No greeting from server "..o.host.." on port "..o.port)
    end
    o.__welcome = o.__current_response
    if o.__welcome.__untagged['PREAUTH'] then
        o.__state = AUTH
    elseif o.__welcome.__untagged['OK'] then
        o.__state = NONAUTH
    elseif o.__welcome.__untagged['BYE'] then
        error("Connection refused by "..o.host)
    else
        for k,v in pairs(o.__welcome.__untagged) do
            error("Unrecognized greeting: "..k.." "..v[1])
        end
    end
   
    -- prevents modification of the object once created
    o.__newindex = function(t, k, v) 
                       error("Adding new members to IMAP4 object not allowed")
                   end
    return o
end

-- export the objects
imaplib = { IMAP4 = IMAP4, 
            Response = Response }
return imaplib
