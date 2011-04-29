-- define the install path for the library
--local path = "/usr/local/share/lua/5.1/imap4/"
local path = "./imap4/"

-- define a loader function that can load a file from a known spot
local function loader(modulename)

    return function()
           local filename = path..modulename..".lua"
           -- IMPORTANT: loadfile compiles the code in 'filename' and returns a
           --            function.  In order to *define* the function, it has to
           --            executed- then the module will be available.
           return assert(loadfile(filename)())
           end
end

-- assign preload values for the helper modules that 'imap4' uses, we do this
-- because those modules are not in the module search path for lua since we 
-- decided to put everything under an 'imap4' directory.
package.preload['auth'] = loader('auth')
package.preload['utils'] = loader('utils')

-- All of this gets kicked off by someone using "local imaplib = require("imap4")
-- Preloads and the like don't work because the loader found *this* file
-- (init.lua) under the directory /impa4, so it is already going to assign
-- package.loaded['imap4'] to this module- which we don't want.  We want it to
-- make the assignment to compiled code of 'imap4.lua'  So we use the loader
-- function above to configure a loader for 'imap4.lua' and then we invoke the
-- function and make the assignment ourselves.
package.loaded[...] = loader(...)()


print("Using local copy")
