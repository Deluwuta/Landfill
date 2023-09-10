local val = function ()
    -- local handle = io.popen("uname -r | cut -d'-' -f-2")
    local handle = io.popen("uname -r") -- With -lts (I like it)
    if handle ~= nil then
        local result = handle:read("*a")
        handle:close()
        return result
    end
    return "Fuck"
end

-- print(val())
return val
