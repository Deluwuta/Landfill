local val = function ()
    local handle = io.popen("uname -r | cut -d'-' -f-2")
    if handle ~= nil then
        local result = handle:read("*a")
        handle:close()
        return result
    end
    return "Fuck"
end

return val
