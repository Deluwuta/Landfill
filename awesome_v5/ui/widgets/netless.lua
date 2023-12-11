local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")

local indicator = {}
local function worker(args)
    local args = args or {}
    local widget = wibox.widget.textbox()
    local timeout = 20

    local real = nil

    local function get_interfaces()
        local f = io.popen("ip -o link show")
        local state = io.popen("/home/delta/.config/awesome/ui/scripts/lewifi.sh"):read("*a")
        -- local state = 3
        for line in f:lines() do
            local iface, state = line:match("(%S+):%s+<(%S+)>")
            -- State aqui tiene el valor de dentro de los <...>
            -- Queda meter otro match para sacar la tercera columna
            if iface and state == "UP" then
                break
            end
        end
        f:close()
        return state
    end

    local function net_update()
        local state = get_interfaces()
        widget:set_text(state)
        return true
    end

    net_update()
    gears.timer.start_new(timeout, net_update)

    return widget
end

return setmetatable(indicator, {__call =
    function(_, ...)
        return worker(...)
    end
})
