local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")

local indicator = {}
Ether_state = ""
Wifi_state = ""

local function command_maker(conc_string)
    local comando = [[bash -c '
        cat /sys/class/net/"$CONC_STRING"/operstate
    ']]
    return comando:gsub("$CONC_STRING", conc_string)
end

local function worker(args)
    local ether_iface = args.ether or ""
    local wifi_iface = args.wifi or ""

    local widget = wibox.widget.textbox()
    local timeout = 10

    local function get_ether_state()
        awful.spawn.easy_async(command_maker(ether_iface), function (stdout, _, _, _)
            Ether_state = stdout
        end)
    end

    local function get_wifi_state()
        awful.spawn.easy_async(command_maker(wifi_iface), function (stdout, _, _, _)
            Wifi_state = stdout
        end)
    end

    local function net_update()
        get_ether_state()
        get_wifi_state()

        local state = "Netless"

        if string.match(Ether_state, "up") then
            state = "Ethernet"
        elseif string.match(Wifi_state, "up") then
            state = "Wifi"
        end

        widget:set_text(state)
    end

    net_update()
    local timer = gears.timer.start_new(timeout, net_update)
    widget:connect_signal("destroy", function() timer:stop() end)

    return widget

end

return setmetatable(indicator, {__call =
    function(_, ...)
        return worker(...)
    end
})
