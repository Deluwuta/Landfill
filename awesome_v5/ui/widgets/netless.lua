local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")

local indicator = {}
local function worker(ether, wifi)
    local ether_iface = ether or ""
    local wifi_iface = wifi or ""

    local widget = wibox.widget.textbox()
    local timeout = 20

    local function get_interfaces()
        if ether_iface == "" and wifi_iface == "" then
            return "Empty"
        end

        local ether_state = io.popen("cat /sys/class/net/"..ether_iface.."/operstate"):read("*a")
        local wifi_state = io.popen("cat /sys/class/net/"..wifi_iface.."/operstate"):read("*a")

        if string.match(ether_state, "up") then
            return "Ethernet"
        elseif wifi_state == "up" then
            return "Wifi"
        else
            return "Netless"
        end
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
