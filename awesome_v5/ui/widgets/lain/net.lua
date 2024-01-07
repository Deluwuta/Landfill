local naughty = require("naughty")
local wibox = require("wibox")
local string = string

local spawn = require("awful.spawn")
local timer = require("gears.timer")

local function line_callback(cmd, callback)
    return spawn.with_line_callback(cmd, {
        stdout = function (line)
            callback(line)
        end,
    })
end

local function first_line(path)
    local file, first = io.open(path, "rb"), nil
    if file then
        first = file:read("*l")
        file:close()
    end
    return first
end

local function lines_from(path)
    local lines = {}
    for line in io.lines(path) do
        lines[#lines + 1] = line
    end
    return lines
end

local timer_table = {}
local function newtimer(name, timeout, fun, nostart, stoppable)
    if not name or #name == 0 then return end
    name = (stoppable and name) or timeout
    if not timer_table[name] then
        timer_table[name] = timer({ timeout = timeout })
        timer_table[name]:start()
    end
    timer_table[name]:connect_signal("timeout", fun)
    if not nostart then
        timer_table[name]:emit_signal("timeout")
    end
    return stoppable and timer_table[name]
end

-- Info about the network
local function factory(args)
    args = {}
    local net = { wibox.widget.textbox(), devices = {} }
    local timeout = 10
    local wifi_state = "off"
    local eth_state = "off"

    net.iface = {}

    function net.get_devices()
        net.iface = {} -- Reset at every call
        line_callback("ip link", function(line)
            net.iface[#net.face + 1] = not string.match(line, "LOOPBACK") and
            string.match(line, "(%w+): <") or nil
        end)
    end

    if #net.iface == 0 then net.get_devices() end

    function net.update()
        local net_now = {
            devices = {},
            sent = 0,
            received = 0
        }

        for _, dev in ipairs(net.iface) do
            local dev_now    = {}
            local dev_before = net.devices[dev] or { last_t = 0, last_r = 0 }
            local now_t = tonumber(first_line(string.format(
                "/sys/class/net/%s/statistics/tx_bytes", dev)) or 0)
            local now_r = tonumber(first_line(string.format(
                "/sys/class/net/%s/statistics/rx_bytes", dev)) or 0)

            dev_now.carrier = first_line(string.format(
                "/sys/class/net/%s/carrier", dev)) or "0"
            dev_now.state = first_line(string.format(
                "/sys/class/net/%s/operstate", dev)) or "down"

            dev_now.last_t   = now_t
            dev_now.last_r   = now_r

            if wifi_state == "on" and first_line(string.format(
                "/sys/class/net/%s/uevent", dev)) == "DEVTYPE=wlan"
            then
                dev_now.wifi = true
                if string.match(dev_now.carrier, "1") then
                        dev_now.signal = tonumber(string.match(lines_from(
                        "/proc/net/wireless")[3], "(%-%d+%.)")) or nil
                end
            else
                dev_now.wifi   = false
            end

            if eth_state == "on" and first_line(string.format(
                "/sys/class/net/%s/uevent", dev)) ~= "DEVTYPE=wlan" 
            then
                dev_now.ethernet = true
            else
                dev_now.ethernet = false
            end

            net.devices[dev] = dev_now
        end
        widget = net.widget
    end
    newtimer("network", timeout, net.update)
    return net
end

return factory
