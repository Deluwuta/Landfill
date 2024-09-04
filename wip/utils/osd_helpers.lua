local gears = require("gears")

local _aux = require("widgets.pruebas")
local volume_osd, _ = table.unpack(_aux)

local _bux = require("widgets.brightness_osd")
local brightness_osd, _ = table.unpack(_bux)

local timers = {}

timers.volume = gears.timer {
    timeout = 2,
    autostart = true,
    callback = function ()
        volume_osd.visible = false
    end
}

timers.brightness = gears.timer {
    timeout = 2,
    autostart = true,
    callback = function ()
        brightness_osd.visible = false
    end
}

volume_osd:connect_signal("mouse::enter", function ()
    timers.volume:stop()
    volume_osd.visible = true
end)

volume_osd:connect_signal("mouse::leave", function ()
    timers.volume:again()
end)

brightness_osd:connect_signal("mouse::enter", function ()
    timers.brightness:stop()
    brightness_osd.visible = true
end)

brightness_osd:connect_signal("mouse::leave", function ()
    timers.brightness:again()
end)

return timers
