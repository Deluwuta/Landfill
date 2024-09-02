local gears = require("gears")

local _aux = require("widgets.pruebas")
local volume_osd, _ = table.unpack(_aux)

local timers = {}

timers.volume = gears.timer {
    timeout = 2,
    autostart = true,
    callback = function ()
        volume_osd.visible = false
    end
}

volume_osd:connect_signal("mouse::enter", function ()
    timers.volume:stop()
    volume_osd.visible = true
end)

volume_osd:connect_signal("mouse::leave", function ()
    timers.volume:again()
end)

return timers
