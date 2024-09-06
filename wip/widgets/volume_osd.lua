local awful = require("awful")

local ICON_DIR = os.getenv("HOME") .. '/.config/awesome/utils/icons/'

local osd_maker = require("utils.osd_template")

-- Image of the osd
local image = osd_maker.make_image(ICON_DIR .. "volume-up.png")

-- Slider
local volume_slider = osd_maker.make_slider(100)

-- Percentage
local volume_percentage = osd_maker.make_percentage(volume_slider)

-- Handling of the widget update on change
local update_osd_slider = function ()
    awful.spawn.easy_async("amixer sget Master", function (stdout)
        local val = tonumber(string.match(stdout, "(%d?%d?%d)%%"))
        volume_slider.value = val or 0
        volume_percentage.markup = '<span color="' ..
    "#d3c6aa" .. '" font="Ubuntu Nerd Font bold 14">' .. val .. '</span>'
    end)
end

local schedule_update = osd_maker.make_scheduler(update_osd_slider)

-- Main popup
local osd_box = osd_maker.make_osd(image, volume_slider, volume_percentage)

-- Timer
local volume_timer = osd_maker.make_timer(osd_box)

-- Signals
volume_slider:connect_signal("property::value", function (slider)
    local osd_level = math.floor(slider.value)
    awful.spawn("amixer set Master " .. osd_level .. "%")
    volume_percentage.markup = '<span color="' ..
    "#d3c6aa" .. '" font="Ubuntu Nerd Font bold 14">' .. osd_level .. '</span>'
end)

osd_box:connect_signal("mouse::enter", function ()
    volume_timer:stop()
    osd_box.visible = true
end)

osd_box:connect_signal("mouse::leave", function ()
    volume_timer:again()
end)

return { osd_box, schedule_update, volume_timer }
