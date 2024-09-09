local awful = require("awful")

local ICON_DIR = os.getenv("HOME") .. '/.config/awesome/utils/icons/'

local user = require("utils.user_variables")
local colors = require("themes." .. user.theme)

local osd_maker = require("utils.osd_template")

-- Image of the osd
local image = osd_maker.make_image("/home/delta/Pictures/nixos-icon.svg")

-- Slider
local brightness_slider = osd_maker.make_slider(1)

-- Percentage 
local brightness_percentage = osd_maker.make_percentage(brightness_slider)

-- Handling of the widget update on change
local update_osd_slider = function ()
    awful.spawn.easy_async("brightnessctl g", function (stdout)
        local val = tonumber(stdout)
        brightness_slider.value = val or 0
        brightness_percentage.markup = '<span color="' ..
    colors.fg_normal .. '" font="Ubuntu Nerd Font bold 14">' .. val .. '</span>'
    end)
end

local schedule_update = osd_maker.make_scheduler(update_osd_slider)

-- Main popup
local osd_box = osd_maker.make_osd(image, brightness_slider, brightness_percentage)

-- Timer
local brightness_timer = osd_maker.make_timer(osd_box)

-- Signals
brightness_slider:connect_signal("property::value", function (slider)
    local osd_level = math.floor(slider.value)
    awful.spawn("brightnessctl s" .. osd_level)
    brightness_percentage.markup = '<span color="' ..
    colors.fg_normal .. '" font="Ubuntu Nerd Font bold 14">' .. osd_level .. '</span>'
end)

osd_box:connect_signal("mouse::enter", function ()
    brightness_timer:stop()
    osd_box.visible = true
end)

osd_box:connect_signal("mouse::leave", function ()
    brightness_timer:again()
end)

return { osd_box, schedule_update, brightness_timer }
