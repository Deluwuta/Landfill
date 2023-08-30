local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local naughty = require("naughty")

-- Basic
naughty.persistence_enabled = true
naughty.config.defaults.ontop = true
naughty.config.defaults.timeout = 5
naughty.config.defaults.title = "System notif"
naughty.config.defaults.position = "bottom_right"

require(... .. ".error")
