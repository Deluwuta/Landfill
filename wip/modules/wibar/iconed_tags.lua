local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

local helpers = require("utils.helpers")

local vars = require("utils.user_variables")
local mod = vars.mod

local function create_taglist(s)
    local buttons_table = {
        awful.button({ }, 1, function(t) t:view_only() end),
        awful.button({ mod }, 1, function (t)
            if client.focus then
                client.focus:move_to_tag(t)
            end
        end),

        awful.button({ }, 3, awful.tag.viewtoggle),
        awful.button({ mod }, 3, function (t)
            if client.focus then
                client.focus:toggle_tag(t)
            end
        end),

        awful.button({ }, 4, function(t) awful.tag.viewprev(t.screen) end),
        awful.button({ }, 5, function(t) awful.tag.viewnext(t.screen) end),
    }

    local taglist = awful.widget.taglist {
        screen = s,
        filter = awful.widget.taglist.filter.all,
        layout = {
            layout = wibox.layout.fixed.horizontal,
            spacing = dpi(100),
            shape = gears.shape.circle,
        },
        -- style = { font = "Ubuntu nerd font 16 bold" }
        buttons = buttons_table,
    }

    return taglist
end

return create_taglist

-- s.mytaglist = awful.widget.taglist {
--         screen  = s,
--         filter  = awful.widget.taglist.filter.all,
--         buttons = {
--             awful.button({ }, 1, function(t) t:view_only() end),
--             awful.button({ mod }, 1, function(t)
--                                             if client.focus then
--                                                 client.focus:move_to_tag(t)
--                                             end
--                                         end),
--             awful.button({ }, 3, awful.tag.viewtoggle),
--             awful.button({ mod }, 3, function(t)
--                                             if client.focus then
--                                                 client.focus:toggle_tag(t)
--                                             end
--                                         end),
--             awful.button({ }, 4, function(t) awful.tag.viewprev(t.screen) end),
--             awful.button({ }, 5, function(t) awful.tag.viewnext(t.screen) end),
--         }
--     }
