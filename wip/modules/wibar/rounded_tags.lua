local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

local helpers = require("utils.helpers")

local vars = require("utils.user_variables")
local mod = vars.mod

local function create_taglist(s)
    local update_tag = function (self, c3, _)
        if c3.selected then
            -- self:get_children_by_id('tags')[1].forced_height = dpi(45)
            self:get_children_by_id('tags')[1].forced_width = dpi(24)
            self:get_children_by_id('tags')[1].bg = "#FFF"

        elseif #c3:clients() == 0 then
            self:get_children_by_id('tags')[1].forced_width = dpi(10)
            self:get_children_by_id('tags')[1].bg = "#343"

        else
            if c3.urgent then
                self:get_children_by_id('tags')[1].forced_width = dpi(10)
                self:get_children_by_id('tags')[1].bg = "#FA0"
            else
                self:get_children_by_id('tags')[1].forced_width = dpi(10)
                self:get_children_by_id('tags')[1].bg = "#84A"
            end
        end
    end

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
            spacing = dpi(4),
            shape = gears.shape.circle,
        },
        -- style = { font = "Ubuntu nerd font 16 bold" }
        widget_template = {
            id = 'tags',
            widget = wibox.container.background,
            bg = "#2e2e2f",
            forced_height = dpi(8),
            -- forced_width = dpi(20),
            shape = gears.shape.rounded_bar,
            create_callback = function (self, c3, _)
                update_tag(self, c3, _)
            end,
            update_callback = function (self, c3, _)
                update_tag(self, c3, _)
            end
        },
        buttons = buttons_table,
    }

    local the_taglist = helpers.margin(wibox.widget
        {
            {
                {
                    nil,
                    layout = wibox.layout.align.vertical,
                    taglist,
                    nil,
                    expand = 'none'
                },
                widget = wibox.container.margin,
                margin = dpi(15)
            },
            widget = wibox.container.background,
            bg = beautiful.bg,
            shape = helpers.rrect(dpi(4))
        },
        5, 5, 5, 5)

    the_taglist.forced_height = dpi(50)

    return the_taglist
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
