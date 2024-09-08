local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

local helpers = require("utils.helpers")
local colors = require("themes.biscuit_dark")

local vars = require("utils.user_variables")
local mod = vars.mod

local function create_taglist(s)
    -- Function to update tags
    local update_tags = function (self, c3, _)
        local tagicon = self:get_children_by_id('tags')[1]
        if c3.selected then
            tagicon.text = c3.name
            self:get_children_by_id('tags')[1].forced_width = dpi(30)
            self.fg = colors.mid_light

        elseif #c3:clients() == 0 then
            tagicon.text = c3.name
            self:get_children_by_id('tags')[1].forced_width = dpi(30)
            self.fg = colors.bg_light

        else
            tagicon.text = c3.name
            self:get_children_by_id('tags')[1].forced_width = dpi(30)
            self.fg = colors.mid_light
        end
    end

    local tags_template = {
        {
            {
                id = "tags",
                halign = "center",
                font = beautiful.font_name .. "Bold 10",
                widget = wibox.widget.textbox,
            },
            widget = wibox.container.margin,
        },
        id = "background_role",
        widget = wibox.container.background,
        bg = colors.bg_dim,
        shape = gears.shape.rounded_bar,
        create_callback = function (self, c3, _)
            update_tags(self, c3, _)
        end,
        update_callback = function (self, c3, _)
            update_tags(self, c3, _)
        end
    }

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

    local taglist = wibox.widget({
        {

            {
                awful.widget.taglist {
                    screen = s,
                    filter = awful.widget.taglist.filter.all,
                    layout = {
                        layout = wibox.layout.fixed.horizontal,
                        -- spacing = dpi(6),
                        shape = gears.shape.circle,
                    },
                    style = {
                        shape = gears.shape.rounded_bar,

                        -- font = beautiful.font_name .. "Bold 10",
                        -- spacing = dpi(1),

                        bg_focus = colors.blue,
                        -- fg_focus = colors.mid_light,

                        -- bg_occupied = colors.bg_dim,
                        -- fg_occupied = colors.lightblue,
                    },
                    widget_template = tags_template,
                    buttons = buttons_table,
                },
                -- left = dpi(3),
                -- right = dpi(3),
                widget = wibox.container.margin,
            },
            bg = colors.bg_dim,
            fg = colors.mid_light,
            shape = gears.shape.rounded_rect,
            widget = wibox.container.background,
        },
        top = dpi(3),
        bottom = dpi(3),
        widget = wibox.container.margin,
    })

    return taglist
end

return create_taglist
