-- This file needs to have same structure as nvconfig.lua 
-- https://github.com/NvChad/ui/blob/v3.0/lua/nvconfig.lua
-- Please read that file to know all available options :( 

---@class ChadrcConfig
local M = {}

-- Theme and transparency options for Base46
M.base46 = {
    theme = "ashes",
    transparency = false, -- Enable transparency
}

-- UI customizations
M.ui = {
    cmp = {
        icons_left = false,
        style = "flat_dark", -- default / flat_light / flat_dark / atom / atom_colored
        format_colors = {
            tailwind = false,
            icon = "󱓻"
        }
    },
    telescope = { style = "bordered" },
    statusline = {
        theme = "default", -- default / vscode / vscode_colored / minimal
        separator_style = "default", -- default / rounded / block / arrow
        order = nil,
        modules = nil,
    },
    tabufline = { enabled = false },
}

M.nvdash = {
    load_on_startup = false,
    header = {
        "                            ",
        "     ▄▄         ▄ ▄▄▄▄▄▄▄   ",
        "   ▄▀███▄     ▄██ █████▀    ",
        "   ██▄▀███▄   ███           ",
        "   ███  ▀███▄ ███           ",
        "   ███    ▀██ ███           ",
        "   ███      ▀ ███           ",
        "   ▀██ █████▄▀█▀▄██████▄    ",
        "     ▀ ▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀   ",
        "                            ",
        "     Powered By  eovim    ",
        "                            ",
    },

    buttons = {
        { txt = "  File Browser", keys = "SPC . ", cmd = "Telescope file_browser" },
        { txt = "  Find File", keys = "SPC ff", cmd = "Telescope find_files" },
        { txt = "  Recent Files", keys = "SPC fr", cmd = "Telescope oldfiles" },
        { txt = "󰈭  Find Word", keys = "SPC fw", cmd = "Telescope live_grep" },
        { txt = "󱥚  Themes", keys = "SPC th", cmd = ":lua require('nvchad.themes').open()" },

        -- This is used to insult my setup
        { txt = "─", hl = "NvDashFooter", no_gap = true, rep = true },

        {
            txt = function()
                local stats = require("lazy").stats()
                local ms = math.floor(stats.startuptime) .. " ms"
                return "  Loaded " .. stats.loaded .. "/" .. stats.count .. " plugins in " .. ms
            end,
            hl = "NvDashFooter",
            no_gap = true,
        },

        { txt = "─", hl = "NvDashFooter", no_gap = true, rep = true },
    },
}

M.lsp = { signature = true }

M.colorify = {
    enabled = true,
    mode = "virtual", -- fg, bg, virtual
    virt_text = "󱓻 ",
    highlight = { hex = true, lspvars = true },
}

return M
