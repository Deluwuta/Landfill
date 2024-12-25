-- Bindings
vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46/"

vim.loader.enable()

vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Lazy Nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- Stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

local opts = {
    defaults = {
        lazy = true,
        version = "*",
    },

    install = {
        missing = true,
        colorscheme = { "decay" },
    },

    performance = {
        cache = { enabled = true },
        rtp = {
            disabled_plugins = {
                "tohtml",
                "gzip",
                "matchit",
                "matchparen",
                "rrhelper",
                "tarPlugin",
                "tutor",
                "vimball",
                "vimballPlugin",
                "zipPlugin",
            }
        },
    },

    change_detection = {
        enabled = true,
        notify = false,
    },

    ui = {
        size = { width = 0.6, heigh = 0.6 },
        border = "rounded",
        icons = {
            loaded = "✓",
            not_loaded = "✗",
            ft = "",
            lazy = "󰂠 ",
        },
    },
}
require("lazy").setup("plugins", opts)

-- Load NVChad theming
dofile(vim.g.base46_cache .. "defaults")
dofile(vim.g.base46_cache .. "statusline")

-- Load files
require "options"
require "settings.autocmds"

vim.schedule(function()
    require "mappings"
end)

