vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46_cache/"

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

require("set")
require("keymaps")
require("lazy").setup("plugins", opts)

dofile(vim.g.base46_cache .. "defaults")
dofile(vim.g.base46_cache .. "statusline")

require("configs.autocmds")
