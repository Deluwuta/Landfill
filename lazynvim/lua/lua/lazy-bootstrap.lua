-- Lazy Nvim Setup
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("set")
require("keymaps")

local opts = {
    defaults = {
        lazy = true,
    },
    rtp = {
        disabled_plugins = {
            "gzip",
            "matchit",
            "matchparen",
            "tarPlugin",
            "tutor",
            "zipPlugin",
        }
    },
    change_detection = {
        enabled = true,
        notify = true,
    },
}

require("lazy").setup("plugins", opts)
