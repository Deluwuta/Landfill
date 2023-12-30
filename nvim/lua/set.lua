local opt = vim.opt

vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.o.termguicolors = true
-- vim.o.guicursor = "n-v-c:block,i-ci-r-cr:hor25"

vim.o.clipboard = "unnamedplus"

vim.o.splitright = true
vim.o.splitbelow = true

opt.nu = true
opt.relativenumber = true

opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4

opt.expandtab = true
opt.smartindent = true

opt.wrap = true
opt.linebreak = true
opt.mouse = "a"

opt.swapfile = false
opt.backup = false
opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
opt.undofile = true

opt.scrolloff = 8
-- opt.signcolumn = "yes"
opt.isfname:append("@-@")

-- Idk what most of this does
opt.backspace = '2'
opt.showcmd = true
opt.laststatus = 2
opt.autowrite = true
opt.cursorline = true
opt.autoread = true

-- Do you like the not at all noticeable line in the middle of the screen? :^)
opt.colorcolumn = "80"
