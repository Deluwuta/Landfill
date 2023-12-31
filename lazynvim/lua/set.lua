local opt = vim.opt

vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.o.termguicolors = true

-- vim.o.guicursor = "n-v-c:block,i-ci-r-cr:hor25"

-- Nvim and OS clipboard are friends again
vim.o.clipboard = "unnamedplus"

-- Split rules
vim.o.splitright = true
vim.o.splitbelow = true

-- Line numbers
opt.nu = true
opt.relativenumber = true

opt.wrap = true
opt.linebreak = true
opt.mouse = "a"

-- Tabs
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4

-- Indentation
opt.expandtab = true -- Converts tabs to spaces
opt.smartindent = true

-- Search options
opt.incsearch = true
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true

-- Backup
opt.swapfile = false
opt.backup = false
opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
opt.undofile = true

-- Scroll
opt.scrolloff = 8

-- Idk what most of this does
opt.backspace = 'indent,eol,start'
opt.showcmd = true
opt.laststatus = 2
opt.autowrite = true
opt.cursorline = true
opt.autoread = true

-- Do you like the not at all noticeable line in the middle of the screen? :^)
opt.colorcolumn = "80"

opt.cmdheight = 1
opt.signcolumn = "yes"
opt.isfname:append("@-@")
opt.iskeyword:append("-")
opt.completeopt = "menuone,noinsert,noselect"
opt.encoding = "UTF-8"
