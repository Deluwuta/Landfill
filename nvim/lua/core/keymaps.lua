--LAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSALAFKJASLKFAKLSFKLJSA

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.opt.backspace = '2'
vim.opt.showcmd = true
vim.opt.laststatus = 2
vim.opt.autowrite = true
vim.opt.cursorline = true
vim.opt.autoread = true

vim.o.number = true
vim.o.relativenumber = true

vim.wo.wrap = true
vim.wo.linebreak = false

-- Use spaces for tabs and whatnot
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.shiftround = true
vim.opt.expandtab = true

vim.keymap.set('n', '<leader>h', ':nohlsearch<CR>')

-- Neotree keys
vim.keymap.set("n", "<leader>e", ":NvimTreeFindFileToggle<CR>")

-- Telescope keys
vim.keymap.set("n", "<leader>ff", ":Telescope find_files hidden=true<CR>")
vim.keymap.set("n", "<leader>fr", ":Telescope oldfiles hidden=true<CR>")
vim.keymap.set("n", "<leader>.", ":Telescope file_browser hidden=true<CR>")
vim.keymap.set("n", "<leader>fw", ":Telescope live_grep<CR>")
vim.keymap.set("n", "<leader>ht", ":Telescope colorscheme<CR>")

-- Commenting keys
vim.keymap.set("n", "<leader>l", ":CommentToggle<CR>")
