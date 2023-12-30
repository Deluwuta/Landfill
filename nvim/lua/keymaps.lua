-- Clear search
vim.keymap.set('n', '<ESC>', ':nohlsearch<CR>')

-- Saving and Exiting bindings
-- vim.keymap.set("n", "<leader>wf", ":w<CR>")
-- vim.keymap.set("n", "<leader>wq", ":wq<CR>")
vim.keymap.set("n", "<leader>qq" , ":q<CR>")
vim.keymap.set("n", "<leader>qa", ":qa<CR>")

-- Closing windows
vim.keymap.set("n", "<leader>wc", ":close<CR>")

-- Navigation between splits
vim.keymap.set("n", "<leader>ww", '<C-w>w')
vim.keymap.set("n", "<leader>wh", '<C-w>h')
vim.keymap.set("n", "<leader>wj", '<C-w>j')
vim.keymap.set("n", "<leader>wk", '<C-w>k')
vim.keymap.set("n", "<leader>wl", '<C-w>l')

-- Toggle split
vim.keymap.set("n", "<leader>wv", ":vsp<CR>")
vim.keymap.set("n", "<leader>ws", ":sp<CR>")

-- Toggle terminal
vim.keymap.set("n", "<leader>ot", ":ToggleTerm<CR>")

-- Nvimtree keys
-- vim.keymap.set("n", "<leader>e" , ":NvimTreeFindFileToggle<CR>")

-- Neotree keys
vim.keymap.set("n", "<leader>e" , ":NeoTreeFocusToggle<CR>")

-- Telescope keys
vim.keymap.set("n", "<leader>ff", ":Telescope find_files hidden=true<CR>")
vim.keymap.set("n", "<leader>fr", ":Telescope oldfiles hidden=true<CR>")
vim.keymap.set("n", "<leader>." , ":Telescope file_browser hidden=true<CR>")
vim.keymap.set("n", "<leader>fw", ":Telescope live_grep<CR>")
vim.keymap.set("n", "<leader>ht", ":Telescope colorscheme<CR>")

-- Commenting keys
vim.keymap.set("n", "<leader>l", ":CommentToggle<CR>")
