local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- File explorer
map("n", "<leader>pv", vim.cmd.Ex)

-- Primeagen Magic
map("v", "J", ":m '>+1<CR>gv=gv")
map("v", "K", ":m '<-2<CR>gv=gv")

map("n", "J", "mzJ`z")

map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")

map("n", "n", "nzzzv")
map("n", "N", "Nzzzv")

map({ "n", "v" }, "<leader>y", [["+y]])
map("n", "<leader>Y", [["+Y"]])

-- Removed char with "x" does not stay in the clipboard
-- map("n", "x", '"_x"')

map("n", "Q", "<nop>")

map("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

-- Indenting
map("v", ">", ">gv")
map("v", "<", "<gv")

-- Comment
map("n", "<leader>l", "gcc", { desc = "toggle comment", remap = true })
map("v", "<leader>l", "gc", { desc = "toggle comment", remap = true })

-- Increment/Decrement numbers with +/-
map("n", "+", "<C-a>")
map("n", "-", "<C-x>")

-- Clear search
map("n", "<ESC>", ":nohlsearch<CR>", opts)

-- Toggle split
map("n", "<leader>wv", ":vsp<CR>")
map("n", "<leader>ws", ":sp<CR>")

-- Closing windows
map("n", "<leader>wc", ":close<CR>")

-- Navigation between splits
map("n", "<leader>ww", '<C-w>w')
map("n", "<leader>wh", '<C-w>h')
map("n", "<leader>wj", '<C-w>j')
map("n", "<leader>wk", '<C-w>k')
map("n", "<leader>wl", '<C-w>l')

-- Navigation between buffers
map("n", "<leader>bk", ":bd <CR>") -- *Kill current buffer
map("n", "<leader>bn", ":bnext <CR>")
map("n", "<leader>bp", ":bprev <CR>")

-- Tmux bindings
map("n", "<C-h>", "<cmd> TmuxNavigateLeft<CR>")
map("n", "<C-l>", "<cmd> TmuxNavigateRight<CR>")
map("n", "<C-j>", "<cmd> TmuxNavigateDown<CR>")
map("n", "<C-k>", "<cmd> TmuxNavigateUp<CR>")

-- Extension bindings
--- Telescope
map("n", "<leader>ff", ":Telescope find_files<CR>")
map("n", "<leader>fr", ":Telescope oldfiles<CR>")
-- map("n", "<leader>." , ":Telescope file_browser<CR>")
map("n", "<leader>." , ":Telescope file_browser hidden=true path=%:p:h select_buffer=true<CR>")
map("n", "<leader>fw", ":Telescope live_grep<CR>")

-- Buffers
map("n", "<leader>fb", ":Telescope buffers<CR>")

-- Colorscheme
-- map("n", "<leader>ht", ":Telescope colorscheme<CR>")
map("n", "<leader>th", ":lua require('nvchad.themes').open()<CR>")

-- Diagnostics
map("n", "<C-j>", function()
    vim.diagnostic.goto_next()
end, opts)
