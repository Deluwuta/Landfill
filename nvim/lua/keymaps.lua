local keymap = vim.keymap
local opts = { noremap = true, silent = true }

-- File explorer
keymap.set("n", "<leader>pv", vim.cmd.Ex)

-- Primeagen Magic
keymap.set("v", "J", ":m '>+1<CR>gv=gv")
keymap.set("v", "K", ":m '<-2<CR>gv=gv")

keymap.set("n", "J", "mzJ`z")

keymap.set("n", "<C-d>", "<C-d>zz")
keymap.set("n", "<C-u>", "<C-u>zz")

keymap.set("n", "n", "nzzzv")
keymap.set("n", "N", "Nzzzv")

keymap.set({ "n", "v" }, "<leader>y", [["+y]])
keymap.set("n", "<leader>Y", [["+Y"]])

-- Removed char with "x" does not stay in the clipboard
-- keymap.set("n", "x", '"_x"')

keymap.set("n", "Q", "<nop>")

keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

-- Indenting
keymap.set("v", ">", ">gv")
keymap.set("v", "<", "<gv")

-- Increment/Decrement numbers with +/-
keymap.set("n", "+", "<C-a>")
keymap.set("n", "-", "<C-x>")

-- Clear search
keymap.set("n", "<ESC>", ":nohlsearch<CR>", opts)

-- Toggle split
keymap.set("n", "<leader>wv", ":vsp<CR>")
keymap.set("n", "<leader>ws", ":sp<CR>")

-- Closing windows
keymap.set("n", "<leader>wc", ":close<CR>")

-- Navigation between splits
keymap.set("n", "<leader>ww", '<C-w>w')
keymap.set("n", "<leader>wh", '<C-w>h')
keymap.set("n", "<leader>wj", '<C-w>j')
keymap.set("n", "<leader>wk", '<C-w>k')
keymap.set("n", "<leader>wl", '<C-w>l')

-- Navigation between buffers
keymap.set("n", "<leader>bk", ":bd <CR>") -- *Kill current buffer
keymap.set("n", "<leader>bn", ":bnext <CR>")
keymap.set("n", "<leader>bp", ":bprev <CR>")

-- Tmux bindings
keymap.set("n", "<C-h>", "<cmd> TmuxNavigateLeft<CR>")
keymap.set("n", "<C-l>", "<cmd> TmuxNavigateRight<CR>")
keymap.set("n", "<C-j>", "<cmd> TmuxNavigateDown<CR>")
keymap.set("n", "<C-k>", "<cmd> TmuxNavigateUp<CR>")

-- Diagnostics
keymap.set("n", "<C-j>", function()
    vim.diagnostic.goto_next()
end, opts)
