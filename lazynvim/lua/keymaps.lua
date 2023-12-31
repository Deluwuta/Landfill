local keymap = vim.keymap

-- Opens file explorer (netwr)
keymap.set("n", "<leader>pv", vim.cmd.Ex)

-- The Primeagen Magic
keymap.set("v", "J", ":m '>+1<CR>gv=gv")
keymap.set("v", "K", ":m '<-2<CR>gv=gv")

keymap.set("n", "J", "mzJ`z")
keymap.set("n", "<C-d>", "<C-d>zz")
keymap.set("n", "<C-u>", "<C-u>zz")
keymap.set("n", "n", "nzzzv")
keymap.set("n", "N", "nzzzv")

keymap.set("x", "<leader>p", [["_dP]])

keymap.set({ "n", "v" }, "<leader>y", [["+y]])
keymap.set("n", "<leader>Y", [["+Y"]])

keymap.set("n", "Q", "<nop>")

keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

-- Clear search
keymap.set("n", "<ESC>", ":nohlsearch<CR>")

-- Saving and Exiting bindings
-- keymap.set("n", "<leader>wf", ":w<CR>")
-- keymap.set("n", "<leader>wq", ":wq<CR>")
-- keymap.set("n", "<leader>qq" , ":q<CR>")
-- keymap.set("n", "<leader>qa", ":qa<CR>")

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

-- Indenting
keymap.set("v", ">", ">gv")
keymap.set("v", "<", "<gv")

-- Toggle terminal
-- keymap.set("n", "<leader>ot", ":ToggleTerm<CR>")

-- Tmux bindings
keymap.set("n", "<C-h>", "<cmd> TmuxNavigateLeft<CR>")
keymap.set("n", "<C-l>", "<cmd> TmuxNavigateRight<CR>")
keymap.set("n", "<C-j>", "<cmd> TmuxNavigateDown<CR>")
keymap.set("n", "<C-k>", "<cmd> TmuxNavigateUp<CR>")
