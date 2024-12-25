local opt = vim.opt

vim.o.termguicolors = true
vim.o.guicursor = "n-v-c:block,i-ci-r-cr:hor25"

-- Nvim and OS clipboard are friends now
vim.o.clipboard = "unnamedplus"

-- Split rules
vim.o.splitright = true
vim.o.splitbelow = true
vim.o.splitkeep = "cursor"

-- opt.title = true

-- Line numbers
opt.nu = true
opt.relativenumber = false

opt.wrap = true
opt.linebreak = true
opt.mouse = "a" -- Enable mouse support

-- Tabs
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4

-- Indentation
opt.autoindent = true
opt.smartindent = true
opt.expandtab = true
opt.smarttab = true
opt.breakindent = true

-- Searching
opt.incsearch = true
opt.inccommand = "split"
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true

-- Backup file
opt.swapfile = false
opt.backup = false
opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
opt.undofile = true

-- Scroll limit
opt.scrolloff = 10

-- Other (idk)
opt.backspace = {"start", "eol", "indent"}
opt.showcmd = true
opt.laststatus = 2
opt.autowrite = true
opt.cursorline = true
opt.autoread = true

-- Do you like the not at all noticeable line in the middle of the screen? :^)
opt.colorcolumn = "80"

opt.cmdheight = 1
opt.signcolumn = "yes"
opt.completeopt = "menuone,noinsert,noselect"
opt.encoding = "UTF-8"

opt.isfname:append("@-@")
opt.iskeyword:append("-")
opt.path:append({"**"})

opt.formatoptions:append({"r"})

-- Go to the previous/next line when cursor reaches end/beginning of it
opt.whichwrap:append "<>[]hl"

-- Disable some default providers
vim.g.loaded_node_provider = 0
vim.g.loaded_python3_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0

-- This is ugly af my god
vim.g.loaded_matchparen = nil
vim.cmd("runtime! plugin/matchparen.vim")
