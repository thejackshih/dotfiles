-- ========================================================================== --
-- INDENTATION (Global Defaults)
-- ========================================================================== --
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true

-- ========================================================================== --
-- VISUALS & SEARCH
-- ========================================================================== --
vim.opt.cursorline = true
vim.opt.lazyredraw = true

-- Hybrid line numbers
vim.opt.number = true
vim.opt.relativenumber = true

-- ========================================================================== --
-- FOLDING
-- ========================================================================== --
vim.opt.foldenable = true
vim.opt.foldlevelstart = 10
vim.opt.foldnestmax = 10
vim.opt.foldmethod = "indent"

-- ========================================================================== --
-- KEYMAPS
-- ========================================================================== --
vim.g.mapleader = ","

local keymap = vim.keymap.set
local opts = { noremap = true }

keymap("n", "<space>", "za", opts)
keymap("n", "j", "gj", opts)
keymap("n", "k", "gk", opts)
keymap("n", "gV", "`[v`]", opts)
keymap("i", "jk", "<esc>", opts)
keymap("n", "<leader>s", ":mksession<CR>", opts)
keymap("n", "<leader><space>", ":nohlsearch<CR>", opts)

-- ========================================================================== --
-- AUTOCOMMANDS (Filetype Specific Indentation)
-- ========================================================================== --
local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup
local indent_group = augroup("FiletypeIndent", { clear = true })

autocmd("FileType", {
  pattern = { "html", "python", "vim" },
  group = indent_group,
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.expandtab = true
  end,
})