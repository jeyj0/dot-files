local opts = { noremap = true, silent = true }

local keymap = vim.api.nvim_set_keymap

vim.g.mapleader = " "
vim.g.maplocalleader = " "

keymap("n", "<leader><leader>", "<cmd>Telescope git_files<CR>", opts)
keymap("n", "<leader>fs", "<cmd>write<CR>", opts)
keymap("n", "<leader>fe", "<cmd>NvimTreeToggle<CR>", opts)
keymap("n", "<leader>fo", "<cmd>SymbolsOutline<CR>", opts)
keymap("n", "<leader>f%", "<cmd>luafile %<CR>", opts)

keymap("n", "<leader>t", "<cmd>:Telescope<CR>", opts)

keymap("n", "<esc>", "<cmd>noh<CR>", opts)

keymap("n", "<leader><left>", "<C-w>h", opts)
keymap("n", "<leader><up>", "<C-w>j", opts)
keymap("n", "<leader><down>", "<C-w>k", opts)
keymap("n", "<leader><right>", "<C-w>l", opts)

keymap("n", "<leader>bb", "<cmd>Telescope buffers<CR>", opts)
keymap("n", "<leader>bn", "<cmd>lua require('bufferline').cycle(1)<CR>", opts)
keymap("n", "<leader>bp", "<cmd>lua require('bufferline').cycle(-1)<CR>", opts)
keymap("n", "<leader>bk", "<cmd>lua require('close_buffers').delete({type='this'})<CR>", opts)
keymap("n", "<leader>bo", "<cmd>lua require('close_buffers').delete({type='other'})<CR>", opts)

