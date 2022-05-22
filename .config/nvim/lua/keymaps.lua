local opts = { noremap = true, silent = true }

local keymap = vim.api.nvim_set_keymap
function normal(keys, effects)
	keymap("n", keys, effects, opts)
end

vim.g.mapleader = " "
vim.g.maplocalleader = " "

normal("<leader><leader>", "<cmd>Telescope git_files<CR>")

normal("<leader>fs", "<cmd>write<CR>")
normal("<leader>fe", "<cmd>NvimTreeToggle<CR>")
normal("<leader>fo", "<cmd>SymbolsOutline<CR>")
normal("<leader>f%", "<cmd>luafile %<CR>")

normal("<leader>t", "<cmd>Telescope<CR>")
normal("<leader>g", "<cmd>Neogit<CR>")

normal("<esc>", "<cmd>noh<CR>")

normal("<leader><left>" , "<C-w>h")
normal("<leader><up>"   , "<C-w>j")
normal("<leader><down>" , "<C-w>k")
normal("<leader><right>", "<C-w>l")
normal("<leader>z"      , "<cmd>ZenMode<CR>")

normal("<leader>bb", "<cmd>Telescope buffers<CR>")
normal("<leader>bn", "<cmd>lua require('bufferline').cycle(1)<CR>")
normal("<leader>bp", "<cmd>lua require('bufferline').cycle(-1)<CR>")
normal("<leader>bk", "<cmd>lua require('close_buffers').delete({type='this'})<CR>")
normal("<leader>bo", "<cmd>lua require('close_buffers').delete({type='other'})<CR>")

