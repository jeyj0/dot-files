local opts = { noremap = true, silent = true }

local keymap = vim.api.nvim_set_keymap
function normal(keys, effects)
	keymap("n", keys, effects, opts)
end
function visual(keys, effects)
    keymap("v", keys, effects, opts)
end

vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- file search via leader-leader
normal("<leader><leader>", "<cmd>Telescope git_files<CR>")

-- system clipboard via leader
normal("<leader>p", '"+p')
visual("<leader>p", '"+p')
normal("<leader>P", '"+P')
visual("<leader>P", '"+P')
normal("<leader>y", '"+y')
visual("<leader>y", '"+y')
normal("<leader>Y", '"+y$')

-- file interactions
normal("<leader>fs", "<cmd>write<CR>")
normal("<leader>fe", "<cmd>NvimTreeToggle<CR>")
normal("<leader>fo", "<cmd>SymbolsOutline<CR>")
normal("<leader>f%", "<cmd>luafile %<CR>")
normal("<leader>fd", "<cmd>Trouble document_diagnostics<CR>")

-- common actions via leader
normal("<leader>t", "<cmd>Telescope<CR>")
normal("<leader>g", "<cmd>Neogit<CR>")

-- lsp code action
normal("<leader>.", "<cmd>lua vim.lsp.buf.code_action()<CR>")

-- clear search highlight
normal("<esc>", "<cmd>noh<CR>")

-- window navigation and management
normal("<leader><left>" , "<C-w>h")
normal("<leader><up>"   , "<C-w>k")
normal("<leader><down>" , "<C-w>j")
normal("<leader><right>", "<C-w>l")
normal("<leader>z"      , "<cmd>ZenMode<CR>")

-- buffer navigation and management
normal("<leader>bb", "<cmd>Telescope buffers<CR>")
normal("<leader>bn", "<cmd>lua require('bufferline').cycle(1)<CR>")
normal("<leader>bp", "<cmd>lua require('bufferline').cycle(-1)<CR>")
normal("<leader>bk", "<cmd>lua require('close_buffers').delete({type='this'})<CR>")
normal("<leader>bo", "<cmd>lua require('close_buffers').delete({type='other'})<CR>")

-- copilot extra accept mapping
vim.cmd [[
    imap <silent><script><expr> <C-J> copilot#Accept("\<CR>")
]]

