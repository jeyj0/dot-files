-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- taken from https://www.lazyvim.org/configuration/general
local Util = require("lazyvim.util")
local function map(mode, lhs, rhs, opts)
  local keys = require("lazy.core.handler").handlers.keys
  ---@cast keys LazyKeysHandler
  -- do not create the keymap if a lazy keys handler exists
  if not keys.active[keys.parse({ lhs, mode = mode }).id] then
    opts = opts or {}
    opts.silent = opts.silent ~= false
    vim.keymap.set(mode, lhs, rhs, opts)
  end
end

-- Move to window using the <ctrl> hjkl keys
map("n", "<leader><left>", "<C-w>h", { desc = "Go to left window" })
map("n", "<leader><down>", "<C-w>j", { desc = "Go to lower window" })
map("n", "<leader><up>", "<C-w>k", { desc = "Go to upper window" })
map("n", "<leader><right>", "<C-w>l", { desc = "Go to right window" })

-- Navigating buffers
if Util.has("bufferline.nvim") then
  map("n", "<leader>bp", "<cmd>BufferLineCyclePrev<cr>", { desc = "Prev buffer" })
  map("n", "<leader>bn", "<cmd>BufferLineCycleNext<cr>", { desc = "Next buffer" })
else
  map("n", "<leader>bp", "<cmd>bprevious<cr>", { desc = "Prev buffer" })
  map("n", "<leader>bn", "<cmd>bnext<cr>", { desc = "Next buffer" })
end

-- File handling
map("n", "<leader>fs", "<Cmd>w<CR>", { desc = "Save current file" })

-- OS/System Clipboard via leader
map("n", "<leader>p", '"+p', { desc = "Paste from OS clipboard" })
map("v", "<leader>p", '"+p', { desc = "Paste from OS clipboard" })
map("n", "<leader>P", '"+P', { desc = "Paste before from OS clipboard" })
map("v", "<leader>P", '"+P', { desc = "Paste before from OS clipboard" })
map("n", "<leader>y", '"+y', { desc = "Copy to OS clipboard" })
map("v", "<leader>y", '"+y', { desc = "Copy to OS clipboard" })
map("n", "<leader>Y", '"+y$', { desc = "Copy line to OS clipboard" })

