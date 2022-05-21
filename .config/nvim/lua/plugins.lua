local fn = vim.fn

-- automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system {
		"git", "clone", "--depth", "1",
		"https://github.com/wbthomason/packer.nvim", install_path
	}
	print "Installing packer; close and reopen Neovim..."
	vim.cmd [[packadd packer.nvim]]
end

vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]

local status_ok, packer = pcall(require, "packer")
if not status_ok then return end

packer.init {
	display = {
		open_fn = function()
			return require("packer.util").float { border = "rounded" }
		end
	}
}

return packer.startup(function(use)
	use "wbthomason/packer.nvim"

	-- lsp
	use "williamboman/nvim-lsp-installer"
	use 'neovim/nvim-lspconfig' -- Collection of configurations for the built-in LSP client
	use "nvim-lua/popup.nvim" -- An implementation of the Popup API from vim in Neovim
	use "nvim-lua/plenary.nvim" -- Useful lua functions used ny lots of plugins
	use "jose-elias-alvarez/null-ls.nvim"

	-- autocompletion
	use "hrsh7th/nvim-cmp"
	use "hrsh7th/cmp-nvim-lsp"
	use "hrsh7th/cmp-buffer"
	use "hrsh7th/cmp-path"
	use "hrsh7th/cmp-cmdline"
	-- use "hrsh7th/cmp-copilot"
	use "hrsh7th/cmp-nvim-lua"
	-- use {
	--	 'tzachar/cmp-tabnine',
	--	 run = './install.sh',
	--	 requires = 'hrsh7th/nvim-cmp'
	-- }
	use "onsails/lspkind-nvim"
	use "saadparwaiz1/cmp_luasnip"
	use "L3MON4D3/LuaSnip"
	use "rafamadriz/friendly-snippets" -- a bunch of snippets to use
	-- use "f3fora/cmp-spell"
	-- use "github/copilot.vim"
	use {
		"zbirenbaum/copilot.lua",
		event = { "VimEnter" },
		config = function()
			vim.defer_fn(function() require("copilot").setup {
					ft_disable = { "markdown", "terraform", "cpp" },
				}
			end, 100)
		end
	}
	use { "zbirenbaum/copilot-cmp", after = { "copilot.lua", "nvim-cmp" } }


	-- treesitter plugins
	use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
	use "nvim-treesitter/nvim-treesitter-refactor"
	use "windwp/nvim-ts-autotag"
	use {
		"SmiteshP/nvim-gps",
		requires = "nvim-treesitter/nvim-treesitter"
	}
	use {
		"danymat/neogen",
		config = function()
			require('neogen').setup({ snippet_engine = "luasnip" })
		end,
		requires = "nvim-treesitter/nvim-treesitter",
	}

	-- telescope plugins
	use {
		'nvim-telescope/telescope.nvim',
		requires = { { 'nvim-lua/plenary.nvim' } }
	}
	use "nvim-telescope/telescope-vimspector.nvim"
	use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }
	-- use "tami5/sqlite.lua"
	-- use "nvim-telescope/telescope-frecency.nvim"
	use "nvim-telescope/telescope-file-browser.nvim"
	use "nvim-telescope/telescope-ui-select.nvim"
	use "nvim-telescope/telescope-project.nvim"
	
	-- UI
	use "rcarriga/nvim-notify"
	use "lukas-reineke/indent-blankline.nvim"
	use "norcalli/nvim-colorizer.lua"
	use "Mofiqul/vscode.nvim"
	use "ellisonleao/gruvbox.nvim"
	use "goolord/alpha-nvim"
	use { 'akinsho/bufferline.nvim', tag = "v2.*", requires = "kyazdani42/nvim-web-devicons" }
	use { 'kyazdani42/nvim-tree.lua', tag = 'nightly', requires = 'kyazdani42/nvim-web-devicons' }

	-- UI.statusline
	use "nvim-lualine/lualine.nvim"
	use "arkav/lualine-lsp-progress"
	use "lewis6991/gitsigns.nvim"

	use "luukvbaal/stabilize.nvim"
	use "simrat39/symbols-outline.nvim"

	-- reasonable Bdelete command
	use "kazhala/close-buffers.nvim"

	if PACKER_BOOTSTRAP then require("packer").sync() end
end)

