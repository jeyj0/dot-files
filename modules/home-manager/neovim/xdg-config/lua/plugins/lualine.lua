require("nvim-gps").setup()
local gps = require("nvim-gps")
local function diff_source()
	local gitsigns = vim.b.gitsigns_status_dict
	if gitsigns then
		return {
			added = gitsigns.added,
			modified = gitsigns.changed,
			removed = gitsigns.removed,
		}
	end
end

require("lualine").setup({
	options = {
		icons_enabled = true,
		theme = "gruvbox_dark", -- "auto",
		-- component_separators = { left = "", right = "" },
		component_separators = '',
		-- section_separators = { left = " ", right = " " }, -- l:  r: 
		section_separators = '',
		disabled_filetypes = { "alpha", "dashboard", "toggleterm" },
		always_divide_middle = true,
		globalstatus = true,
	},
	sections = {
		lualine_a = { "branch" },
		lualine_b = { { "diff", source = diff_source }, "diagnostics" },
		lualine_c = { "tabs", "filename", "lsp_progress", { gps.get_location, cond = gps.is_available } },
		lualine_x = { "encoding", "filetype" },
		lualine_y = { "progress" },
		lualine_z = { "location" },
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = { "filename" },
		lualine_x = { "location" },
		lualine_y = {},
		lualine_z = {},
	},
	tabline = {},
	extensions = {},
})

