local config = function()
	require("nvim-treesitter.configs").setup({
		indent = {
			enable = true,
		},
		autotag = {
			enable = true,
		},
		ensure_installed = {
			"markdown",
			"json",
			"javascript",
			"typescript",
			"yaml",
			"html",
			"css",
			"bash",
			"lua",
			"dockerfile",
			"solidity",
			"gitignore",
			"python",
			"rust",
			"cpp",
			"c",
			"c_sharp",
			"cmake",
			"glsl",
			"hlsl",
			"ron",
		},
		auto_install = true,
		highlight = {
			enable = true,
			additional_vim_regex_highlighting = true,
		},
		rainbow = {
				enable = true,
				extended_mode = true,
				max_file_lines = nil,
		}
	})
end

return {
	"nvim-treesitter/nvim-treesitter",
  lazy = false,
  config = config
}
