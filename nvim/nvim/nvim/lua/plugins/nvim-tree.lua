return {
	"nvim-tree/nvim-tree.lua",
	lazy = false,
	config = function()
		require('nvim-tree').setup({
				sort = {
				sorter = "case_sensitive",
			},
			view = {
				width = 48,
			},
			renderer = {
				group_empty = true,
			},
			filters = {
				dotfiles = true,
			}
	})
	end
}