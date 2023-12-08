local opts = {
	ensure_installed = {
		"efm",
		-- "bashls",
		-- "pyright",
		-- "html",
		"lua_ls",
		-- "jsonls",
		"clangd",
		-- "cmake",
		"rust_analyzer",
		"taplo"
	},

	automatic_installation = true,
}

return {
	"williamboman/mason-lspconfig.nvim",
	opts = opts,
	event = "BufReadPre",
	dependencies = "williamboman/mason.nvim",
}
