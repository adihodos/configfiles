return {
	server = {        
		on_attach = require("user.lsp.handlers").on_attach,
		capabilities = require("user.lsp.handlers").capabilities,
		cmd = {
			"clangd",
			"--background-index",
			"--pch-storage=memory",
			"--clang-tidy",
			"--suggest-missing-includes",
			"--all-scopes-completion",
			"--pretty",
			"--header-insertion=never",
			"-j=4",
			"--inlay-hints",
			"--header-insertion-decorators",
	  },
	  filetypes = {"c", "cpp", "cc", "objc", "objcpp"},
	  root_dir = vim.fs.dirname(vim.fs.find({'compile_commands.json', '.clangd', "compile_flags.txt", ".git"}, { upward = true })[1]),
	  -- root_dir = utils.root_pattern("compile_commands.json", "compile_flags.txt", ".git")
	  init_option = { fallbackFlags = {  "-std=c++2a"  } }
	}
}
