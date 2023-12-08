local keymap = vim.keymap

local opts = { noremap=true, silent=true }
-- Directory navigation
keymap.set("n", "<leader>m", ":NvimTreeFocus<CR>", opts)
keymap.set("n", "<leader>f", ":NvimTreeToggle<CR>", opts)

-- Pane Navigation

						keymap.set("n", "<C-h>", "<C-w>h", opts)
						keymap.set("n", "<C-j>", "<C-w>j", opts)
						keymap.set("n", "<C-k>", "<C-w>k", opts)
						keymap.set("n", "<C-l>", "<C-w>l", opts)

-- Window Management
 
keymap.set("n", "<leader>sv", ":vsplit<CR>", opts)
keymap.set("n", "<leader>sh", ":split<CR>", opts)
keymap.set("n", "<leader>sm", ":MaximizerToggle<CR>", opts)

-- Indenting
keymap.set("v", "<", "<gv") -- Shift Indentation to Left
keymap.set("v", ">", ">gv") -- Shift Indentation to Right


-- Comments
vim.api.nvim_set_keymap("n", "<C-_>", "gtc", { noremap = false })
vim.api.nvim_set_keymap("v", "<C-_>", "goc", { noremap = false })
