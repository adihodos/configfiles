-- You can add your own plugins here or in other files in this directory!
--  I promise not to create any merge conflicts in this directory :)
--
-- See the kickstart.nvim README for more information
return {
    {
        'direnv/direnv.vim', opts = {}, config = function(_, opts)
        end
    },
    {
        'rebelot/kanagawa.nvim'
    },
    {
        'mg979/vim-visual-multi',
        opts = {},
        config = function(_, opts)
            vim.g.VM_maps["Select Cursor Down"]          = '<M-C-Down>'
            vim.g.VM_maps["Select Cursor Up"]            = '<M-C-Up>'
        end
    },
    {
        "nvim-telescope/telescope-file-browser.nvim",
        dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
        config = function()
            local builtin = require 'telescope'
            builtin.load_extension 'file_browser'

            vim.keymap.set('n', '<leader>sb', function()
                builtin.extensions.file_browser.file_browser()
            end, { desc = '[S]earch File [B]rowser' })
        end
    },
    {
      "ecthelionvi/NeoSwap.nvim",
      opts = {},
      config = function(_, opts)
        vim.keymap.set("n", "<C-t>e", "<cmd>NeoSwapNext<cr>", { noremap = true, silent = true })
        vim.keymap.set("n", "<leader>E", "<cmd>NeoSwapPrev<cr>", { noremap = true, silent = true })
      end
    }
}
