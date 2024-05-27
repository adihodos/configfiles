-- You can add your own plugins here or in other files in this directory!
--  I promise not to create any merge conflicts in this directory :)
--
-- See the kickstart.nvim README for more information
return {
  {
    'direnv/direnv.vim',
    opts = {},
    config = function(_, opts) end,
  },
  {
    'rebelot/kanagawa.nvim',
  },
  {
    'mg979/vim-visual-multi',
    opts = {},
    config = function(_, opts)
      vim.g.VM_maps['Select Cursor Down'] = '<M-C-Down>'
      vim.g.VM_maps['Select Cursor Up'] = '<M-C-Up>'
    end,
  },
  {
    'nvim-telescope/telescope-file-browser.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim', 'nvim-lua/plenary.nvim' },
    config = function()
      local builtin = require 'telescope'
      builtin.load_extension 'file_browser'

      vim.keymap.set('n', '<leader>sb', function()
        builtin.extensions.file_browser.file_browser()
      end, { desc = '[S]earch File [B]rowser' })
    end,
  },
  {
    'ecthelionvi/NeoSwap.nvim',
    opts = {},
    config = function(_, opts)
      vim.keymap.set('n', '<C-t>e', '<cmd>NeoSwapNext<cr>', { noremap = true, silent = true })
      vim.keymap.set('n', '<leader>E', '<cmd>NeoSwapPrev<cr>', { noremap = true, silent = true })
    end,
  },
  {
    'akinsho/toggleterm.nvim',
    version = '*',
    opts = {
      open_mapping = [[<c-\>]],
      --[[ things you want to change go here]]
    },
  },
  {
    'stevearc/overseer.nvim',
    opts = {},
  },
  {
    'Civitasv/cmake-tools.nvim',
    opts = {
      cmake_build_options = { '-j' },
      cmake_build_directory = function()
        local osys = require 'cmake-tools.osys'
        if osys.iswin32 then
          return 'out\\${variant:buildType}'
        end
        return 'out/${variant:buildType}'
      end, -- this is used to specify generate directory for cmake, allows macro expansion, can be a string or a function returning the string, relative to cwd.
    },
  },
  {
    'tikhomirov/vim-glsl',
    opts = {},
    config = function() end,
  },
}
