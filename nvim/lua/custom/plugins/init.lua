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
      cmake_regenerate_on_save = false,
      cmake_build_directory = function()
        local osys = require 'cmake-tools.osys'
        if osys.iswin32 then
          return 'build\\${variant:buildType}'
        end
        return 'build/${variant:buildType}'
      end, -- this is used to specify generate directory for cmake, allows macro expansion, can be a string or a function returning the string, relative to cwd.
    },
  },
  {
    'tikhomirov/vim-glsl',
    opts = {},
    config = function() end,
  },
  {
    'fnune/recall.nvim',
    config = function()
      local recall = require 'recall'

      recall.setup {}
      vim.keymap.set('n', '<leader>mm', recall.toggle, { noremap = true, silent = true })
      vim.keymap.set('n', '<leader>mn', recall.goto_next, { noremap = true, silent = true })
      vim.keymap.set('n', '<leader>mp', recall.goto_prev, { noremap = true, silent = true })
      vim.keymap.set('n', '<leader>mc', recall.clear, { noremap = true, silent = true })
      vim.keymap.set('n', '<leader>ml', ':Telescope recall<CR>', { noremap = true, silent = true })
    end,
  },
  -- {
  --   'mfussenegger/nvim-dap',
  --   opts = {},
  --   config = function(_, opts)
  --     local dap = require 'dap'
  --     dap.adapters.gdb = {
  --       type = 'executable',
  --       command = 'gdb',
  --       args = { '-i', 'dap' },
  --     }
  --
  --     local setup = {
  --       name = 'Launch',
  --       type = 'gdb',
  --       request = 'launch',
  --       program = function()
  --         return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
  --       end,
  --       cwd = '${workspaceFolder}',
  --       stopAtBeginningOfMainSubprogram = false,
  --     }
  --
  --     dap.configurations.c = { setup }
  --     dap.configurations.cpp = { setup }
  --
  --     local keys_setup = {
  --       {
  --         '<F5>',
  --         function()
  --           dap.continue()
  --         end,
  --       },
  --       {
  --         '<F10>',
  --         function()
  --           dap.step_over()
  --         end,
  --       },
  --       {
  --         '<F9>',
  --         function()
  --           dap.toggle_breakpoint()
  --         end,
  --       },
  --       {
  --         '<F11>',
  --         function()
  --           dap.step_into()
  --         end,
  --       },
  --       {
  --         '<F12>',
  --         function()
  --           dap.step_out()
  --         end,
  --       },
  --     }
  --
  --     for _, key_data in pairs(keys_setup) do
  --       vim.keymap.set('n', key_data[1], key_data[2])
  --     end
  --   end,
  -- },
  -- {
  --   'rcarriga/nvim-dap-ui',
  --   dependencies = { 'mfussenegger/nvim-dap', 'nvim-neotest/nvim-nio' },
  --   config = function(_, opts)
  --     require('dapui').setup()
  --   end,
  -- },
}
