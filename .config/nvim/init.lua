local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'

if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable',
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

require('settings')
require('keys')

require('lazy').setup({
  spec = {
    { import = 'plugins.treesitter' },
    { import = 'plugins.comment' },
    { import = 'plugins.lsp' },
    { import = 'plugins.cmp' },
    { import = 'plugins.langs' },

    {
      'nvim-telescope/telescope.nvim',
      dependencies = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'},
      cmd = 'Telescope',
      opts = {
        file_ignore_patterns = {'node_modules/.*', '.git/.*', 'target/.*'},
      },
      config = true,
    },

    {
      'nvim-lualine/lualine.nvim',
      dependencies = { 'kyazdani42/nvim-web-devicons', lazy = true },
      event = 'VeryLazy',
      config = function ()
        local colors = require('colors')
        require('lualine').setup {
          options = {
            theme = {
              normal = {
                a = {bg = colors.white, fg = colors.black, gui = 'bold'},
                b = {bg = colors.black, fg = colors.white},
                c = {bg = colors.gray2, fg = colors.white}
              },
              insert = {
                a = {bg = colors.green, fg = colors.black, gui = 'bold'},
                b = {bg = colors.black, fg = colors.green},
              },
              visual = {
                a = {bg = colors.blue, fg = colors.black, gui = 'bold'},
                b = {bg = colors.black, fg = colors.blue},
              },
              replace = {
                a = {bg = colors.orange, fg = colors.black, gui = 'bold'},
                b = {bg = colors.black, fg = colors.orange},
              },
              command = {
                a = {bg = colors.red, fg = colors.black, gui = 'bold'},
                b = {bg = colors.black, fg = colors.red},
              },
              inactive = {
                a = {bg = colors.gray1, fg = colors.gray4, gui = 'bold'},
                b = {bg = colors.gray1, fg = colors.gray4},
                c = {bg = colors.gray1, fg = colors.gray4}
              },
            },
            component_separators = '',
            section_separator = '',
          }
        }
      end,
    },

    {
      'lewis6991/gitsigns.nvim',
      dependencies = { 'nvim-lua/plenary.nvim' },
      event = { 'BufReadPre', 'BufNewFile' },
      opts = {
        signs = {
          add          = {hl = 'GitSignsAdd',       text = '▌'},
          change       = {hl = 'GitSignsChange',    text = '▌'},
          delete       = {hl = 'GitSignsDelete',    text = '▁'},
          topdelete    = {hl = 'GitSignsDelete',    text = '▔'},
          changedelete = {hl = 'GitSignsChange',    text = '~'},
          untracked    = {hl = 'GitSignsUntracked', text = '▌'},
        },
        on_attach = function (buffer)
          vim.keymap.set('n', '<leader>ghb', function ()
            package.loaded.gitsigns.blame_line({ full = true })
          end, { buffer = buffer })
        end
      },
      config = true,
    },

    {
      'windwp/nvim-autopairs',
      event = { 'InsertEnter' },
      config = true,
    },

    {
      'folke/todo-comments.nvim',
      dependencies = { 'nvim-lua/plenary.nvim' },
      event = { 'BufReadPost', 'BufNewFile' },
      opts = {
        highlight = {
          keyword = 'bg',
        },
      },
      config = true,
    },

    {
      'lukas-reineke/indent-blankline.nvim',
      event = { 'BufReadPost', 'BufNewFile' },
      opts = {
        show_end_of_line = true,
        show_current_context = true,
        space_char_blankline = ' ',
        char = '▏',
        context_char = '▏',
        show_trailing_blankline_indent = false,
      },
      config = function (_, opts)
        vim.opt.list = true
        vim.opt.listchars:append('eol:↴')

        require('indent_blankline').setup(opts)
      end,
    },
    --     -- local dapui = require('dapui')
    --     --
    --     -- dap.listeners.after.event_initialized['dapui_config'] = function ()
    --     --   dapui.open()
    --     -- end
    --     -- dap.listeners.before.event_terminated['dapui_config'] = function ()
    --     --   dapui.close()
    --     -- end
    --     -- dap.listeners.before.event_exited['dapui_config'] = function ()
    --     --   dapui.close()
    --     -- end
    --     --
    --     -- require("project_nvim").setup {}

  },
})


-- vim.loader.enable()
--
-- local _, _ = pcall(require, 'impatient')
--
--   use {
--     'norcalli/nvim-colorizer.lua',
--     ft = {'css', 'scss'},
--     config = function ()
--       require('colorizer').setup()
--     end,
--   }
--
--   use {
--     'windwp/nvim-ts-autotag',
--     ft = 'html',
--     requires = "nvim-treesitter/nvim-treesitter",
--   }
--
--
--   use {
--     'rcarriga/nvim-dap-ui',
--     requires = "mfussenegger/nvim-dap",
--     config = function ()
--       require('dapui').setup()
--     end
--   }
--
--   use {
--     'NTBBloodbath/rest.nvim',
--     requires = {'nvim-lua/plenary.nvim'},
--     ft = 'http',
--     config = function ()
--       local rest_nvim = require('rest-nvim')
--       rest_nvim.setup({})
--       vim.keymap.set('n', '<leader>rn', rest_nvim.run)
--     end,
--   }
