vim.loader.enable()

local utils = require('utils')
utils.ensure_lazy()

require('ft')
require('settings')
require('keys')

require('lazy').setup({
  install = {
    colorscheme = { "base16-default-dark" },
  },
  dev = {
    path = '~/dev/',
  },
  spec = {
    { import = 'plugins.treesitter' },
    { import = 'plugins.comment' },
    { import = 'plugins.lsp' },
    { import = 'plugins.cmp' },
    { import = 'langs' },

    {
      'nvim-telescope/telescope.nvim',
      dependencies = {
        'nvim-lua/plenary.nvim',
        'nvim-telescope/telescope-ui-select.nvim',
      },
      -- cmd = 'Telescope',
      opts = {
        file_ignore_patterns = {'node_modules/.*', '.git/.*', 'target/.*'},
        extensions = {
          ['ui-select'] = {},
        },
      },
      config = function (opts)
        local telescope = require('telescope')
        telescope.setup(opts)
        telescope.load_extension('ui-select')
      end
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
          add          = { text = '▌'},
          change       = { text = '▌'},
          delete       = { text = '▁'},
          topdelete    = { text = '▔'},
          changedelete = { text = '~'},
          untracked    = { text = '▌'},
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
      main = 'ibl',
      opts = {
        indent = {
          char = '▏',
        },
      },
    },

    {
      'notjedi/nvim-rooter.lua',
      opts = {},
    },
  },
})
