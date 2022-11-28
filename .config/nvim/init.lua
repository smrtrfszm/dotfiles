local ensure_packer = function ()
  local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

  if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.fn.system {'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path}
    vim.cmd 'packadd packer.nvim'
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

local status_ok, packer = pcall(require, 'packer')
if not status_ok then
  return
end

require('impatient')

require('settings')
require('keys')

packer.init({
  display = {
    open_fn = function()
      return require('packer.util').float({ border = 'rounded' })
    end
  }
})

packer.startup(function(use)
  use 'wbthomason/packer.nvim'
  use 'lewis6991/impatient.nvim'

  use 'airblade/vim-rooter'
  use 'editorconfig/editorconfig-vim'

  use {
    'nvim-treesitter/nvim-treesitter',
    requires = 'p00f/nvim-ts-rainbow',
    run = ':TSUpdate',
    config = function ()
      require('plugins.treesitter')
    end,
  }

  use {
    'nvim-treesitter/playground',
    requires = 'nvim-treesitter/nvim-treesitter',
  }

  use {
    'nvim-telescope/telescope.nvim',
    requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'},
    config = function ()
      require('telescope').setup {
        file_ignore_patterns = {'node_modules/.*', '.git/.*', 'target/.*'}
      }
    end,
  }

  use {
    'lewis6991/gitsigns.nvim',
    requires = {'nvim-lua/plenary.nvim'},
    config = function ()
      require('gitsigns').setup {
        signs = {
          add          = {hl = 'GitSignsAdd',       text = '▌'},
          change       = {hl = 'GitSignsChange',    text = '▌'},
          delete       = {hl = 'GitSignsDelete',    text = '▁'},
          topdelete    = {hl = 'GitSignsDelete',    text = '▔'},
          changedelete = {hl = 'GitSignsChange',    text = '~'},
          untracked    = {hl = 'GitSignsUntracked', text = '▌'},
        },
      }
    end,
  }

  use {
    'norcalli/nvim-colorizer.lua',
    config = function ()
      require('colorizer').setup()
    end,
  }

  use 'kdheepak/lazygit.nvim'

  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons', opt = true },
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
  }

  use 'onsails/lspkind-nvim'

  use {
    'windwp/nvim-autopairs',
    config = function ()
      require('nvim-autopairs').setup {}
    end,
  }

  use {
    'windwp/nvim-ts-autotag',
    requires = "nvim-treesitter/nvim-treesitter",
  }

  use {
    'folke/todo-comments.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function ()
      require('todo-comments').setup {
        highlight = {
          keyword = 'bg',
        }
      }
    end,
  }

  use {
    'williamboman/mason.nvim',
    requires = {
      'williamboman/mason-lspconfig.nvim',
      'neovim/nvim-lspconfig',
      'hrsh7th/cmp-nvim-lsp',
    },
    config = function ()
      require('mason').setup()
      require('mason-lspconfig').setup {}

      local capabilities = require("cmp_nvim_lsp").default_capabilities()

      require('mason-lspconfig').setup_handlers {
        function (server_name)
          require('lspconfig')[server_name].setup {
            capabilities = capabilities,
          }
        end,
        sumneko_lua = function ()
          require('lspconfig').sumneko_lua.setup {
            settings = {
              Lua = {
                diagnostics = {
                  globals = { 'vim' },
                },
              },
            },
          }
        end,
      }
    end,
  }

  use {
    'numToStr/Comment.nvim',
    requires = {'JoosepAlviste/nvim-ts-context-commentstring', branch = 'main'},
    config = function ()
      require('plugins.comment')
    end,
  }

  use {
    'NTBBloodbath/rest.nvim',
    requires = {'nvim-lua/plenary.nvim'},
    ft = 'http',
    config = function ()
      local rest_nvim = require('rest-nvim')
      rest_nvim.setup({})
      vim.keymap.set('n', '<leader>rn', rest_nvim.run)
    end,
  }

  use {
    'lukas-reineke/indent-blankline.nvim',
    config = function ()
      vim.opt.list = true
      vim.opt.listchars:append('eol:↴')

      require('indent_blankline').setup({
        show_end_of_line = true,
        show_current_context = true,
        space_char_blankline = ' ',
        char = '▏',
        show_trailing_blankline_indent = false,
      })
    end,
  }

  use {
    'hrsh7th/nvim-cmp',
    requires = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-nvim-lua',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
    },
    config = function ()
      require('plugins.cmp')
    end,
  }

  use 'L3MON4D3/LuaSnip'
  use 'saadparwaiz1/cmp_luasnip'

  if packer_bootstrap then
    require('packer').sync()
  end
end)
