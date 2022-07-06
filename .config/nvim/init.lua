local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  packer_bootstrap = vim.fn.system({
    'git',
    'clone',
    '--depth', '1',
    'https://github.com/wbthomason/packer.nvim',
    install_path
  })
end

require('settings')
require('keys')

local status_ok, packer = pcall(require, 'packer')
if not status_ok then
  return
end

packer.init({
  display = {
    open_fn = function()
      return require('packer.util').float({ border = 'rounded' })
    end
  }
})

packer.startup(function(use)
  use 'wbthomason/packer.nvim'

  use 'airblade/vim-rooter'
  use 'editorconfig/editorconfig-vim'

  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = function ()
      require('plugins.treesitter')
    end,
  }

  use {
    'nvim-telescope/telescope.nvim',
    requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'},
    config = function ()
      require('plugins.telescope')
    end,
  }

  use {
    'neovim/nvim-lspconfig',
    config = function ()
      require('plugins.lspconfig')
    end,
  }

  use {
    'lewis6991/gitsigns.nvim',
    requires = {'nvim-lua/plenary.nvim'},
    config = function ()
      require('gitsigns').setup {
        signs = {
          add          = {hl = 'GitSignsAdd',    text = '▌'},
          change       = {hl = 'GitSignsChange', text = '▌'},
          delete       = {hl = 'GitSignsDelete', text = '▁'},
          topdelete    = {hl = 'GitSignsDelete', text = '▔'},
          changedelete = {hl = 'GitSignsChange', text = '~'},
        },
      }
    end,
  }

  use {
    'tjdevries/colorbuddy.nvim',
    config = function ()
      require('colorscheme')
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
    'williamboman/nvim-lsp-installer',
    requires = 'neovim/nvim-lspconfig',
    config = function ()
      require('plugins.lspinstall')
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
        char_highlight_list = {'IndentLine'},
        char = '▏',
        context_highlight_list = {'IndentLineContext'},
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
