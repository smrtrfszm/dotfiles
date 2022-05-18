local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
  execute 'packadd packer.nvim'
end

local status_ok, packer = pcall(require, "packer")
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
  use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
  use 'nvim-treesitter/playground'
  use {'nvim-telescope/telescope.nvim', requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'}}
  use 'neovim/nvim-lspconfig'
  use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'}}
  use 'tjdevries/colorbuddy.nvim'
  use 'norcalli/nvim-colorizer.lua'
  use 'kdheepak/lazygit.nvim'
  use {'glepnir/galaxyline.nvim', branch = 'main'}
  use 'onsails/lspkind-nvim'
  use 'andweeb/presence.nvim'
  use 'windwp/nvim-autopairs'
  use 'windwp/nvim-ts-autotag'
  use {'folke/todo-comments.nvim', requires = 'nvim-lua/plenary.nvim'}
  use {'williamboman/nvim-lsp-installer', requires = 'neovim/nvim-lspconfig'}
  use 'nvim-lua/plenary.nvim'
  use 'numToStr/Comment.nvim'
  use {'JoosepAlviste/nvim-ts-context-commentstring', branch = 'main'}

  use {
    'NTBBloodbath/rest.nvim',
    requires = {'nvim-lua/plenary.nvim'},
    ft = 'http',
    config = function ()
      local rest_nvim = require('rest_nvim')
      rest_nvim.setup({})
      vim.keymap.set('n', '<leader>rn', rest_nvim.run)
    end,
  }

  use 'hrsh7th/nvim-cmp'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-nvim-lua'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'

  use 'L3MON4D3/LuaSnip'
  use 'saadparwaiz1/cmp_luasnip'

  use 'pantharshit00/vim-prisma'
end)

-- Plugin configs
require('plugins.lspconfig')
require('plugins.lspinstall')
require('plugins.treesitter')
require('plugins.gitsigns')
require('plugins.colorizer')
require('plugins.galaxyline')
require('plugins.telescope')
require('plugins.presence')
require('plugins.autopairs')
require('plugins.autotag')
require('plugins.todo-comments')
require('plugins.cmp')
require('plugins.comment')
