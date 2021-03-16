local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
  execute 'packadd packer.nvim'
end

require('packer').startup(function()
  use 'wbthomason/packer.nvim'
  use 'itchyny/lightline.vim'
  use 'airblade/vim-rooter'
--   use {'neoclide/coc.nvim', branch = 'release'}
  use 'editorconfig/editorconfig-vim'
  use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
  use 'nvim-treesitter/playground'
  use {'b3nj5m1n/kommentary', branch = 'main'}
  use {'nvim-telescope/telescope.nvim', requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'}}
  use 'neovim/nvim-lspconfig'
  use 'hrsh7th/nvim-compe'
  use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'}}
  use 'tjdevries/colorbuddy.nvim'
  use 'norcalli/nvim-colorizer.lua'
end)

-- Plugin configs
require('plugins.lspconfig')
require('plugins.kommentary')
require('plugins.treesitter')
require('plugins.gitsigns')
require('plugins.nvim-compe')
require('plugins.colorizer')
