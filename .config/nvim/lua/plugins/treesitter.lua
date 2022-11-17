require'nvim-treesitter.configs'.setup {
  ensure_installed = 'all',
  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
  },
  playground = {
    enable = true,
  },
  autotag = {
    enable = true,
  },
  rainbow = {
    enable = true,
    disable = {'html', 'php'},
  },
}
