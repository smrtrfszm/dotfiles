return {
  {
    'nvim-treesitter/nvim-treesitter',
    version = false,
    build = ':TSUpdate',
    event = { 'BufReadPost', 'BufNewFile' },
    cmd = { 'TSUpdateSync' },
    opts = {
      ensure_installed = {
        'lua',
        'bash',
      },
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
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
        extended_mode = false,
      },
    },
    config = function (_, opts)
      require('nvim-treesitter.configs').setup(opts)
    end,
  },
}
