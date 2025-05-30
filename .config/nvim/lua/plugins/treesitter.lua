return {
  {
    'nvim-treesitter/nvim-treesitter',
    lazy = false,
    branch = 'main',
    build = ':TSUpdate',

    opts = {
      ensure_installed = {},
    },
    config = function (_, opts)
      require('nvim-treesitter').install(opts.ensure_installed)
    end,
  },
}
