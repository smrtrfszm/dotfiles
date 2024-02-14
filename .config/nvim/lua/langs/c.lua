return {
  {
    'nvim-treesitter',
    opts = function (_, opts)
      vim.list_extend(opts.ensure_installed, { 'c', 'cpp', 'meson', 'cmake' })
    end,
  },

  {
    'nvim-lspconfig',
    opts = {
      servers = {
        clangd = {
        },
        cmake = {
        },
      }
    }
  },
}
