return {
  {
    'nvim-treesitter',
    opts = function (_, opts)
      vim.list_extend(opts.ensure_installed, { 'c', 'cpp' })
    end,
  },

  {
    'nvim-lspconfig',
    opts = {
      servers = {
        clangd = {
        }
      }
    }
  },
}
