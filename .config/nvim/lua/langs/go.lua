return {
  {
    "nvim-treesitter",
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { "go", "gomod", "gosum", "gowork", "proto" })
    end
  },

  {
    "nvim-lspconfig",
    opts = {
      servers = {
        gopls = {
        }
      }
    }
  }
}
