local M = {}

function M.set_opts(options)
  vim.opt_local.tabstop = options.width
  vim.opt_local.shiftwidth = options.width
  vim.opt_local.smarttab = true

  if options.style == 'space' then
    vim.opt_local.expandtab = true
    vim.opt_local.softtabstop = options.width
  end

  if options.ruler then
    vim.opt_local.colorcolumn = { options.ruler }
  end
end

function M.ensure_lazy()
  local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"

  if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })

    if vim.v.shell_error ~= 0 then
      vim.api.nvim_echo({
        { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
        { out, "WarningMsg" },
        { "\nPress any key to exit..." },
      }, true, {})

      vim.fn.getchar()

      os.exit(1)
    end
  end

  vim.opt.rtp:prepend(lazypath)
end

return M
