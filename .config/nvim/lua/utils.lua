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

return M
