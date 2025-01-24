local M = {}

function M.set(opts)
  for lang, options in pairs(opts) do
    create_indent_autocmd(lang, options)
  end
end

function create_indent_autocmd(lang, options)
  vim.api.nvim_create_autocmd('Filetype', {
    pattern = lang,
    callback = function ()
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
    end,
  })
end


function M.lang(langs)
  for lang, opts in pairs(langs) do
    create_indent_autocmd(lang, opts.indent)
  end
end

return M
