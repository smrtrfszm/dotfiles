local vim = _G.vim
local gls = require('galaxyline').section
local condition = require('galaxyline.condition')
local colors = require('colors')

local bg_color = colors.gray1

local modes = {
  n =      {'NORMAL', colors.green},
  i =      {'INSERT', colors.blue},
  c =      {'COMMND', colors.magenta},
  v =      {'VISUAL', colors.orange},
  V =      {'V LINE', colors.orange},
  [''] = {'V BLCK', colors.orange},
  t =      {'TERMNL', colors.yellow},
  R =      {'REPLCE', colors.cyan},
}

local fileformats = {
  dos  = 'CRLF',
  unix = 'LF',
  mac  = 'CR',
}

gls.left = {
  {ViMode = {
    provider = function()
      local mode = modes[vim.fn.mode()]
      vim.cmd('hi GalaxyViMode guibg=' .. mode[2])
      return '  ' .. mode[1] .. ' '
    end,
    separator = ' ',
    separator_highlight = {'NONE', bg_color},
    highlight = {bg_color, colors.red, 'bold'},
  }},
  {FileName = {
    provider = 'FileName',
    condition = condition.buffer_not_empty,
    highlight = {colors.white, bg_color},
  }},
}

gls.mid = {
}

gls.right = {
  {FileType = {
    provider = function()
      return vim.bo.filetype
    end,
    separator = '  ',
    separator_highlight = {'none', bg_color},
    highlight = {colors.white, bg_color, 'bold'}
  }},
  {FileFormat = {
    provider = function()
      return fileformats[vim.bo.fileformat]
    end,
    condition = condition.hide_in_width,
    separator = '  ',
    separator_highlight = {'none', bg_color},
    highlight = {colors.white, bg_color }
  }},
  {FileEncode = {
    provider = function()
      local encode = vim.bo.fenc ~= '' and vim.bo.fenc or vim.o.enc
      return encode:upper()
    end,
    condition = condition.hide_in_width,
    separator = ' ',
    separator_highlight = {'none', bg_color},
    highlight = {colors.white, bg_color}
  }},
  {GitBranch = {
    provider = 'GitBranch',
    icon = ' ',
    condition = condition.check_git_workspace,
    separator = '  ',
    separator_highlight = {'none', bg_color},
    highlight = {colors.magenta, bg_color}
  }},
  {LineInfo = {
    provider = function()
      local col = vim.fn.col('.')
      local line = vim.fn.line('.')

      local info = '  ' .. line .. ':' .. col

      local curr = vim.fn.line('.')
      local total = vim.fn.line('$')
      local percent = math.floor(curr * 100 / total) .. '%'

      local mode = modes[vim.fn.mode()]
      vim.cmd('hi GalaxyLineInfo guibg=' .. mode[2])

      if curr == 1 then
        percent = 'Top'
      elseif curr == total then
        percent = 'Bottom'
      end

      return info .. ' | ' .. percent .. ' '
    end,
    separator = ' ',
    separator_highlight = {'none', bg_color},
    highlight = {bg_color, colors.red},
  }},
}
