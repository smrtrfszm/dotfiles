local gls = require('galaxyline').section
local condition = require('galaxyline.condition')
local colors = require('colors')

local bg_color = colors.gray1

local modes = {
  n = {'NORMAL', colors.green},
  i = {'INSERT', colors.blue},
  c = {'COMMAND', colors.cyan},
  v = {'VISUAL', colors.orange},
  V = {'VISUAL LINE', colors.orange},
  [''] = {'VISUAL BLOCK', colors.orange},
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
  {FileEncode = {
    provider = 'FileEncode',
    condition = condition.hide_in_width,
    separator = ' ',
    separator_highlight = {'none', bg_color},
    highlight = {colors.green, bg_color, 'bold'}
  }},
  {FileFormat = {
    provider = 'FileFormat',
    condition = condition.hide_in_width,
    separator = ' ',
    separator_highlight = {'none', bg_color},
    highlight = {colors.green, bg_color, 'bold'}
  }},
  {GitBranch = {
    provider = 'GitBranch',
    icon = ' ',
    condition = condition.check_git_workspace,
    separator = ' ',
    separator_highlight = {'none', bg_color},
    highlight = {colors.magenta, bg_color}
  }},
  {LineInfo = {
    provider = 'LineColumn',
    separator = ' ',
    separator_highlight = {'none', bg_color},
    highlight = {colors.gray6, bg_color}
  }},
  {Percent = {
    provider = 'LinePercent',
    separator = ' ',
    separator_highlight = {'none', bg_color},
    highlight = {bg_color, colors.blue, 'bold'},
  }},
}
