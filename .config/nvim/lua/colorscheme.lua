-- Based on https://github.com/chriskempson/base16-vim/blob/master/colors/base16-default-dark.vim

local Color, colors, Group, groups, styles = require('colorbuddy').setup()

Color.new('black',   '#181818')
Color.new('gray1',   '#282828')
Color.new('gray2',   '#383838')
Color.new('gray3',   '#585858')
Color.new('gray4',   '#b8b8b8')
Color.new('gray5',   '#d8d8d8')
Color.new('gray6',   '#e8e8e8')
Color.new('white',   '#f8f8f8')
Color.new('red',     '#ab4642')
Color.new('orange',  '#dc9656')
Color.new('yellow',  '#f7ca88')
Color.new('green',   '#a1b56c')
Color.new('cyan',    '#86c1b9')
Color.new('blue',    '#7cafc2')
Color.new('magenta', '#ba8baf')
Color.new('brown',   '#a16946')

-- Vim editor colors
Group.new('Normal',        colors.white, colors.black,  styles.none)
Group.new('Bold',          colors.none,  colors.none,   styles.bold)
Group.new('Debug',         colors.red,   colors.none,   styles.none)
Group.new('Directory',     colors.blue,  colors.none,   styles.none)
Group.new('Error',         colors.black, colors.red,    styles.none)
Group.new('ErrorMsg',      colors.red,   colors.black,  styles.none)
Group.new('Exception',     colors.red,   colors.none,   styles.none)
Group.new('FoldColumn',    colors.cyan,  colors.gray1,  styles.none)
Group.new('Folded',        colors.gray3, colors.gray1,  styles.none)
Group.new('IncSearch',     colors.gray1, colors.orange, styles.none)
Group.new('Italic',        colors.none,  colors.none,   styles.italic)
Group.new('Macro',         colors.red,   colors.none,   styles.none)
Group.new('MatchParen',    colors.none,  colors.gray3,  styles.none)
Group.new('ModeMsg',       colors.green, colors.none,   styles.none)
Group.new('MoreMsg',       colors.green, colors.none,   styles.none)
Group.new('Question',      colors.blue,  colors.none,   styles.none)
Group.new('Search',        colors.gray1, colors.yellow, styles.none)
Group.new('Substitute',    colors.gray1, colors.yellow, styles.none)
Group.new('SpecialKey',    colors.gray3, colors.none,   styles.none)
Group.new('TooLong',       colors.red,   colors.none,   styles.none)
Group.new('Underlined',    colors.red,   colors.none,   styles.none)
Group.new('Visual',        colors.none,  colors.gray2,  styles.none)
Group.new('VisualNOS',     colors.red,   colors.none,   styles.none)
Group.new('WarningMsg',    colors.red,   colors.none,   styles.none)
Group.new('WildMenu',      colors.red,   colors.yellow, styles.none)
Group.new('Title',         colors.blue,  colors.none,   styles.none)
Group.new('Conceal',       colors.blue,  colors.black,  styles.none)
Group.new('Cursor',        colors.black, colors.gray5,  styles.none)
Group.new('NonText',       colors.gray3, colors.none,   styles.none)
Group.new('LineNr',        colors.gray3, colors.gray1,  styles.none)
Group.new('SignColumn',    colors.gray3, colors.gray1,  styles.none)
Group.new('StatusLine',    colors.gray4, colors.gray2,  styles.none)
Group.new('StatusLineNC',  colors.gray3, colors.gray1,  styles.none)
Group.new('VertSplit',     colors.gray2, colors.gray2,  styles.none)
Group.new('ColorColumn',   colors.none,  colors.gray1,  styles.none)
Group.new('CursorColumn',  colors.none,  colors.gray1,  styles.none)
Group.new('CursorLine',    colors.none,  colors.gray1,  styles.none)
Group.new('CursorLineNr',  colors.gray4, colors.gray1,  styles.none)
Group.new('QuickFixLine',  colors.none,  colors.gray1,  styles.none)
Group.new('PMenu',         colors.gray5, colors.gray1,  styles.none)
Group.new('PMenuSel',      colors.gray1, colors.gray5,  styles.none)
Group.new('TabLine',       colors.gray3, colors.gray1,  styles.none)
Group.new('TabLineFill',   colors.gray3, colors.gray1,  styles.none)
Group.new('TabLineSel',    colors.green, colors.gray1,  styles.none)

-- Standard syntax highlighting
Group.new('Boolean',       colors.orange,  colors.none,  styles.none)
Group.new('Character',     colors.red,     colors.none,  styles.none)
Group.new('Comment',       colors.gray3,   colors.none,  styles.none)
Group.new('Conditional',   colors.magenta, colors.none,  styles.none)
Group.new('Constant',      colors.orange,  colors.none,  styles.none)
Group.new('Define',        colors.magenta, colors.none,  styles.none)
Group.new('Delimiter',     colors.brown,   colors.none,  styles.none)
Group.new('Float',         colors.orange,  colors.none,  styles.none)
Group.new('Function',      colors.blue,    colors.none,  styles.none)
Group.new('Identifier',    colors.red,     colors.none,  styles.none)
Group.new('Include',       colors.blue,    colors.none,  styles.none)
Group.new('Keyword',       colors.magenta, colors.none,  styles.none)
Group.new('Label',         colors.yellow,  colors.none,  styles.none)
Group.new('Number',        colors.orange,  colors.none,  styles.none)
Group.new('Operator',      colors.gray5,   colors.none,  styles.none)
Group.new('PreProc',       colors.yellow,  colors.none,  styles.none)
Group.new('Repeat',        colors.yellow,  colors.none,  styles.none)
Group.new('Special',       colors.cyan,    colors.none,  styles.none)
Group.new('SpecialChar',   colors.brown,   colors.none,  styles.none)
Group.new('Statement',     colors.red,     colors.none,  styles.none)
Group.new('StorageClass',  colors.yellow,  colors.none,  styles.none)
Group.new('String',        colors.green,   colors.none,  styles.none)
Group.new('Structure',     colors.magenta, colors.none,  styles.none)
Group.new('Tag',           colors.yellow,  colors.none,  styles.none)
Group.new('Todo',          colors.yellow,  colors.gray1, styles.none)
Group.new('Type',          colors.yellow,  colors.none,  styles.none)
Group.new('Typedef',       colors.yellow,  colors.none,  styles.none)

-- C highlighting
Group.new('cOperator',     colors.cyan,    colors.none, styles.none)
Group.new('cPreCondit',    colors.magenta, colors.none, styles.none)

-- C# highlighting
Group.new('csClass',                 colors.yellow,  colors.none, styles.none)
Group.new('csAttribute',             colors.yellow,  colors.none, styles.none)
Group.new('csModifier',              colors.magenta, colors.none, styles.none)
Group.new('csType',                  colors.red,     colors.none, styles.none)
Group.new('csUnspecifiedStatement',  colors.blue,    colors.none, styles.none)
Group.new('csContextualStatement',   colors.magenta, colors.none, styles.none)
Group.new('csNewDecleration',        colors.red,     colors.none, styles.none)

-- CSS highlighting
Group.new('cssBraces',      colors.gray5,   colors.none, styles.none)
Group.new('cssClassName',   colors.magenta, colors.none, styles.none)
Group.new('cssColor',       colors.cyan,    colors.none, styles.none)

-- Diff highlighting
Group.new('DiffAdd',      colors.green, colors.gray1, styles.none)
Group.new('DiffChange',   colors.gray3, colors.gray1, styles.none)
Group.new('DiffDelete',   colors.red,   colors.gray1, styles.none)
Group.new('DiffText',     colors.blue,  colors.gray1, styles.none)
Group.new('DiffAdded',    colors.green, colors.black, styles.none)
Group.new('DiffFile',     colors.red,   colors.black, styles.none)
Group.new('DiffNewFile',  colors.green, colors.black, styles.none)
Group.new('DiffLine',     colors.blue,  colors.black, styles.none)
Group.new('DiffRemoved',  colors.red,   colors.black, styles.none)

-- Git highlighting
Group.new('gitcommitOverflow',       colors.red,     colors.none, styles.none)
Group.new('gitcommitSummary',        colors.green,   colors.none, styles.none)
Group.new('gitcommitComment',        colors.gray3,   colors.none, styles.none)
Group.new('gitcommitUntracked',      colors.gray3,   colors.none, styles.none)
Group.new('gitcommitDiscarded',      colors.gray3,   colors.none, styles.none)
Group.new('gitcommitSelected',       colors.gray3,   colors.none, styles.none)
Group.new('gitcommitHeader',         colors.magenta, colors.none, styles.none)
Group.new('gitcommitSelectedType',   colors.blue,    colors.none, styles.none)
Group.new('gitcommitUnmergedType',   colors.blue,    colors.none, styles.none)
Group.new('gitcommitDiscardedType',  colors.blue,    colors.none, styles.none)
Group.new('gitcommitBranch',         colors.orange,  colors.none, styles.bold)
Group.new('gitcommitUntrackedFile',  colors.yellow,  colors.none, styles.none)
Group.new('gitcommitUnmergedFile',   colors.red,     colors.none, styles.bold)
Group.new('gitcommitDiscardedFile',  colors.red,     colors.none, styles.bold)
Group.new('gitcommitSelectedFile',   colors.green,   colors.none, styles.bold)

-- HTML highlighting
Group.new('htmlBold',    colors.yellow,  colors.none, styles.none)
Group.new('htmlItalic',  colors.magenta, colors.none, styles.none)
Group.new('htmlEndTag',  colors.gray5,   colors.none, styles.none)
Group.new('htmlTag',     colors.gray5,   colors.none, styles.none)

-- JavaScript highlighting
Group.new('javaScript',        colors.gray5,  colors.none, styles.none)
Group.new('javaScriptBraces',  colors.gray5,  colors.none, styles.none)
Group.new('javaScriptNumber',  colors.orange, colors.none, styles.none)

-- pangloss/vim-javascript highlighting
Group.new('jsOperator',          colors.blue,    colors.none, styles.none)
Group.new('jsStatement',         colors.magenta, colors.none, styles.none)
Group.new('jsReturn',            colors.magenta, colors.none, styles.none)
Group.new('jsThis',              colors.red,     colors.none, styles.none)
Group.new('jsClassDefinition',   colors.yellow,  colors.none, styles.none)
Group.new('jsFunction',          colors.magenta, colors.none, styles.none)
Group.new('jsFuncName',          colors.blue,    colors.none, styles.none)
Group.new('jsFuncCall',          colors.blue,    colors.none, styles.none)
Group.new('jsClassFuncName',     colors.blue,    colors.none, styles.none)
Group.new('jsClassMethodType',   colors.magenta, colors.none, styles.none)
Group.new('jsRegexpString',      colors.cyan,    colors.none, styles.none)
Group.new('jsGlobalObjects',     colors.yellow,  colors.none, styles.none)
Group.new('jsGlobalNodeObjects', colors.yellow,  colors.none, styles.none)
Group.new('jsExceptions',        colors.yellow,  colors.none, styles.none)
Group.new('jsBuiltins',          colors.yellow,  colors.none, styles.none)

-- Markdown highlighting
Group.new('markdownCode',              colors.green, colors.none,  styles.none)
Group.new('markdownError',             colors.gray5, colors.black, styles.none)
Group.new('markdownCodeBlock',         colors.green, colors.none,  styles.none)
Group.new('markdownHeadingDelimiter',  colors.blue,  colors.none,  styles.none)

-- PHP highlighting
Group.new('phpMemberSelector',  colors.gray5, colors.none, styles.none)
Group.new('phpComparison',      colors.gray5, colors.none, styles.none)
Group.new('phpParent',          colors.gray5, colors.none, styles.none)
Group.new('phpMethodsVar',      colors.cyan,  colors.none, styles.none)

-- Python highlighting
Group.new('pythonOperator',  colors.magenta, colors.none, styles.none)
Group.new('pythonRepeat',    colors.magenta, colors.none, styles.none)
Group.new('pythonInclude',   colors.magenta, colors.none, styles.none)
Group.new('pythonStatement', colors.magenta, colors.none, styles.none)

-- Ruby highlighting
Group.new('rubyAttribute',               colors.blue,   colors.none, styles.none)
Group.new('rubyConstant',                colors.yellow, colors.none, styles.none)
Group.new('rubyInterpolationDelimiter',  colors.brown,  colors.none, styles.none)
Group.new('rubyRegexp',                  colors.cyan,   colors.none, styles.none)
Group.new('rubySymbol',                  colors.green,  colors.none, styles.none)
Group.new('rubyStringDelimiter',         colors.green,  colors.none, styles.none)

-- SASS highlighting
Group.new('sassidChar',     colors.red,     colors.none, styles.none)
Group.new('sassClassChar',  colors.orange,  colors.none, styles.none)
Group.new('sassInclude',    colors.magenta, colors.none, styles.none)
Group.new('sassMixing',     colors.magenta, colors.none, styles.none)
Group.new('sassMixinName',  colors.blue,    colors.none, styles.none)

-- Spelling highlighting
Group.new('SpellBad',     colors.none, colors.none, styles.undercurl)
Group.new('SpellLocal',   colors.none, colors.none, styles.undercurl)
Group.new('SpellCap',     colors.none, colors.none, styles.undercurl)
Group.new('SpellRare',    colors.none, colors.none, styles.undercurl)

-- Java highlighting
Group.new('javaOperator',     colors.blue, colors.none, styles.none)

-- Gitsigns
Group.new('GitSignsAdd',    colors.green, colors.gray1, styles.none)
Group.new('GitSignsChange', colors.blue,  colors.gray1, styles.none)
Group.new('GitSignsDelete', colors.red,   colors.gray1, styles.none)

Group.new('LspDiagnosticsDefaultError',       colors.red,    colors.none, styles.none)
Group.new('LspDiagnosticsDefaultWarning',     colors.yellow, colors.none, styles.none)
Group.new('LspDiagnosticsDefaultInformation', colors.blue,   colors.none, styles.none)
Group.new('LspDiagnosticsDefaultHint',        colors.gray4,  colors.none, styles.none)
