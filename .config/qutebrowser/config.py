c.aliases = {
    'q': 'close',
    'qa': 'quit',
    'w': 'session-save',
    'wq': 'quit --save',
}

# Dont play videos automatically
c.content.autoplay = False

config.set('content.cookies.accept', 'all', 'chrome-devtools://*')
config.set('content.cookies.accept', 'all', 'devtools://*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}', 'https://web.whatsapp.com/')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/71.0', 'https://accounts.google.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99 Safari/537.36', 'https://*.slack.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/71.0', 'https://docs.google.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/71.0', 'https://drive.google.com/*')

# Load images automatically in web pages.
config.set('content.images', True)

# Enable JavaScript.
config.set('content.javascript.enabled', True)

## Downloads ##
c.downloads.open_dispatcher = None
c.downloads.position = 'bottom'

c.editor.encoding = 'utf-8'
c.scrolling.bar = 'when-searching'
c.statusbar.show = 'always'
c.tabs.show = 'always'

c.url.searchengines = {
    'DEFAULT': 'https://duckduckgo.com/?q={}',
    'aw': 'https://wiki.archlinux.org/?search={}',
    'mdn': 'https://developer.mozilla.org/en-US/search?q={}',
}

# Available zoom levels.
c.zoom.levels = ['25%', '33%', '50%', '67%', '75%', '90%', '100%', '110%', '125%', '150%', '175%', '200%', '250%', '300%', '400%', '500%']


### Completion settings ###
c.completion.height = '50%'

c.colors.completion.fg = '#d8d8d8'
c.colors.completion.odd.bg = '#282828'
c.colors.completion.even.bg = '#181818'

c.colors.completion.category.fg = '#f7ca88'
c.colors.completion.category.bg = '#181818'
c.colors.completion.category.border.top = '#181818'
c.colors.completion.category.border.bottom = '#181818'

c.colors.completion.item.selected.fg = '#d8d8d8'
c.colors.completion.item.selected.bg = '#383838'
c.colors.completion.item.selected.border.top = '#383838'
c.colors.completion.item.selected.border.bottom = '#383838'
c.colors.completion.item.selected.match.fg = '#a1b56c'

c.colors.completion.match.fg = '#ab4642'

c.colors.completion.scrollbar.fg = '#d8d8d8'
c.colors.completion.scrollbar.bg = '#181818'
