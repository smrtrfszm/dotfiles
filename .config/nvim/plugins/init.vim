call plug#begin()

Plug 'chriskempson/base16-vim'
Plug 'itchyny/lightline.vim'
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'sheerun/vim-polyglot'
Plug 'editorconfig/editorconfig-vim'

call plug#end()

" Plugin configs
source $HOME/.config/nvim/plugins/coc.vim
source $HOME/.config/nvim/plugins/base16.vim
source $HOME/.config/nvim/plugins/lightline.vim
source $HOME/.config/nvim/plugins/ale.vim
