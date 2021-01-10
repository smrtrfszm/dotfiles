call plug#begin()

Plug 'chriskempson/base16-vim'
Plug 'itchyny/lightline.vim'
Plug 'preservim/nerdtree'
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
source $HOME/.config/nvim/plugin-configs/coc.vim
source $HOME/.config/nvim/plugin-configs/base16.vim
source $HOME/.config/nvim/plugin-configs/lightline.vim
source $HOME/.config/nvim/plugin-configs/ale.vim
