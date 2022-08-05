inoremap jj <Esc>


call plug#begin()
Plug 'jpalardy/vim-slime'
Plug 'airblade/vim-gitgutter'
Plug 'morhetz/gruvbox'
call plug#end()


let g:slime_target = "kitty"

set number                     " Show current line number
set relativenumber             " Show relative line numbers

set background=dark
colorscheme gruvbox
