" Plugins will be downloaded under the specified directory.

set nocompatible              " be iMproved, required
filetype off                  " required

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vim-plug Managing Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.vim/plugged')

" Declare the list of plugins.

Plug 'tpope/vim-sensible'
Plug 'junegunn/seoul256.vim'

"{{ The Basics }}
    Plug 'itchyny/lightline.vim'                       " Lightline statusbar
    Plug 'suan/vim-instant-markdown', {'rtp': 'after'} " Markdown Preview
    Plug 'frazrepo/vim-rainbow'

"{{ File management }}
    Plug 'scrooloose/nerdtree'                         " Nerdtree
    Plug 'tiagofumo/vim-nerdtree-syntax-highlight'     " Highlighting Nerdtree
    Plug 'ryanoasis/vim-devicons'                      " Icons for Nerdtree

"{{ Productivity }}
    Plug 'vimwiki/vimwiki'                             " VimWiki 
    Plug 'jreybert/vimagit'                            " Magit-like plugin for vim
    Plug 'neovimhaskell/haskell-vim'                   " Syntax Highlighting and Indentation for Haskell and Cabal
    Plug 'Yggdroot/LeaderF'                            

"{{ Tim Pope Plugins }}
    Plug 'tpope/vim-surround'                          " Change surrounding marks

"{{ Syntax Highlighting and Colors }}
    Plug 'kovetskiy/sxhkd-vim'                         " sxhkd highlighting
    Plug 'ap/vim-css-color'                            " Color previews for CSS

"{{ Junegunn Choi Plugins }}
    Plug 'junegunn/goyo.vim'                           " Distraction-free viewing
    Plug 'junegunn/limelight.vim'                      " Hyperfocus on a range
    Plug 'junegunn/vim-emoji'                          " Vim needs emojis!
    " Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
    Plug 'junegunn/vim-easy-align'


" List ends here. Plugins become visible to Vim after this call.
call plug#end()
