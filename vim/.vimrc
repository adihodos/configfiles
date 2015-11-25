set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
Plugin 'L9'
" Git plugin not hosted on GitHub
Plugin 'git://git.wincent.com/command-t.git'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Avoid a name conflict with L9
"Plugin 'user/L9', {'name': 'newL9'}

" All of your Plugins must be added before the following line

Plugin 'Molokai'
Plugin 'FX-HLSL'
Plugin 'Cpp11-Syntax-Support'
Plugin 'STL-improved'
Plugin 'jellybeans.vim'
Plugin 'Zenburn'
Plugin 'JSON.vim'
Plugin 'DoxyGen-Syntax'
Plugin 'sexy-railscasts'
Plugin 'railscasts'
Plugin 'Railscasts-Theme-GUIand256color'
Plugin 'Zen-Color-Scheme'
Plugin 'DarkZen-Color-Scheme'

call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


" mine

set background=dark
set t_Co=256
colorscheme molokai
let g:load_doxygen_syntax=1
set ff=unix
set matchpairs+={:},(:),[:],<:>
set guioptions=gR
set showtabline=2
set ignorecase
set smartcase
set incsearch
set hlsearch
filetype plugin indent on
set autowrite
syntax on
set cindent
"set cinoptions=4,g0,t0,c0,(s,(0,w1,(0,W4,(s,m1,:0,b1,l1
"set cinoptions=(0,l1,g0
set cinoptions=(s,(0,W4,l1,g0
"set cino=>2,:0,=0,l1,g0,t0,c0,(0,w1,(s,m1,)100,*100
set autoindent
set autoread
set cmdheight=1
set textwidth=80
set columns=80
set cursorline
set errorbells
set fo=tcrn
set foldcolumn=2
set foldmethod=indent
set foldenable
set foldminlines=50
set foldopen=all

set history=100
set ruler
set scrolloff=2
set shiftwidth=4
set showbreak="+++"
set showmode
set showcmd
set showfulltag
set showmatch
"set smartindent
set smarttab
set expandtab
set splitright
set statusline=%<%f%=\ [%1*%M%*%n%R%H]\ %-19(%3l,%02c%03V%)%O'%02b'
set number
set numberwidth=4
set tabstop=8
set softtabstop=4
set visualbell
set wrap
set wildmenu
set showfulltag
set display+=lastline
set printoptions=syntax:y,wrap:y

" c syntax highlight
let c_c_vim_compatible = 1
let c_gnu = 1
let c_ansi_typedefs = 1
let c_ansi_constants = 1
let c_posix = 1
let c_math = 1
let c_C99 = 1
let c_syntax_for_h = 1  
"let c_comment_strings = 1
"let c_comment_numbers = 1
"let c_comment_types = 1
"let c_comment_date_time = 1

if has("win32")
  set guifont=Consolas:h14
elseif has("unix")
  "set guifont=Liberation\ Mono\ 14
  set guifont=Source\ Code\ Pro\ 14
  "set guifont=Envy\ Code\ R\ 10
  "set guifont=Consolas\ 14
endif


augroup json_autocmd
  autocmd!
  autocmd FileType json set autoindent
  autocmd FileType json set formatoptions=tcq2l
  autocmd FileType json set textwidth=78 shiftwidth=2
  autocmd FileType json set softtabstop=2 tabstop=8
  autocmd FileType json set expandtab
  autocmd FileType json set foldmethod=syntax
augroup END 

autocmd BufNewFile,BufRead *.h set filetype=cpp

" end mine
"
"
