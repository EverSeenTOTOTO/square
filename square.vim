" Vim syntax file
" Language: Square
" Forked from scheme.vim

if exists('b:current_syntax')
  finish
endif

let s:cpo = &cpo
set cpo&vim

syn spell notoplevel

syn match squareParentheses "[^ '`\t\n()\[\]";]\+"
syn match squareParentheses "[)\]]"

syn match squareIdentifier /[^ '`\t\n()\[\]"|;][^ '`\t\n()\[\]"|;]*/

syn region squareString start=/\(\\\)\@<!'/ skip=/\\[\\']/ end=/'/ contains=@Spell

syn match squareNumber "-\=\<\d\+L\=\>\|0[xX][0-9a-fA-F]\+\>"
syn match squareNumber "-\=\<\d\+\%(_\d\+\)*\>"

syn match squareBoolean /true/
syn match squareBoolean /false/
syn match squareBoolean /nil/

syn match squareComment /;.*$/ contains=@Spell

syn region schemeForm matchgroup=schemeParentheses start="\[" end="\]" contains=ALL
syn cluster schemeSyntaxCluster contains=schemeFunction,schemeKeyword,schemeSyntax

syn keyword squareSyntax let
syn keyword squareSyntax begin
syn keyword squareSyntax if
syn keyword squareSyntax cond
syn keyword squareSyntax while
syn keyword squareSyntax print
syn keyword squareSyntax println
syn keyword squareSyntax callcc
syn keyword squareSyntax vec
syn keyword squareSyntax obj

syn keyword squareFunction +
syn keyword squareFunction -
syn keyword squareFunction *
syn keyword squareFunction /
syn keyword squareFunction +=
syn keyword squareFunction -=
syn keyword squareFunction *=
syn keyword squareFunction /=
syn keyword squareFunction <
syn keyword squareFunction <=
syn keyword squareFunction =
syn keyword squareFunction >
syn keyword squareFunction >=
syn keyword squareFunction %
syn keyword squareFunction %=
syn keyword squareFunction ==
syn keyword squareFunction !=
syn keyword squareFunction >>
syn keyword squareFunction <<
syn keyword squareFunction >>=
syn keyword squareFunction <<=

hi def link squareBoolean Boolean
hi def link squareComment Comment
hi def link squareFunction Function
hi def link squareIdentifier Normal
hi def link squareKeyword Type
hi def link squareNumber Number
hi def link squareParentheses Normal
hi def link squareString String
hi def link squareSyntax Statement

let b:did_square_syntax = 1

unlet b:did_square_syntax
let b:current_syntax = 'square'
let &cpo = s:cpo
unlet s:cpo
