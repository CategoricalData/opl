" Vim syntax file
" Language: Operad Programming Language (OPL)
" Maintainer: David Darais (david.darais@gmail.com)
" Latest Revision: Feb 4, 2014

if exists("b:current_syntax")
  finish
endif

syntax clear

highlight def link oplKeyword          Keyword
highlight          oplPunctuation      ctermfg=Gray guifg=Gray
highlight def link oplSingleComment    Comment
highlight def link oplMultiComment     Comment
highlight def link oplBinderName       Identifier
highlight def link oplRenamingName     Identifier
highlight def link oplSpecial          Special

""""" keywords

set iskeyword+=.
syntax keyword oplKeyword as
syntax keyword oplKeyword box
syntax keyword oplKeyword composition
syntax keyword oplKeyword diagram
syntax keyword oplKeyword end
syntax keyword oplKeyword export
syntax keyword oplKeyword external
syntax keyword oplKeyword input
syntax keyword oplKeyword internal
syntax keyword oplKeyword output
syntax keyword oplKeyword w.d.
syntax keyword oplKeyword wiring
syntax keyword oplKeyword with

""""" punctuation

syntax match oplPunctuation ","
syntax match oplPunctuation ";"
syntax match oplPunctuation "\."
syntax match oplPunctuation ":"
syntax match oplPunctuation "<-"

""""" comments

syntax match oplSingleComment "#.*$"
syntax region oplMultiComment start="#|" end="|#" contains=oplMultiComment

""""" binders

syntax match oplBinder           "\(\w\|[_']\)\+\s*:\s*\(\w\|[_']\)\+" contains=oplBinderName
syntax match oplBinderName       contained "\(\w\|[_']\)\+\s*"         nextgroup=oplBinderSeparator
syntax match oplBinderSeparator  contained ":\s*"                      contains=oplPunctuation nextgroup=oplBinderClassifier
syntax match oplBinderClassifier contained "\(\w\|[_']\)\+\s*"

syntax match oplRenaming          "\(\w\|[_']\)\+\s*as\s*\(\w\|[_']\)\+" contains=oplRenamingName,oplRenamingSeparator
syntax match oplRenamingName      contained "\(\w\|[_']\)\+"                       
syntax match oplRenamingSeparator contained "as"                         contains=oplKeyword

""""" regions

"syntax region oplBox       keepend   start="box" end="end" contains=oplKeyword,oplPunctuation,oplBoxCommand
"syntax region oplBoxCommand keepend start="input" end=";"              nextgroup=oplBinderList
"syntax region oplBoxCommand keepend start="output" end=";"             nextgroup=oplBinderList

"syntax region oplWiringDiagram keepend start="wiring diagram" end="end" contains=oplKeyword,oplPunctuation
"syntax match oplWiringDiagramCommand "internal" nextgroup=oplSpecial contained
"syntax match oplWiringDiagramCommand "external\s\+box" nextgroup=oplClassifier contained

"syntax region oplWiringComposition keepend start="wiring composition" end="end" contains=oplKeyword,oplPunctuation
"syntax match oplWiringCompositionCommand "internal w.d." nextgroup=oplBinder contained

"syntax match oplSpecial ".*" contained

