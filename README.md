# html2org - Convert HTML to org mode

*Author:* Qiqi Jin <ginqi7@gmail.com><br>


## Commands

### Below are complete command list

 `html2org-fetch-url`
   Fetch URL.
   Keybinding: M-x html2org-fetch-url
 `html2org`
   Convert HTML buffer/string/file and return as org string.
   Keybinding: M-x html2org
 `html2org-paste-from-clipboard`
   In MacOs: 1. copy contents from browser.
   Keybinding: M-x html2org-paste-from-clipboard

### Customizable Options

### Below are customizable option list

 `html2org-shift-heading-level`
   Shift heading levels by a positive or negative integer.
   default = 0
 `html2org-retrieve-command`
   Command for fetch web url.
   default = "curl"

Installation:
Manual:
Download the source code and put it wherever you like, e.g. into
~/.emacs.d/html2org/
```
git clone git@github.com:ginqi7/html2org.git
```
Add the downloaded directory to the load path:
```
(add-to-list 'load-path "~/.emacs.d/html2org/")
(require 'html2org)
```

Code:


---
Converted from `html2org.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
