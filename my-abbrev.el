;; -*- coding: utf-8; lexical-binding: t; -*-
;; sample use of abbrev

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ;; net abbrev
    ("afaik" "as far as i know")
    ("atm" "at the moment")
    ("dfb" "difference between")
    ("ty" "thank you")
    ("ui" "user interface")
    ("uns" "understand")
    ("ur" "you are")
    ("btw" "by the way")

    ("cnt" "can't")
    ("ddnt" "didn't")
    ("dnt" "don't")

    ;; english word abbrev
    ("ann" "announcement")
    ("arg" "argument")
    ("autom" "automatic")
    ("bc" "because")
    ("bg" "background")
    ("bt" "between")
    ("math" "mathematics")

    ;; computing
    ("ahk" "AutoHotkey")
    ("cfg" "context-free grammar")
    ("cj" "Clojure")
    ("cs" "computer science")

    ;; tech company
    ("Chrome" "browser")
    ("Firefox" "browser")
    ("gm" "Google Map")
    ("macos" "Mac OS")
    ("msw" "Microsoft Windows")

    ;; programing
    ("env" "environment variable")
    ("ip" "IP address")
    ("jvm" "Java Virtual Machine")
    ("rsi" "Repetitive Strain Injury")
    ("subdir" "sub-directory")
    ("webdev" "web development")

    ("db" "database")
    ("gui3" "graphical user interface")
    ("oop3" "object oriented programing")

    ("os3" "operating system")

    ;; programing
    ("eq" "==")
    ("r" "return")
    ("utf8" "-*- coding: utf-8 -*-")

    ;; regex
    ("xaz" "\\([A-Za-z0-9]+\\)")

    ;; unicode
    ("md" "—")

    ("bu" "•")
    ("catface" "😸")
    ("hearts" "♥💕💓💔💖💗💘💝💞💟💙💚💛💜")
    ("ra" "→")

    ;; url
    ("urlemacs" "http://ergoemacs.org/")

    ;;
    ))

;; define abbrev for specific major mode
;; the first part of the name should be the value of the variable major-mode of that mode
;; e.g. for go-mode, name should be go-mode-abbrev-table
(define-abbrev-table 'go-mode-abbrev-table
  '(
    ("g3" "package main
import \"fmt\"
func main() {
        fmt.Println(\"3\")
}")

    ("for" "for i := 0; i < 4; i++ { i }")
    ("if" "if x < 0 { 3 }")
    ("r" "return")
    ("ps" "+")
    ("eq" "==")
    ("pt" "fmt.Println(3)")
    ("fu" "func(x int) int { return 1 }")
    ("v" "var = 3")
    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
