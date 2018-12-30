- ドキュメント(`README.md` と docstring)の充実
- ユーザーが変更する可能性のあるものは `defvar` じゃなくて `defcustom` を使うべき？
- `htprofile-show-log` での filtering の実装
    * 例えば，「elapsed-time が 100ms 以上の関数だけ表示」とか
    * `htprofile-show-statistics` にあっても便利かもしれない
- ずっと有効にしてると重くなったりしない？
- `htprofile-show-*` のバッファで interactive に sort とか filtering の設定ができるように
    * text property の button あたりを使うと良さげ？
- 色の指定は直接やるんじゃなくて，適切な face から inherit する
