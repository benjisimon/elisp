;;; bs-claude.el --- Additions to claude-code-ide  -*- lexical-binding: t; -*-

;;;###autoload
(defun bs-claude-buffer-name (directory)
  "Name the Claude Code buffer for DIRECTORY after its whole path.

claude-code-ide names session buffers after the last path component
alone, so under a ~/<project>/src/<branch> layout the name is really the
branch: ~/dt/i2x/tenten/src/main and ~/dt/i2x/blogbyben/src/main both
want to be *claude-code[main]*.

That collision is destructive, not cosmetic.  Sessions are tracked by
project directory, so starting the second one finds no process of its
own and takes the create path, where the eat backend does
`get-buffer-create' on the colliding name -- handing it the first
project's live buffer -- and `eat-exec's over the running process.  The
first session dies.

The full path is the only thing guaranteed distinct across two
checkouts, so spend the mode-line width and use it: paths under home
lose the ~/ , everything else keeps its leading slash."
  (let ((dir (directory-file-name
              (abbreviate-file-name (expand-file-name directory)))))
    (format "*claude-code[%s]*"
            (if (string-prefix-p "~/" dir) (substring dir 2) dir))))

(provide 'bs-claude)
