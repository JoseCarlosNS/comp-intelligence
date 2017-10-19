(defvar win-cond (list (list 0 1 2)
                        (list 3 4 5)
                        (list 6 7 8)
                        (list 0 3 6)
                        (list 1 4 7)
                        (list 2 5 8)
                        (list 0 4 8)
                        (list 2 4 6)))

;; Verifica se todos os argumentos são iguais
(defun meq (&rest arguments)
    (or (endp arguments)
        (let ((x))
            (setf x (first arguments))
            (every (lambda (y)
                    (equal x y))
                (rest arguments)))))

;; Verifica se algum jogador já alcançou a vitória no tabuleiro bs.
;;         Se sim - Retorna a marca do jogador que venceu
;;         Se não - Retorna nil
(defun endstatep (bs)
    (dolist (x win-cond)
        (let ((index1) (index2) (index3) (y))      
            (setf index1 (first x))
            (setf index2 (second x))
            (setf index3 (third x))      
            (setf y (nth index1 bs))
            (if (and (not (null y)) (meq y (nth index2 bs) (nth index3 bs)))
                (return y)))))


;; "Retorna um tabuleiro de tictactoe vazio. 
;; É preenchido com números que indicam o índice da casa correspondente"
(defun make-board()
    (let ((board) (index))
        (setf index 8)
        (loop
            (push index board)
            (setf index (- index 1))
            (when (= index -1) (return board)))))

;; "Retorna uma lista com os índices das casas vazias no tabuleiro bs"
(defun empty-places (bs)
    (let ((plays))
        (dolist (x bs)
            (if (numberp x)
                (push x plays)))
        plays))

;; "Mostra na saída o tabuleiro"
(defun print-board (board)
    (if (endp board)
        (terpri)
        (progn
            (format t "~A ~A ~A" (first board) (second board) (third board))
            (terpri)
            (print-board (nthcdr 3 board)))))
