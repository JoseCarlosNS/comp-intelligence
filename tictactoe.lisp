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
                (rest arguments)
            )
        )
    )
)

;; "Retorna um tabuleiro de tictactoe vazio. 
;; É preenchido com números que indicam o índice da casa correspondente"
(defun make-board()
    '(0 1 2 3 4 5 6 7 8)
)

;; Verifica se algum jogador já alcançou a vitória no tabuleiro bs.
;;         Se sim - Retorna a marca do jogador que venceu
;;         Se não - Retorna nil
(defun end-statep (bs)
    (dolist (x win-cond)
        (let ((index1) (index2) (index3) (y))      
            (setf index1 (first x))
            (setf index2 (second x))
            (setf index3 (third x))      
            (setf y (nth index1 bs))
            (if (and (not (null y)) (meq y (nth index2 bs) (nth index3 bs)))
                (return y)
            )
        )
    )
)

;; "Retorna uma lista com os índices das casas vazias no tabuleiro bs"
(defun empty-places (bs)
    (let ((plays '()))
        (loop for x in bs do
            (if (numberp x)
                (push x plays)
            )
        )
        plays
    )
)

;; "Mostra na saída o tabuleiro"
(defun print-board (board)
    (if (endp board)
        (terpri)
        (progn
            (format t "~A ~A ~A" (first board) (second board) (third board))
            (terpri)
            (print-board (nthcdr 3 board))
        )
    )
)

;; "O jogador p marca a casa i do tabuleiro b"
(defun make-play (b p i)
    (if (numberp (nth i b))
        (setf (nth i b) p)
    )
)

;; "A IA realiza uma jogada utilizando a função function. Caso esta seja nula fica como uma jogada aleatória
;; Obs.: A função deve receber 3 parâmetros, o tabuleiro, a marca que representa a IA e 
;; a lista de jogadas possíveis"
(defun ai-play (b p plays &optional (function nil))
    (if (null function)
        (setf function #'random-play-ai)
    )    
    (let ((index))
        (setf index (funcall function b p plays))
        (make-play b p index)
        index
    )
)

;; "Executa uma jogada aleatória de p no tabuleiro b, em uma das possíveis jogadas plays
;; e retorna o índice da casa que foi jogada"
(defun random-play-ai (b p plays)
    (setf *random-state* (make-random-state t))
    (let ((play-index))
        (setf play-index (nth (random (list-length plays)) plays))
        play-index
    )
)

;; "Inicia um jogo de tic-tac-toe com o tabuleiro b (vazio por padrão)
;;     Modos:
;;         0- PvP
;;         1- PvE
;;         2- EvE"
(defun start-game (&optional &key (board nil) (p1 'x) (p2 'o) (game-mode 0) (ai1-mode nil) (ai2-mode))
    (if (null board)
        (setf board (make-board))
    )
    (let ((turn-player) (winner) (play-index) (plays) (ai))
        (setf turn-player p1)
        (setf plays (empty-places board))
        (setf ai ai1-mode)
        (loop 
            (print-board board)
            (when (or (setf winner (end-statep board)) (= 0 (list-length plays))) 
                (return 
                    (progn
                        (print-board board)
                        (format t "~A É O VENCEDOR!" (if winner winner "NINGUÉM"))
                        (terpri)
                        winner
                    )
                )
            )
            (case game-mode
                (0
                    (progn
                        (format t "VEZ DO JOGADOR ~A ~%DIGITE O ÍNDICE DA CASA:" turn-player)
                        (terpri)
                        (setf play-index (read))
                        (make-play board turn-player play-index)
                        (setf plays (remove play-index plays))
                    )
                )
                (1
                    (progn
                        (if (eq turn-player p1)
                            (progn
                                (print "VEZ DO JOGADOR. DIGITE O ÍNDICE DA CASA:")
                                (terpri)
                                (setf play-index (read))
                                (make-play board turn-player play-index) 
                                (setf plays (remove play-index plays))
                            )
                            (progn
                                (setf play-index (ai-play board turn-player plays ai1-mode))
                                (format t "VEZ DA IA. CASA JOGADA: ~A" play-index)
                                (terpri)                               
                                (setf plays (remove play-index plays))
                            )
                        )
                    )
                )
                (2
                    (progn
                        (setf play-index (ai-play board turn-player plays ai))
                        (format t "VEZ DA IA ~A. CASA JOGADA: ~A" turn-player play-index)
                        (terpri)
                        (setf plays (remove play-index plays))
                        (setf ai (if (eq ai ai1-mode) ai2-mode ai1-mode))
                    )  
                )
            )
            (setf turn-player (if (eql turn-player p1) p2 p1))
        )
    )
)
