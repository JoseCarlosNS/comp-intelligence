(load "board.lisp")

;; "O jogador p marca a casa i do tabuleiro b"
(defun make-play (b p i)
    (if (numberp (nth i b))
        (setf (nth i b) p)))

;; "A IA realiza uma jogada utilizando a função function. Caso esta seja nula fica como uma jogada aleatória
;; Obs.: A função deve receber 3 parâmetros, o tabuleiro, a marca que representa a IA e 
;; a lista de jogadas possíveis"
(defun ai-play (b p plays &optional (function nil))
    (if (null function)
        (setf function #'random-play-ai))    
    (let ((index))
        (setf index (funcall function b p plays))
        (make-play b p index)
        index))

;; "Executa uma jogada aleatória de p no tabuleiro b, em uma das possíveis jogadas plays
;; e retorna o índice da casa que foi jogada"
(defun random-play-ai (b p plays)
    (setf *random-state* (make-random-state t))
    (let ((play-index))
        (setf play-index (nth (random (list-length plays) *random-state*) plays))
        play-index))


;; Função que faz duas IAs lutarem contra elas mesmas e conta o número de vitórias de cada uma e os Empates
;;     Parâmetros
;;         times - número de partidas a serem jogadas
;;         p1 - A marca da IA #1
;;         p2 - A marca da IA #2
;;         ai1-mode - A função que a IA #1 irá usar para decidir sua jogada
;;         ai2-mode - A função que a IA #2 irá usar para decidir sua jogada
;;     Retorna
;;         Uma lista (x y z), onde:
;;             x - Número de vitórias da IA #1
;;             y - Número de vitórias da IA #2
;;             z - Número de empates
(defun play-itself (times p1 p2 ai1-mode ai2-mode print)
    (let ((wins) (losses) (draws))
        (setf wins 0)
        (setf losses 0)
        (setf draws 0)
        (dotimes (i times)
            (let ((winner))
                (setf winner (start-game :game-mode 2 :p1 p1 :p2 p2 :ai1-mode ai1-mode :ai2-mode ai2-mode :print-output print))
                (if (equal winner p1)
                    (setf wins (+ wins 1))
                    (if (equal winner p2)
                        (setf losses (+ losses 1))
                        (setf draws (+ draws 1))))))
        (list wins losses draws)))

;; Inicia um jogo de tic-tac-toe
;;     Parâmetros
;;         board - o tabuleiro do jogo (vazio por padrão)
;;         p1 - A marca do Jogador 1, padrão é X
;;         p2 - A marca do jogador 2, padrão é O
;;         game-mode: os modos de jogo.
;;             Modos:
;;                 0- PvP
;;                 1- PvE
;;                 2- EvE
;;         ai1-mode - A função de jogada para a primeira IA, aleatória por padrão
;;         ai2-mode - A função de jogada para a segunda IA, aleatória por padrão
;;         print-output - Ativa ou desativa o print das saídas, nil por padrão
;;     Retorna
;;         A marca do jogador vencedor
(defun start-game (&optional &key (board nil) (p1 'x) (p2 'o) (game-mode 0) (ai1-mode #'random-play-ai) (ai2-mode #'random-play-ai) (print-output nil))
    (let ((board-temp))
        (if (null board)
            (setf board-temp (make-board))
            (setf board-temp (copy-list board)))
        (let ((turn-player) (winner) (play-index) (plays) (ai))
            (setf turn-player p1)
            (setf plays (empty-places board-temp))
            (setf ai ai1-mode)
            (loop 
                (when (eval print-output)
                    (print-board board-temp))
                (when (or (setf winner (endstatep board-temp)) (= 0 (list-length plays))) 
                    (return 
                        (progn
                            (when (eval print-output)
                                (progn
                                    (print-board board-temp)
                                    (format t "~A É O VENCEDOR!" (if winner winner "NINGUÉM"))
                                    (terpri)))
                            winner)))
                (case game-mode
                    (0
                        (progn
                            (when (eval print-output)
                                (progn
                                    (format t "VEZ DO JOGADOR ~A ~%DIGITE O ÍNDICE DA CASA:" turn-player)
                                    (terpri)))
                            (setf play-index (read))
                            (make-play board-temp turn-player play-index)
                            (setf plays (remove play-index plays))))
                    (1
                        (progn
                            (if (eq turn-player p1)
                                (progn
                                    (when (eval print-output)
                                        (progn
                                            (print "VEZ DO JOGADOR. DIGITE O ÍNDICE DA CASA:")
                                            (terpri)))
                                    (setf play-index (read))
                                    (make-play board-temp turn-player play-index) 
                                    (setf plays (remove play-index plays)))
                                (progn
                                    (setf play-index (ai-play board-temp turn-player plays ai1-mode))
                                    (when (eval print-output)
                                        (progn
                                            (format t "VEZ DA IA. CASA JOGADA: ~A" play-index)
                                            (terpri)))                       
                                    (setf plays (remove play-index plays))))))
                    (2
                        (progn
                            (setf play-index (ai-play board-temp turn-player plays ai))
                            (when (eval print-output)
                                (progn
                                    (format t "VEZ DA IA ~A. CASA JOGADA: ~A" turn-player play-index)
                                    (terpri)))
                            (setf plays (remove play-index plays))
                            (setf ai (if (eq ai ai1-mode) ai2-mode ai1-mode)))))
                (setf turn-player (if (eql turn-player p1) p2 p1))))))
