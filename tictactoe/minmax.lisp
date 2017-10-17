(load "board.lisp")

(defvar minmax-hash (make-hash-table :test #'equal))

;; Função de MinMax para o tictactoe que recebe como entrada o estado e o jogador atual
;;     Os valores são:
;;         1: Jogada ótima
;;         -1: Jogada péssima
;;         0: Empate
;;     Parâmetros:
;;         state: o estado do tabuleiro
;;         p1: a marca do jogador atual, ou seja, aquele que fez a jogada
;;     Retorna:
;;         O valor do estado atual segundo o algoritmo minmax
(defun minmax (state p1)
    (let ((play-values) (winner) (opponent))
        (setf play-values '())
        (setf winner (endstatep state))
        (setf opponent (apply 
                (lambda (&rest a) 
                    (let ((op))
                        (dolist (e a) 
                            (if (and (not (numberp e)) (not (eq e p1)))
                                (progn
                                    (setf op e)
                                    (return)
                                )   
                            )
                        )
                        op                
                    )
                )
                state            
            )        
        )
        (if (gethash (list state p1) minmax-hash)
            (gethash (list state p1) minmax-hash)
            (if (eq p1 winner)
                (setf (gethash (list state p1) minmax-hash) 1)
                (if (eq opponent winner)
                    (setf (gethash (list state p1) minmax-hash) -1)
                    (if (some #'numberp state)
                        (progn
                            (dolist (e state)
                                (if (numberp e)
                                    (let ((new-state) (state-value))
                                        (setf new-state (copy-list state))
                                        (setf (nth e new-state) opponent)
                                        (setf state-value (- (minmax new-state opponent)))
                                        (push state-value play-values)
                                    )
                                )
                            )                        
                            (setf (gethash (list state p1) minmax-hash) (apply #'min play-values))                            
                        )                    
                        (setf (gethash (list state p1) minmax-hash) 0)
                    )   
                )
            )
        )
    )
)

;; Função que recebe como parâmetro o estado atual, o jogador que fará a próxima jogada e o seu oponente e usando
;; o algoritmo do minmax retorna o índice da posição ótima a se jogar.
;;     Parâmetros:
;;         state: o estado atual do tabuleiro
;;         p1: a marca do jogador atual, que fará a próxima jogada
;;         p2: a marca do oponente
;;     Retorna:
;;         O índice que o jogador p1 deverá jogar para ter a jogada ótima
(defun minmax-play-ai (state p1 plays)
    (let ((play-values))
        (dolist (e plays)
            (let ((new-state) (state-value))
                (setf new-state (copy-list state))
                (setf (nth e new-state) p1)
                (setf state-value (minmax new-state p1))
                (push state-value play-values)
            )
        )
        (setf play-values (reverse play-values))
        (let ((optimal-play))
            (setf optimal-play (position (apply #'max play-values) play-values))
            (nth optimal-play plays)
        )        
    )
)