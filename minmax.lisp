(load "board.lisp")

;; Função de MinMax para o tictactoe que recebe como entrada o estado, o jogador atual e o jogador inimigo
;; e retorna o valor da jogada atual
;;     Os valores são:
;;         1: Jogada ótima
;;         -1: Jogada péssima
;;         0: Empate
;;     Parâmetros:
;;         state: o estado do tabuleiro
;;         p1: a marca do jogador atual, ou seja, aquele que fez a jogada
;;         p2: a marca do jogador oponente ao atual.
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
        
        (if (eq p1 winner)
            1
            (if (eq opponent winner)
                -1
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
                        (apply #'min play-values)  
                    )                    
                    0
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