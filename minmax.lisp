(load "board.lisp")

(defstruct node
    value
    child-nodes
)

(defvar minmax-tree nil)

;; Função de MinMax para o tictactoe que recebe como entrada o estado, o jogador atual e o jogador inimigo
;; e retorna o valor da jogada atual
;;     Os valores são:
;;         1: Jogada ótima
;;         -1: Jogada péssima
;;         0: Empate
;;     Parâmetros:
;;         state: o estado do tabuleiro
;;         p1: a marca do jogador atual, ou seja, aquele que fez a jogada
;;         p2: a marca do jogador oponente
;;         maxp: t para a jogada atual ser de maximizar, nil para minimizar
;;     Retorna:
;;         Uma árvore, contendo o estado atual seguido dos estados seguintes, todos com seus valores segundo o algoritmo
;;         minmax
(defun minmax (state p1 p2 maxp)
    (let ((winner))
        (setf play-values '())
        (setf winner (endstatep state))
        (if (eq p1 winner)
            (if maxp
                (make-node :value (list state 1) :child-nodes nil)
                (make-node :value (list state -1) :child-nodes nil)
            )
            (if (eq p2 winner)
                (if maxp
                    (make-node :value (list state -1) :child-nodes nil)
                    (make-node :value (list state 1) :child-nodes nil)
                )
                (if (some #'numberp state)
                    (progn
                        (let ((states-values) (value))
                            (dolist (e state)
                                (if (numberp e)
                                    (let ((new-state) (new-value))
                                        (setf new-state (copy-list state))
                                        (setf (nth e new-state) p2)
                                        (push (minmax new-state p2 p1 (not maxp)) states-values)
                                    )
                                )
                            )
                            (let ((max-min))
                                (if maxp (setf max-min #'min) (setf max-min #'max))
                                (setf value (apply max-min (mapcar #'second (mapcar #'node-value states-values))))
                            )
                            (make-node :value (list state value) :child-nodes states-values)
                        )
                    )
                    (make-node :value (list state 0) :child-nodes nil)
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
    (let ((play-values) (opponent))
        (dolist (e state)
            (when (and (not (eq e p1)) (not (numberp e)))
                (return (setf opponent e))
            )
        )
        (when (null minmax-tree)
            (setf minmax-tree (minmax (make-board) p1 opponent t))
        )
        (dolist (e minmax-tree)
            (when (equal state (first (node-value e)))
                (return (second (node-value e)))
            )
        )
    )
)
