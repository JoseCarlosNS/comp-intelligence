(load "board.lisp")

(defvar win-block-plays '())
(defvar default-plays '())

;; "Cada elemento da lista é uma outra lista que contém os índices das casas que devem estar ocupadas pelo mesmo jogador
;; para que seja realizada a jogada na casa cujo índice é a posição da lista"
(setf win-block-plays '(
; Condições para jogar na posição 0
    ((1 2) (3 6) (4 8))
; Condições para jogar na posição 1
    ((0 2) (4 7))
; Condições para jogar na posição 2
    ((0 1) (4 6) (5 8))
; Condições para jogar na posição 3
    ((0 6) (4 5))
; Condições para jogar na posição 4
    ((0 8) (2 6) (1 7) (3 5))
; Condições para jogar na posição 5
    ((3 4) (2 8))
; Condições para jogar na posição 6
    ((0 3) (4 2) (7 8))
; Condições para jogar na posição 7
    ((6 8) (1 4))
; Condições para jogar na posição 8
    ((6 7) (2 5) (0 4))
    )
)

;; "Lista de jogadas padrão, em ordem de maior chance de vitória, caso o jogador não tenha encontrado nenhuma condicional
;; para vencer/bloquear uma jogada"
(setf default-plays '(4 0 2 6 8 1 3 5 7))
;; "Função para usar o database para realizar uma jogada, recebe como parâmetros:
;;     b - Tabuleiro para jogar
;;     p1 - O jogador que vai realizar a jogada
;;     plays - Lista de jogadas possíveis (casas vazias)
;;     Retorna - o índice que o jogador p1 irá jogar"
(defun database-play (b p1 plays)
    (loop for rule in win-block-plays and index1 from 0 do
        (if (find index1 plays)
            (dolist (condi rule)
                (let ((first-play))
                    (setf first-play (nth (first condi) b))
                    (if (and (eq first-play p1) (meq first-play (nth (second condi) b)))
                        (return-from database-play index1)
                    )
                )
            )
        )
    )
    (loop for rule in win-block-plays and index1 from 0 do
        (if (find index1 plays)
            (dolist (condi rule)
                (let ((first-play))
                    (setf first-play (nth (first condi) b))
                    (if (and (not (numberp first-play)) (and (not (eq first-play p1)) (meq first-play (nth (second condi) b))))
                         (return-from database-play index1)
                    )
                )
            )
        )
    )

    (dolist (index1 default-plays)
        (if (find index1 plays)
             (return-from database-play index1)
        )
    )
)
