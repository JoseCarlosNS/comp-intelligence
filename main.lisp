(load "tictactoe.lisp")
(load "database.lisp")
(load "minmax.lisp")
(load "board.lisp")

(let ((option))
    (setf option -1)
    (loop
        (loop
            (format t "##################################################################
                    Bem vindo ao Jogo da Velha v1.0. Insira sua opção de jogo:
                        0 - PvP (Player vs Player)
                        1 - PvC (Player vs Computer)
                        2 - CvC (Computer vs Computer)
                       -1 - Sair")
            (terpri)
            (setf option (read))
            (if (and (numberp option) (>= option -1) (<= option 2))
                (return option)
                (progn
                    (format t "Insira um número válido!~%")
                    (terpri)
                )
            )
        )
        (if (= option 0)
            (let ((p1) (p2))
                (format t "Insira a marca do Jogador #1:")
                (terpri)
                (setf p1 (read))
                (format t "Insira a marca do Jogador #2:")
                (terpri)
                (setf p2 (read))
                (start-game :p1 p1 :p2 p2 :print-output t)
            )
            (if (= option 1)
                (let ((p1) (p2) (ai))
                    (format t "Insira a marca do Jogador #1:")
                    (terpri)
                    (setf p1 (read))
                    (format t "Insira a marca do Jogador #2:")
                    (terpri)
                    (setf p2 (read))
                    (loop
                        (format t "Insira o tipo de IA:
                                    1 - Aleatória
                                    2 - MinMax")
                        (terpri)
                        (setf ai (read))
                        (if (and (numberp ai) (or (= ai 1) (= ai 2)))
                            (progn
                                (if (= ai 1)
                                    (setf ai #'random-play-ai)
                                    (if (= ai 2)
                                        (setf ai #'minmax-play-ai)
                                        nil
                                    )
                                )
                                (return ai)
                            )
                            (progn
                                (format t "Digite um número válido!")
                                (terpri)
                            )
                        )
                    )
                    (start-game :game-mode 1 :p1 p1 :p2 p2 :ai1-mode ai :print-output t)
                )
                (if (= option 2)
                    (let ((p1) (p2) (ai1) (ai2) (print-output) (games))
                        (format t "Insira a marca da IA #1:")
                        (terpri)
                        (setf p1 (read))
                        (format t "Insira a marca do IA #2:")
                        (terpri)
                        (setf p2 (read))
                        (loop
                            (format t "Insira o tipo de IA #1:
                                        1 - Aleatória
                                        2 - MinMax")
                            (terpri)
                            (setf ai1 (read))
                            (if (and (numberp ai1) (or (= ai1 1) (= ai1 2)))
                                (progn
                                    (if (= ai1 1)
                                        (setf ai1 #'random-play-ai)
                                        (if (= ai1 2)
                                            (setf ai1 #'minmax-play-ai)
                                            nil
                                        )
                                    )
                                    (return ai1)
                                )
                                (progn
                                    (format t "Digite um número válido!")
                                    (terpri)
                                )
                            )
                        )
                        (loop
                            (format t "Insira o tipo de IA #2:
                                        1 - Aleatória
                                        2 - MinMax")
                            (terpri)
                            (setf ai2 (read))
                            (if (and (numberp ai2) (or (= ai2 1) (= ai2 2)))
                                (progn
                                    (if (= ai2 1)
                                        (setf ai2 #'random-play-ai)
                                        (if (= ai2 2)
                                            (setf ai2 #'minmax-play-ai)
                                            nil
                                        )
                                    )
                                    (return ai2)
                                )
                                (progn
                                    (format t "Digite um número válido!")
                                    (terpri)
                                )
                            )
                        )
                        (loop
                            (format t "Deseja ver as jogadas das IAs?
                                        Sim - s
                                        Não - n")
                            (terpri)
                            (setf print-output (read))
                            (if (equal print-output 's)
                                (progn
                                    (setf print-output t)
                                    (return t)
                                )
                                (if (equal print-output 'n)
                                    (progn
                                        (setf print-output nil)
                                        (return t)
                                    )
                                    (progn
                                        (format t "Digite um valor válido!")
                                        (terpri)
                                    )
                                )
                            )
                        )
                        (loop
                            (format t "Quantas vezes deseja que as IAs joguem?")
                            (terpri)
                            (setf games (read))
                            (if (numberp games)
                                (return games)
                                (progn
                                    (format t "Digite um número válido!")
                                    (terpri)
                                )
                            )
                        )
                        (let ((result))
                            (setf result (play-itself games p1 p2 ai1 ai2 print-output))
                            (format t " 
                                        Vitórias da IA #1: ~A
                                        Vitórias da IA #2: ~A
                                        Empates: ~A" (first result) (second result) (third result))
                            (terpri)
                        ) 
                    )                 
                    nil
                )
            )
        )
        (when (= option -1) (return 1))
    )
)