(load "minmax.lisp")
(load "board.lisp")

; Função que retorna uma lista onde cada elemento é um possível estado de
; tabuleiro derivada de 'estado'
; 	Argumentos:
; 		estado - o estado atual
; 		base - a variável que armazena a base de dados
; 		p1 - o jogador atual
; 		p2 - o jogador adversário
; 	Retorna:
; 		Uma lista contendo todos os estados possíveis derivados do estado atual
(defun gerar-base-dados (estado base p1 p2)
  (progn
   	(if (and (not (endstatep estado)) (null (empty-places estado)))
      (progn
				(print (minmax-perceptron estado p1 p2))
				(push estado base))
			(when (not (endstatep estado))
				(let ((casas-vazias))
					(setf casas-vazias (empty-places estado))
					(when casas-vazias
	      		(dolist (casa-vazia casas-vazias)
			              (let ((novo-estado))
			                (setf novo-estado (copy-list estado))
			                (setf (nth casa-vazia novo-estado) p1)
			                (gerar-base-dados novo-estado base p2 p1)))))))))

; Função feita para mapear o estilo de tabuleiro do tictactoe para ser lido
; pelo perceptron.
; 	Argumentos:
; 		tab-minmax - o tabuleiro no formato usado no MinMax
; 		p1 - o jogador atual
; 		p2 - o oponente
; 	Retorna:
; 		O tabulado de tictactoe no formato 0 = casa vazia; 1 = jogador atual;
; 		2 = jogador oponente
(defun minmax-perceptron (tab-minmax p1 p2)
  (let ((tab-perceptron))
    (dolist (casa tab-minmax)
            (if (equal casa p1)
              (push 1 tab-perceptron)
              (if (equal casa p2)
                (push -1 tab-perceptron)
                (push 0 tab-perceptron))))
    (reverse tab-perceptron)))

; Função criada para mapear o tabuleiro do formato perceptron ao MinMax
; 	Argumentos:
; 		tab-perceptron - o tabuleiro a ser convertido
; 		p1 - o jogador atual
; 		p2 - o oponente
; 	Retorna:
; 		O tabuleiro no qual cada casa corresponde ao seu índice, p1 = jogador atual
; 		e p2 = oponente.
(defun perceptron-minmax (tab-perceptron p1 p2)
(let ((tab-minmax) (cont 0))
	(dolist (casa tab-perceptron)
					(if (equal casa 1)
						(push p1 tab-minmax)
						(if (equal casa -1)
							(push p2 tab-minmax)
							(push cont tab-minmax)))
         (incf cont))
	(reverse tab-minmax)))

; Cria um novo tabuleiro no formato do perceptron
(defun novo-tabuleiro ()
  (let ((tabuleiro))
    (setf tabuleiro (list 0 0 0 0 0 0 0 0 0 ))
    tabuleiro))
